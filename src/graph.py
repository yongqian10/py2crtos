#!/usr/bin/env python3

# functional stack
from functools import reduce
from typing import Any, Callable, List, Dict, Tuple
#from pymonad.maybe import *
from networkx import DiGraph, topological_sort, dfs_preorder_nodes, find_cycle
from decimal import Decimal
import re

from efx_ipmgr.production_api.helper import merge_two_dicts, _log2
from efx_ipmgr.production_api.type_class.applicative import alternate
from efx_ipmgr.production_api.type_class.monad import monad
#from efx_ipmgr.production_api.type_class.eval import Eval
from efx_ipmgr.production_api.type_class.list import uncons
from efx_ipmgr.production_api.type_class.show import show
from efx_ipmgr.production_api.type_class.unlift import Unlift
from efx_ipmgr.production_api.monad.either import Either
from efx_ipmgr.production_api.monad.maybe import Maybe
from efx_ipmgr.production_api.parser.parser import *
from efx_ipmgr.production_api.type.fb_type import Fb, SyntaxError, InterpretError, addNote
from efx_ipmgr.production_api.type.hdl_type import HDLType, HDLBitStringType, getValue
from efx_ipmgr.production_api.type.node_type import NodeType, getKey, isParamBitstring, getName
from efx_ipmgr.production_api.type.expr_type import ArithmeticExpr, ConditionalExpr, FuncExpr, LogicalExpr, RelationalExpr
from efx_ipmgr.production_api.eval.arithmetic_eval import Eval
from efx_ipmgr.production_api.eval.func_eval import Eval
from efx_ipmgr.production_api.helper import convertBinaryToUnsigned, convertHexToUnsigned, convertBinaryToSigned, convertHexToSigned, convertDecToSigned
from efx_ipmgr.production_api.checker.checker import checkAssignment
from efx_ipmgr.production_api.type.graph_type import *
from efx_ipmgr.production_api.type.graph_type import _identity, _bail, _chainWithSkip, _getNonExistingNodeName
from efx_ipmgr.production_api.type.component_type import *

def _linkVectorNode(node: NodeType) -> Graph[Any]:
    return monad(getNode(f'param {getName(node)} vector_right'), lambda a:
                monad(getNode(f'param {getName(node)} vector_left'), lambda b:
                    monad(addEdge((f'param {getName(node)} vector_right', show(node))), lambda c:
                          addEdge((f'param {getName(node)} vector_left', show(node)))))) if isParamBitstring(node) else _identity('')


def addNode(node: Node) -> Graph[List[Node]]:
    #if param type bitstring add edge to its left/right node to avoid circular dep and trigger recursive eval and check
    return chain1(getGraph(), lambda a:
                chain1(getNode(show(node.name)), lambda b:
                    b.match(
                        just=lambda b: _bail(f'{_getNonExistingNodeName(show(b.name))} already existed.'),
                        nothing=lambda :
                            _chainWithSkip(_identity(a.add_node(show(node.name),
                                                                value=Maybe.JUST(node))),
                                _chainWithSkip(_linkVectorNode(node.name),
                                    _chainWithSkip(putGraph(a),
                                        evalNodes(show(node.name))))))) if a.has_node(show(node.name)) else
                _chainWithSkip(_identity(a.add_node(show(node.name), value=Maybe.JUST(node))),
                    _chainWithSkip(_linkVectorNode(node.name),
                        _chainWithSkip(putGraph(a), chain1(evalNode(Maybe.JUST(node)), lambda b: _identity([b]))))))


def detectCircularDependency(g: DiGraph, s: NodeId) -> Maybe[List[Tuple[str]]]:
    try:
        edges=list(find_cycle(g, s, None))
    except:
        return Maybe.NOTHING()
    return Maybe.JUST(edges)

# return source node
def addEdge(edge: Edge) -> Graph[Node]:
    return chain1(getGraph(), lambda a:
                    chain1(cacheMissingNode(edge), lambda b:
                        chain1(getGraph(), lambda c:_chainWithSkip(_identity(c.add_edge(*edge)),
                            detectCircularDependency(c, edge[0]).match(
                                just=lambda d: _bail(InterpretError.CIRCULARDEPENDENCYFOUND(f'Found circular dependencies {d}')),
                                nothing=lambda :  _chainWithSkip(putGraph(c), _identity(b))))) if not a.has_edge(*edge) else _identity(b)))


# if source node not there yet, we create a tmp node to enable edge building
def cacheMissingNode(edge: Edge) -> Graph[Node]:
    return chain1(getGraph(), lambda g:
        chain1(getNode(edge[1]), lambda a: a.match(
            just=lambda a: chain1(getNode(edge[0]), lambda b: b.match(
                just=lambda c: _identity(c),
                nothing=lambda: updateNodeNonVolatileFb(a, [InterpretError.DEPENDENCYNOTFOUND(f'{_getNonExistingNodeName(edge[0])} not existed.')]))) if g.has_node(edge[0]) else
                _chainWithSkip(_identity(g.add_node(edge[0], value=Maybe.NOTHING())),
                        _chainWithSkip(putGraph(g), updateNodeNonVolatileFb(a, [InterpretError.DEPENDENCYNOTFOUND(f'{_getNonExistingNodeName(edge[0])} not existed in {edge[1]}.')]))),
            nothing=lambda: _bail(f'temporary node {edge[1]} should not be a target'))))


def formatExprString(expr: str) -> str:
    #return re.sub(r"[\n\t\s ]*", "", expr)
    if expr == '':
        return ''

    substring = re.finditer(r'[^"]*(?="[^"]*?")', expr)
    if expr[0] == '"':
        isstring = False
    else:
        isstring = True

    substring = list(substring)
    if substring == []:
        return re.sub(r"[\n\t\s ]*", "", expr)

    for match in substring:
        if str(match.group()) != '':
            if isstring == True:
                formatted = re.sub(r"[\n\t\s ]*", "", str(match.group()))
                expr = re.sub(f'{re.escape(str(match.group()))}', formatted, expr)
            isstring = not isstring
    return expr

# return evaluated node
def evalNode(node: Maybe[Node]) -> Graph[Node]:
    def parseExprOrRef(nodeId: str):
        return monad(getState(), lambda initState: uncons(initState.string).match(
            nothing=lambda: bail(SyntaxError.INVALID(f'Expr cannot be empty')),
            just=lambda a: parseChoiceRef(nodeId) if a[0] == '#' else monad(parseCondExpr(nodeId), lambda b:
                monad(getState(), lambda s: uncons(s.string).match(
                    nothing=lambda: identity(b),
                    just=lambda c: bail(SyntaxError.INVALID(f'Invalid syntax near {c[0]}')))))))

    return node.match(
            just=lambda a:
                chain1(resetNodeNonVolatileFb(show(a.name)), lambda a:
                    chain1(getGraph(), lambda b:
                        Unlift(parseExprOrRef(show(a.name)))(ParseState(string=formatExprString(a.val),
                                                                 offset=0,
                                                                 parenthesisCounter=0,
                                                                 withinBracketCounter=0,
                                                                 withinConditionalCounter=0,
                                                                 withinFunctionArg=False,
                                                                 graph=b)).match(
                            right=lambda c:
                                _chainWithSkip(putGraph(c[1].graph), Unlift(Eval(c[0])).match(writer=lambda t: t[0].match(
                                right=lambda r:
                                    chain1(getGraph(), lambda d:
                                        chain1(getNode(show(a.name)), lambda e:
                                            e.match(
                                                just=lambda e:
                                                    chain1(_identity(Node(name=e.name,
                                                                          val=e.val,
                                                                          metadata=e.metadata,
                                                                          solved=r,
                                                                          volatilefb=e.volatilefb,
                                                                          nonvolatilefb=e.nonvolatilefb+t[1])), lambda f:
                                                        chain1(checkAssignment(f), lambda f:
                                                            _chainWithSkip(_identity(d.add_node(show(f.name), value=Maybe.JUST(f))),
                                                                _chainWithSkip(putGraph(d), _identity(f))))),
                                                nothing=lambda :
                                                    _bail(f'evaluated node {show(a.name)} should not be temporary')))),
                                left=lambda l: _bail([addNote(l, f'in {show(a.name)}')]+t[1])))),
                            left=lambda e: _bail(addNote(e, f'in {show(a.name)}'))))),
            nothing=lambda: _bail(f'temporary node should not be evaluated'))

def evalNodes(node: NodeId) -> Graph[List[Node]]:
# TODO handle user / generate mode during ip manager
# TODO remove boilerplate
    return chain1(getNode(node), lambda n:
        n.match(
            just=lambda a:
                chain1(getGraph(), lambda b:
                    chain1(_identity(list(dfs_preorder_nodes(b, node, 1))), lambda c:
                              reduce(lambda x,y:
                                     chain1(x, lambda i:
                                        _chainWithSkip(removeEdge((node, y)),
                                               chain1(evalNodes(y), lambda j: _identity(i+j)))),
                                     c[1:], chain1(evalNode(n), lambda d: _identity([d]))))

                    if list(b.neighbors(node)) != [] else chain1(evalNode(n), lambda d: _identity([d]))),
            nothing=lambda :
                chain1(getGraph(), lambda b:
                    chain1(_identity(list(dfs_preorder_nodes(b, node, 1))), lambda c:
                              reduce(lambda x,y:
                                     chain1(x, lambda i:
                                        _chainWithSkip(removeEdge((node, y)),
                                               chain1(evalNodes(y), lambda j: _identity(i+j)))),
                                     c[1:], evalNodes(c[1])))

                    if list(b.neighbors(node)) != [] else chain1(evalNode(n), lambda d: _identity([d])))))


def removeEdge(edge: Edge) -> Graph[DiGraph]:
    return chain1(getGraph(), lambda a:
                _chainWithSkip(_identity(a.remove_edge(*edge)),
                    putGraph(a)) if a.has_edge(*edge) else _bail('edge is not existed'))


def removeNode(node: NodeId) -> Graph[List[Node]]:
    def modGraphNodeWithEffect(node: NodeId, graph: DiGraph) -> DiGraph:
        graph.nodes[node]['value'] = Maybe.NOTHING()
        return graph

    return chain1(getGraph(), lambda g: chain1(getNode(node), lambda a: a.match(
        just=lambda b: _chainWithSkip(putGraph(modGraphNodeWithEffect(node, g)), evalNodes(node)),
        nothing=lambda : evalNodes(node)) if len(list(dfs_preorder_nodes(g, node, 1))[1:]) > 0 else _chainWithSkip(_identity(g.remove_node(node)),
            _chainWithSkip(putGraph(g), _identity([])))))


def removeNode2(node: NodeId) -> Graph:
    #return chain1(getGraph(), lambda g: _chainWithSkip(_identity(g.remove_node(node)),
    #                                                   _chainWithSkip(putGraph(g), _identity([]))))
    return chain1(getGraph(), lambda g: chain1(getNode(node), lambda a: a.match(
        just=lambda b: _chainWithSkip(_identity(g.remove_node(node)), putGraph(g)),
        nothing=lambda : _chainWithSkip(_identity(g.remove_node(node)), putGraph(g)))))


# we not allow whole replacement for Node as NodeId actually depend on Node name
def putNodeType(node: NodeId, name: NodeType, value:str) -> Graph[List[Node]]:
    def modGraphNodeWithEffect(oldNode:Node, node: Node, graph: DiGraph) -> DiGraph:
        graph.nodes[show(oldNode.name)]['value'] = Maybe.JUST(node)
        return graph

    return chain1(getNode(node), lambda a: a.match(
        just=lambda b: chain1(_identity(Node(name=name,
                                             metadata=b.metadata,
                                             val=value,
                                             solved=HDLType.UNRESOLVED(),
                                             volatilefb=[],
                                             nonvolatilefb=[])), lambda c:
                            _chainWithSkip(_linkVectorNode(c.name),
                                chain1(getGraph(), lambda d:
                                    _chainWithSkip(putGraph(modGraphNodeWithEffect(b, c, d)),
                                                   evalNodes(node))))) if show(name) == show(b.name) else _bail('cant change the name, delete the node if u want to do so'),
        nothing=lambda:_bail(f'cant modify a temporary node {node} value')))


def putNodeMetadata(node: NodeId, metadata: str) -> Graph[Node]:
    def modGraphNodeWithEffect(node: Node, graph: DiGraph) -> DiGraph:
        graph.nodes[show(node.name)]['value'] = Maybe.JUST(node)
        return graph

    return chain1(getNode(node), lambda a: a.match(
        just=lambda b: chain1(_identity(Node(name=b.name,
                                             metadata=metadata,
                                             solved=b.solved,
                                             volatilefb=b.volatilefb,
                                             nonvolatilefb=b.nonvolatilefb)), lambda c:
                             chain1(getGraph(), lambda d:
                                    _chainWithSkip(putGraph(modGraphNodeWithEffect(c,d)), _identity(c)))),
        nothing=lambda:_bail(f'cant modify a temporary node {node} value')))

def putNodeValue(node: NodeId, val: str) -> Graph[List[Node]]:
    def modGraphNodeWithEffect(node: Node, graph: DiGraph) -> DiGraph:
        graph.nodes[show(node.name)]['value'] = Maybe.JUST(node)
        return graph

    return chain1(getNode(node), lambda a: a.match(
        just=lambda b: chain1(_identity(Node(name=b.name,
                                             metadata=b.metadata,
                                             val=val,
                                             solved=HDLType.UNRESOLVED(),
                                             volatilefb=[],
                                             nonvolatilefb=[])), lambda c:
                              chain1(getGraph(), lambda d:
                                     _chainWithSkip(putGraph(modGraphNodeWithEffect(c, d)),
                                             evalNodes(node)))),
        nothing=lambda:_bail(f'cant modify a temporary node {node} value')))

################################################################################################
# FIXME should move this snippet somewhere else but face some circular dep prob
# expr parser
# conditional parser

def parseCond() -> Parser[Callable[[Any], Parser[Callable[[Any, Any], ConditionalExpr]]]]:
    return infixOp('?', lambda a: parseStatement(a))

def parseStatement(cond) -> Parser[Callable[[Any, Any], ConditionalExpr]]:
    return infixOp(':', lambda a,b: ConditionalExpr.COND(cond, a, b))

def parseCondExpr(curr: NodeId) -> Parser[ConditionalExpr]:
    return chainl2(parseInfixLogicalExpr(curr), parseCond())

# logical parser
def parseInfixLogical() -> Parser[Callable[[Any, Any], LogicalExpr]]:
    return alternate(infixOp("&&", lambda a,b: LogicalExpr.LOGICALAND(a, b)),
                     infixOp("||", lambda a,b: LogicalExpr.LOGICALOR(a, b)))

def parsePrefixLogical() -> Parser[Callable[[Any], LogicalExpr]]:
    return infixOp("!", lambda a: LogicalExpr.NEGATE(a))

def parseInfixLogicalExpr(curr: NodeId) -> Parser[LogicalExpr]:
    return chainl1(parsePrefixLogicalExpr(curr), parseInfixLogical())

def parsePrefixLogicalExpr(curr: NodeId) -> Parser[LogicalExpr]:
    return chainl(parseRelationalExpr(curr), parsePrefixLogical())

def parseNegateLeftover(curr: NodeId) -> Parser[Any]:
    return monad(parseSaidChar('!'), lambda a: monad(getState(), lambda b:
                                                     bail(SyntaxError.NEGATEERROR(f'Syntax error near !{b.string} parenthesis () needed'))))

# relational parser
def parseEquality() -> Parser[Callable[[Any, Any], RelationalExpr]]:
    return alternate(infixOp("==", lambda a,b: RelationalExpr.EQUAL(a, b)),
                alternate(infixOp("!=", lambda a,b: RelationalExpr.NOTEQUAL(a, b)),
                    alternate(infixOp(">=", lambda a,b: RelationalExpr.GREATERTHANOREQUAL(a, b)),
                        alternate(infixOp("<=", lambda a,b: RelationalExpr.LOWERTHANOREQUAL(a, b)),
                            alternate(infixOp("<", lambda a,b: RelationalExpr.LOWERTHAN(a, b)),
                                infixOp(">", lambda a,b: RelationalExpr.GREATERTHAN(a, b)))))))


def parseRelationalExpr(curr: NodeId) -> Parser[RelationalExpr]:
    return chainl1(parseArithExpr(curr), parseEquality())


# arithmetic parser
def parseAdd() -> Parser[Callable[[Any, Any], ArithmeticExpr]]:
    return alternate(infixOp("+", lambda a,b: ArithmeticExpr.ADD(a, b)),
                      infixOp("-", lambda a,b: ArithmeticExpr.SUBTRACT(a, b)))

def parseMul() -> Parser[Callable[[Any, Any], ArithmeticExpr]]:
    return alternate(infixOp("*", lambda a,b: ArithmeticExpr.MULTIPLY(a, b)),
                      infixOp("/", lambda a,b: ArithmeticExpr.DIVIDE(a, b)))

def parsePow() -> Parser[Callable[[Any, Any], ArithmeticExpr]]:
    return  infixOp("**", lambda a,b: ArithmeticExpr.POWER(a, b))

def parseArithExpr(curr: NodeId) -> Parser[ArithmeticExpr]:
    return chainl1(chainl1(chainl1(parseExpr(curr), parsePow()), parseMul()), parseAdd())

def parseArray(curr) -> Parser[ArithmeticExpr]:
    return ignoreRight(monad(parseSaidChar('['), lambda a:
                 alternate(monad(parseSaidChar(']'), lambda a: bail(SyntaxError.INVALID('List cannot be empty'))),
                           monad(getState(), lambda initState:
                                 chainWithSkip(putState(ParseState(string=initState.string,
                                                                   offset=initState.offset,
                                                                   parenthesisCounter=initState.parenthesisCounter,
                                                                   withinBracketCounter=initState.withinBracketCounter+1,
                                                                   withinConditionalCounter=initState.withinConditionalCounter,
                                                                   withinFunctionArg=initState.withinFunctionArg,
                                                                   graph=initState.graph)),
                                         monad(separatedBy(monad(parseCondExpr(curr), lambda b: identity([b])), parseSaidChar(',')), lambda c:
                                               identity(ArithmeticExpr.LIT(HDLType.HDLLISTTYPE(c)))))))),
                                              # monad(parseSaidChar(']'), lambda d:
                                                chainWithSkip(satisfy3(lambda a: a == ']', f'expecting closing bracket, ]'),
                                                     monad(getState(), lambda initState:
                                                           putState(ParseState(string=initState.string,
                                                                               offset=initState.offset,
                                                                               parenthesisCounter=initState.parenthesisCounter,
                                                                               withinBracketCounter=initState.withinBracketCounter-1,
                                                                               withinConditionalCounter=initState.withinConditionalCounter,
                                                                               withinFunctionArg=initState.withinFunctionArg,
                                                                               graph=initState.graph)) if initState.withinBracketCounter > 0 else \
                                                                         bail(SyntaxError.INVALID('array closing bracket, ] not match')))))

def parseExpr(curr: NodeId) -> Parser[Any]:
    return monad(getState(), lambda initState: uncons(initState.string).match(
        nothing=lambda: bail(SyntaxError.INVALID(f'Expr not allowed to be empty')),
        just=lambda a:  monad(parseEfxVar(curr), lambda b: alternate(monad(parseLeftover(), lambda c: identity(b)), identity(b)))                 if a[0] == '@' else
                        monad(parseValueRef(curr), lambda b: alternate(monad(parseLeftover(), lambda c: identity(b)), identity(b)))               if a[0] == '$' else
                        monad(parseParens(parseCondExpr(curr)), lambda b: alternate(monad(parseLeftover(), lambda c: identity(b)), identity(b)))  if a[0] == '(' else
                        monad(parseNegateLeftover(curr), lambda b: alternate(monad(parseLeftover(), lambda c: identity(b)), identity(b)))         if a[0] == '!' else
                        monad(parseArray(curr), lambda b: alternate(monad(parseLeftover(), lambda c: identity(b)), identity(b)))                  if a[0] == '[' else
                        monad(alternate(parseLit(curr, a[0]),
                            alternate(parseFunc(curr),
                                      bail(SyntaxError.INVALID(f'Invalid syntax near {a[0]}')))), lambda b: alternate(monad(parseLeftover(), lambda c: identity(b)), identity(b)))))

def parseLeftover() -> Parser[str]:
    def checkFinal(initState, parenthesisCount, bracketCount):
                        # primary infix operator
        return False if ((initState.string[0] in ['+', '-', '*', '/', '>', '<', '?']) or \
                        (initState.string[:2] in ['!=', '==', '>=', '<=', '||', '&&']) or \
                        # postfix
                        # closing braces, } leftover will immediately decide to error
                        (initState.string[0] == "]" and checkClosed(initState, parenthesisCount, bracketCount)) or \
                        (initState.string[0] == ")" and checkClosed(initState, parenthesisCount, bracketCount)) or \
                        # secondary infix
                        (initState.string[0] == "," and (initState.withinBracketCounter > 0 or initState.withinFunctionArg)) or \
                        (initState.string[0] == ":" and initState.withinConditionalCounter > 0)) else True

    def checkClosed(initState, parenthesisCount, bracketCount):
        r = len(initState.string)-1
        l = 0

        while r >= l:
            if initState.string[l] == ')':
                parenthesisCount += 1
                if parenthesisCount > initState.parenthesisCounter:
                    return False
                l+=1
                continue

            if initState.string[l] == ']':
                bracketCount += 1
                if bracketCount > initState.withinBracketCounter:
                    return False
                l+=1
                continue
            break

        # if expected char no longer exist, we check next char
        new_initState = ParseState(string=initState.string[parenthesisCount+bracketCount:],
                                                offset=initState.offset,
                                                parenthesisCounter=initState.parenthesisCounter-parenthesisCount,
                                                withinBracketCounter=initState.withinBracketCounter-bracketCount,
                                                withinConditionalCounter=initState.withinConditionalCounter,
                                                withinFunctionArg=initState.withinFunctionArg,
                                                graph=initState.graph)

        return not checkFinal(new_initState, parenthesisCount, bracketCount) if initState.string[parenthesisCount+bracketCount:] != '' else True

    return monad(getState(), lambda initState: uncons(initState.string).match(
        just=lambda a: bail(SyntaxError.INVALID(f'Invalid syntax near {a[0]}')) if checkFinal(initState, 0, 0) else identity(''),
        nothing=lambda: bail(SyntaxError.EOL('eol'))))


def parseExceptReservedKeyword() -> Parser[str]:
    return parseAnyExceptSome(['(', '!', ',', '.', ')', '*' ,'/', '^', '&', '|', '$', '@', '%', '"', "'", '>', '<', '?', '{', '}', '[', ']'])

# TODO handle empty function, test()
def parseFunc(curr) -> Parser[FuncExpr]:
    def _parseArgs() -> Parser[List[str]]:
        return ignoreRight(monad(getState(), lambda initState:
                    chainWithSkip(putState(ParseState(string=initState.string,
                                              offset=initState.offset,
                                              parenthesisCounter=initState.parenthesisCounter,
                                              withinBracketCounter=initState.withinBracketCounter,
                                              withinConditionalCounter=initState.withinConditionalCounter,
                                              withinFunctionArg=True,
                                              graph=initState.graph)),
                          separatedBy(monad(parseCondExpr(curr), lambda b: identity([b])), parseSaidChar(',')))),
            monad(getState(), lambda initState:
                    putState(ParseState(string=initState.string,
                                              offset=initState.offset,
                                              parenthesisCounter=initState.parenthesisCounter,
                                              withinBracketCounter=initState.withinBracketCounter,
                                              withinConditionalCounter=initState.withinConditionalCounter,
                                              withinFunctionArg=False,
                                              graph=initState.graph))))

    return monad(parseExceptReservedKeyword(), lambda a:
                 monad(parseParens(_parseArgs()), lambda b:
                       identity(FuncExpr.FUNC(a, b))))

def parseLit(curr: NodeId, lookahead: str) -> Parser[ArithmeticExpr]:
        return alternate(monad(parseString2(), lambda a:
                              identity(ArithmeticExpr.LIT(HDLType.HDLSTRINGTYPE(a)))),
               alternate(monad(parseBool(), lambda a:
                              identity(ArithmeticExpr.LIT(HDLType.HDLBITTYPE(int(a))))),
               #alternate(monad(parseBit(), lambda a:
               #              identity(ArithmeticExpr.LIT(a))),
               alternate(monad(parseBitstring(), lambda a:
                             identity(ArithmeticExpr.LIT(a))),
               alternate(monad(parseReal(), lambda a:
                             identity(ArithmeticExpr.LIT(a))),
               monad(parseInt32(), lambda a:
                             identity(ArithmeticExpr.LIT(HDLType.HDLINTTYPE(a))))))))


def parseValueRef(curr: NodeId) -> Parser[ArithmeticExpr]:
    return monad(alternate(chainWithSkip(parseSaidChar('$'), surroundedBy('{', '}')),
                 bail(SyntaxError.INVALID(f'Invalid syntax near $, expecting ${{ref_param}}'))), lambda a:
                            monad(getState(), lambda s:
                              Unlift(addEdge((f'param {a} value', curr)))(s.graph).match(
                                  left=lambda e: bail(e),
                                  right=lambda r: monad(putState(ParseState(string=s.string,
                                                                            offset=s.offset,
                                                                            parenthesisCounter=s.parenthesisCounter,
                                                                            withinBracketCounter=s.withinBracketCounter,
                                                                            withinConditionalCounter=s.withinConditionalCounter,
                                                                            withinFunctionArg=s.withinFunctionArg,
                                                                            #statePos=s.statePos,
                                                                            graph=r[1])), lambda b:
                                      r[1].nodes[f'param {a} value']['value'].match(just=lambda a: identity(ArithmeticExpr.LIT(a.solved)),
                                                                                    nothing=lambda: identity(ArithmeticExpr.LIT(HDLType.UNRESOLVED())))))))
def parseEfxVar(curr: NodeId) -> Parser[ArithmeticExpr]:
    return monad(alternate(chainWithSkip(parseSaidChar('@'), surroundedBy('{', '}')),
                 bail(SyntaxError.INVALID(f'Invalid syntax near @, expecting @{{ref_efx_value}}'))), lambda a:
                            identity(ArithmeticExpr.LIT(HDLType.EFXDEVICE(a)))  if a == 'EFXDEVICE' else
                            identity(ArithmeticExpr.LIT(HDLType.EFXFAMILY(a)))  if a == 'EFXFAMILY' else
                            bail(SyntaxError.INVALID(f'Invalid variable keyword: {a}')))


def parseChoiceRef(curr: NodeId) -> Parser[ArithmeticExpr]: # HDLType.HDLDICTTYPE[Dict[str, HDLType]]
    #  TODO convert choices(multiple node) to list of choice(only one node) in graph
    return monad(alternate(chainWithSkip(parseSaidChar('#'), surroundedBy('{', '}')),
                 bail(SyntaxError.INVALID(f'Invalid syntax near #, expecting #{{choice_ref}}'))), lambda a:
                            monad(getState(), lambda s:
                                Unlift(monad(getChoiceRefsList2(a), lambda b: b.match(
                                    just=lambda c: monad(reduce(lambda x, y: monad(x, lambda i: addEdge((show(y.name), curr))), c, _identity('')), lambda d: _identity(a)),
                                    #_identity(merge_two_dicts(i, {getKey(j.name): j.solved})))), c, _identity({})),
                                    nothing=lambda : monad(getNode(curr), lambda k: k.match(
                                        nothing=lambda : _bail(f'{curr} cant be a temporary node'),
                                        # FIXME: choiceref not existed msg dun show up for unknown reason
                                        just=lambda d: monad(updateNodeNonVolatileFb(d, [InterpretError.DEPENDENCYNOTFOUND(f'Choiceref {a} not existed.')]), lambda e: _identity(a)))))))(s.graph).match(
                                            left=lambda e: bail(e),
                                            right=lambda r: monad(putState(ParseState(string=s.string,
                                                                                      offset=s.offset,
                                                                                      parenthesisCounter=s.parenthesisCounter,
                                                                                      withinBracketCounter=s.withinBracketCounter,
                                                                                      withinConditionalCounter=s.withinConditionalCounter,
                                                                                      withinFunctionArg=s.withinFunctionArg,
                                                                                      #statePos=s.statePos,
                                                                                      graph=r[1])), lambda b: identity(ArithmeticExpr.LIT(HDLType.HDLSTRINGTYPE(r[0])))))))

def parseUnsignedBitstring() -> Parser[HDLType]:
    return monad(parseDecimal(), lambda a:
                 monad(parseSaidChar("'"), lambda b:
                    alternate(monad(parseBitstringHex(), lambda b:
                        ((identity(HDLType.HDLBITSTRINGTYPE(HDLBitStringType._UNSIGNED(convertHexToUnsigned(b)), a)) if a > 1 else identity(HDLType.HDLBITTYPE(int(b)))) if a >= _log2(convertHexToUnsigned(b))+1 else bail(SyntaxError.BITSTRINGLENGTHINVALID(f'Bitstring with length {a} cant hold {b} with length {_log2(convertHexToUnsigned(b))+1}'))) if a > 0 else bail(SyntaxError.BITSTRINGLENGTHINVALID('Bitstring length cant be 0'))),
                    alternate(monad(parseBitstringBinary(), lambda b:
                        ((identity(HDLType.HDLBITSTRINGTYPE(HDLBitStringType._UNSIGNED(convertBinaryToUnsigned(b)), a)) if a > 1 else identity(HDLType.HDLBITTYPE(int(b)))) if a >= _log2(convertBinaryToUnsigned(b))+1 else bail(SyntaxError.BITSTRINGLENGTHINVALID(f'Bitstring with length {a} cant hold {b} with length {_log2(convertBinaryToUnsigned(b))+1}'))) if a > 0 else bail(SyntaxError.BITSTRINGLENGTHINVALID('Bitstring length cant be 0'))),
                    monad(parseBitstringDecimal(), lambda b:
                        ((identity(HDLType.HDLBITSTRINGTYPE(HDLBitStringType._UNSIGNED(b), a)) if a > 1 else identity(HDLType.HDLBITTYPE(int(b)))) if a >= _log2(b)+1 else bail(SyntaxError.BITSTRINGLENGTHINVALID(f'Bitstring with length {a} cant hold {b} with length {_log2(b)+1}'))) if a > 0 else bail(SyntaxError.BITSTRINGLENGTHINVALID('Bitstring length cant be 0')))))))



def parseSignedBitstring() -> Parser[HDLType]:
    return monad(parseDecimal(), lambda a:
                 monad(parseSaidChar("'"), lambda b:
                    monad(parseSaidChar('s'), lambda b:
                       alternate(monad(parseBitstringHex(), lambda b:
                           (identity(HDLType.HDLBITSTRINGTYPE(HDLBitStringType._SIGNED(convertHexToSigned(b, a)), a)) if a > 1 else bail(SyntaxError.BITSTRINGLENGTHINVALID('Signed length has to be larger than 1'))) if a >= _log2(convertHexToUnsigned(b))+1 else bail(SyntaxError.BITSTRINGLENGTHINVALID(f'Bitstring with length {a} cant hold {b} with length {_log2(convertHexToUnsigned(b))+1}'))),
                       alternate(monad(parseBitstringBinary(), lambda b:
                           (identity(HDLType.HDLBITSTRINGTYPE(HDLBitStringType._SIGNED(convertBinaryToSigned(b, a)), a)) if a > 1 else bail(SyntaxError.BITSTRINGLENGTHINVALID('Signed length has to be larger in 1'))) if a >= _log2(convertBinaryToUnsigned(b))+1 else bail(SyntaxError.BITSTRINGLENGTHINVALID(f'Bitstring with length {a} cant hold {b} with length {_log2(convertBinaryToUnsigned(b))+1}'))),
                       monad(parseBitstringDecimal(), lambda b:
                            #  in convertDecToSigned, int(a,b), a has to be a str. But parseBitStringDecimal return int
                           (identity(HDLType.HDLBITSTRINGTYPE(HDLBitStringType._SIGNED(convertDecToSigned(str(b), a)), a)) if a > 1 else bail(SyntaxError.BITSTRINGLENGTHINVALID('Signed length has to be larger in 1'))) if a >= _log2(b)+1 else bail(SyntaxError.BITSTRINGLENGTHINVALID(f'Bitstring with length {a} cant hold {b} with length {_log2(b)+1}'))))))))

#def parseBit() -> Parser[HDLType]:
#    return monad(parseSaidChar("1"), lambda a:
#                 monad(parseSaidChar("'"), lambda b:
#                       monad(parseSaidChar('b'), lambda c:
#                            monad(alternate(parseSaidChar('0'), alternate(parseSaidChar('1'),
#                                                                          bail(SyntaxError.BITVALUEINVALID(f'unexpected value {a} for bit, bit only accept 0 or 1')))), lambda d:
#                                  identity(HDLType.HDLBITTYPE(int(d)))))))

def parseReal() -> Parser[HDLType]:
    return monad(parseInt(), lambda a:
                 monad(parseSaidChar('.'), lambda b:
                       monad(parseDecimal(), lambda c: identity(HDLType.HDLREALTYPE(Decimal(str(a)+'.'+str(c)))))))

#def parseSignedBitstring() -> Parser[HDLType]:
#    return monad(parseDecimal(), lambda a:
#                 monad(parseSaidChar('s'), lambda b:
#                    alternate(monad(parseBitstringHex(), lambda b:
#                        identity(HDLType.HDLBITSTRINGTYPE(HDLBitStringType._SIGNED(convertHexToSigned(b, a)), a))),
#                        monad(parseBitstringBinary(), lambda c:
#                        identity(HDLType.HDLBITSTRINGTYPE(HDLBitStringType._SIGNED(convertBinaryToSigned(c, a)), a))))))
#
#def parseVector() -> Parser[HDLType]:
#    return alternate(monad(parseBitstringHex(), lambda b:
#                identity(HDLType.HDLBITSTRINGTYPE(HDLBitStringType._VECTOR(convertHexToUnsigned(b[0])), b[1]))),
#                monad(parseBitstringBinary(), lambda c:
#                identity(HDLType.HDLBITSTRINGTYPE(HDLBitStringType._VECTOR(convertBinaryToUnsigned(c[0])), c[1]))))

def parseBitstring() -> Parser[HDLType]:
    return alternate(parseUnsignedBitstring(), parseSignedBitstring())

# input -> '0b00000010`
#       -> '0x02'
#       -> '2'


@Unlift.instance(Graph)
def _unlift_Graph(instance: Graph):
   return instance.match(
    graph= lambda a: a)
