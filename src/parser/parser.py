#!/usr/bin/env python3
import re

# functional stack
from toolz.functoolz import memoize
from dataclasses import dataclass
from typing import Any, Callable, List, TypeVar
from typing import Generic, Tuple
from networkx.classes.function import chain
from toolz.functoolz import curry
from adt.decorator import adt
from adt.case import Case
#from pymonad.maybe import *
from networkx import DiGraph

from src.monad.either import Either
from src.typeClass.functor import fmap
from src.typeClass.applicative import alternate, applicative, ignoreRight
from src.typeClass.monad import _return, monad
from src.typeClass.list import uncons, cons
from src.type.fbTy import SyntaxError
# TODO display display line, char seq num during syntax error

T=TypeVar('T')

@dataclass
class ParseState:
    string: str
    offset: int
    parenthesisCounter: int
    withinBracketCounter: int
    withinConditionalCounter: int
    withinFunctionArg: bool
    graph: DiGraph # ast dependency graph

    #statePos: SourcePos
    #pragmaReg: List[Tuple]=dataclasses.field(default_factory=list)
    #line: Line=0
    #Indents: Column=0

def parseState(_input: str) -> ParseState:
    return ParseState(string=_input,
                      offset=0,
                      parenthesisCounter=0,
                      withinBracketCounter=0,
                      withinConditionalCounter=0,
                      withinFunctionArg=False,
                      graph=DiGraph())

@adt
class Parser(Generic[T]):
    PARSER : Case[Callable[[ParseState], Either[SyntaxError, Tuple[T, ParseState]]]]

@fmap.instance(Parser)
def _fmap_Parser(instance: Parser, func: Callable) -> Parser: # the func is just something simple
    return instance.match(
        parser=lambda f: Parser.PARSER(lambda s: f(s).match(
            left=lambda error: Either.LEFT(error),
            right=lambda a: Either.RIGHT((curry(func)(a[0]), a[1])))))


@applicative.instance(Parser)
def _applicative_Parser(instance1: Parser, instance2: Parser) -> Parser:
    return instance1.match(
        parser=lambda f1:
            instance2.match(
                parser=lambda f2: Parser.PARSER(lambda s: f1(s).match(
                    left=lambda a: Either.LEFT(a),
                    right=lambda a: f2(a[1]).match(
                        left=lambda b: Either.LEFT(b),
                        right=lambda b: Either.RIGHT((a[0](b[0]), b[1])))

                #parser=lambda f2: Parse.PARSER(lambda s: (func(f2(f1(s)[1])[0]), f2(f1(s)[1])[1]))
                #parser=lambda f2: Parse.PARSER(fmap(a, CurryRegisterApp(b=b, func=func))),
        ))))

@monad.instance(Parser)
def _monad_Parser(instance: Parser, func: Callable[[Any], Parser]):
    return chain1(instance, func)

#def monad1(instance: Parser, func: Callable[[Any], Parser]) -> Parser:
#    return Parser.PARSER(lambda s: instance.match(
#            parser=lambda f: f(s).match(
#                left=lambda errorMessage: Either.LEFT(errorMessage),
#                right=lambda t: monad(t, lambda a:
#                                      func(a[0])(a[1])))))

@_return.instance(Parser)
def __return_Parser(instance: Any):
    return identity(instance)


@alternate.instance(Parser)
def _alternate_Parser(instance1: Parser, instance2: Parser):
    return option(instance1, instance2)

############################################################################################
# state
def getState() -> Parser[ParseState]:
    return Parser.PARSER(lambda a: Either.RIGHT((a, a)))

def putState(state: ParseState) -> Parser[Any]:
    return Parser.PARSER(lambda a: Either.RIGHT(('', state)))

def setState(val: Any, state: ParseState) -> Parser[Any]:
    return Parser.PARSER(lambda a: Either.RIGHT((val, state)))

def modifyOffset(initState: ParseState, newOffset: int) -> ParseState:
     initState.offset = newOffset
     return initState

def stopParse() -> Parser[ParseState]:
    return putState(parseState(''))

############################################################################################
# unit
def identity(basic: Any) -> Parser[Any]:
    return Parser.PARSER(lambda s: Either.RIGHT((basic, s)))

# little hack as return would not get ref correctly(cause by python weak type inferring)
def _identity(basic: Any) -> Parser[str]:
    return Parser.PARSER(lambda s: Either.RIGHT(Graph.GRAPH((basic, s))))

def bail(error: SyntaxError) -> Parser[Any]:
    return Parser.PARSER(lambda s: Either.LEFT(error))

def _assert(criteria: bool, error: SyntaxError) -> Parser[Any]:
    if criteria == True:
        return identity('')
    else:
        return bail(error)

############################################################################################
# combinator

#bind :: Parser a -> (a -> Parser b) -> Parser b
#bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

# basic chain
def chain1(p1: Parser[Any], p2: Callable[[Any], Parser[Any]]) -> Parser[Any]:
    return Parser.PARSER(lambda initState: p1.match(
            parser=lambda f1: f1(initState).match(
                left=lambda error: Either.LEFT(error),
                right=lambda t: p2(t[0]).match(
                    parser=lambda f2: f2(t[1])))))

def chainWithSkip(p1: Parser[Any], p2: Parser[Any]) -> Parser[Any]:
    return chain1(p1, lambda a: p2)


# if we hit some known error, stop the parse
def option(p1: Parser[Any], p2: Parser[Any]) -> Parser[Any]:
    return Parser.PARSER(lambda s: p1.match(
            parser=lambda f1: f1(s).match(
                left=lambda e: e.match(
                    mismatch=lambda f: p2.match(parser=lambda f2: f2(s)),
                    eol=lambda f: p2.match(parser=lambda f2: f2(s)),
                    missingoperand=lambda f: Either.LEFT(e),
                    negateerror=lambda f: Either.LEFT(e),
                    bitstringlengthinvalid=lambda f: Either.LEFT(e),
                    bitvalueinvalid=lambda f: Either.LEFT(e),
                    intoverflow=lambda f: Either.LEFT(e),
                    invalid=lambda f: Either.LEFT(e),
                    fatal=lambda f: Either.LEFT(e)
                ),

                right=lambda t: Either.RIGHT(t))))


def option1(p1: Parser, p2: Parser, f: Callable[[SyntaxError], bool]):
    return Parser.PARSER(lambda s: p1.match(
            parser=lambda f1: f1(s).match(
                left=lambda e: Either.LEFT(e) if f(e) else p2.match(parser=lambda f2: f2(s)),
                right=lambda t: Either.RIGHT(t))))


def satisfy(f: Callable[[str], bool], msg: str='') -> Parser[str]:
    return chain1(getState(), lambda initState: uncons(initState.string).match(
        # if there are targets, update the state
        just=lambda a: chain1(putState(ParseState(string=a[1],
                                                  offset=initState.offset+1,
                                                  parenthesisCounter=initState.parenthesisCounter,
                                                  withinBracketCounter=initState.withinBracketCounter,
                                                  withinConditionalCounter=initState.withinConditionalCounter,
                                                  withinFunctionArg=initState.withinFunctionArg,
                                                  graph=initState.graph
                                                  #statePos=initState.statePos
                                                 )),
                              lambda b: identity(a[0]) if f(a[0])
                              else bail(SyntaxError.MISMATCH(f"Syntax error near {a[0]}, {msg}"))),
        nothing=lambda: bail(SyntaxError.EOL(f"EOL reached, {msg}"))))

# NOTE: python3 not build to handle this lot recursive call using many1 and some1, can replace them using satisfyMany()
def satisfyMany(f: Callable[[str], Either[str, str]]) -> Parser[str]:
    return chain1(getState(), lambda initState: uncons(initState.string).match(
        # if there are targets, update the state
        just=lambda a: f(initState.string).match(
            right=lambda b: chain1(putState(ParseState(string=initState.string[len(b):],
                                                  offset=initState.offset+len(b),
                                                  parenthesisCounter=initState.parenthesisCounter,
                                                  withinBracketCounter=initState.withinBracketCounter,
                                                  withinConditionalCounter=initState.withinConditionalCounter,
                                                  withinFunctionArg=initState.withinFunctionArg,
                                                  graph=initState.graph
                                                  #statePos=initState.statePos
                                                 )), lambda c: identity(b)),
            left=lambda b: bail(SyntaxError.MISMATCH(f"Syntax error near {a[0]}, {b}."))),
        nothing=lambda: bail(SyntaxError.EOL(f"EOL reached."))))

def satisfy1(p1: Parser[Any], f: Callable[[str], bool], msg: str='') -> Parser[str]:
    return chain1(getState(), lambda s: p1.match(
            parser=lambda f1: f1(s).match(
                left=lambda e: bail(e),
                right=lambda a: chain1(putState(ParseState(string=a[1].string,
                                                  offset=a[1].offset,
                                                  parenthesisCounter=a[1].parenthesisCounter,
                                                  withinBracketCounter=a[1].withinBracketCounter,
                                                  withinConditionalCounter=a[1].withinConditionalCounter,
                                                  withinFunctionArg=a[1].withinFunctionArg,
                                                  graph=a[1].graph
                                                  #statePos=initState.statePos
                                                )), lambda b: identity(a[0])) if f(a[0])
                                                else bail(SyntaxError.INVALID(f"Syntax error near {a[0]}, {msg}")))))

def satisfy3(f: Callable[[str], bool], msg: str='') -> Parser[str]:
    return chain1(getState(), lambda initState: uncons(initState.string).match(
        # if there are targets, update the state
        just=lambda a: chain1(putState(ParseState(string=a[1],
                                                  offset=initState.offset+1,
                                                  parenthesisCounter=initState.parenthesisCounter,
                                                  withinBracketCounter=initState.withinBracketCounter,
                                                  withinConditionalCounter=initState.withinConditionalCounter,
                                                  withinFunctionArg=initState.withinFunctionArg,
                                                  graph=initState.graph
                                                  #statePos=initState.statePos
                                                 )),
                              lambda b: identity(a[0]) if f(a[0])
                              else bail(SyntaxError.INVALID(f"Syntax error near {a[0]}, {msg}"))),
        nothing=lambda: bail(SyntaxError.EOL(f"EOL reached, {msg}"))))

def chainl(p1: Parser[Any], p2: Parser[Callable[[Any], Any]]) -> Parser[Any]:
    def rest(_rest: Callable, p1: Parser[Any]):
        return chain1(getState(), lambda initState: uncons(initState.string).match(
            nothing=lambda: bail(SyntaxError.MISSINGOPERAND('Missing operand after negate operator, !')),
            just=lambda a: chain1(p1, lambda a: identity(_rest(a)))))

    # determine if there a left over, if so, its a syntax error
    return alternate(chain1(p2, lambda f: rest(f, p1)), p1)


def chainl1(p1: Parser[Any], p2: Parser[Callable[[Any, Any], Any]]) -> Parser[Any]:
    def rest(_rest: Any, p1: Parser[Any], p2: Parser[Callable[[Any, Any], Any]]):
        return chain1(getState(), lambda initState: uncons(initState.string).match(
            nothing=lambda: identity(_rest),
            just=lambda a: Parser.PARSER(lambda s: p2.match(
                parser=lambda f2: f2(s).match(
                    left=lambda l: Either.RIGHT((_rest, s)),
                    right=lambda t: runParser(chain1(p1, lambda b:
                                                  rest(t[0](_rest, b), p1, p2)))(t[1]).match(
                                                      left=lambda l: Either.LEFT(l),
                                                      right=lambda t: Either.RIGHT(t)))))))
    return chain1(p1, lambda a: rest(a, p1, p2))


def chainl2(p1: Parser[Any], p2: Parser[Callable[[Any], Parser[Callable[[Any, Any], Any]]]]) -> Parser[Any]:
    def rest(_rest: Any, p1: Parser[Any], p2: Parser[Callable[[Any], Parser[Callable[[Any, Any], Any]]]]):
        return chain1(getState(), lambda initState: uncons(initState.string).match(
            nothing=lambda: identity(_rest),
            just=lambda a: alternate(chain1(p2, lambda f:
                                            chain1(p1, lambda b:
                                                   chain1(rest(b, p1, p2), lambda c:
                                                        chain1(f(_rest), lambda g:
                                                            chain1(p1, lambda d:
                                                                chain1(rest(d, p1, p2), lambda e:
                                                                    rest(g(c, e), p1, p2))))))),
                                          identity(_rest))))

    return chain1(p1, lambda a: rest(a, p1, p2))

# zero or more
def many1(p: Parser[Any]) -> Parser[Any]:
    # get first char and try to match the first result to make a lookahead
    return chain1(alternate(p, identity('')), lambda start: uncons(start).match(
        nothing=lambda: identity(''),     #replaceRight("", parseString('"')),
        just=lambda a: fmap(many1(p), curry(cons)(start))))

# one or more
def some1(a: Parser[str]) -> Parser[str]:
    return applicative(fmap(a, curry(cons)), many1(a))

def oneOf(charToMatch: str) -> Parser[str]:
    return satisfy(lambda a: a in charToMatch, f'expect {charToMatch}')

def noneOf(charNotToMatch: str) -> Parser[str]:
    return satisfy(lambda a: not a in charNotToMatch, f'unexpected {charNotToMatch}')

def separatedBy(a: Parser[List[str]], s: Parser[Any]) -> Parser[List[Any]]:
    return applicative(fmap(a, cons), many1(chainWithSkip(s, a)))

def surroundedBy(s1: str, s2: str) -> Parser[Any]:
        return chainWithSkip(parseSaidChar(s1), alternate(chain1(parseAnyExceptMany([s2, '{']), lambda a:
                                                   chainWithSkip(parseSaidChar(s2), identity(a))), bail(SyntaxError.INVALID(f"Syntax error near {s1}, expect closing {s2}"))))
    #return chainWithSkip(p1, chain1(p2, lambda a: chainWithSkip(p1, identity(a))))

def infixOp(inp: str, command: Callable[[Any, Any], Any]) -> Parser[Callable[[Any, Any], Any]]:
    return chainWithSkip(parseString(inp),
                 monad(getState(), lambda initState:
                       chainWithSkip(putState(ParseState(string=initState.string,
                                                         offset=initState.offset,
                                                         parenthesisCounter=initState.parenthesisCounter,
                                                         withinBracketCounter=initState.withinBracketCounter,
                                                         withinConditionalCounter=initState.withinConditionalCounter+1,
                                                         withinFunctionArg=initState.withinFunctionArg,
                                                         graph=initState.graph)), identity(command)) if inp == '?' else
                       (chainWithSkip(putState(ParseState(string=initState.string,
                                                         offset=initState.offset,
                                                         parenthesisCounter=initState.parenthesisCounter,
                                                         withinBracketCounter=initState.withinBracketCounter,
                                                         withinConditionalCounter=initState.withinConditionalCounter-1,
                                                         withinFunctionArg=initState.withinFunctionArg,
                                                         graph=initState.graph)), identity(command)) if initState.withinConditionalCounter > 0 else \
                       bail(SyntaxError.FATAL('statement div, : not matched'))) if inp == ':' else identity(command)))

def lookahead(a: str) -> Parser[bool]:
    return chain1(getState(), lambda initState: uncons(initState.string).match(
        just=lambda b: identity(True) if a == b[0] else identity(False),
        nothing=lambda: bail(SyntaxError.EOL("EOL reached"))))


############################################################################################
# general parser

# match whatever char
def parseChar() -> Parser[str]:
    return satisfy(lambda a: True)

def parseCharSome() -> Parser[str]:
    def _parseCharSome(_str: str) -> Either[str, str]:
        match = re.match(r'.*', _str)
        if match != None:
            return Either.RIGHT(str(match[0]))
        return Either.LEFT(f'expect char')

    return satisfyMany(_parseCharSome)

def parseSaidChar(saidChar: str) -> Parser[str]:
    return satisfy(lambda a: a == saidChar, f'expect {saidChar}')


#def parseString(strParseTarget: str) -> Parser[str]:
#    if strParseTarget.__len__() == 0:
#        return identity('')
#    else:
#        return chainWithSkip(chainWithSkip(parseSaidChar(strParseTarget[0]),
#                                    parseString(strParseTarget[1:])),
#                                    identity(strParseTarget))

def parseString(strParseTarget: str) -> Parser[str]:
    def _parseString(_str: str) -> Either[str, str]:
        regexFormatList= ['[', ']', '{', '}', '(', ')', '^', '$', '*', '+', '-', '?', '.', '\\', '|', '<', '=', '>', '"', "\'", '!', '&', '@', '%', ':']
        patternlist = map(lambda x: f'\\{x}' if x in regexFormatList else x, list(strParseTarget))
        pattern= ''.join(patternlist)
        match = re.match(r'{}'.format(pattern), _str)
        if match != None:
            return Either.RIGHT(str(match[0]))
        return Either.LEFT(f'expect {strParseTarget}')

    return satisfyMany(_parseString)


#def parseInt() -> Parser[int]:
#    return chain1(alternate(parseSaidChar('-'), identity('')), lambda s:
#                  chain1(some1(parseDigitStr()), lambda cs:
#                         identity(int(s+cs))))

def parseInt() -> Parser[int]:
    def _parseManyDigitStr(_str: str) -> Either[str, str]:
        match = re.match(r'\d+', _str)
        if match != None:
            return Either.RIGHT(str(match[0]))
        return Either.LEFT('expect integer')

    return chain1(alternate(parseSaidChar('-'), identity('')), lambda s:
                  chain1(satisfyMany(_parseManyDigitStr), lambda cs:
                         identity(int(s+cs))))

#def parseInt32() -> Parser[int]:
#    return chain1(alternate(parseSaidChar('-'), identity('')), lambda s:
#                  chain1(some1(parseDigitStr()), lambda cs:
#                         identity(int(s+cs)) if isInt(cs) else bail(f'integer {s+cs} overflow')))

#def parseDecimal() -> Parser[int]:
#    return chain1(some1(parseDigitStr()), lambda cs: identity(int(cs)))

def parseDecimal() -> Parser[int]:
    def _parseManyDigitStr(_str: str) -> Either[str, str]:
        match = re.match(r'\d+', _str)
        if match != None:

            return Either.RIGHT(str(match[0]))
        return Either.LEFT('expect integer')

    return chain1(satisfyMany(_parseManyDigitStr), lambda cs: identity(int(cs)))

def parseString2() -> Parser[str]:
    return surroundedBy('"', '"')

def parseParens(p: Parser[Any]) -> Parser[Any]:
    return ignoreRight(chainWithSkip(parseSaidChar('('),
                                     monad(getState(), lambda initState:
                                           chainWithSkip(putState(ParseState(string=initState.string,
                                                                             offset=initState.offset,
                                                                             parenthesisCounter=initState.parenthesisCounter+1,
                                                                             withinBracketCounter=initState.withinBracketCounter,
                                                                             withinConditionalCounter=initState.withinConditionalCounter,
                                                                             withinFunctionArg=initState.withinFunctionArg,
                                                                             graph=initState.graph)), p))),
                       chainWithSkip(satisfy3(lambda a: a == ')', f'expecting closing parenthesis, )'),
                                     monad(getState(), lambda initState:
                                           putState(ParseState(string=initState.string,
                                                               offset=initState.offset,
                                                               parenthesisCounter=initState.parenthesisCounter-1,
                                                               withinBracketCounter=initState.withinBracketCounter,
                                                               withinConditionalCounter=initState.withinConditionalCounter,
                                                               withinFunctionArg=initState.withinFunctionArg,
                                                               graph=initState.graph)) if initState.parenthesisCounter > 0 else bail(SyntaxError.INVALID('closing parenthesis not matched')))))

def parseDigit() -> Parser[int]:
    return fmap(satisfy(lambda a: a.isdigit(), 'expect a digit'), digitToInt)

# as int in python are not list
def parseDigitStr() -> Parser[str]:
    return satisfy(lambda a: a.isdigit(), 'expect a digit')

def parseBool() -> Parser[bool]:
    return alternate(chainWithSkip(parseString("True"), identity(True)),
                     chainWithSkip(parseString("False"), identity(False)))

def parseSpace() -> Parser[str]:
    return satisfy(lambda a: a.isspace(), 'expect a space')

# limit int within 32 bit
def parseInt32() -> Parser[int]:
    return satisfy1(parseInt(), lambda b: (b > -2147483648) and (b < 2147483648), msg=f'integer exceed 32bit')

def parseAnyExcept(exceptionStrList: List[str]) -> Parser[str]:
    return satisfy(lambda a: False if a in exceptionStrList else True)

def parseAnyExceptMany(exceptionStrList: List[str]) -> Parser[str]:
    def _parseAnyExceptMany(_str: str) -> Either[str, str]:
        regexFormatList= ['[', ']', '{', '}', '(', ')', '^', '$', '*', '+', '-', '?', '.', '\\', '|', '<', '=', '>', '"', "\'", '!', '&', '@', '%', ':']
        patternlist = map(lambda x: f'\\{x}' if x in regexFormatList else x, exceptionStrList)
        pattern= ''.join(patternlist)
        match = re.match(r'[^{}]+'.format(pattern), _str)

        if match != None:
            return Either.RIGHT(str(match[0]))
        return Either.RIGHT('')

    return satisfyMany(_parseAnyExceptMany)

def parseAnyExceptSome(exceptionStrList: List[str]) -> Parser[str]:
    def _parseAnyExceptMany(_str: str) -> Either[str, str]:
        regexFormatList= ['[', ']', '{', '}', '(', ')', '^', '$', '*', '+', '-', '?', '.', '\\', '|', '<', '=', '>', '"', "\'", '!', '&', '@', '%', ':']
        patternlist = map(lambda x: f'\\{x}' if x in regexFormatList else x, exceptionStrList)
        pattern= ''.join(patternlist)
        match = re.match(r'[^{}]+'.format(pattern), _str)

        if match != None:
            return Either.RIGHT(str(match[0]))
        return Either.LEFT('expect char')

    return satisfyMany(_parseAnyExceptMany)

#def parseAlphabet() -> Parser[str]:
#    # TODO: use regex instead
#    return alternate(parseSaidChar('a'),
#           alternate(parseSaidChar('b'),
#           alternate(parseSaidChar('c'),
#           alternate(parseSaidChar('d'),
#           alternate(parseSaidChar('e'),
#           alternate(parseSaidChar('f'),
#           alternate(parseSaidChar('g'),
#           alternate(parseSaidChar('h'),
#           alternate(parseSaidChar('i'),
#           alternate(parseSaidChar('j'),
#           alternate(parseSaidChar('k'),
#           alternate(parseSaidChar('l'),
#           alternate(parseSaidChar('m'),
#           alternate(parseSaidChar('n'),
#           alternate(parseSaidChar('o'),
#           alternate(parseSaidChar('p'),
#           alternate(parseSaidChar('q'),
#           alternate(parseSaidChar('r'),
#           alternate(parseSaidChar('s'),
#           alternate(parseSaidChar('t'),
#           alternate(parseSaidChar('u'),
#           alternate(parseSaidChar('v'),
#           alternate(parseSaidChar('w'),
#           alternate(parseSaidChar('x'),
#           alternate(parseSaidChar('y'),
#           parseSaidChar('z'))))))))))))))))))))))))))

# xAAAAAB123
def parseBitstringHex() -> Parser[str]:
    def _parseHexStr(_str: str) -> Either[str, str]:
        match = re.match(r'[a-fA-F\d]+', _str)
        if match != None:
            return Either.RIGHT(str(match[0]))
        return Either.LEFT('expect hex value after \'h')

    return monad(parseString('h'), lambda a: satisfyMany(_parseHexStr))

    #return monad(parseString('h'), lambda a:
    #              some1(alternate(parseDigitStr(),
    #                           alternate(parseSaidChar('A'),
    #                           alternate(parseSaidChar('B'),
    #                           alternate(parseSaidChar('C'),
    #                           alternate(parseSaidChar('D'),
    #                           alternate(parseSaidChar('E'),
    #                           alternate(parseSaidChar('F'),
    #                           alternate(parseSaidChar('a'),
    #                           alternate(parseSaidChar('b'),
    #                           alternate(parseSaidChar('c'),
    #                           alternate(parseSaidChar('d'),
    #                           alternate(parseSaidChar('e'),
    #                           (parseSaidChar('f'))))))))))))))))

def parseBitstringDecimal() -> Parser[int]:
    return monad(parseString('d'), lambda a: parseDecimal())

def parseBitstringBinary() -> Parser[int]:
    def _parseBinaryStr(_str: str) -> Either[str, str]:
        match = re.match(r'[01]+', _str)
        if match != None:
            return Either.RIGHT(str(match[0]))
        return Either.LEFT('expect binary after \'b')
    return monad(parseString('b'), lambda a: satisfyMany(_parseBinaryStr))

############################################################################################
def runParser(instance: Parser):
    return instance.match(
        parser=lambda a : a
    )

def digitToInt(digit: str):
    return int(digit)


#def parseNewline() -> Parser[str]:
#    return chainWithSkip(parseSaidChar('\n'), chain1(getState(), lambda initState: uncons(initState.string).match(
#        just=lambda a: putState(ParseState(string=initState.string,
#                                                  offset=initState.offset,
#                                                  line=initState.line+1,
#                                                  statePos=initState.statePos)),
#        nothing=lambda a: Either.LEFT("no newline"))))
