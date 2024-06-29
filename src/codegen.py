from typing import Any, List, Dict, Tuple, Callable, TypeVar, Generic
from adt.decorator import adt
from adt.case import Case
from dataclasses import dataclass
from functools import reduce

from src.typeClass.monad import monad, _return
from src.typeClass.monoid import mappend
from src.monad._except import Except, returnExcept, runExcept, throwE
from src.monad.either import Either, returnEither
from src.monad.maybe import Maybe
from src.type.twTy import TwTm, TwTy, TwCommonCommand, TwTaskCommand
from src.lang.C import CTm, CTy

A = TypeVar('A') # reader

@dataclass
class Ctx:
    tmBindEnv: Dict[str, TwTy]

AST = List[CTm]

@adt
class PlacementMode:
    COPY:           Case
    REF:            Case

@adt
class Placement:
    NIL:            Case
    INTRO:          Case[str]
    #OPT:            Case[str]
    VAR:            Case[str, PlacementMode]

# R -> Ctx
# W -> Tuple
# S -> int
@adt
class CodeGen(Generic[A]):
    CODEGEN: Case[Callable[[Ctx, int], Except[str, Tuple[A, int, Tuple]]]]

@monad.instance(CodeGen)
def _monad_CodeGen(instance: CodeGen, func: Callable[[A], CodeGen]) -> CodeGen[A]:
    return CodeGen.CODEGEN(lambda t:
                monad(runCodeGen(instance, t[0], t[1]), lambda t1:
                      monad(runCodeGen(func(t1[0]), t[0], t1[1]), lambda t2:
                           returnExcept((t2[1], mappend(t1[2], t2[2]))))))

def returnCodeGen(pure: A) -> CodeGen[A]:
    return CodeGen.CODEGEN(lambda t: returnExcept((pure, t[1], ())))

def returnCodeGen2(_except: Except) -> CodeGen[A]:
    return CodeGen.CODEGEN(lambda t: returnExcept(_except))

def failCodeGen(errormsg: str) -> CodeGen:
    return CodeGen.CODEGEN(lambda t: throwE(errormsg))

def runCodeGen(f: CodeGen, r, s) -> Except:
    return f.match(codegen=lambda a : a(r, s))

def runCodeGen2(tm: TwTm) -> Either[str, CTm]:
    def _runCodeGen(f: CodeGen, r, s) -> Except:
        return f.match(codegen=lambda a : a(r, s))

    return monad(runExcept(_runCodeGen(infer(tm, Placement.NIL()), Ctx(tmBindEnv={}), 0)), lambda t:
                 returnEither(CTm.FUNCTION('main', [], CTm.BLOCK(t[0]+[CTm.RETURN(CTm.VAR(t[1]))]))))

###################################################################
# reader ops
###################################################################
# fetch value of env
def ask() -> CodeGen[Ctx]:
    return CodeGen.CODEGEN(lambda t: returnExcept((t[0], t[1], ())))

# apply func to env
def asks(f: Callable[[Ctx], Ctx]) -> CodeGen[Ctx]:
    return CodeGen.CODEGEN(lambda t: returnExcept((f(t[0]), t[1], ())))

# mod env with closure
def local(f: Callable[[Ctx], Ctx], codegen: CodeGen[A]) -> CodeGen[A]:
    return CodeGen.CODEGEN(lambda t: runCodeGen(codegen, f(t[0]), t[1]))

###################################################################
# writer ops

# construct new writer with comp res
def writer(a: A, w: Tuple) -> CodeGen[A]:
    return CodeGen.CODEGEN(lambda t: returnExcept((a, t[1], w)))

# construct new writer
def tell(w: Tuple) -> CodeGen:
    return CodeGen.CODEGEN(lambda t: returnExcept(('', t[1], w)))

# exec rwst in closure and return its writer
def listen(codegen: CodeGen[A]) -> CodeGen[A]:
    return CodeGen.CODEGEN(lambda t:
                     monad(runCodeGen(codegen, t[0], t[1]), lambda t1:
                           returnExcept(((t1[0], t1[2]), t[1], t1[2]))))

# exec rwst in closure and return its writer
def listens(codegen: CodeGen[A], f: Callable[[Tuple], Tuple]) -> CodeGen[Tuple[A, Tuple]]:
    return CodeGen.CODEGEN(lambda t:
                     monad(runCodeGen(codegen, t[0], t[1]), lambda t1:
                           returnExcept(((t1[0], f(t1[2])), t[1], t1[2]))))

###################################################################
# state ops

# construct new writer
# maybe we can simply accept a new state and replace it
S = int

def state(f: Callable[[S], Tuple[A, S]]) -> CodeGen[A]:
    def _state(s: S) -> Tuple[A, S]:
        return f(s)

    return CodeGen.CODEGEN(lambda t:
                     monad(returnExcept(_state(t[1])), lambda t1:
                           returnExcept((t1[0], t1[1], ()))))

def get() -> CodeGen[A]:
    return CodeGen.CODEGEN(lambda t:
                    returnExcept((t[1], t[1], ())))

def gets(f: Callable[[S], A]) -> CodeGen[A]:
    return CodeGen.CODEGEN(lambda t:
                    returnExcept((f(t[1]), t[1], ())))

def modify(f: Callable[[S], S]) -> CodeGen:
    return CodeGen.CODEGEN(lambda t: returnExcept(('', f(t[1]), ())))

###################################################################
# generator

# generate a unique var name
def freshVarName():
    return monad(modify(lambda a: a+1), lambda count: returnCodeGen(f'var{count}'))

def toIndexer():
    pass

# check for var instance
def lookupTmVarBind(name: str) -> CodeGen[Tuple[TwTy, str]]:
    return monad(asks(lambda ctx: ctx.tmBindEnv), lambda env: returnCodeGen((env[name], name)) if name in env else returnCodeGen2(throwE('')))

# add var into closure
def addTmVarBind(name: str, typ: TwTy, codegen: CodeGen):
    return local(lambda ctx: ctx._replace(tmBindEnv={**ctx.tmBindEnv, **{name: typ}}), codegen=codegen)

# NOTE: ha, we not going to do curry
def addTmVarBinds(namedict: Dict[str, TwTy], codegen: CodeGen):
    return local(lambda ctx: ctx._replace(tmBindEnv={**ctx.tmBindEnv, **namedict}), codegen=codegen)

# general codegen
def infer(tm: TwTm, placement: Placement) -> CodeGen[Tuple(AST, TwTy, Maybe[str])]: # return associated var name if present
    def _intLiteralPlace(_int) -> CodeGen[Tuple(AST, TwTy, Maybe[str])]:
        # NOTE: intro goes to TmFix
        # TODO: enable char&short support
        return placement.match(
            nil=lambda : returnCodeGen(([CTm.INT(_int)], TwTy.INT(), Maybe.NOTHING())),
            #intro=lambda a: returnCodeGen([_intro(a)]),
            var=lambda a, mode: mode.match(
                copy=lambda: returnCodeGen(([CTm.VARCOPYASSIGNMENT(CTm.VAR(a), CTm.INT(_int))], TwTy.INT(), Maybe.JUST(a))),
                ref=lambda: returnCodeGen(([CTm.VARREFASSIGNMENT(CTm.VAR(a), CTm.INT(_int))], TwTy.INT(), Maybe.JUST(a))))
            #opt=lambda a:
        )

    def _stringLiteralPlace(_str) -> CodeGen[Tuple(AST, TwTy, Maybe[str])]:
        return placement.match(
            nil=lambda : returnCodeGen(([CTm.STRING(_str)], TwTy.STRING(), Maybe.NOTHING())),
            var=lambda a, mode: mode.match(
                copy=lambda: returnCodeGen(([CTm.VARCOPYASSIGNMENT(CTm.VAR(a), CTm.STRING(_str))], TwTy.STRING(), Maybe.JUST(a))),
                ref=lambda: returnCodeGen(([CTm.VARREFASSIGNMENT(CTm.VAR(a), CTm.STRING(_str))], TwTy.STRING(), Maybe.JUST(a))))
        )

    def _doubleLiteralPlace(_fp) -> CodeGen[Tuple(AST, TwTy, Maybe[str])]:
        # TODO: support more length option, float & long double
        return placement.match(
            nil=lambda : returnCodeGen(([CTm.DOUBLE(_fp)], TwTy.DOUBLE(), Maybe.NOTHING())),
            var=lambda a, mode: mode.match(
                copy=lambda: returnCodeGen(([CTm.VARCOPYASSIGNMENT(CTm.VAR(a), CTm.DOUBLE(_fp))], TwTy.DOUBLE(), Maybe.JUST(a))),
                ref=lambda: returnCodeGen(([CTm.VARREFASSIGNMENT(CTm.VAR(a), CTm.DOUBLE(_fp))], TwTy.DOUBLE(), Maybe.JUST(a))))
        )

    def _boolLiteralPlace(_bool) -> CodeGen[Tuple(AST, TwTy, Maybe[str])]:
        return placement.match(
            nil=lambda : returnCodeGen(([CTm.BOOLEAN(_bool)], TwTy.BOOLEAN(), Maybe.NOTHING())),
            var=lambda a, mode: mode.match(
                copy=lambda: returnCodeGen(([CTm.VARCOPYASSIGNMENT(CTm.VAR(a), CTm.BOOLEAN(_bool))], TwTy.BOOLEAN(), Maybe.JUST(a))),
                ref=lambda: returnCodeGen(([CTm.VARREFASSIGNMENT(CTm.VAR(a), CTm.BOOLEAN(_bool))], TwTy.BOOLEAN(), Maybe.JUST(a))))
        )

    def _arrayLiteralPlace(_list: List[TwTm]) -> CodeGen[Tuple(AST, TwTy, Maybe[str])]:
        # TODO provide option to enable dynamic allocation
        def _traverse():
            return reduce(lambda a, acc: monad(acc, lambda b:
                                                monad(infer(a, placement), lambda c:
                                                    returnCodeGen((b[0]+[c[0]], c[1])))), _list, returnCodeGen(([], TwTy.UNIT())))

        return monad(_traverse(), lambda ctmlist: placement.match(
            # FIXME how about no assigment for nil, just pass down literal
            nil=lambda : returnCodeGen(([CTm.ARRAY(ctmlist[0])], TwTy.LIST(ctmlist[1]), Maybe.NOTHING())),
            var=lambda a, mode: mode.match(
                copy=lambda: returnCodeGen(([CTm.VARCOPYASSIGNMENT(CTm.VAR(a), CTm.ARRAY(ctmlist[0]))], TwTy.LIST(ctmlist[1]), Maybe.JUST(a))),
                ref=lambda: returnCodeGen(([CTm.VARREFASSIGNMENT(CTm.VAR(a), CTm.ARRAY(ctmlist[0]))], TwTy.LIST(ctmlist[1]), Maybe.JUST(a))))
            #intro=lambda a: returnCodeGen([_intro(a, ctmlist)]),
            #opt=lambda a:
        ))

    def _varPlace(_var) -> CodeGen[Tuple(AST, TwTy, Maybe[str])]:
        return placement.match(
            nil=lambda : monad(lookupTmVarBind(_var), lambda typ: returnCodeGen(([CTm.VAR(_var)], typ, Maybe.NOTHING()))),
            var=lambda a, mode: mode.match(
                copy=lambda: monad(lookupTmVarBind(_var), lambda typ: returnCodeGen(([CTm.VARCOPYASSIGNMENT(CTm.VAR(_var), a)], typ, Maybe.JUST(a)))),
                ref=lambda: monad(lookupTmVarBind(_var), lambda typ: returnCodeGen(([CTm.VARREFASSIGNMENT(CTm.VAR(_var), a)], typ, Maybe.JUST(a)))))
            #opt=lambda a:
        )

    # function declation
    def _funcPlace(name: str, args: Dict[str, TwTy], block: TwTm, ret: TwTy) -> CodeGen[Tuple(AST, TwTy, Maybe[str])]:
        # skip for now no subtype supported
        #def _inferSubType(blocktm: TwTm, rety: TwTy, placement: Placement) -> CodeGen[A]:
        #    pass

        def _inferFunc():
            return monad(addTmVarBinds(args, infer(block, Placement.NIL())), lambda ast:
                         # NOTE: C func not necessary has return
                         monad(inferTypes(args), lambda ctyps: returnCodeGen(([CTm.FUNCTION(name, ctyps, CTm.BLOCK([ast[0]]))], ret, Maybe.NOTHING()))))

        return placement.match(
            nil=lambda : _inferFunc(),
            intro=lambda a: _inferFunc(),
            # NOTE: we r not following haskell syntax, func cant assign as value
            var=lambda a, mode: failCodeGen('cant assign func intro to var')
            #opt=lambda a:
        )

    # intro var with closure
    def _introPlace(var: str, closure: TwTm, typ: TwTy) -> CodeGen[Tuple(AST, TwTy, Maybe[str])]:
        return monad(addTmVarBind(var, typ, closure), lambda ast:
                     monad(inferType(typ), lambda ctyp: returnCodeGen(([CTm.BLOCK([CTm.VARINTRODUCTION(var, ctyp), ast[0]])], typ, Maybe.JUST(var)))))

    # function call with args as closure
    # TODO: should we check args type match?
    def _appPlace(funcVar: TwTm, args: List[TwTm]) -> CodeGen[Tuple(AST, TwTy, Maybe[str])]:
        def _place(funcName: Maybe[str], funcType: TwTy, args: List[CTm]):
            return funcName.match(
                just=lambda funcname: funcType.match(
                    function=lambda argstyp, retyp: returnCodeGen(([CTm.APP(CTm.VAR(funcname), args)], retyp)),
                    # TODO complete rest of the matches
                ),
                nothing=lambda : failCodeGen('function has no name'))

        return monad(infer(funcVar, Placement.NIL()),lambda a: # varast, retyp, funcvar
                     monad(reduce(lambda x,y: monad(infer(x, Placement.NIL()), lambda z: returnCodeGen(y+z[0])), args, returnCodeGen([])), lambda cargs: # argsast, argstyp, argsvar
                           monad(_place(a[2], a[1], cargs), lambda b:
                                 returnCodeGen((a[0] + cargs + b[0], b[1], Maybe.NOTHING())))))

    return tm.match(
        int=lambda a: _intLiteralPlace(a),
        string=lambda a: _stringLiteralPlace(a),
        double=lambda a: _doubleLiteralPlace(a),
        bool=lambda a: _boolLiteralPlace(a),
        list=lambda t, a: _arrayLiteralPlace(a),
        function=lambda name, args, bdy, retyp: _funcPlace(name, args, bdy, retyp),
        #var=lambda name, args, bdy, retyp: _funcPlace(name, args, bdy, retyp),
        intro=lambda name, bdy, typ: _introPlace(name, bdy, typ))


def inferType(typ: TwTy) -> CodeGen[CTy]:
    return typ.match(
        int=lambda: returnCodeGen(CTy.INT()),
        bool=lambda: returnCodeGen(CTy.BOOLEAN()),
        string=lambda: returnCodeGen(CTy.STRING()),
        float=lambda: returnCodeGen(CTy.DOUBLE()),
        unit=lambda: returnCodeGen(CTy.VOID()),
        # FIXME: length?
        list=lambda a: returnCodeGen(CTy.ARRAY(inferType(a))),
        # FIXME: c dict equivalent?
        # dict
        function=lambda : failCodeGen('function not first class in C'),
        var=lambda a: failCodeGen('cant infer a Twin var type to C'),
        record=lambda a: returnCodeGen(CTy.STRUCT(a))
    )

def inferTypes(typs: Dict[str, TwTy]) -> CodeGen[Dict[str, CTy]]:
    #TODO: proper do it for dict
    return reduce(lambda x, y: monad(inferType(x), lambda a: y+[a]), typs, returnExcept([]))

# command based codegen
def inferCommnad():
    pass
