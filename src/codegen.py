from typing import Any, List, Dict, Tuple, Callable, TypeVar, Generic
from adt.decorator import adt
from adt.case import Case
from dataclasses import dataclass

from src.typeClass.monad import monad, _return
from src.typeClass.monoid import mappend
from src.monad._except import Except, returnExcept, runExcept, throwE
from src.monad.either import Either, returnEither
from src.type.twTy import TwTm, TwTy, TwCommonCommand, TwTaskCommand
from src.lang.C import CTm, CTy

A = TypeVar('A') # reader

@dataclass
class Ctx:
    tmBindEnv: Dict[str, TwTm]

type AST = List[CTm]

@adt
class Placement:
    NIL:            Case                #
    INTRO:          Case[str]
    #OPT:            Case[str]
    VAR:            Case[str]

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

def runCodeGen(f: CodeGen, r, s) -> Except:
    return f.match(codegen=lambda a : a(r, s))

def runCodeGen2(tm: TwTm) -> Either[str, C]:
    def _runCodeGen(f: CodeGen, r, s) -> Except:
        return f.match(codegen=lambda a : a(r, s))

    return monad(runExcept(_runCodeGen(infer(tm), Ctx(tmBindEnv={}), 0)), lambda t:
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
type S = int

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

# general codegen
def infer(tm: TwTm, placement: Placement) -> CodeGen[AST]:
    def _intLiteralPlace(_int):
        # TODO: enable char&short support
        # NOTE: _declare func locate in var intro
        # def _declare():
        #    pass

        def _intro(name):
            return CTm.VARINTODUCTION(name, CTy.INT, CTm.INT(_int))

        def _assign(name):
            return CTm.VARASSIGNMENT(CTm.VAR(name), CTm.INT(_int))

        return placement.match(
            # FIXME how about no assigment for nil, just pass down literal
            nil=lambda : monad(freshVarName(), lambda b: returnExcept([_intro(b)])),
            intro=lambda a: returnExcept([_intro(a)]),
            var=lambda a: returnExcept([_assign(a)])
            #opt=lambda a:
        )

    def _stringLiteralPlace(_str):
        # TODO: support resp in ptr
        def _intro(name):
            return CTm.VARINTODUCTION(name, CTy.STRING, CTm.STRING(_str))

        def _assign(name):
            return CTm.VARASSIGNMENT(CTm.VAR(name), CTm.STRING(_str))

        return placement.match(
            # FIXME how about no assigment for nil, just pass down literal
            nil=lambda : monad(freshVarName(), lambda b: returnExcept([_intro(b)])),
            intro=lambda a: returnExcept([_intro(a)]),
            var=lambda a: returnExcept([_assign(a)])
            #opt=lambda a:
        )

    def _doubleLiteralPlace(_fp):
        # TODO: support more length option, float & long double
        def _intro(name):
            return CTm.VARINTODUCTION(name, CTy.DOUBLE, CTm.DOUBLE(_fp))

        def _assign(name):
            return CTm.VARASSIGNMENT(CTm.VAR(name), CTm.DOUBLE(_fp))

        return placement.match(
            # FIXME how about no assigment for nil, just pass down literal
            nil=lambda : monad(freshVarName(), lambda b: returnExcept([_intro(b)])),
            intro=lambda a: returnExcept([_intro(a)]),
            var=lambda a: returnExcept([_assign(a)])
            #opt=lambda a:
        )

    def _boolLiteralPlace(_bool):
        def _intro(name):
            return CTm.VARINTODUCTION(name, CTy.BOOLEAN, CTm.BOOLEAN(_bool))

        def _assign(name):
            return CTm.VARASSIGNMENT(CTm.VAR(name), CTm.DOUBLE(_bool))

        return placement.match(
            # FIXME how about no assigment for nil, just pass down literal
            nil=lambda : monad(freshVarName(), lambda b: returnExcept([_intro(b)])),
            intro=lambda a: returnExcept([_intro(a)]),
            var=lambda a: returnExcept([_assign(a)])
            #opt=lambda a:
        )

    return tm.match(
        tmint=lambda a: _intLiteralPlace(a),
        tmstring=lambda a: _stringLiteralPlace(a),
        tmdouble=lambda a: _doubleLiteralPlace(a),
        tmbool=lambda a: _boolLiteralPlace(a),
        TmVar=lambda a: AST.AST(), lit=a)


# command based codegen
def inferCommnad():
    pass
