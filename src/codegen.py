from typing import Any, List, Dict, Tuple, Callable, TypeVar, Generic
from adt.decorator import adt
from adt.case import Case
from dataclasses import dataclass

from src.typeClass.monad import monad, _return
from src.typeClass.monoid import mappend
from src.monad._except import Except, returnExcept, runExcept, throwE
from src.monad.either import Either, returnEither
from src.type.twTy import TwTm, TwTy

A = TypeVar('A') # reader

@dataclass
class Ctx:
    tmBindEnv: Dict[str, TwTm]

@adt
class C:
    CTemplateLiteral:   Case[str]
    CAssignment:        Case['C', 'C']
    CIndexer:           Case['C', 'C']
    CVar:               Case[str]

    CFunction:          Case[str, List['C'], 'C']   # accept list of func args
    CBlock:             Case[List['C']]
    CReturn:            Case[List['C']]             # return list of p

type AST =  List[C]

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

def runCodeGen(tm: TwTm) -> Either[str, C]:
    def _runCodeGen(f: CodeGen, r, s) -> Except:
        return f.match(codegen=lambda a : a(r, s))

    return monad(runExcept(_runCodeGen(infer(tm), Ctx(tmBindEnv={}), 0)), lambda t:
                 returnEither(C.CFunction('main', [], C.CBlock(t[0]+[C.CReturn(C.CVar(t[1]))]))))

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
# generator

def toIndexer():
    pass

# check for var instance
def lookupBind(name: str) -> CodeGen[Tuple[TwTy, str]]:
    return monad(asks(lambda ctx: ctx.tmBindEnv), lambda env: returnCodeGen((env[name], name)) if name in env else returnCodeGen2(throwE('')))

# assignment codegen
def infer(tm: TwTm, varName: str) -> CodeGen[AST]:
    return tm.match(
        TmInt=lambda a: [C.CAssignment(C.CIndexer(C.CVar(varName), C.CTemplateLiteral('int')), int(a))],
        TmString=lambda a: [C.CAssignment(C.CIndexer(varName=C.CVar(varName), varType=C.CTemplateLiteral('str'), lit=a))],
        TmDouble=lambda a: [C.CAssignment(C.CIndexer(varName=C.CVar(varName), varType=C.CTemplateLiteral('double'), lit=a))],
        TmVar=lambda a: AST.AST(), lit=a)
