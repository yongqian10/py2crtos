from typing import Any, List, Dict, Tuple, Callable, TypeVar, Generic
from adt.decorator import adt
from adt.case import Case
from dataclasses import dataclass

from src.typeClass.monad import monad, _return
from src.typeClass.monoid import mappend
from src.monad._except import Except, returnExcept
from src.monad.either import Either
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

@adt
class AST:
    AST:    Case[C]

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

def runCodeGen(f: CodeGen, r, s) -> Except:
    return f.match(codegen=lambda a : a(r, s))

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

def lookupBind(name: str) -> CodeGen[Tuple[TwTy, str]]:
    return ask()

# assignment codegen
def infer(tm: TwTm, varName: str) -> CodeGen[AST]:
    return tm.match(
        TmInt=lambda a: AST.AST(C.CAssignment(C.CIndexer(C.CVar(varName), C.CTemplateLiteral('int')), int(a))),
        TmString=lambda a: AST.AST(C.CAssignment(C.CIndexer(varName=C.CVar(varName), varType=C.CTemplateLiteral('str'), lit=a)),
        TmDouble=lambda a: AST.AST(C.CAssignment(C.CIndexer(varName=C.CVar(varName), varType=C.CTemplateLiteral('double'), lit=a)),
        TmVar=lambda a: AST.AST(C.CAssignment(C.CIndexer(varName=C.CVar(varName), varType=C.CTemplateLiteral('double'), lit=a)),
    )
