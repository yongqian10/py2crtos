#!/usr/bin/env python3

# functional stack
from typing import Any, Callable, TypeVar
from typing import Generic, Tuple
from adt.decorator import adt
from adt.case import Case
#from pymonad.maybe import *

from src.typeClass.monad import _return, monad
from src.typeClass.monoid import mappend

R = TypeVar('R') # reader
W = TypeVar('W') # writer
S = TypeVar('S') # state
M = TypeVar('M') # monad
A = TypeVar('A')

@adt
class RwsT(Generic[R, W, S, M, A]):
    RWST: Case[Callable[[R, S], M[Tuple[A, S, W]]]]

@monad.instance(RwsT)
def _monad_RwsT(instance: RwsT, func: Callable[[A], RwsT], m: M):
    return RwsT.RWST(lambda t:
                monad(runRwsT(instance, t[0], t[1]), lambda t1:
                      monad(runRwsT(func(t1[0]), t[0], t1[1]), lambda t2:
                           _return(m, (t2[1], mappend(t1[2], t2[2]))))))

@_return.instance(RwsT)
def _return_RwsT(instance: RwsT, pure: A, m: M):
    return RwsT.RWST(lambda t: _return(m, (pure, t[1], ())))

def runRwsT(f: RwsT, r, s) -> M:
    return f.match(
        rswt=lambda a : a(r, s))

###################################################################
# operations

###################################################################
# reader ops

# fetch value of env
def ask(m) -> RwsT:
    return RwsT.RWST(lambda t: _return(m, (t[0], t[1], ())))

# apply func to env
def asks(f: Callable[[R], R], m) -> RwsT:
    return RwsT.RWST(lambda t: _return(m, (f(t[0]), t[1], ())))

# mod env with closure
def local(f: Callable[[R], R], rwst: RwsT) -> RwsT:
    return RwsT.RWST(lambda t: runRwsT(rwst, f(t[0]), t[1]))

###################################################################
# writer ops

# construct new writer with comp res
def writer(a, w, m) -> RwsT:
    return RwsT.RWST(lambda t: _return(m, (a, t[1], w)))

# construct new writer
def tell(w, m) -> RwsT:
    return RwsT.RWST(lambda t: _return(m, ('', t[1], w)))

# exec rwst in closure and return its writer
def listen(rwst: RwsT, m: M) -> RwsT:
    return RwsT.RWST(lambda t:
                     monad(runRwsT(rwst, t[0], t[1]), lambda t1:
                           _return(m, ((t1[0], t1[2]), t[1], t1[2]))))

# exec rwst in closure and return its writer
def listens(rwst: RwsT, f: Callable[[W], Any], m: M) -> RwsT[R, W, S, M, Tuple[R,Any]]:
    return RwsT.RWST(lambda t:
                     monad(runRwsT(rwst, t[0], t[1]), lambda t1:
                           _return(m, ((t1[0], f(t1[2])), t[1], t1[2]))))

###################################################################
# state ops

# construct new writer
# maybe we can simply accept a new state and replace it
def state(f: Callable[[S], Tuple[A,S]], m: M) -> RwsT:
    def _state(s: S) -> Tuple[A,S]:
        return f(s)

    return RwsT.RWST(lambda t:
                     monad(_return(m, _state(t[1])), lambda t1:
                           _return(m, (t1[0], t1[1], ()))))

def get(m, M) -> RwsT:
    return RwsT.RWST(lambda t:
                    _return(m, (t[1], t[1], ())))

def gets(m: M, f: Callable[[S], A]) -> RwsT:
    return RwsT.RWST(lambda t:
                    _return(m ,(f(t[1]), t[1], ())))

def modify(m: M, f: Callable[[S], S]) -> RwsT:
    return RwsT.RWST(lambda t: _return(m, ('', f(t[1]), ())))
