#!/usr/bin/env python3

# functional stack
from typing import Any, Callable, TypeVar
from typing import Generic, Tuple
from adt.decorator import adt
from adt.case import Case
#from pymonad.maybe import *

from src.type_class.monad import _return, monad

R = TypeVar('R') #
M = TypeVar('M') # monad
A = TypeVar('A')

@adt
class ReaderT(Generic[R, M, A]):
    READERT: Case[Callable[[R], M]]

@monad.instance(ReaderT)
def _monad_ReaderT(instance: ReaderT, func: Callable[[Any], ReaderT]):
    return ReaderT.READERT(lambda r:
                monad(runReaderT(instance, r), lambda a:
                    runReaderT(func(a), r)))

# need m as arg make it hard to use
@_return.instance(ReaderT)
def _return_ReaderT(instance: ReaderT, m: M, pure: Any):
        return ReaderT.READERT(lambda r: _return(m, pure))


def runReaderT(f: ReaderT, r) -> M:
    return f.match(
        readert=lambda a : a(r))

###################################################################
# operations

def ask(m) -> ReaderT:
    return ReaderT.READERT(lambda r: _return(m, r))

# apply func to env
def asks(f: Callable[[R], R], m) -> ReaderT:
    return ReaderT.READERT(lambda r: _return(m, f(r)))

# mod env
def local(f: Callable[[R], R], reader: ReaderT) -> ReaderT:
    return ReaderT.READERT(lambda r: runReaderT(reader, f(r)))
