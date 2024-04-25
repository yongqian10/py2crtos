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
    READERT: Case[Callable[R, M]]

@monad.instance(ReaderT)
def _monad_ReaderT(instance: ReaderT, func: Callable[[Any], ReaderT]):
    return ReaderT.READERT(lambda r:
                monad(runReaderT(instance, r), lambda a:
                    runReaderT(func(a), r)))

@_return.instance(ReaderT)
def __return_ReaderT(instance: ReaderT, pure: Any):
        return ReaderT.READERT(lambda r: _return(pure))  # not going to work this way, no infer in python


def runReaderT(f: ReaderT, r) -> M:
    return f.match(
        readert=lambda a : a(r))

###################################################################
# operations

def asks(m, a) -> ReaderT:
    return ReaderT.READERT(lambda r: _return(m, a))
