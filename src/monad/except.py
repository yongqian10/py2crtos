#!/usr/bin/env python3

# functional stack
from typing import Any, Callable, TypeVar
from typing import Generic, Tuple
from adt.decorator import adt
from adt.case import Case
#from pymonad.maybe import *

from src.type_class.monad import _return, monad
from src.type_class.monoid import mappend
from src.monad.either import Either

E = TypeVar('E') # error type
A = TypeVar('A') # any type

# NOTE: exception, just a wrapper of either
@adt
class Except(Generic[E, A]):
    EXCEPT: Case[Either[E, A]]

@monad.instance(Except)
def _monad_Except(instance: Except, func: Callable[[A], Except]):
    return Except.EXCEPT(monad(runExcept(instance), lambda a: runExcept(func(a))))

@_return.instance(Except)
def _return_Except(instance: Except, pure: A):
    return Except.EXCEPT(Either.RIGHT(pure))

def fail(fb):
    return Except.EXCEPT(Either.LEFT(fb))

def runExcept(f: Except) -> Either:
    return f.match(EXCEPT=lambda a: a)

###################################################################
# operations
