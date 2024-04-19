#!/usr/bin/env python3

# functional stack
from typing import Any, Callable, TypeVar
from toolz.functoolz import curry
from classes import typeclass
#from pymonad.maybe import *

from efx_ipmgr.production_api.type_class.functor import fmap

F = TypeVar('F')

@typeclass
def pure_applicative(instance: Any) -> F:
    ...

@typeclass
def applicative(instance1: F, instance2: F) -> F:
    ...

@typeclass
def alternate(instance1: F, instance2: F) -> F:
    ...


@typeclass
def empty_alternate() -> F:
    ...


def liftA2(func: Callable[[Any, Any], Any], f1: F, f2: F) -> F:
    return applicative(fmap(f1, func), f2)


#def ignoreLeft(instance1: T, instance2: T) -> T:  # = >>
#    ...

def ignoreRight(instance1: F, instance2: F) -> F:
    return liftA2(curry(lambda a, b: a), instance1, instance2)
