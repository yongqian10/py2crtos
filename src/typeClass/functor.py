#!/usr/bin/env python3

# functional stack
from typing import Any, Callable, TypeVar
from toolz.functoolz import compose, curry, flip
from classes import typeclass
#from pymonad.maybe import *

F = TypeVar('F')

@typeclass
def fmap(instance: F, func: Callable) -> F:
    ...

@typeclass
def fmap2(instance: F, func: Callable):
    ...


def replaceRight(val: Any, instance: F) -> F:
    return compose(flip(fmap), curry(lambda a,b: a))(val)(instance)


def replaceLeft(instance: F, val: Any) -> F:
    return flip(replaceRight, instance, val)


#@typeclass
#def replaceRight2(val: Any, instance: T) -> T:
#    ...
#
#@typeclass
#def replaceLeft2(val: Any, instance: T) -> T:
#    ...
