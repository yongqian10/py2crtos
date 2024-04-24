#!/usr/bin/env python3

# functional stack
from typing import Any, List, TypeVar
from classes import typeclass
#from pymonad.maybe import *

M = TypeVar('M')   # T is monad

#@typeclass
#def mempty() -> T:
#    ...


@typeclass
def mappend(instance1: M, instance2: M) -> M:
    ...


@typeclass
def mconcat(instance1: List[M]) -> M:
    ...



@mappend.instance(str)
def _mappend_str(w1: str, w2: str) -> str:
    return w1 + w2


@mappend.instance(list)
def _mappend_list(w1: List[Any], w2: List[Any]) -> List[Any]:
    return w1 + w2
