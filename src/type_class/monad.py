#!/usr/bin/env python3

# functional stack
from typing import Any, Callable, TypeVar
from classes import typeclass
#from pymonad.maybe import *

M = TypeVar('M')

@typeclass
def _return(instance: Any) -> M:
    ...

@typeclass
def monad(instance: M, func: Callable) -> M:
    ...

@typeclass
def monad2(instance: M, func: Callable) -> M:
    ...
