#!/usr/bin/env python3

# functional stack
from typing import Callable, TypeVar
from classes import typeclass
#from pymonad.maybe import *

T = TypeVar('T')
F = TypeVar('F') # functor
M = TypeVar('M') # monoid

@typeclass
def foldMap(instance: F, func: Callable[[T], M]) -> M:
    ...

A = TypeVar('A') # acc type

@typeclass
def foldr(instance: F, acc: A, func: Callable[[T, A], A]) -> A:
    ...

@typeclass
def foldl(instance: F, acc: A, func: Callable[[A, T], A]) -> A:
    ...
