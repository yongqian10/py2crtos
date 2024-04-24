#!/usr/bin/env python3

# functional stack
from typing import TypeVar
from classes import typeclass
#from pymonad.maybe import *

T = TypeVar('T')
F = TypeVar('F')

@typeclass
def Eval(instance: T) -> F:
    ...
