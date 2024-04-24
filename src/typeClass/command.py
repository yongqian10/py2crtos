#!/usr/bin/env python3

# functional stack
from typing import TypeVar
from classes import typeclass

T = TypeVar('T')
F = TypeVar('F')

@typeclass
def Interpreter(instance: T) -> F:
    ...
