#!/usr/bin/env python3

# functional stack
#from dataclasses import dataclass
from typing import TypeVar
from classes import typeclass
#from pymonad.maybe import *

# OOP use only
T = TypeVar('T')
@typeclass
def Unlift(instance) -> T:
    ...
