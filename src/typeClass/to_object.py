#!/usr/bin/env python3

# functional stack
from typing import TypeVar
from classes import typeclass
#from pymonad.maybe import *


T = TypeVar('T')

@typeclass
def toObject(instance: T):
    ...

@toObject.instance(str)
def _toObject_str(instance: str) -> str:
    return instance


@typeclass
def fromObject(instance: T):
    ...

@typeclass
def toSerializedObject(instance: T):
    ...

@typeclass
def fromSerializedObject(instance: T):
    ...
