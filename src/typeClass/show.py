#!/usr/bin/env python3

# functional stack
from functools import reduce
from typing import TypeVar
from classes import typeclass
#from pymonad.maybe import *

S = TypeVar('S')

@typeclass
def show(instance: S):
    ...

@show.instance(str)
def _show_str(instance: str) -> str:
    return instance

@show.instance(list)
def _show_list(instance: list) -> list:
    return list(map(lambda x: show(x), instance))
    # POSSIBLE_PERFORMANCE_IMPROVEMENT: return [show(x) for x in instance]

@show.instance(tuple)
def _show_tuple(instance: tuple) -> str:
    return f'({show(instance[0])}, {show(instance[1])})'

@show.instance(dict)
def _show_dict(instance: dict) -> str:
    return f'{dict(map(lambda a: (show(a[0]), show(a[1])), instance.items()))}'

@typeclass
def show2(instance: S):
    ...

@show2.instance(list)
def _show2_list(instance: list, tabs: int=0) -> str:
    return '[' + reduce(lambda x, y: x + ''.join(show(y)) + ',\n\t'.expandtabs(tabs), instance, '') + ']'

@show2.instance(dict)
def _show2_dict(instance: dict, tabs: int=0) -> str:
    return '{' + reduce(lambda x, y: x + ''.join(show(y[0])) + ':' + ''.join(show2(y[1], 30)) + ',\n\t'.expandtabs(tabs), instance.items(), '') + '}'

@typeclass
def show3(instance: S):
    ...

@show3.instance(list)
def _show3_list(instance: list, tabs: int=0) -> str:
    return '[' + reduce(lambda x, y: x + ''.join(show(y)) + ', ', instance, '')[:-2] + ']'
