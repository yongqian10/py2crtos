#!/usr/bin/env python3

# functional stack
from typing import Any, Callable, List, Tuple
from itertools import chain
from classes import typeclass
#from pymonad.maybe import *

from efx_ipmgr.production_api.monad.maybe import Maybe

from efx_ipmgr.production_api.type_class.foldable import foldr
from efx_ipmgr.production_api.type_class.functor import fmap


@typeclass
def uncons(instance) -> Any:
    ...

@typeclass
def cons(instance) -> Any:
    ...


@fmap.instance(list)
def _fmap_ModuleCommand(instance: list, func: Callable[[Any], list]):
    if not instance:
        return []
    else:
        return list(chain(*[[func(instance[0])], fmap(instance[1:], func)]))

@foldr.instance(list)
def _foldr_list(instance: List[Any], _to: Any, func: Callable[[Any, Any], Any]) -> Any:
    if not instance:
        return _to
    else:
        return foldr(instance[1:], func(instance[0], _to), func)

@cons.instance(str)
def _cons_str(a: str, b: str):
    return a + b

@cons.instance(int)
def _cons_int(a: int, b: int):
    if b == None:
        return a
    else:
        return int(str(a) + str(b))

@cons.instance(list)
def _cons_str(a: list, b: list):
    if b == None:
        return a
    else:
        return list(chain(*[a, b]))

@cons.instance(tuple)
def _cons_tuple(a: tuple, b: List):
    if (b == [(None, None)]):
        return [a]
    else:
        return list(chain(*[[a], b])) # NOTE: since input is lazy


@uncons.instance(str)
def _uncons_str(string: str) -> Maybe[Tuple[str, str]]:
    if string:
        return Maybe.JUST((string[0], string[1:]))
    else:
        return Maybe.NOTHING()

@uncons.instance(list)
def _uncons_list(_list: list) -> Maybe[List[str]]:
        return Maybe.JUST(_list)


## NOTE: stupid impl, a hack to handle eager matching
#@uncons.instance(dict)
#def _uncons_dict(kv: dict) -> Maybe[dict]:
#    if kv:
#        return Maybe.JUST(kv)
#    else:
#        return Maybe.NOTHING("")
#
#@uncons.instance(Either)
#def _uncons_str(a: Either) -> Maybe[Either]:
#        return Maybe.JUST(a)
