#!/usr/bin/env python3


# functional stack

from typing import Any, Callable, TypeVar
from typing import Generic
from adt.decorator import adt
from adt.case import Case
#from pymonad.maybe import *

from efx_ipmgr.production_api.type_class.monad import _return, monad
from efx_ipmgr.production_api.type_class.unlift import Unlift

from efx_ipmgr.production_api.type_class.show import show

L = TypeVar('L')
R = TypeVar('R')

@adt
class Either(Generic[L, R]):
    LEFT: Case[L]
    RIGHT: Case[R]


@monad.instance(Either)
def _monad_Either(instance: Either, func: Callable[[Any], Either]):
    return instance.match(
        right=lambda a: func(a),
        left=lambda e: Either.LEFT(e)
    )

@_return.instance(Either)
def __return_Either(instance: Any):
    return Either.RIGHT(instance)

@Unlift.instance(Either)
def _unlift_Either(instance: Either):
    return instance.match(
        left=lambda a : a,
        right=lambda a : a
    )

@show.instance(Either)
def _show_Either(instance: Either) -> str:
    return instance.match(
        left=lambda a: show(a),
        right=lambda a: show(a))
