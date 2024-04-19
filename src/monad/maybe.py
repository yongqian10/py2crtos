#!/usr/bin/env python3

# functional stack
from typing import Callable, Generic, TypeVar, Any
from adt.decorator import adt
from adt.case import Case

from efx_ipmgr.production_api.type_class.functor import fmap
from efx_ipmgr.production_api.type_class.applicative import applicative
from efx_ipmgr.production_api.type_class.show import show, show2
from efx_ipmgr.production_api.type_class.to_object import toObject
from efx_ipmgr.production_api.type_class.monad import monad


T = TypeVar('T')

@adt
class Maybe(Generic[T]):
    JUST            :  Case[T]
    NOTHING         :  Case

@fmap.instance(Maybe)
def _fmap_Maybe(instance: Maybe, func: Callable) -> Maybe:
    return instance.match(
        just= lambda a: Maybe.JUST(func(a)),
        nothing= lambda: Maybe.NOTHING()
    )

@applicative.instance(Maybe)
def _applicative_Maybe(instance1: Maybe[Callable], instance2: Maybe) -> Maybe:
    return instance1.match(
        just= lambda f: instance2.match(
            just= lambda b: Maybe.JUST(f(b)),
            nothing= lambda: Maybe.NOTHING()),
        nothing= lambda: Maybe.NOTHING())

@monad.instance(Maybe)
def _monad_Maybe(instance: Maybe, func: Callable[[Any], Maybe]):
    return instance.match(
        just=lambda a: func(a),
        nothing=lambda : Maybe.NOTHING())

@show.instance(Maybe)
def _show_Maybe(instance: Maybe) -> str:
    return instance.match(
        just=lambda a: show(a),
        nothing=lambda : '')

@show2.instance(Maybe)
def _show2_Maybe(instance: Maybe, tabs: int=0) -> str:
    return instance.match(
        just=lambda a: show2(a, tabs),
        nothing=lambda : '')

@toObject.instance(Maybe)
def _toObject_Maybe(instance: Maybe) -> str:
    return instance.match(
        just=lambda a: toObject(a),
        nothing=lambda : None)
