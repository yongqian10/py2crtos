#!/usr/bin/env python3


# functional stack
from typing import Any, Callable, Generic, Tuple, TypeVar
from toolz.functoolz import curry
from adt.decorator import adt
from adt.case import Case
#from pymonad.maybe import *

from efx_ipmgr.production_api.type_class.functor import fmap
from efx_ipmgr.production_api.type_class.applicative import applicative
from efx_ipmgr.production_api.type_class.monad import monad
from efx_ipmgr.production_api.type_class.monoid import mappend, mconcat
from efx_ipmgr.production_api.type_class.unlift import Unlift

#from Monad.Free import Lift, Free


T = TypeVar('T')

# Holder serve as default monadic Holder for monadic result with state
@adt
class Holder(Generic[T]):
    HOLDER  :   Case[Tuple[T, T]] #newtype  # T = Free[T]

    # first param for holder serve as type identifier for holder


@fmap.instance(Holder)
def _fmap_Holder(instance: Holder, func: Callable):
    return instance.match(
        holder=lambda a: Holder.HOLDER(func(a)))


@applicative.instance(Holder)
def _applicative_Holder(instance1: Holder, instance2: Holder, func: Callable):
    return instance1.match(
        holder=lambda a: instance2.match(
                holder=lambda b: Holder.HOLDER(func(a, b))))


@monad.instance(Holder)
def _monad_Holder(instance: Holder, func: Callable[[Any], Holder]):
    return instance.match(
        holder=lambda a: Holder.HOLDER(fmap(a, CurryRegister(func=func))))


@curry
def CurryRegister(a: Any, func: Callable):
    return monad(a, func)


# as a monoid instance
# Holder serve as monoid has several uses:
#   1) contain logic to 'fill' partial function using monoid
#   2) serve as accumulator and final result holder for fold ops

#@mempty.instance(Holder)
def mempty_Holder():
    return Holder.HOLDER(None)

@mappend.instance(Holder)
def mappend_Holder(instance1: Holder, instance2: Holder) -> Holder:
    return instance1.match(
        holder=lambda a:
            instance2.match(
                holder=lambda b: Holder.HOLDER((monad(a[0], lambda t: b[0]),
                                                None))
    ))


@mconcat.instance(Holder)
def mconcat_Holder():
    pass

## free Foldable
#@foldMap.instance(Free)
#def _foldMap_Free(instance: Free, func: Callable) -> Holder:
#    return instance.match(
#        pure: lambda a: mempty(),
#        free: lambda a: mappend(func(a), foldMap(func(a)[-1], func))  # use mappend to complete the partial func
#    )


@Unlift.instance(Holder)
def _unlift_Holder(instance: Holder):
    return instance.match(
        holder=lambda a : a
    )
