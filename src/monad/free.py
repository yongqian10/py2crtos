#!/usr/bin/env python3

# functional stack
from typing import Any, Callable
from toolz.functoolz import curry
from classes import typeclass
from adt.decorator import adt
from adt.case import Case
#from pymonad.maybe import *

from src.type_class.functor import fmap
from src.type_class.applicative import applicative
from src.type_class.monad import monad
from src.type_class.foldable import foldMap
from src.type_class.unlift import Unlift

from src.monad.holder import Holder

# NOTE: HKT is not supported hence just use Any
#T = TypeVar('T')
#F = TypeVar('F')

# use FreeMonad instead of building custom monad to standardize command monad
@adt
class Free:
    PURE            :  Case[Any]    # Pure a
    FREE            :  Case[Any]    # Free (f (Free f a))

    # just use Any, generic without advanced type inference is difficult to work with
    # generic not supporting HKT


@fmap.instance(Free)
def _fmap_Free(instance: Free, func: Callable):
    return instance.match(
        pure=lambda a: Free.PURE(a),
        free=lambda a: Free.FREE(fmap(a, func)))


@applicative.instance(Free)
def _applicative_Free(instance1: Free, instance2: Free, func: Callable):
    return instance1.match(
        pure=lambda a: instance2.match(
                pure=lambda b: Free.PURE(func(a, b)),
                free=lambda b: Free.FREE(fmap(b, curry(func)(a=a)))),
        free=lambda a: instance2.match(
                pure=lambda b: Free.FREE(fmap(a, curry(func)(b=b))),
                free=lambda b: Free.FREE(applicative(a, b, func))))


@monad.instance(Free)
def _monad_Free(instance: Free, func: Callable[[Any], Free]):
    return instance.match(
        pure=lambda a: func(a),
        free=lambda a: Free.FREE(fmap(a, CurryRegister(func=func))))

#@monad2.instance(Free)
#def _monad2_Free(instance: Free, func: Callable[[Any], Free]):
#    return instance.match(
#        pure=lambda a: func(a),
#        free=lambda a: Free.FREE(fmap2(a, CurryRegister2(func=func)))
#    )

@curry
def CurryRegister(a: Any, func: Callable[[Any], Free]):
    return monad(a, func)

#@curry
#def CurryRegister2(a: Any, func: Callable[[Any], Free]):
#    return monad2(a, func)

@typeclass
def LiftF(instance) -> Free:
    ...

@typeclass
def Lift(instance, func: Callable) -> Free: # lift callable to Free
    ...

@typeclass
def LiftI(instance, func: Callable) -> Free: # inverse lift callable to Free
    ...

# class(Functor f) => Lift f where
#   LiftSingle :: f (a -> b) -> SinglePasser f
#   LiftTwo :: f (a -> a -> b) -> TwoPasser f

# class(Functor f) => XX f where
#   LiftA :: (a -> b) -> f c
#   LiftB :: (a -> a -> b) -> f c

@foldMap.instance(Free)
def _foldMap_Free(instance: Free, func: Callable[[Any], Holder]) -> Holder:
    return instance.match(
        pure=lambda a: Holder.HOLDER((Free.PURE(a), Free.PURE(a))),
        free=lambda a: foldMap(a, func))

#@foldMap.instance(Free)
#def _foldMap_Free(instance: Free, func: Callable[[Any], Holder]) -> Holder:
#    return instance.match(
#        pure=lambda a: Holder.HOLDER((Free.PURE(a), Free.PURE(a))),
#        free=lambda a: #foldMap(a, func))
#            mappend(foldMap(func(a).match(
#                holder=lambda t: t[-1]), func), func(a)).match(
#                    holder=lambda t: foldMap(t[0], func)))



# monoid impl is very much different with monad
#   using monoid, we allowed to build and fill up expression from right to left
#   while using monad, we build and fill up expression from left to right



## lifter to lift functor to Free
#@LiftF.instance(Callable)
#def _liftF_Callable(instance: Callable):
#    return Free.FREE(fmap(instance, lambda t: t))


@Unlift.instance(Free)
def _unlift_Free(instance: Free):
    return instance.match(
        pure=lambda a : a,
        free=lambda a : a
    )
