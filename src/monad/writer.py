#!/usr/bin/env python3

# functional stack
from typing import Any, Callable, TypeVar
from typing import Generic, Tuple
from adt.decorator import adt
from adt.case import Case
#from pymonad.maybe import *

from efx_ipmgr.production_api.type_class.monad import _return, monad
from efx_ipmgr.production_api.type_class.monoid import mappend
from efx_ipmgr.production_api.type_class.unlift import Unlift


W = TypeVar('W') # W has to be monoid
A = TypeVar('A')

@adt
class Writer(Generic[W, A]):
    WRITER: Case[Tuple[A, W]]


@monad.instance(Writer)
def _monad_Writer(instance: Writer, func: Callable[[Any], Writer]):
    return instance.match(
        writer=lambda a:
            func(a[0]).match(
                writer=lambda b:
                    Writer.WRITER((b[0], mappend(a[1], b[1])))))

# FIXME: this instance work for list only
@_return.instance(Writer)
def __return_Writer(instance: Writer, pure: Any):
    return Writer.WRITER((pure, []))   # python not allow None type concat


@Unlift.instance(Writer)
def _unlift_Writer(instance: Writer):
    return instance.match(
        Writer=lambda a : a
    )
