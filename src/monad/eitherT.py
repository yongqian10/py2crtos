#!/usr/bin/env python3

# functional stack
from typing import Any, Callable, TypeVar
from typing import Generic
from adt.decorator import adt
from adt.case import Case
#from pymonad.maybe import *

from src.type_class.monad import _return, monad
from src.type_class.unlift import Unlift

from src.monad.either import Either


M = TypeVar('M')

@adt
class EitherT(Generic[M]):
    EITHERT: Case[M]

@monad.instance(EitherT)
def _monad_EitherT(instance: EitherT, func: Callable[[Any], EitherT]) -> EitherT:
    return EitherT.EITHERT(instance.match(
        eithert=lambda m: monad(m, lambda e: e.match(
            left=lambda l: _return(m, Either.LEFT(l)),
            right=lambda r: Unlift(func(r))))))

#@_return.instance(EitherT)
#def __return_EitherT(instance: EitherT, pure: Any) -> EitherT:
#    return EitherT.EITHERT(_return(Unlift(instance), pure))

@Unlift.instance(EitherT)
def _unlift_EitherT(instance: EitherT):
    return instance.match(
        eitherT=lambda a : a
    )
