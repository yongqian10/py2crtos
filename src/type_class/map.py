#!/usr/bin/env python3

# functional stack
from typing import Any, List, Tuple

from efx_ipmgr.production_api.monad.maybe import Maybe

from efx_ipmgr.production_api.type_class.foldable import foldr

def findKey(key: str, kvs: List[Tuple[str, Any]]) -> Maybe[Any]:
    return foldr(kvs, Maybe.NOTHING(None),
        lambda kv, acc: Maybe.JUST(kv[1] if key == kv[0] else acc))
