from typing import Any, List, Dict, Tuple
from dataclasses import dataclass
from src.typeClass.monad import monad
from src.monad.rwsT import RwsT
from src.monad._except import Except
from src.type.twTy import TwTm

@dataclass
class Ctx:
    tmBindEnv: Dict[str, TwTm]

@adt
class CodeGen():
    CODEGEN: Case[Callable[[Ctx, Int], Except[str, Tuple[Any, Int, Unit]]]]

def codegen():
    pass
