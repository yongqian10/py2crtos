from typing import Any, List, Dict, Tuple
from decimal import Decimal
from adt.decorator import adt
from adt.case import Case
from src.type.twTy import TwTy

@adt
class TwTm:
    INT                 :   Case[int]
    BOOL                :   Case[bool]
    STRING              :   Case[str]
    FLOAT               :   Case[Decimal]
    VAR                 :   Case[str]

    UNDEFINED           :   Case
    UNIT                :   Case
    LISTTYPE            :   Case[List['TwTm']]
    DICTTYPE            :   Case[Dict[str, 'TwTm']]

    FUNCTION            :   Case[List[Tuple[str, TwTy]], 'TwTm', TwTy]
    INTRO               :   Case[str, 'TwTm', TwTy]

    #PREFIXOPS           :   Case[]

    # TODO: support custom type
    #RECORDTYPE          :   Case[Any]

@adt
class TwCommonCommand:
    ADDGLOBALVAR        :   Case[str, TwTm]
    ADDQUEUE            :   Case[str, TwTm] # name, length

@adt
class TwTaskCommand:
    ADDVAR              :   Case[str, TwTm]
    QUEUERECEIVE        :   Case[str, TwTm]
    QUEUESEND           :   Case[str, TwTm]
    PRINTSTRING         :   Case[TwTm]

@adt
class TwProg:
    PROG                :   Case[List[TwCommonCommand], List[List[TwTaskCommand]]]
