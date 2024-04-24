from typing import Any, List, Dict
from decimal import Decimal
from adt.decorator import adt
from adt.case import Case

@adt
class TwTm:
    TmInt               :   Case[int]
    TmBool              :   Case[bool]
    TmString            :   Case[str]
    TmFloat             :   Case[Decimal]
    TmVar               :   Case[str]

    # TODO support complex type
    #LISTTYPE            :   Case[List['TwLit']]
    #DICTTYPE            :   Case[Dict[str, 'TwLit']]

    # TODO: support custom type
    #RECORDTYPE          :   Case[Any]
    #PYBITTYPE           :   Case[int]
    #PYBITSTRINGTYPE     :   Case[PyBitStringType, int]

@adt
class TwCommonCommand:
    AddGlobalVar        :   Case[TwTm, TwTm]
    AddQueue            :   Case[TwTm, TwTm] # name, length

@adt
class TwTaskCommand:
    AddVar              :   Case[TwTm, TwTm]
    QueueReceive        :   Case[TwTm, TwTm]
    QueueSend           :   Case[TwTm, TwTm]
    PrintString         :   Case[TwTm]

@adt
class TwCommand:
    Command             :   Case[str, List[TwTm]]     # -> name, list of params

@adt
class TwProg:
    Prog                :   Case[List[List[TwCommand]]]
