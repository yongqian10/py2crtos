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
    AddGlobalVar        :   Case[str, TwTm]
    AddQueue            :   Case[str, TwTm] # name, length

@adt
class TwTaskCommand:
    AddVar              :   Case[str, TwTm]
    QueueReceive        :   Case[str, TwTm]
    QueueSend           :   Case[str, TwTm]
    PrintString         :   Case[TwTm]

@adt
class TwProg:
    Prog                :   Case[List[TwCommonCommand], List[List[TwTaskCommand]]]
