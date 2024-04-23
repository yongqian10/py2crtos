from typing import Any, List, Dict
from decimal import Decimal
from adt.decorator import adt
from adt.case import Case

@adt
class TwinTm:
    INTTYPE             :   Case[int]
    BOOLTYPE            :   Case[bool]
    STRINGTYPE          :   Case[str]
    FLOATTYPE           :   Case[Decimal]

    VARTYPE             :   Case[str]

    # TODO support complex type
    #LISTTYPE            :   Case[List['TwinLit']]
    #DICTTYPE            :   Case[Dict[str, 'TwinLit']]

    # TODO: support custom type
    #RECORDTYPE          :   Case[Any]
    #PYBITTYPE           :   Case[int]
    #PYBITSTRINGTYPE     :   Case[PyBitStringType, int]

@adt
class TwinCommonCommand:
    ADDGLOBALVAR        :   Case[TwinTm, TwinTm]
    ADDQUEUE            :   Case[TwinTm, TwinTm] # name, length

@adt
class TwinTaskCommand:
    ADDVAR              :   Case[TwinTm, TwinTm]
    QUEUERECEIVE        :   Case[TwinTm, TwinTm]
    QUEUESEND           :   Case[TwinTm, TwinTm]
    PRINTSTRING         :   Case[TwinTm]

@adt
class Command:
    COMMAND             :   Case[str, List[TwinTm]]     # -> name, list of params

@adt
class TwinProg:
    PROG                :   Case[List[List[Command]]]
