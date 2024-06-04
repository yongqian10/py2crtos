from typing import Any, List, Dict, Tuple, Callable, TypeVar, Generic
from decimal import Decimal
from adt.decorator import adt
from adt.case import Case
from dataclasses import dataclass

from src.monad.maybe import Maybe

# C built-in
@adt
class CPrefixOperator:
    NEGATE:                     Case
    NOT:                        Case
    BITWISENOT:                 Case

@adt
class CInfixOperator:
    ADD:                        Case
    SUBTRACT:                   Case
    MULTIPLY:                   Case
    DIVIDE:                     Case
    MODULUS:                    Case

    EQUALTO:                    Case
    NOTEQUALTO:                 Case
    LESSTHAN:                   Case
    LESSTHANOREQUALTO:          Case
    GREATERTHAN:                Case
    GREATERTHANOREQUALTO:       Case

    AND:                        Case
    OR:                         Case

    BITWISEAND:                 Case
    BITWISEOR:                  Case
    BITWISEXOR:                 Case

    SHIFTLEFT:                  Case
    SHIFTRIGHT:                 Case
    ZEROFILLSHIFTRIGHT:         Case

@adt
class CTy:
    VOID:                    Case
    # -----------------------------------------------------------------------------
    # integer
    CHAR:                    Case
    UCHAR:                   Case

    SHORT:                   Case
    USHORT:                  Case

    INT:                     Case
    UINT:                    Case

    LONG:                    Case
    ULONG:                   Case

    # -----------------------------------------------------------------------------
    # floating point
    FLOAT:                   Case
    DOUBLE:                  Case
    LONGDOUBLE:              Case

    # -----------------------------------------------------------------------------
    # sugar
    STRING:                  Case[int]
    BOOLEAN:                 Case

    # -----------------------------------------------------------------------------
    ARRAY:                   Case['CTy', int]
    #CTYALIAS:                   Case[str, 'CTy']
    #CTYVAR:                     Case[str]                       # same with alias but point to any type
    POINTER:                 Case['CTy']

    #CTYSTRUCT:                  Case[str, 'CTy']
    STRUCT:                  Case[List[Tuple[str, 'CTy']]]


@adt
class CTm:
    # -----------------------------------------------------------------------------
    # literal
    # -----------------------------------------------------------------------------
    # numerical
    # integer
    # 1 byte
    CHAR:                    Case[int]                       # C singed char
    UCHAR:                   Case[int]                       # C unsigned char

    # 2 bytes
    SHORT:                   Case[int]                       # C short
    USHORT:                  Case[int]                       # C unsigned short

    # 4 bytes
    INT:                     Case[int]                       # C int
    UINT:                    Case[int]                       # C unsigned int

    # 8 bytes
    LONG:                    Case[int]                       # C long
    ULONG:                   Case[int]                       # C unsigned long

    # floating point
    FLOAT:                   Case[Decimal]
    DOUBLE:                  Case[Decimal]
    LONGDOUBLE:              Case[Decimal]

    # -----------------------------------------------------------------------------
    STRING:                  Case[str]
    BOOLEAN:                 Case[bool]
    ARRAY:                   Case[List['CTm']]
    STRUCT:                  Case[List[Tuple[str, 'CTm']]]
    POINTER:                 Case[str]
    POINTERADDR:             Case[str]
    POINTERDATA:             Case[str]

    # -----------------------------------------------------------------------------
    # operation call
    PREFIX:                  Case[CPrefixOperator, 'CTm']
    INFIX:                   Case[CInfixOperator, 'CTm', 'CTm']

    # -----------------------------------------------------------------------------
    # function
    FUNCTION:                Case[str, List[Tuple['CTm', 'CTy']], Tuple['CTm', 'CTy']]   # accept list of func args
    BLOCK:                   Case[List['CTm']]               # expression enclose in {}
    RETURN:                  Case[List['CTm']]               # return list of p

    FUNCTIONACCESSOR:        Case[str, 'CTm']                # function accessor/call

    # -----------------------------------------------------------------------------
    ARRAYINDEXER:            Case['CTm']                     # C only support single indexing
    VAR:                     Case[str]

    VARINTODUCTION:          Case[str, 'CTy', 'Ctm']
    VARCOPYASSIGNMENT:      Case['CTm', 'CTm']
    VARREFASSIGNMENT:       Case['CTm', 'CTm']

    # -----------------------------------------------------------------------------
    # flow control
    CONDITIONAL:             Case['CTm', 'Ctm', 'CTm']       # if else in expression
    IFELSE:                  Case['CTm', 'CTm', Maybe['CTm']]

    # -----------------------------------------------------------------------------
    # loop
    WHILE:                   Case['Ctm', 'CTm']
    FOR:                     Case['Ctm', 'CTm', 'CTm']       # left, right, inc/dec expression
    #FORIN:                   Case[str, 'CTm', 'CTm']

    BREAK:                   Case[str]
    CONTINUE:                Case[str]


    # -----------------------------------------------------------------------------
    # main
    #CTmTemplate:                Case[str]
    APP:                     Case['CTm', List['CTm']]        # return, list of args
