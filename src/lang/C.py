from typing import Any, List, Dict, Tuple, Callable, TypeVar, Generic
from decimal import Decimal
from adt.decorator import adt
from adt.case import Case
from dataclasses import dataclass

# C built-in
@adt
class CUnaryOperator:
    Negate:                     Case
    Not:                        Case
    BitwiseNot:                 Case

@adt
class CBinaryOperator:
    Add:                        Case
    Subtract:                   Case
    Multiply:                   Case
    Divide:                     Case
    Modulus:                    Case

    EqualTo:                    Case
    NotEqualTo:                 Case
    LessThan:                   Case
    LessThanOrEqualTo:          Case
    Greaterthan:                Case
    GreaterThanOrEqualTo:       Case

    And:                        Case
    Or:                         Case

    BitwiseAnd:                 Case
    BitwiseOr:                  Case
    BitwiseXor:                 Case

    ShiftLeft:                  Case
    ShiftRight:                 Case
    ZeroFillShiftRight:         Case

@adt
class CTy:
    # -----------------------------------------------------------------------------
    # integer
    CTyChar:                    Case
    CTyUChar:                   Case

    CTyShort:                   Case
    CTyUShort:                  Case

    CTyInt:                     Case
    CTyUInt:                    Case

    CTyLong:                    Case
    CTyULong:                   Case

    # -----------------------------------------------------------------------------
    # floating point
    CTyFloat:                   Case
    CTyDouble:                  Case
    CTyLongDouble:              Case

    # -----------------------------------------------------------------------------
    # sugar
    CTyString:                  Case[int]
    CTyBoolean:                 Case

    # -----------------------------------------------------------------------------
    CTyArray:                   Case['CTy', int]
    CTyVoid:                    Case
    CTyAlias:                   Case[str, 'CTy']
    CTyVar:                     Case[str]                       # same with alias but point to any type
    CTyPointer:                 Case['CTy']

    #CTyStruct:                  Case[str, 'CTy']
    CTyStruct:                  Case[List[Tuple[str, 'CTy']]]


@adt
class CTm:
    # -----------------------------------------------------------------------------
    # literal
    # -----------------------------------------------------------------------------
    # numerical
    # integer
    # 1 byte
    CTmChar:                    Case[int]                       # C singed char
    CTmUChar:                   Case[int]                       # C unsigned char

    # 2 bytes
    CTmShort:                   Case[int]                       # C short
    CTmUShort:                  Case[int]                       # C unsigned short

    # 4 bytes
    CTmInt:                     Case[int]                       # C int
    CTmUInt:                    Case[int]                       # C unsigned int

    # 8 bytes
    CTmLong:                    Case[int]                       # C long
    CTmULong:                   Case[int]                       # C unsigned long

    # floating point
    CTmFloat:                   Case[Decimal]
    CTmDouble:                  Case[Decimal]
    CTmLongDouble:              Case[Decimal]

    # -----------------------------------------------------------------------------
    CTmString:                  Case[str]
    CTmBoolean:                 Case[bool]
    CTmArray:                   Case[List['CTm']]
    CTmStruct:                  Case[List[Tuple[str, 'CTm']]]
    CTmPointer:                 Case[str]
    CTmPointerAddr:             Case[str]
    CTmPointerData:             Case[str]

    # -----------------------------------------------------------------------------
    CTmUnary:                   Case[CUnaryOperator, 'CTm']
    CTmBinary:                  Case[CBinaryOperator, 'CTm', 'CTm']

    CTmArrayIndexer:            Case['CTm']                     # C only support single indexing
    CTmVar:                     Case[str]

    CTmVarIntoduction:          Case[str, 'Ctm']
    CTmAssignment:              Case['Ctm', 'CTm']

    CTmFunction:                Case[str, List['CTm'], 'CTm']   # accept list of func args
    CTmBlock:                   Case[List['CTm']]
    CTmReturn:                  Case[List['CTm']]               # return list of p

    CTmFunctionAccessor:        Case[str, 'CTm']                # function accessor/call

    CTmConditional:             Case['CTm', 'Ctm', 'CTm']

    CTmTemplate:                Case[str]
    CApp:                       Case['CTm', List['CTm']]
