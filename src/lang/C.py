from typing import Any, List, Dict, Tuple, Callable, TypeVar, Generic
from adt.decorator import adt
from adt.case import Case
from dataclasses import dataclass

# C built-in
@adt
class CUnaryOperator:
    Negate:                 Case
    Not:                    Case
    BitwiseNot:             Case

@adt
class CBinaryOperator:
    Add:                    Case
    Subtract:               Case
    Multiply:               Case
    Divide:                 Case
    Modulus:                Case

    EqualTo:                Case
    NotEqualTo:             Case
    LessThan:               Case
    LessThanOrEqualTo:      Case
    Greaterthan:            Case
    GreaterThanOrEqualTo:   Case

    And:                    Case
    Or:                     Case

    BitwiseAnd:             Case
    BitwiseOr:              Case
    BitwiseXor:             Case

    ShiftLeft:              Case
    ShiftRight:             Case
    ZeroFillShiftRight:     Case

@adt
class CTy:
    CTyInt8:                    Case
    CTyInt32:                   Case

@adt
class CTm:
    # literal
    CTmNumericLiteral:        Case[int]
    CTmStringLiteral:         Case[str]
    CTmBooleanLiteral:        Case[bool]
    CTmArrayLiteral:          Case[List['C']]
    CTmStructLiteral:         Case[List[Tuple[str, 'C']]]

    CTmUnary:                 Case[CUnaryOperator, 'C']
    CTmBinary:                Case[CBinaryOperator, 'C', 'C']

    CTmArrayIndexer:          Case['C', 'C']

    CTmTemplateLiteral:       Case[str]
    CTmAssignment:            Case['C', 'C']
    CTmVar:                   Case[str]

    CTmFunction:              Case[str, List['C'], 'C']   # accept list of func args
    CTmBlock:                 Case[List['C']]
    CTmReturn:                Case[List['C']]             # return list of p
