#!/usr/bin/env python3

# functional stack
from dataclasses import dataclass
from functools import reduce
from toolz.functoolz import reduce
from pampy import match, _
#from pymonad.maybe import *

Line = int
Column = int
SourceName = str

@dataclass
class SourcePos:
    sourceName: SourceName    # eg: filename
    line: Line
    column: Column

def newPos(sourceName: SourceName, line: Line, column: Column) -> SourcePos:
    return SourcePos(sourceName, line, column)

def initialPos(sourceName: SourceName) -> SourcePos:
    return newPos(sourceName, 1, 1)

def sourceName(sourcePos: SourcePos) -> SourceName:
    return sourcePos.sourceName

def sourceLine(sourcePos: SourcePos) -> Line:
    return sourcePos.line

def sourceColumn(sourcePos: SourcePos) -> Line:
    return sourcePos.column

def incSourceLine(sourcePos: SourcePos, n: Line) -> SourcePos:
    sourcePos.line += n
    return sourcePos

def incSourceColumn(sourcePos: SourcePos, n: Column) -> SourcePos:
    sourcePos.column += n
    return sourcePos

def setSourceName(sourcePos: SourcePos, name: SourceName) -> SourcePos:
    sourcePos.sourceName += name
    return sourcePos

def setSourceLine(sourcePos: SourcePos, line: Line) -> SourcePos:
    sourcePos.line += line
    return sourcePos

def updatePosChar(sourcePos: SourcePos, char: str) -> SourcePos:
    return match(char,
          '\n', lambda a: SourcePos(sourcePos.sourceName, sourcePos.line+1, 1),
          '\t', lambda a: SourcePos(sourcePos.sourceName, sourcePos.line,
                                    mod(sourcePos.column-1, 8)),
          _ , lambda a: SourcePos(sourcePos.sourceName, sourcePos.line, SourcePos.column + 1)
    )

def updatePosString(sourcePos: SourcePos, string: str) -> SourcePos:
    return reduce(updatePosChar, string, sourcePos)
