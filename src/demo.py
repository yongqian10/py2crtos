import re
from networkx import DiGraph
from typing import Any, List, Dict
from src.typeClass.monad import monad
from src.typeClass.applicative import alternate, ignoreRight
from src.parser.parser import Parser, ParseState, chainWithSkip, parseSaidChar, satisfy, separatedBy, parseParens, parseString, parseAnyExceptSome, parseInt32, parseString2
from src.parser.parser import bail, identity, runParser
from src.type.twinTy import TwProg, TwTm, TwCommand
from src.type.fbTy import SyntaxError

###########################################################################
# parser
def parseExceptReservedChar() -> Parser[str]:
    return parseAnyExceptSome(['=','(', '!', ',', '.', ')', '*' ,'/', '^', '&', '|', '$', '@', '%', '"', "'", '>', '<', '?', '{', '}', '[', ']'])

def parseIdentifier():
    return parseExceptReservedChar()

def parseTwTm():
           # TODO this a sandbox we want to make it with primitive type only
    return alternate(monad(parseInt32(), lambda a: identity(TwTm.TmInt(a))),
           alternate(monad(parseString2(), lambda a: identity(TwTm.TmString(a))),
           monad(parseIdentifier(), lambda a: identity(TwTm.TmVar(a)))))

def parseTwCommand():
    def _parseArgs():
        return monad(separatedBy(monad(parseTwTm(), lambda b:
                                       identity([b])), parseSaidChar(',')), lambda arglist: identity(arglist))

    return monad(parseIdentifier(), lambda identifier:
                 monad(parseParens(_parseArgs()), lambda args:
                       identity(TwCommand.Command(identifier, args))))

def parseTwCommandList() -> Parser[List[TwCommand]]:
    return ignoreRight(chainWithSkip(parseSaidChar('['),
                 alternate(chainWithSkip(parseSaidChar(']'), bail(SyntaxError.INVALID('List cannot be empty'))),
                                         separatedBy(monad(parseTwCommand(), lambda b: identity([b])), parseSaidChar(',')))),
                                                satisfy(lambda a: a == ']', f'expecting closing bracket, ]'))

def parseTwApp() -> Parser[TwProg]:
    return monad(parseIdentifier(), lambda a: chainWithSkip(parseSaidChar('='), ignoreRight(chainWithSkip(parseSaidChar('['),
                 alternate(chainWithSkip(parseSaidChar(']'), bail(SyntaxError.INVALID('List cannot be empty'))),
                                         separatedBy(monad(parseTwCommandList(), lambda b: identity([b])), parseSaidChar(',')))),
                                                satisfy(lambda a: a == ']', f'expecting closing bracket, ]'))))

###########################################################################
# main

# remove whitespace
def formatExprString(expr: str) -> str:
    expr = re.sub(r"[\n\t\s]*", "", expr)
    return expr

def sandboxCompile(twFile):
    f = open(twFile, 'r')
    code = f.read()
    f.close()
    print(formatExprString(code))
    res = runParser(parseTwApp())(ParseState(string=formatExprString(code),
                                              offset=0,
                                              parenthesisCounter=0,
                                              withinBracketCounter=0,
                                              withinConditionalCounter=0,
                                              withinFunctionArg=False,
                                              graph=DiGraph()))

    print(res)

if __name__=="__main__":
    sandboxCompile('./main.twin')
