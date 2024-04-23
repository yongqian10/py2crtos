from typing import Any, List, Dict
from src.type_class.monad import monad
from src.parser.parser import Parser, chainWithSkip, parseSaidChar, satisfy, separatedBy, parseParens, parseString, parseAnyExceptSome
from src.parser.parser import bail, identity
from src.type.twinTy import TwinProg, Command
from src.type_class.applicative import alternate, ignoreRight
from src.type.fbTy import SyntaxError

###########################################################################
# parse main.twin to graph
def parseExceptReservedChar() -> Parser[str]:
    return parseAnyExceptSome(['(', '!', ',', '.', ')', '*' ,'/', '^', '&', '|', '$', '@', '%', '"', "'", '>', '<', '?', '{', '}', '[', ']'])

def parseIdentifier():
    return parseExceptReservedChar()

def parseLit():
    return alternate(monad(parseString2(), lambda a:
                          identity(ArithmeticExpr.LIT(HDLType.HDLSTRINGTYPE(a)))),
           alternate(monad(parseBool(), lambda a:
                          identity(ArithmeticExpr.LIT(HDLType.HDLBITTYPE(int(a))))),
           alternate(monad(parseReal(), lambda a:
                         identity(ArithmeticExpr.LIT(a))),
           alternate(monad(parseIdentifier(), lambda a:
                         identity(ArithmeticExpr.LIT(a))),
           alternate(monad(parseArray(), lambda a:
                         identity(ArithmeticExpr.LIT(a))),
           alternate(monad(parseRecord(), lambda a:
                         identity(ArithmeticExpr.LIT(a))),
           monad(parseInt32(), lambda a:
                         identity(ArithmeticExpr.LIT(HDLType.HDLINTTYPE(a))))))))

def parseCommand():
    def _parseArgs():
        return monad(separatedBy(monad(parseLit(), lambda b:
                                       identity([b])), parseSaidChar(',')), lambda arglist: identity(arglist))

    return monad(parseIdentifier(), lambda identifier:
                 monad(parseParens(_parseArgs()), lambda args:
                       identity(Command.COMMAND(identifier, args))))

def parseCommandList() -> Parser[List[Command]]:
    return ignoreRight(chainWithSkip(parseSaidChar('['),
                 alternate(chainWithSkip(parseSaidChar(']'), bail(SyntaxError.INVALID('List cannot be empty'))),
                                         separatedBy(monad(parseCommand(), lambda b: identity([b])), parseSaidChar(',')))),
                                                satisfy(lambda a: a == ']', f'expecting closing bracket, ]'))

def parseTwinApp() -> Parser[TwinProg]:
    return ignoreRight(chainWithSkip(parseSaidChar('['),
                 alternate(chainWithSkip(parseSaidChar(']'), bail(SyntaxError.INVALID('List cannot be empty'))),
                                         separatedBy(monad(parseCommandList(), lambda b: identity([b])), parseSaidChar(',')))),
                                                satisfy(lambda a: a == ']', f'expecting closing bracket, ]'))
