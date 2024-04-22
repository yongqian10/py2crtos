from typing import Any, List, Dict
from src.type_class.monad import monad
from src.parser.parser import Parser, chainWithSkip, parseSaidChar, satisfy, separatedBy, parseParens, parseString, parseAnyExceptSome
from src.parser.parser import bail, identity
from src.type.twinAst import TwinAst, TwinLit, TwinTaskCommand
from src.type_class.applicative import alternate, ignoreRight
from src.type.fb import SyntaxError

###########################################################################
# parse main.twin to graph
def parseExceptReservedKeyword() -> Parser[str]:
    return parseAnyExceptSome(['(', '!', ',', '.', ')', '*' ,'/', '^', '&', '|', '$', '@', '%', '"', "'", '>', '<', '?', '{', '}', '[', ']'])

def parseIdentifier():
    return

def parseCommand():
    def _parseArgs():
        return monad(separatedBy(monad(parseLit(), lambda b:
                                       identity([b])), parseSaidChar(',')), lambda arglist: identity(arglist))

    return monad(parseIdentifier(), lambda identifier:
                 monad(parseParens(_parseArgs()), lambda args:
                       identity(Command.COMMAND(identifier, args))))

def parseCommandList() -> Parser[List[TwinTaskCommand]]:

    return ignoreRight(chainWithSkip(parseSaidChar('['),
                 alternate(chainWithSkip(parseSaidChar(']'), bail(SyntaxError.INVALID('List cannot be empty'))),
                                         separatedBy(monad(parseCommand(), lambda b: identity([b])), parseSaidChar(',')))),
                                                satisfy(lambda a: a == ']', f'expecting closing bracket, ]'))

def parseTwinApp() -> Parser[TwinProg]:
    return ignoreRight(chainWithSkip(parseSaidChar('['),
                 alternate(chainWithSkip(parseSaidChar(']'), bail(SyntaxError.INVALID('List cannot be empty'))),
                                         separatedBy(monad(parseCommandList(), lambda b: identity([b])), parseSaidChar(',')))),
                                                satisfy(lambda a: a == ']', f'expecting closing bracket, ]'))



#def parseAddVarCommand():
#    def _parseArgs():
#        return monad(separatedBy(monad(parseLit(), lambda b:
#                                       identity([b])), parseSaidChar(',')), lambda arglist:
#                     identity(arglist) if len(arglist) == 2 else bail(SyntaxError.INVALID(f'addVar accepts 2 args, but {len(arglist)} is supplied.')))
#
#    return chainWithSkip(parseString('addVar'), monad(parseParens(_parseArgs()), lambda args:
#                                                     identity(TwinTaskCommand.ADDVAR(args[0], args[1]))))
#
#def parseQueueReceiveCommand():
#    def _parseArgs():
#        return monad(separatedBy(monad(parseExceptReservedKeyword(), lambda b:
#                                       identity([b])), parseSaidChar(',')), lambda arglist:
#                     identity(arglist) if len(arglist) == 2 else bail(SyntaxError.INVALID(f'addVar accepts 2 args, but {len(arglist)} is supplied.')))
#
#    return chainWithSkip(parseString('QueueReceive'), monad(parseParens(_parseArgs()), lambda args:
#                                                     identity(TwinTaskCommand.QUEUERECEIVE(args[0], TwinLit.VARTYPE(args[1])))))
