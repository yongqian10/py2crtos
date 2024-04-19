#!/usr/bin/env python3

from toolz.functoolz import curry

class IO:
    def __init__(self, action):
        self.action = action

    def __rshift__(self, func):
        return func(self.action())

    @staticmethod
    def unit(v):            # unit(None)
        return IO(lambda: v)

    @staticmethod
    def putStrLn(text):
        return IO(lambda: print(text))

    @staticmethod
    def OpenFile(path):
        return IO(lambda: open(path, 'r+'))

    @staticmethod
    def OpenFileToAppend(path):
        return IO(lambda: open(path, 'a+'))

    @staticmethod
    @curry
    def putStrLnToFile(file, text):
        return IO(lambda: file.write(text))

    @staticmethod
    def getLine():
        return IO(lambda: input())

    @staticmethod
    def getLineFromFile(file):
        return IO(lambda: file.readline())



#v = IO.putStrLn("Hello")
#
def dummy(x):
    pass # do nothing

#IO.getLine() >> (lambda text: IO.putStrLn(text)) >> dummy



# unit() usage
#promptTwoLines = (lambda prompt1, prompt2:
#    promptLine(prompt1) >> (lambda line1:
#        promptLine(prompt2) >> (lambda line2:
#            IO.unit(line1 + " and " + line2)
#        )
#    )
#)

# read/write file IO monad test
# open file and append
#IO.OpenFileToAppend('./test/testfile.md') >> (lambda file: IO.getLine()
#                                  >> (lambda text: IO.putStrLnToFile(file, text))
#                                  >> dummy)
#
## open file and read
#IO.OpenFile('./test/testfile.md') >> (lambda file: IO.getLineFromFile(file)
#                                  >> (lambda text: IO.putStrLn(text))
#                                  >> dummy)
