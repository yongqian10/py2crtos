from adt.decorator import adt
from adt.case import Case
from src.typeClass.show import show

@adt
class SyntaxError:
    EOL                             :   Case[str]
    INVALID                         :   Case[str]
    MISMATCH                        :   Case[str]

class Error:
    SYNTAXERROR                     :   Case[SyntaxError]

@adt
class Warning:
    pass

@adt
class Fb:
    ERROR                           :   Case[Error]
    WARNING                         :   Case[Warning]

@show.instance(SyntaxError)
def _show_SyntaxError(instance: SyntaxError) -> str:
    return instance.match(
                eol=lambda c: f'[error.end_of_line]{c}',
                invalid=lambda c: f'[error.invalid_syntax]{c}',
                mismatch=lambda c: f'[error.mismatch_syntax]{c}')
