"""
Contains application constants
"""
import pyqode.core
# cobol use - extensively for complex identifier, don't break them!
import sys

pyqode.core.constants.WORD_SEPARATORS.remove("-")


ICON_PARAGRAPH = ":/ide-icons/rc/paragraph"
ICON_VAR = ":/ide-icons/rc/var"
ICON_KEYWORD = ":/ide-icons/rc/keyword"

EXECUTABLE_EXTENSION = ".exe"
MODULE_EXTENSION = ".so"
if sys.platform == "win32":
    MODULE_EXTENSION = ".dll"

class ProgramType:
    """
    Enumerates the supported file types along with their base compile command
    format
    """
    #: Cobol program (executable compiled with -x switch)
    Executable = (0, 'cobc -x {0} -o {1} {2}', EXECUTABLE_EXTENSION)
    #: Cobol subprogram (shared object/dll compiled without the -x switch)
    Module = (1, 'cobc {0} -o {1} {2}', MODULE_EXTENSION)
