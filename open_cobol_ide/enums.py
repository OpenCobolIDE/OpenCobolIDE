from enum import IntEnum


class FileType(IntEnum):
    """
    Enumerates the different source file types:
        - executable (.exe)
        - module (.dll)
    """
    #: Executable file (produces an executable binary that can be run)
    EXECUTABLE = 0
    #: Module file (produces a shared library that can be used from other
    #: modules or executables)
    MODULE = 1


class GnuCobolStandard(IntEnum):
    """
    Enumerates the different GnuCOBOL standards.
    """
    default = 0
    cobol2002 = 1
    cobol85 = 2
    ibm = 3
    mvs = 4
    bs2000 = 5
    mf = 6
    cobol2014 = 7
    acu = 8
    none = 9
