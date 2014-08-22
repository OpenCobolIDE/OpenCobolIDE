"""
This package contains the compiler API used by the compilation manager to
compile a series of files.

It also contains utility functions for e.g. parsing the dependencies of a file
or determine the file type (dll vs executable).

"""
from .gnu_cobol import GnuCobolCompiler, GnuCobolStandard
from .utils import check_compiler, get_file_type, FileType, CompilerNotFound
