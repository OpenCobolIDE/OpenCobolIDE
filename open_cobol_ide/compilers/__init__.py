"""
This package contains the compiler API used by the compilation manager to
compile a series of files.

It also contains utility functions for e.g. parsing the dependencies of a file
or determine the file type (dll vs executable).

"""
from enum import IntEnum
import logging
import sys
from ..settings import Settings
from . import gnu_cobol


class CompilerNotFound(Exception):
    pass


def check_compiler():
    """
    Checks if a valid cobol compiler could be found.

    :raises: CompilerNotFound if no compiler could be found.

    """
    if not gnu_cobol.is_working():
        if sys.platform == 'win32':
            msg = 'OpenCobol is bundled with the IDE. Ensure that ' \
                  'the IDE is installed in a path without spaces and ' \
                  'that the OpenCobol folder sits next to the executable.'
        elif sys.platform == 'darwin':
            msg = 'You have to install the package open-cobol using Homebrew ' \
                  'or MacPorts. \n' \
                  'If you installed open-cobol in a ' \
                  'non standard directory, you can specify that directory ' \
                  'in the preferences dialog (build & run tab).'
        else:
            msg = "You have to install the package open-cobol using your " \
                  "distribution's package manager"
        raise CompilerNotFound(msg)


class FileType(IntEnum):
    """
    Enumerates the different source file types:
        - executable (.exe)
        - module (.dll)

    """
    EXECUTABLE = 0
    MODULE = 1


def _logger():
    return logging.getLogger(__name__)


def get_file_type(path, encoding):
    _logger().debug('detecting file type: %s - %s', path, encoding)
    try:
        ftype = Settings().get_file_type(path)
    except KeyError:
        ftype = FileType.EXECUTABLE
        try:
            with open(path, 'r', encoding=encoding) as f:
                lines = f.readlines()
                for l in lines:
                    # This is a subprogram
                    if "PROCEDURE DIVISION USING" in l.upper():
                        ftype = FileType.MODULE
                        break
        except IOError or OSError:
            pass
    _logger().info('%s file type: %r', path, ftype)
    return ftype
