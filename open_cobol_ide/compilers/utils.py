import logging
import sys
from enum import IntEnum
from .gnu_cobol import GnuCobolCompiler
from ..settings import Settings


def _logger():
    return logging.getLogger(__name__)


class CompilerNotFound(Exception):
    """
    Error raised when no compiler were found.
    """
    pass


def check_compiler():
    """
    Checks if a valid cobol compiler could be found.

    :raises: CompilerNotFound if no compiler could be found.

    """
    if not GnuCobolCompiler().is_working():
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
            msg = 'You have to install the package open-cobol using your ' \
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


def get_file_type(path, encoding):
    """
    Detects file type. If the file contains 'PROCEDURE DIVISION USING', then it
    is considered as a module else it is considered as an executable.

    :param path: file path
    :param encoding: file encoding
    :return: FileType
    """
    _logger().debug('detecting file type: %s - %s', path, encoding)
    try:
        ftype = Settings().get_file_type(path)
    except KeyError:
        ftype = FileType.EXECUTABLE
        try:
            with open(path, 'r', encoding=encoding) as f:
                if 'PROCEDURE DIVISION USING' in f.read().upper():
                    ftype = FileType.MODULE
        except IOError or OSError:
            pass
    _logger().info('%s file type: %r', path, ftype)
    return ftype

