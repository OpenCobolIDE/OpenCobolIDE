"""
This module contains function and classes for interfacing with the GnuCobol
compiler.
"""
import locale
import logging
import os
import re
import subprocess
import sys
from enum import IntEnum
from pyqode.cobol.widgets import CobolCodeEdit
from pyqode.core.cache import Cache
from pyqode.core.modes import CheckerMessages
from pyqode.qt import QtCore
from . import system


def _logger():
    return logging.getLogger(__name__)


def _get_encoding(filename):
    """
    Gets a filename encoding from the pyqode.core cache. If the requested file
    path could not be found in cache, the locale preferred encoding is used.

    :param filename: path of the file in cache.
    :return: cached encoding
    """
    try:
        encoding = Cache().get_file_encoding(filename)
    except KeyError:
        encoding = locale.getpreferredencoding()
        _logger().warning(
            'encoding for %s not found in cache, using locale preferred '
            'encoding instead: %s', encoding)
    return encoding


def get_file_type(path):
    """
    Detects file type. If the file contains 'PROCEDURE DIVISION USING', then it
    is considered as a module else it is considered as an executable.

    :param path: file path
    :return: FileType
    """
    encoding = _get_encoding(path)
    _logger().debug('detecting file type: %s - %s', path, encoding)
    try:
        from .settings import Settings
        ftype = Settings().get_file_type(path)
    except KeyError:
        ftype = FileType.EXECUTABLE
        with open(path, 'r', encoding=encoding) as f:
            if 'PROCEDURE DIVISION USING' in f.read().upper():
                ftype = FileType.MODULE
    _logger().info('file type: %r', ftype)
    return ftype


def check_compiler():
    """
    Checks if a valid cobol compiler can be found.

    :raises: CompilerNotFound if no compiler could be found.
    """
    import sys
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
    #: Executable file (produces an executable binary that can be run)
    EXECUTABLE = 0
    #: Module file (produces a shared library that can be used from other
    #: modules or executables)
    MODULE = 1


class GnuCobolStandard(IntEnum):
    """
    Enumerates the differen cobol standards supported by the GnuCobolCompiler.
    """
    default = 0
    cobol2002 = 1
    cobol85 = 2
    ibm = 3
    mvs = 4
    bs2000 = 5
    mf = 6


class CompilerNotFound(Exception):
    """
    Error raised when no compiler were found.
    """
    pass


class GnuCobolCompiler:
    """
    Provides an interface to the GnuCobol compiler (cobc)
    """

    def __init__(self):
        #: platform specifc extensions, sorted per file type
        self.extensions = [
            # no extension for exe on linux and mac
            '.exe' if system.windows else '',
            # dll on windows, so everywhere else
            '.dll' if system.windows else '.so'
        ]

    @staticmethod
    def get_version():
        """
        Returns the GnuCobol compiler version as a string
        """
        cmd = ['cobc', '--version']
        try:
            _logger().debug('getting cobc version: %s' % ' '.join(cmd))
            if sys.platform == 'win32':
                startupinfo = subprocess.STARTUPINFO()
                startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
                p = subprocess.Popen(
                    cmd, shell=False, startupinfo=startupinfo,
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                    stdin=subprocess.PIPE)
            else:
                p = subprocess.Popen(cmd, shell=False, stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE)
        except OSError:
            _logger().exception('OpenCobol compiler not found')
            return 'Not installed'
        else:
            stdout, stderr = p.communicate()
            stdout = str(stdout)
            lversion = stdout.splitlines()[0]
            _logger().debug('parsing version line: %s' % lversion)
            prog = re.compile(r'\d.\d.\d')
            for v in prog.finditer(lversion):
                s, e = v.span()
                return lversion[s: e]

    def is_working(self):
        """
        Checks if the GNU Cobol compiler is working.
        """
        version = self.get_version()
        if version == 'Not installed':
            if sys.platform == 'win32':
                expected_root_path = os.path.join(os.getcwd(), 'OpenCobol')
                expected_cobc_path = os.path.join(
                    expected_root_path, 'bin', 'cobc.exe')
                if not os.path.exists(expected_root_path):
                    _logger().warning(
                        '%s does not exists' % expected_root_path)
                elif not os.path.exists(expected_cobc_path):
                    _logger().warning(
                        '%s does not exists' % expected_cobc_path)
                else:
                    _logger().warning('cobc.exe found but not usable.')
            return False
        _logger().info('OpenCobol compiler v.%s' % version)
        return True

    def extension_for_type(self, file_type):
        """
        Returns a platform specific extension for the specified file type.

        :param file_type: file type
        :return: extension
        """
        return self.extensions[int(file_type)]

    def compile(self, file_path, file_type):
        """
        Compiles a file. This is a blocking function, it returns only when
        the compiler process finished.

        :param file_path: Path of the file to compile.
        :param file_type: File type (exe or dll)
        :return: status, list of checker messages

        """
        _logger().info('compiling %s' % file_path)
        path, filename = os.path.split(file_path)
        # ensure bin dir exists
        bin_dir = os.path.join(path, 'bin')
        if not os.path.exists(bin_dir):
            os.makedirs(bin_dir)
        # run command using qt process api, this is blocking.
        pgm, options = self.make_command(filename, file_type)
        process = QtCore.QProcess()
        process.setWorkingDirectory(path)
        process.setProcessChannelMode(QtCore.QProcess.MergedChannels)
        process.start(pgm, options)
        process.waitForFinished()
        status = process.exitCode()
        messages = self.parse_output(
            process.readAllStandardOutput().data().decode('utf-8'),
            file_path)
        _logger().debug('compile results: %r - %r', status, messages)
        return status, messages

    def make_command(self, input_file_name, file_type):
        """
        Makes the command needed to compile the specified file.

        :param input_file_name: Input file name (without path)
        :param output_file_name: Output file base name (without path and
            extension). None to use the input_file_name base name.
        :param file_type: file type (exe or dll).

        :return: a tuple made up of the program name and the command arguments.
        """
        from .settings import Settings
        settings = Settings()
        output_file_name = os.path.splitext(input_file_name)[0]
        options = []
        if file_type == FileType.EXECUTABLE:
            options.append('-x')
        output_file_name += self.extension_for_type(file_type)
        options.append('-o %s' % (os.path.join('bin', output_file_name)))
        options.append('-std=%s' % str(settings.cobol_standard).replace(
            'GnuCobolStandard.', ''))
        if settings.free_format:
            options.append('-free')
        options.append(input_file_name)
        return 'cobc', options

    @staticmethod
    def parse_output(compiler_output, file_path):
        """
        Parses the compiler output

        :param compiler_output: compiler output
        :param filename: input filename
        :return: list of tuples. Each tuple are made up of the arguments needed
            to create a :class:`pyqode.core.modes.CheckerMessage`.

        """
        _logger().debug('parsing cobc output: %s' % compiler_output)
        retval = []
        exp = r'%s:\d*:.*:.*$' % os.path.split(file_path)[1]
        prog = re.compile(exp)
        # parse compilation results
        for l in compiler_output.splitlines():
            if prog.match(l):
                _logger().debug('MATCHED')
                status = CheckerMessages.INFO
                fn, line, error_type, desc = l.split(":")
                if 'error' in error_type.lower():
                    status = CheckerMessages.ERROR
                if 'warning' in error_type.lower():
                    status = CheckerMessages.WARNING
                msg = (desc.strip(), status, int(line) - 1, 0,
                       None, None, file_path)
                _logger().debug('message: %r', msg)
                retval.append(msg)
        return retval

    def get_dependencies(self, filename, recursive=True):
        """
        Gets the dependencies of a cobol program/module.

        :param filename: path of the file to analyse.
        :param recursive: True to perform recursive analysis (analyses
            dependencies of dependencies recursively).
        :return: The set of dependencies that needs to be compiled to compile
            and use the requested program/module.
        """
        encoding = _get_encoding(filename)
        directory = os.path.dirname(filename)
        dependencies = []
        prog = re.compile(r'CALL ".*"')
        with open(filename, 'r', encoding=encoding) as f:
            for line in f.readlines():
                match = prog.search(line)
                if match:
                    start, end = match.span()
                    txt = line[start:end]
                    module_base_name = txt[txt.find('"'):].replace('"', '')
                    # try to see if the module can be found in the current
                    # directory
                    for ext in CobolCodeEdit.all_extensions():
                        pth = os.path.join(directory, module_base_name + ext)
                        if os.path.exists(pth) and pth not in dependencies:
                            dependencies.append(pth)
                            if recursive:
                                dependencies += self.get_dependencies(pth)
        dependencies = list(set(dependencies))
        _logger().debug('dependencies of %s: %r', filename, dependencies)
        return dependencies
