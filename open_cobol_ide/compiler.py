import logging
import os
import re
import subprocess
import sys
from enum import IntEnum
from pyqode.core.modes import CheckerMessages
from pyqode.qt import QtCore
from . import system


def _logger():
    return logging.getLogger(__name__)


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
        from .settings import Settings
        ftype = Settings().get_file_type(path)
    except KeyError:
        ftype = FileType.EXECUTABLE
        with open(path, 'r', encoding=encoding) as f:
            if 'PROCEDURE DIVISION USING' in f.read().upper():
                ftype = FileType.MODULE
    _logger().info('%s file type: %r', path, ftype)
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
    EXECUTABLE = 0
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
                    _logger().info('cobc.exe found but not usable.')
            return False
        _logger().info('OpenCobol compiler v.%s' % version)
        return True

    def extension_for_type(self, file_type):
        return self.extensions[int(file_type)]

    def compile(self, file_path, file_type):
        """
        Compiles a file. This is a blocking function, it returns only when
        the compiler process finished.

        :param file_path: Path of the file to compile.
        :param file_type: File type (exe or dll)
        :return: status, list of checker messages

        """
        path, filename = os.path.split(file_path)
        # ensure bin dir exists
        bin_dir = os.path.join(path, 'bin')
        if not os.path.exists(bin_dir):
            os.makedirs(bin_dir)
        # run command using qt process api, this is blocking.
        pgm, options = self.make_command(filename, file_type)
        self._process = QtCore.QProcess()
        self._process.setWorkingDirectory(path)
        self._process.setProcessChannelMode(QtCore.QProcess.MergedChannels)
        self._process.start(pgm, options)
        self._process.waitForFinished()
        status = self._process.exitCode()
        messages = self.parse_output(
            self._process.readAllStandardOutput().data().decode('utf-8'),
            filename)
        _logger().debug(status, messages)
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

    def parse_output(self, compiler_output, filename):
        """
        Parses the compiler output

        :param compiler_output: compiler output
        :param filename: input filename
        :return: list of tuples. Each tuple are made up of the arguments needed
            to create a :class:`pyqode.core.modes.CheckerMessage`.

        """
        _logger().debug('parsing cobc output: %s' % compiler_output)
        retval = []
        exp = r'%s:\d*:.*:.*$' % filename
        prog = re.compile(exp)
        # parse compilation results
        for l in compiler_output.splitlines():
            if prog.match(l):
                _logger().debug('MATCHED')
                status = CheckerMessages.INFO
                filename, line, error_type, desc = l.split(":")
                if 'error' in error_type.lower():
                    status = CheckerMessages.ERROR
                if 'warning' in error_type.lower():
                    status = CheckerMessages.WARNING
                msg = (desc.strip(), status, int(line), 0,
                       None, None, filename)
                _logger().debug('message: %r', msg)
                retval.append(msg)
        return retval
