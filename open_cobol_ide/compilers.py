"""
This module contains function and classes for interfacing with the GnuCobol
compiler.
"""
import glob
import locale
import logging
import os
import re
import subprocess
import sys
import shutil
import tempfile

from pyqode.cobol.widgets import CobolCodeEdit
from pyqode.core.cache import Cache
from pyqode.core.modes import CheckerMessages
from pyqode.qt import QtCore

from open_cobol_ide import system
from open_cobol_ide.enums import FileType
from open_cobol_ide.settings import Settings


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
            'encoding instead: %s', filename, encoding)
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
    _logger().debug('file type: %r', ftype)
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
            msg = 'You have to install the package open-cobol using Homebrew '\
                  'or MacPorts. \n' \
                  'If you installed open-cobol in a ' \
                  'non standard directory, you can specify that directory ' \
                  'in the preferences dialog (build & run tab).'
        else:
            msg = 'You have to install the package open-cobol using your ' \
                  "distribution's package manager"
        raise CompilerNotFound(msg)


class CompilerNotFound(Exception):
    """
    Error raised when no compiler were found.
    """
    pass


class VisualStudioWrapperBatch:
    """
    Helps creating a wrapper batch that setup visual studio command line
    environment before running the cobc command, this is needed to use the
    kiska builds on Windows.
    """
    CODE_TEMPLATE = """set OLDDIR=%CD%
chdir {0}
call VCVARS32
chdir /d %OLDDIR%
call cobc %*
"""
    FILENAME = 'cobc_wrapper.bat'

    @classmethod
    def path(cls):
        return os.path.join(tempfile.gettempdir(), cls.FILENAME)

    @classmethod
    def generate(cls):
        with open(cls.path(), 'w') as f:
            f.write(cls.CODE_TEMPLATE.format(
                os.path.dirname(Settings().vcvars32)))


class GnuCobolCompiler:
    """
    Provides an interface to the GnuCobol compiler (cobc)
    """
    EXTENSIONS = [".COB", ".CBL", ".PCO", ".CPY"]

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

    def make_bin_dir(self, path):
        bin_dir = os.path.join(path, 'bin')
        if not os.path.exists(bin_dir):
            os.makedirs(bin_dir)
        if sys.platform == "win32":
            # copy the dll
            files = glob.glob(os.path.join(os.environ["COB_LIBRARY_PATH"],
                                           "*.dll"))
            for f in files:
                shutil.copy(f, bin_dir)

    def compile(self, file_path, file_type, object_files=None,
                additional_options=None):
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
        self.make_bin_dir(path)
        # run command using qt process api, this is blocking.
        if object_files:
            inputs = [filename] + object_files
        else:
            inputs = [filename]
        pgm, options = self.make_command(inputs, file_type, additional_options)
        process = QtCore.QProcess()
        process.setWorkingDirectory(path)
        process.setProcessChannelMode(QtCore.QProcess.MergedChannels)
        _logger().info('command: %s %s', pgm, ' '.join(options))
        _logger().debug('working directory: %s', path)
        _logger().debug('system environment: %s', process.systemEnvironment())
        process.start(pgm, options)
        process.waitForFinished()
        status = process.exitCode()
        output = process.readAllStandardOutput().data().decode(
            locale.getpreferredencoding())
        messages = self.parse_output(output, file_path)
        _logger().info('output: %s', output)
        if status != 0 and not len(messages):
            # compilation failed but the parser failed to extract cobol related
            # messages, there might be an issue at the C level or at the
            # linker level
            messages.append((output, CheckerMessages.ERROR, - 1, 0,
                             None, None, file_path))
        _logger().debug('compile results: %r - %r', status, messages)
        return status, messages

    def make_command(self, input_file_names, file_type,
                     additional_options=None):
        """
        Makes the command needed to compile the specified file.

        :param input_file_names: Input file names (without path).
            The first name must be the source file, other entries can
            be used to link with additional object files.
        :param output_file_name: Output file base name (without path and
            extension). None to use the input_file_name base name.
        :param file_type: file type (exe or dll).

        :return: a tuple made up of the program name and the command arguments.
        """
        from .settings import Settings
        settings = Settings()
        output_file_name = os.path.splitext(input_file_names[0])[0]
        options = []
        if file_type == FileType.EXECUTABLE:
            options.append('-x')
        output_file_name += self.extension_for_type(file_type)
        options.append('-o')
        options.append(os.path.join('bin', output_file_name))
        options.append('-std=%s' % str(settings.cobol_standard).replace(
            'GnuCobolStandard.', ''))
        options += settings.compiler_flags
        if settings.free_format:
            options.append('-free')
        if settings.library_search_path:
            for pth in settings.library_search_path.split(';'):
                if pth:
                    options.append('-L%s' % pth)
        if settings.libraries:
            for lib in settings.libraries.split(' '):
                if lib:
                    options.append('-l%s' % lib)
        if additional_options:
            options += additional_options
        for ifn in input_file_names:
            if system.windows and ' ' in ifn:
                ifn = '"%s"' % ifn
            options.append(ifn)
        if Settings().custom_compiler_path and Settings().vcvars32:
            VisualStudioWrapperBatch.generate()
            pgm = VisualStudioWrapperBatch.path()
        else:
            pgm = 'cobc'
        return pgm, options

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
        exp = r'%s: *\d*:.*:.*$' % os.path.split(file_path)[1]
        prog = re.compile(exp)
        # parse compilation results
        for l in compiler_output.splitlines():
            if prog.match(l):
                _logger().debug('MATCHED')
                status = CheckerMessages.WARNING
                tokens = l.split(':')
                line = tokens[1]
                error_type = tokens[2]
                desc = ''.join(tokens[3:])
                if 'error' in error_type.lower():
                    status = CheckerMessages.ERROR
                # there does not seems to be any 'warning' messages output
                # if 'warning' in error_type.lower():
                #     status = CheckerMessages.WARNING
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


PARAM_FILE_CONTENT = '''DBHOST=%(host)s
DBUSER=%(user)s
DBPASSWD=%(pswd)s
DBNAME=%(dbname)s
DBPORT=%(port)s
DBSOCKET=%(socket)s
'''


class DbpreCompiler:
    """
    Provides an interface to the Dbpre tool.

    Commands:
        - /path/to/dbpre SOURCE.scb -I/path/to/framework -ts=TAB_LEN
        - copy PGCTBBATWS to source dir
        - cobc -x SOURCE.cob /path/to/cobmysqlapi.o -L/usr/lib/mysql
          -lmysqlclient -o bin/SOURCE.exe

    (-L: library search path, -l libraries to link with)
    """

    EXTENSIONS = [".SCB"]

    _INVALID = 'invalid dbpre executable'

    def __init__(self, dbpre_path=None):
        if dbpre_path is None:
            dbpre_path = Settings().dbpre
        self.dbpre_path = dbpre_path

    def get_version(self):
        """
        Returns the GnuCobol compiler version as a string
        """
        program = self.dbpre_path
        cmd = [program, '--version']
        try:
            _logger().debug('getting dbpre version: %s' % ' '.join(cmd))
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
            pass
        else:
            stdout, stderr = p.communicate()
            stdout = stdout.decode()
            lversion = stdout.splitlines()[0]
            if re.match(r'dbpre V \d.\d.+', lversion):
                return lversion
        return self._INVALID

    def is_working(self):
        """

        :return:
        """
        return self.get_version() != self._INVALID

    def make_command(self, file_path):
        """
        Make the dbpre command.

        :param file_path: .scb path
        """
        source = os.path.splitext(os.path.split(file_path)[1])[0]
        return self.dbpre_path, [source, '-I%s' % Settings().dbpre_framework,
                                 '-ts=%d' % Settings().tab_len]

    def _run_dbpre(self, file_path):
        """
        Run the dbpre process and returns it's exit code and
        output (both stderr and stdout).

        :param file_path: .scb path.
        :return: int, str
        """
        path = os.path.dirname(file_path)
        pgm, options = self.make_command(file_path)
        process = QtCore.QProcess()
        process.setWorkingDirectory(path)
        process.setProcessChannelMode(QtCore.QProcess.MergedChannels)
        _logger().info('command: %s %s', pgm, ' '.join(options))
        _logger().debug('working directory: %s', path)
        _logger().debug('system environment: %s', process.systemEnvironment())
        process.start(pgm, options)
        process.waitForFinished()
        status = process.exitCode()
        output = process.readAllStandardOutput().data().decode(
            locale.getpreferredencoding())
        return output, status

    def _copy_dbpre_framework(self, path):
        """
        Copies the dbpre framework files to the source directory.

        :param path: path of the .scb file
        """
        _logger().info('copying dbpre framework to source directory')
        pgctbbat = os.path.join(Settings().dbpre_framework, 'PGCTBBAT')
        pgctbbatws = os.path.join(Settings().dbpre_framework, 'PGCTBBATWS')
        sqlca = os.path.join(Settings().dbpre_framework, 'SQLCA')
        results = os.listdir(os.path.dirname(path))
        if 'PGCTBBAT' not in results:
            _logger().info('copying PGCTBBAT')
            shutil.copy(pgctbbat, os.path.dirname(path))
        if 'PGCTBBATWS' not in results:
            _logger().info('copying PGCTBBATWS')
            shutil.copy(pgctbbatws, os.path.dirname(path))
        if 'SQLCA' not in results:
            _logger().info('copying QSLCA')
            shutil.copy(sqlca, os.path.dirname(path))

    def _compile_with_cobc(self, path):
        """
        Compile the .cob generated by dbpre.

        :param path: path of the .scb file.

        :return: status, messages
        """
        _logger().info('compiling with cobc')
        cob_path = os.path.splitext(path)[0] + '.cob'
        compiler = GnuCobolCompiler()
        return compiler.compile(cob_path, get_file_type(cob_path),
                                object_files=[Settings().cobmysqlapi])

    def _generate_param_file(self, source_path):
        """
        Generate the bin/EXECUTABLE.param file based on the settings defined in
        the preferences dialog.

        :param source_path: path of the .scb file.
        """
        name = os.path.splitext(os.path.split(source_path)[1])[0] + '.param'
        content = PARAM_FILE_CONTENT % {
            'host': Settings().dbhost,
            'user': Settings().dbuser,
            'pswd': Settings().dbpasswd,
            'dbname': Settings().dbname,
            'port': Settings().dbport,
            'socket': Settings().dbsocket
        }
        path = os.path.join(os.path.dirname(source_path), 'bin', name)
        if not os.path.exists(path):
            _logger().info('creating %s', name)
            with open(path, 'w') as f:
                f.write(content)

    def compile(self, path):
        """
        Compile an sql cobol file using dbpre.

        :param path: path of the sql cobol file (.scb)

        :return: build status, list of error/warning messages to
                 display.
        """
        if not self.is_working():
            return -1, []
        output, status = self._run_dbpre(path)
        _logger().info('dbpre output:%s', output)
        if status != 0:
            # dbpre failed with some errors, let the user know about that
            return status, [(output.strip(), CheckerMessages.ERROR, -1, 0,
                             None, None, path)]
        self._copy_dbpre_framework(path)
        status, messages = self._compile_with_cobc(path)
        self._generate_param_file(path)
        return status, messages


class EsqlOCCompiler:
    """
    Provides an interface to esqlOC.exe:

    esqlOC.exe -static -o file.cob file.sqb

    GnuCobol Commands:
    SET OC_RUNTIME=c:\OpenCobol_bin
    SET esqlOC_RUNTIME=c:\esqlOC\release
    SET COB_CFLAGS=-I %OC_RUNTIME%
    SET COB_LIBS=
      %OC_RUNTIME%\libcob.lib %OC_RUNTIME%\mpir.lib %esqlOC_RUNTIME%\ocsql.lib
    SET COB_CONFIG_DIR=
      %OC_RUNTIME%\config\ set PATH=C:\WINDOWS\system32;%OC_RUNTIME%
    call "%PROGRAMFILES%\Microsoft Visual Studio 10.0\VC\vcvarsall.bat"
    %OC_RUNTIME%\cobc.exe -fixed -v -x -static -o c:\Temp\esqlOCGetStart1.exe
      c:\Temp\esqlOCGetStart1.cob
    """

    EXTENSIONS = [".SQB"]

    _INVALID = 'invalid esqlOC executable'

    def __init__(self):
        self.esqloc_path = os.path.join(Settings().esqloc, 'esqlOC.exe')

    def is_working(self):
        """

        :return:
        """
        return os.path.exists(self.esqloc_path)

    def make_command(self, source, destination):
        """
        Make the esqloc command.

        :param file_path: .scb path
        """
        return self.esqloc_path, ['-static', '-o', destination, source]

    def _run_esqloc(self, file_path):
        """
        Run the esqloc process and returns it's exit code and
        output (both stderr and stdout).

        :param file_path: .sqb path.
        :return: int, str, str
        """
        path = os.path.dirname(file_path)
        source = os.path.split(file_path)[1]
        destination = os.path.splitext(source)[0] + '.cob'
        pgm, options = self.make_command(source, destination)
        process = QtCore.QProcess()
        process.setWorkingDirectory(path)
        process.setProcessChannelMode(QtCore.QProcess.MergedChannels)
        _logger().info('command: %s %s', pgm, ' '.join(options))
        _logger().debug('working directory: %s', path)
        _logger().debug('system environment: %s', process.systemEnvironment())
        process.start(pgm, options)
        process.waitForFinished()
        status = process.exitCode()
        output = process.readAllStandardOutput().data().decode(
            locale.getpreferredencoding())
        return output, status, destination

    def _compile_with_cobc(self, path):
        """
        Compile the .cob generated by dbpre.

        :param path: path of the .scb file.

        :return: status, messages
        """
        _logger().info('compiling with cobc')
        cob_path = os.path.splitext(path)[0] + '.cob'
        compiler = GnuCobolCompiler()
        return compiler.compile(
            cob_path, get_file_type(cob_path), additional_options=[
                '-static', '-L%s' % Settings().esqloc, '-locsql.lib'])

    def compile(self, path):
        """
        Compile an sql cobol file using dbpre.

        :param path: path of the sql cobol file (.scb)

        :return: build status, list of error/warning messages to
                 display.
        """
        if not self.is_working():
            return -1, []
        output, status, cob_file = self._run_esqloc(path)
        _logger().info('esqloc output:%s', output)
        if status != 0:
            # esqloc failed with some errors, let the user know about that
            return status, [(output.strip(), CheckerMessages.ERROR, -1, 0,
                             None, None, path)]
        status, messages = self._compile_with_cobc(os.path.join(
            os.path.dirname(path), cob_file))
        return status, messages
