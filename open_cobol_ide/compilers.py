"""
This module contains function and classes for interfacing with the GnuCOBOL
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
from pyqode.core.modes import CheckerMessage, CheckerMessages
from pyqode.qt import QtCore

from open_cobol_ide import system, msvc
from open_cobol_ide.enums import FileType, GnuCobolStandard
from open_cobol_ide.memoize import memoized
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
            content = f.read().upper()
        if re.match(r'.*PROCEDURE[\s\n]+DIVISION[\s\n]+USING', content,
                    re.DOTALL):
            ftype = FileType.MODULE
    _logger().debug('file type: %r', ftype)
    return ftype


def run_command(pgm, args, working_dir=''):
    if ' ' in pgm:
        pgm = '"%s"' % pgm

    path_cpy = os.environ['PATH']

    p_env = GnuCobolCompiler.setup_process_environment()
    os.environ['PATH'] = p_env.value('PATH')
    p = QtCore.QProcess()
    p.setProcessChannelMode(QtCore.QProcess.MergedChannels)
    if working_dir:
        p.setWorkingDirectory(working_dir)
    p.setProcessEnvironment(p_env)
    _logger().debug('command: %s', ' '.join([pgm] + args))
    _logger().debug('working directory: %s', working_dir)
    _logger().debug('environment: %s',
                    p.processEnvironment().toStringList())
    p.start(pgm, args)

    print(' '.join([pgm] + args))

    p.waitForFinished()

    # determine exit code (handle crashed processes)
    if p.exitStatus() != p.Crashed:
        status = p.exitCode()
    else:
        status = 139

    # get compiler output
    raw_output = p.readAllStandardOutput().data()
    try:
        output = raw_output.decode(locale.getpreferredencoding())
    except UnicodeDecodeError:
        # This is a hack to get a meaningful output when compiling a file
        # from UNC path using a batch file on some systems, see
        # https://github.com/OpenCobolIDE/OpenCobolIDE/issues/188
        output = str(raw_output).replace("b'", '')[:-1].replace(
            '\\r\\n', '\n').replace('\\\\', '\\')

    _logger().debug('output: %r', output)
    _logger().debug('exit code: %r', status)

    os.environ['PATH'] = path_cpy

    print(output)

    return status, output


def check_compiler():
    """
    Checks if a valid COBOL compiler can be found.

    :raises: CompilerNotFound if no compiler could be found.
    """
    import sys
    if not GnuCobolCompiler().is_working():
        if sys.platform == 'win32':
            msg = 'GnuCOBOL is bundled with the IDE. Ensure that ' \
                  'the IDE is installed in a path without spaces and ' \
                  'that the GnuCOBOL folder sits next to the executable.'
        elif sys.platform == 'darwin':
            msg = 'You have to install the package open-cobol using Homebrew '\
                  'or MacPorts. \n' \
                  'If you installed open-cobol in a ' \
                  'non standard directory, you can specify that directory ' \
                  'in the preferences dialog (build & run tab).'
        else:
            msg = 'You have to either install one of the packages gnu-cobol or open-cobol ' \
                  "using your distribution's package manager or " \
                  'build it from source.'
        raise CompilerNotFound(msg)


class CompilerNotFound(Exception):
    """
    Error raised when no compiler were found.
    """
    pass


class GnuCobolCompiler(QtCore.QObject):
    """
    Provides an interface to the GnuCOBOL compiler (cobc)
    """
    #: signal emitted when the compilation process started, parameter
    #: is the command
    started = QtCore.Signal(str)

    #: signal emitted when the compilation process finished and its output
    #: is available for parsing.
    output_available = QtCore.Signal(str)

    # GC output messages format depends on the underlying compiler
    # See https://github.com/OpenCobolIDE/OpenCobolIDE/issues/206
    OUTPUT_PATTERN_GCC = re.compile(
        r'^(?P<filename>[\w\.\-_\s]*):(?P<line>\s*\d*):(?P<type>[\w\s]*):(?P<error>.*)$')
    OUTPUT_PATTERN_MSVC = re.compile(
        r'^(?P<filename>[\w\.\-_\s]*)\((?P<line>\s*\d*)\):(?P<type>[\w\s]*):'
        '(?P<error>.*)$')
    OUTPUT_PATTERN_EXCEPTIONAL_MESSAGES = re.compile(r"^cobc: (?P<error>.*)$")

    OUTPUT_PATTERNS = [OUTPUT_PATTERN_GCC, OUTPUT_PATTERN_MSVC, OUTPUT_PATTERN_EXCEPTIONAL_MESSAGES]

    extensions = [
        # no extension for exe on linux and mac
        '.exe' if system.windows else '',
        # .dll on windows, so everywhere else
        '.dll' if system.windows else '.so' if system.linux else '.dylib'
    ]

    def __init__(self):
        super().__init__()

    @staticmethod
    def get_version(include_all=True):
        """
        Returns the GnuCOBOL compiler version as a string
        """
        compiler = Settings().compiler_path
        cmd = compiler, '--version'
        if not compiler:
            return 'compiler not found'
        _logger().debug('getting cobc version: %s' % ' '.join(cmd))
        status, output = run_command(cmd[0], [cmd[1]])
        if status == 0 and output:
            if include_all:
                return output
            else:
                _logger().debug('parsing version line: %s' % output)
                m = re.match(r'cobc\s\([\w\s]*\).*$', output, re.MULTILINE)
                if m:
                    return m.group(0)
                else:
                    return "failed to extract version from output"
        else:
            return output

    @staticmethod
    def setup_process_environment():
        env = QtCore.QProcessEnvironment()
        for k, v in os.environ.items():
            env.insert(k, v)

        s = Settings()

        PATH = ''
        if s.path_enabled:
            PATH = s.path

        if not s.vcvarsall:
            PATH = PATH + os.pathsep + os.environ['PATH']
        else:
            for k, v in msvc.get_vc_vars(
                    s.vcvarsall, s.vcvarsall_arch).items():
                if k == 'PATH':
                    PATH = PATH + os.pathsep + v
                else:
                    env.insert(k, v)

        env.insert('PATH', PATH)

        _logger().debug('PATH=%s', PATH)

        if s.cob_config_dir_enabled and s.cob_config_dir:
            env.insert('COB_CONFIG_DIR', s.cob_config_dir)

        if s.cob_copy_dir_enabled and s.cob_copy_dir:
            env.insert('COB_COPY_DIR', s.cob_copy_dir)

        if s.cob_include_path_enabled and s.cob_copy_dir:
            env.insert('COB_INCLUDE_PATH', s.cob_include_path)

        if s.cob_lib_path and s.cob_copy_dir:
            env.insert('COB_LIB_PATH', s.cob_lib_path)

        return env

    @classmethod
    @memoized
    def check_compiler(cls, compiler):
        def get_output_path(input_path, possible_extensions):
            dirname, filename = os.path.split(input_path)
            basename = os.path.splitext(filename)[0]
            for ext in possible_extensions:
                candidate = os.path.join(dirname, basename + ext)
                if os.path.exists(candidate):
                    return candidate
            return 'none'

        def rm_dest(dest):
            if os.path.exists(dest):
                try:
                    os.remove(dest)
                except OSError:
                    # log something
                    _logger().exception('failed to remove check compiler destination')
                    return False
            return True

        from open_cobol_ide.view.dialogs.preferences import DEFAULT_TEMPLATE
        working_dir = tempfile.gettempdir()
        cbl_path = os.path.join(working_dir, 'test.cbl')
        try:
            with open(cbl_path, 'w') as f:
                f.write(DEFAULT_TEMPLATE)
        except OSError as e:
            return 'Failed to create %s, error=%r' % (cbl_path, e), -1
        dest = os.path.join(tempfile.gettempdir(),
                            'test' + ('.exe' if system.windows else ''))

        _logger().debug('removing executable test if still exists...')
        if not rm_dest(dest):
            return 'Failed to remove %r before checking if compilation of executable works.\n' \
                'Please remove this file before attempting a new compilation check!' % dest, -1
        _logger().debug('check compiler')
        success1 = False
        status, output1 = run_command(compiler, ['-v', '-x', cbl_path], working_dir=working_dir)
        dest = get_output_path(cbl_path, possible_extensions=['.exe', '.bat', ''])
        if dest:
            if os.path.exists(dest):
                success1 = True
                if status != 0:
                    _logger().warn('test executable compilation returned a non-zero return code')
                # detect default executable extension
                GnuCobolCompiler.extensions[0] = os.path.splitext(dest)[1]
                try:
                    os.remove(dest)
                except OSError:
                    _logger().exception('failed to remove destination file: %r' % dest)

        dest = os.path.join(tempfile.gettempdir(),
                            'test' + ('.dll' if system.windows else '.so' if system.linux else '.dylib'))
        _logger().debug('removing executable test if still exists...')
        if not rm_dest(dest):
            return 'Failed to remove %r before checking if compilation of module works.\n' \
                'Please remove this file before attempting a new compilation check!' % dest, -1
        success2 = False
        status, output2 = run_command(compiler, ['-v', cbl_path], working_dir=working_dir)
        dest = get_output_path(cbl_path, possible_extensions=['.so', '.dll', '.dylib'])
        if dest:
            if os.path.exists(dest):
                success2 = True
                if status != 0:
                    _logger().warn('test executable compilation returned a non-zero return code')
                # detect default executable extension
                GnuCobolCompiler.extensions[1] = os.path.splitext(dest)[1]
                try:
                    os.remove(dest)
                except OSError:
                    _logger().exception('failed to remove destination file: %r' % dest)

        _logger().info('GnuCOBOL compiler check: %s (%d/%d)',
                       'success' if success1 and success2 else 'fail',
                       success1, success2)
        if success1:
            _logger().info('Executable extension: %s' % GnuCobolCompiler.extensions[0])

        if success2:
            _logger().info('Module extension: %s' % GnuCobolCompiler.extensions[1])

        try:
            os.remove(cbl_path)
        except OSError:
            _logger().exception('failed to remove test file: %r' % dest)

        if not success1 or not success2:
            status = -1

        return '%s\n%s' % (output1, output2), status

    @staticmethod
    def get_cobc_help():
        compiler = Settings().compiler_path
        args = ['--help']

        if not compiler:
            return 'cannot run command, no compiler path defined'

        status, output = run_command(compiler, args)

        if status != 0:
            output = 'command "cobc --help" failed with exit code %d.\nProcess output: %s' % (status, output)

        return output

    @staticmethod
    def get_cobc_infos():
        compiler = Settings().compiler_path
        args = ['--info']

        if not compiler:
            return 'cannot run command, no compiler path defined'

        status, output = run_command(compiler, args)

        if status != 0:
            output = 'command "cobc --info" failed with exit ' \
                     'code %d.\nNote that this command is supported only by ' \
                     'recent builds of GnuCOBOL\nProcess output: %s' % \
                     (status, output)

        return output

    @staticmethod
    def get_cobcrun_infos():
        env = GnuCobolCompiler.setup_process_environment()
        pgm = shutil.which('cobcrun', path=env.value('PATH'))
        args = ['--runtime-env']
        if not pgm:
            return 'cannot run command, cobcrun could not be found using PATH.'

        status, output = run_command(pgm, args)

        if status != 0:
            output = 'command "cobcrun --runtime-env" failed with exit ' \
                     'code %d.\nNote that this command is supported only by ' \
                     'recent builds of GnuCOBOL\nProcess output: %s' % \
                     (status, output)

        return output

    def is_working(self):
        """
        Checks if the GNUCobol compiler is working.
        """
        pth = Settings().compiler_path
        if pth:
            _, exit_code = self.check_compiler(pth)
            return exit_code == 0
        return False

    def extension_for_type(self, file_type):
        """
        Returns a platform specific extension for the specified file type.

        :param file_type: file type
        :return: extension
        """
        return self.extensions[int(file_type)]

    def prepare_bin_dir(self, path, output_full_path):
        assert not os.path.isfile(path)  # must be a directory
        if not os.path.exists(path):
            os.makedirs(path)
        if Settings().copy_runtime_dlls:
            # copy runtime dlls to output directory
            files = glob.glob(os.path.join(
                os.path.dirname(Settings().full_compiler_path), "*.dll"))
            for f in files:
                shutil.copy(f, path)
        if os.path.exists(output_full_path):
            try:
                os.remove(output_full_path)
            except OSError:
                _logger().exception(
                    'Failed to previous binary file: %s' % output_full_path)

        # check for possible permission error
        test_path = os.path.join(path, 'test_write_access.ocide')
        try:
            with open(test_path, 'w'):
                pass
        except PermissionError:
            msg = 'Build directory not writeable (%s)' % path
            _logger().exception(msg)
            raise PermissionError(msg)
        else:
            os.remove(test_path)

    def compile(self, file_path, file_type, object_files=None,
                additional_options=None, output_dir=None):
        """
        Compiles a file. This is a blocking function, it returns only when
        the compiler process finished.

        :param file_path: Path of the file to compile.
        :param file_type: File type (exe or dll)
        :param output_dir: Output directory, if None, the directory set in the
                           application directory will be used.
        :return: status, list of checker messages
        """
        _logger().info('compiling %s' % file_path)
        path, filename = os.path.split(file_path)
        if output_dir is None:
            output_dir = Settings().output_directory
            original_output_dir = output_dir
        if not os.path.isabs(output_dir):
            output_dir = os.path.abspath(os.path.join(path, output_dir))
        if object_files:
            inputs = [filename] + object_files
        else:
            inputs = [filename]
        # ensure bin dir exists
        output_full_path = os.path.join(
            output_dir, self.get_output_filename(inputs, file_type))
        if os.path.exists(output_full_path) and \
            os.path.getmtime(file_path) <= \
                os.path.getmtime(output_full_path):
            desc = 'compilation skipped, up to date...'
            self.output_available.emit('%s: %s' % (file_path, desc))
            msg = (desc, CheckerMessages.INFO, -1, 0, None, None, file_path)
            _logger().info(desc)

            return 0, [msg]

        self.prepare_bin_dir(output_dir, output_full_path)

        pgm, options = self.make_command(
            inputs, file_type, original_output_dir, additional_options)
        self.started.emit(' '.join([pgm] + options))
        status, output = run_command(pgm, options, working_dir=path)
        self.output_available.emit(output)
        messages = self.parse_output(output, path)
        binary_created = os.path.exists(output_full_path)
        _logger().info('binary file %r created:  %r',
                       output_full_path, binary_created)

        if not len(messages) and (status != 0 or not binary_created):
            # compilation failed but the parser failed to extract COBOL related
            # messages, there might be an issue at the C level or at the
            # linker level
            messages.append((output, CheckerMessages.ERROR, - 1, 0,
                             None, None, file_path))
        _logger().info('compile results: %r - %r', status, messages)

        return status, messages

    def get_output_filename(self, inputs, file_type):
        return os.path.splitext(inputs[0])[0] + self.extension_for_type(
            file_type)

    def make_command(self, input_file_names, file_type, output_dir=None,
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
        output_file_name = self.get_output_filename(
            input_file_names, file_type)
        options = []
        if file_type == FileType.EXECUTABLE:
            options.append('-x')
        options.append('-o')
        options.append(os.path.join(output_dir, output_file_name))
        if GnuCobolStandard(settings.cobol_standard) != GnuCobolStandard.none:
            options.append('-std=%s' % str(settings.cobol_standard).replace(
                'GnuCobolStandard.', ''))
        options += settings.compiler_flags
        if settings.free_format:
            options.append('-free')
        if settings.copybook_paths:
            for pth in settings.copybook_paths.split(';'):
                if not pth:
                    continue
                options.append('-I%s' % pth)
        if settings.library_search_path:
            for pth in settings.library_search_path.split(';'):
                if pth:
                    options.append('-L%s' % pth)
        if settings.libraries:
            for lib in system.shell_split(settings.libraries):
                if lib:
                    options.append('-l%s' % lib)
        if additional_options:
            options += additional_options
        for ifn in input_file_names:
            if system.windows and ' ' in ifn:
                ifn = '"%s"' % ifn
            options.append(ifn)
        pgm = Settings().compiler_path

        return pgm, options

    @staticmethod
    def parse_output(output, working_directory):
        """
        Parses the compiler output.

        :param output: to parse
        :type output: str

        :param working_directory: the working directory where the compiler
            command was executed. This helps resolve path to relative
            copybooks.
        :type working_directory: str
        """
        issues = []
        for l in output.splitlines():
            if not l:
                continue
            for ptrn in GnuCobolCompiler.OUTPUT_PATTERNS:
                m = ptrn.match(l)
                if m is not None:
                    try:
                        filename = m.group('filename')
                        line = int(m.group('line')) - 1
                        error_lvl = m.group('type').lower()
                    except IndexError:
                        filename = ''
                        line = 0
                        error_lvl = 'error'
                    message = m.group('error')
                    lvl = CheckerMessages.WARNING if 'warning' in error_lvl \
                        else CheckerMessages.ERROR
                    if filename:
                        # make relative path absolute
                        path = os.path.abspath(os.path.join(
                            working_directory, filename))
                    else:
                        path = '-'
                    msg = (message, lvl, int(line), 0, None,
                           None, path)
                    issues.append(msg)
        return issues

    @classmethod
    def get_dependencies(cls, filename, recursive=True):
        """
        Gets the dependencies of a COBOL program/module.

        :param filename: path of the file to analyse.
        :param recursive: True to perform recursive analysis (analyses
            dependencies of dependencies recursively).
        :return: The set of dependencies that needs to be compiled to compile
            and use the requested program/module.
        """
        encoding = _get_encoding(filename)
        directory = os.path.dirname(filename)
        dependencies = []
        prog = re.compile(r'^[\s\d\w]*CALL[\s\n]*".*".*$',
                          re.MULTILINE | re.IGNORECASE)
        with open(filename, 'r', encoding=encoding) as f:
            content = f.read()
        for m in prog.findall(content):
            try:
                module_base_name = re.findall('"(.*)"', m)[0]
            except IndexError:
                continue
            # try to see if the module can be found in the current
            # directory
            for ext in Settings().all_extensions:
                pth = os.path.join(directory, module_base_name + ext)
                if os.path.exists(pth) and \
                        pth.lower() not in dependencies:
                    if filename != pth:
                        dependencies.append(system.normpath(pth))
                        if recursive:
                            dependencies += cls.get_dependencies(pth)

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


class DbpreCompiler(QtCore.QObject):
    """
    Provides an interface to the Dbpre tool.

    Commands:
        - /path/to/dbpre SOURCE.scb -I/path/to/framework -ts=TAB_LEN
        - cobc -x SOURCE.cob /path/to/cobmysqlapi.o -L/usr/lib/mysql
          -lmysqlclient -o bin/SOURCE.exe -I/path/to/framework

    (-L: library search path, -l libraries to link with)
    """
    #: signal emitted when the compilation process started, parameter
    #: is the command
    started = QtCore.Signal(str)

    #: signal emitted when the compilation process finished and its output
    #: is available for parsing.
    output_available = QtCore.Signal(str)

    _INVALID = 'invalid dbpre executable'

    def __init__(self, dbpre_path=None):
        super().__init__()
        if dbpre_path is None:
            dbpre_path = Settings().dbpre
        self.dbpre_path = dbpre_path

    def get_version(self, path=None):
        """
        Returns the GnuCOBOL compiler version as a string
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
        cmd = '%s %s' % (pgm, ' '.join(options))
        _logger().info('command: %s', cmd)
        _logger().debug('working directory: %s', path)
        _logger().debug('system environment: %s', process.systemEnvironment())
        process.start(pgm, options)
        self.started.emit(cmd)
        process.waitForFinished()
        status = process.exitCode()
        try:
            output = process.readAllStandardOutput().data().decode(
                locale.getpreferredencoding())
        except UnicodeDecodeError:
            output = 'Failed to decode dbpre output with encoding: %s' % \
                locale.getpreferredencoding()
        self.output_available.emit(output)
        return output, status

    def _compile_with_cobc(self, path):
        """
        Compile the .cob generated by dbpre.

        :param path: path of the .scb file.

        :return: status, messages
        """
        _logger().info('compiling with cobc')
        cob_path = os.path.splitext(path)[0] + '.cob'
        compiler = GnuCobolCompiler()
        compiler.started.connect(self.started.emit)
        compiler.output_available.connect(self.output_available.emit)
        return compiler.compile(cob_path, get_file_type(cob_path),
                                object_files=[Settings().cobmysqlapi],
                                additional_options=['-I{0}'.format(
                                    Settings().dbpre_framework)])

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
        output_dir = Settings().output_directory
        if not os.path.isabs(output_dir):
            output_dir = os.path.abspath(os.path.join(
                os.path.dirname(source_path), output_dir))
        path = os.path.join(output_dir, name)
        if not os.path.exists(path):
            _logger().info('creating %s', name)
            with open(path, 'w') as f:
                f.write(content)

    def compile(self, path):
        """
        Compile an sql COBOL file using dbpre.

        :param path: path of the sql COBOL file (.scb)

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
        status, messages = self._compile_with_cobc(path)
        self._generate_param_file(path)
        return status, messages


class EsqlOCCompiler(QtCore.QObject):
    """
    Provides an interface to esqlOC.exe:

    esqlOC.exe -static -o file.cob file.sqb

    GnuCOBOL Commands:
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
    #: signal emitted when the compilation process started, parameter
    #: is the command
    started = QtCore.Signal(str)

    #: signal emitted when the compilation process finished and its output
    #: is available for parsing.
    output_available = QtCore.Signal(str)

    EXTENSIONS = [".SQB"]

    _INVALID = 'invalid esqlOC executable'

    def __init__(self):
        super().__init__()
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
        cmd = '%s %s' % (pgm, ' '.join(options))
        _logger().info('command: %s', cmd)
        _logger().debug('working directory: %s', path)
        _logger().debug('system environment: %s', process.systemEnvironment())
        process.start(pgm, options)
        self.started.emit(cmd)
        process.waitForFinished()
        status = process.exitCode()
        try:
            output = process.readAllStandardOutput().data().decode(
                locale.getpreferredencoding())
        except UnicodeDecodeError:
            output = 'Failed to decode esqloc output with encoding: ' % \
                locale.getpreferredencoding()
        self.output_available.emit(output)
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
        compiler.started.connect(self.started.emit)
        compiler.output_available.connect(self.output_available.emit)
        return compiler.compile(
            cob_path, get_file_type(cob_path), additional_options=[
                '-static', '-L%s' % Settings().esqloc, '-locsql.lib'])

    def compile(self, path):
        """
        Compile an sql COBOL file using dbpre.

        :param path: path of the sql COBOL file (.scb)

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
