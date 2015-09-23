"""
Controls the COBOL specific action (compile, run and change program type)

"""
import logging
import os
import subprocess
from pyqode.core.api import TextHelper
from pyqode.core.modes import CheckerMessage, CheckerMessages
from pyqode.qt import QtCore, QtGui, QtWidgets
from .base import Controller
from open_cobol_ide import system
from open_cobol_ide.enums import FileType
from open_cobol_ide.compilers import GnuCobolCompiler, get_file_type, \
    DbpreCompiler, EsqlOCCompiler
from open_cobol_ide.settings import Settings


LOG_PAGE_COMPILER = 0
LOG_PAGE_ISSUES = 1
LOG_PAGE_RUN = 2


class CompilationThread(QtCore.QThread):
    """
    Compiles a file and all its dependencies in a background thread.

    """
    #: signal emitted when a file has compile. Parameters are the file path,
    #: the compiler exit code and the list of possible errors messages.
    file_compiled = QtCore.Signal(str, int, list)

    #: signal emitted when an exception was raise by the compiler.
    errored = QtCore.Signal(str, Exception)

    #: signal emitted when the thread finished.
    finished = QtCore.Signal()

    command_started = QtCore.Signal(str)
    output_available = QtCore.Signal(str)

    #: path of the file to compile
    file_path = ''

    def run(self):
        """
        Compiles the file and all its dependencies.
        """
        def is_dbpre_cobol(path):
            if path.lower().endswith('.scb'):
                with open(path, 'r') as f:
                    return 'exec sql' in f.read().lower()
            return False

        def is_esqloc_cobol(path):
            if path.lower().endswith('.sqb'):
                with open(path, 'r') as f:
                    return 'exec sql' in f.read().lower()
            return False

        cobc = GnuCobolCompiler()
        cobc.started.connect(self.command_started.emit)
        cobc.output_available.connect(self.output_available.emit)

        dbpre = DbpreCompiler()
        dbpre.started.connect(self.command_started.emit)
        dbpre.output_available.connect(self.output_available.emit)

        esqloc = EsqlOCCompiler()
        esqloc.started.connect(self.command_started.emit)
        esqloc.output_available.connect(self.output_available.emit)

        files = [self.file_path]
        if Settings().autodetect_submodules:
            files += cobc.get_dependencies(self.file_path, recursive=True)

        _logger().info('running compilation thread: %r', files)

        for f in files:
            try:
                if is_dbpre_cobol(f):
                    status, messages = dbpre.compile(f)
                elif is_esqloc_cobol(f):
                    status, messages = esqloc.compile(f)
                else:
                    status, messages = cobc.compile(f, get_file_type(f))
            except Exception as e:
                self.errored.emit(f, e)
                break
            else:
                self.file_compiled.emit(f, status, messages)
        self.finished.emit()


def _logger():
    return logging.getLogger(__name__)


class CobolController(Controller):
    """
    Controls the COBOL menu (compile, run, file type).

    """
    def __init__(self, app):
        super().__init__(app)
        self._compilation_thread = None
        group = QtWidgets.QActionGroup(self.main_window)
        group.addAction(self.ui.actionProgram)
        group.addAction(self.ui.actionSubprogram)
        self.compile_buttons = []
        self.run_buttons = []
        self.create_bt_compile()
        self.ui.toolBarCode.insertWidget(
            self.ui.actionRun, self.compile_buttons[0])
        group.triggered.connect(self._on_program_type_changed)
        self.ui.actionCompile.triggered.connect(self.compile)
        self.ui.errorsTable.msg_activated.connect(self._goto_error_msg)
        self.ui.actionRun.triggered.connect(self.run)
        self._run_requested = False
        self.ui.consoleOutput.process_finished.connect(self._on_run_finished)
        self.ui.actionCancel.triggered.connect(self.cancel)

    def create_bt_compile(self):
        bt_compile = QtWidgets.QToolButton()
        bt_compile.setIcon(self.ui.actionCompile.icon())
        bt_compile.setMenu(self.ui.menuProgramType)
        bt_compile.setToolTip(self.ui.actionCompile.toolTip())
        bt_compile.setPopupMode(bt_compile.MenuButtonPopup)
        bt_compile.clicked.connect(self.compile)
        self.compile_buttons.append(bt_compile)
        bt_compile.setEnabled(self.ui.actionCompile.isEnabled())
        return bt_compile

    def create_bt_run(self):
        icon = self.ui.actionRun.icon()
        bt = QtWidgets.QToolButton()
        bt.setIcon(icon)
        bt.setToolTip(self.ui.actionRun.toolTip())
        bt.clicked.connect(self.run)
        self.run_buttons.append(bt)
        bt.setEnabled(self.ui.actionRun.isEnabled())
        return bt

    def display_file_type(self, editor):
        """
        Displays the current editor file type (changes the checked action in
        the file type drop down menu).

        """
        try:
            ftype = editor.file_type
        except (AttributeError, UnicodeDecodeError):
            pass
        else:
            self.ui.actionProgram.setChecked(ftype == FileType.EXECUTABLE)
            self.ui.actionSubprogram.setChecked(ftype != FileType.EXECUTABLE)
            for item in self.run_buttons + [self.ui.actionRun]:
                item.setEnabled(not self.ui.consoleOutput.is_running)

    def _on_program_type_changed(self, action):
        """
        Updates current editor file type and enables/disables run action.
        """
        if action == self.ui.actionProgram:
            self.ui.tabWidgetEditors.current_widget().file_type = \
                FileType.EXECUTABLE
            for item in self.run_buttons + [self.ui.actionRun]:
                item.setEnabled(not self.ui.consoleOutput.is_running)
        else:
            self.ui.tabWidgetEditors.current_widget().file_type = \
                FileType.MODULE
            for item in self.run_buttons + [self.ui.actionRun]:
                item.setEnabled(False)

    def check_compiler(self, dotted_extension):
        compiler_works = False
        msg = 'Invalid extension'
        if dotted_extension.lower() in Settings().cobc_extensions:
            compiler_works = GnuCobolCompiler().is_working()
            msg = 'GnuCOBOL compiler not found or not working'
        elif dotted_extension.lower() in Settings().dbpre_extensions:
            compiler_works = DbpreCompiler().is_working()
            msg = 'dbpre compiler not working, please check your SQL COBOL ' \
                'configuration'
        elif dotted_extension.lower() in Settings().esqloc_extensions:
            compiler_works = EsqlOCCompiler().is_working()
            msg = 'esqlOC compiler not workign, please check your SQL COBOL ' \
                'configuration'
        return compiler_works, msg

    def compile(self):
        """
        Compiles the current editor
        """
        # make sure the associated compiler is working, otherwise disable
        # compile/run actions
        path = self.app.edit.current_editor.file.path
        dotted_extension = os.path.splitext(path)[1].upper()
        compiler_works, msg = self.check_compiler(dotted_extension)
        if not compiler_works:
            QtWidgets.QMessageBox.warning(
                self.app.win, 'Cannot compile file',
                'Cannot compile file: %r.\n\nReason: %s' % (
                    os.path.split(path)[1], msg))
            return
        # ensures all editors are saved before compiling
        self.ui.tabWidgetEditors.save_all()
        # disable actions
        self.enable_compile(False)
        self.enable_run(False)
        # reset errors
        self.ui.errorsTable.clear()
        self._errors = 0
        # prepare and start compilation thread
        self.ui.textEditCompilerOutput.clear()
        self.ui.tabWidgetLogs.setCurrentIndex(LOG_PAGE_COMPILER)
        self._compilation_thread = CompilationThread()
        self._compilation_thread.file_path = \
            self.app.edit.current_editor.file.path
        self._compilation_thread.file_compiled.connect(self._on_file_compiled)
        self._compilation_thread.errored.connect(self._on_build_exception)
        self._compilation_thread.command_started.connect(
            self._on_command_started)
        self._compilation_thread.output_available.connect(
            self._on_output_available)
        self._compilation_thread.finished.connect(
            self._on_compilation_finished)
        self._compilation_thread.start()

    def _on_command_started(self, cmd):
        old_color = self.ui.textEditCompilerOutput.textColor()
        color = QtGui.QColor('#629755') if Settings().dark_style else \
            QtGui.QColor('#000080')
        self.ui.textEditCompilerOutput.setTextColor(color)
        self.ui.textEditCompilerOutput.append(cmd)
        self.ui.textEditCompilerOutput.setTextColor(old_color)

    def _on_output_available(self, output):
        if output:
            self.ui.textEditCompilerOutput.append(output)

    def _on_compilation_finished(self):
        """
        Runs the current file if the compilation came from a run request,
        otherwise enables actions/buttons.

        """
        self.enable_compile(True)
        self.enable_run(True)
        self.ui.tabWidgetLogs.setCurrentIndex(LOG_PAGE_ISSUES)
        if self._run_requested:
            self._run_requested = False
            if self._errors == 0:
                self.enable_compile(False)
                self.enable_run(False)
                self._run()

    def _on_build_exception(self, path, exception):
        QtWidgets.QMessageBox.critical(
            self.main_window, 'Exception while compiling a file',
            'An exception occured when compiling %r.\n\nError=%s' %
            (path, exception))

    def _on_file_compiled(self, filename, status, messages):
        """
        Displays compilation status in errors table.
        """
        self.ui.dockWidgetLogs.show()
        self.ui.tabWidgetLogs.setCurrentIndex(LOG_PAGE_COMPILER)
        if len(messages) == 0 and status == 0:
            ext = GnuCobolCompiler().extension_for_type(
                get_file_type(filename))
            name = os.path.splitext(os.path.split(filename)[1])[0]
            output_dir = Settings().output_directory
            if not os.path.isabs(output_dir):
                path = os.path.dirname(filename)
                output_dir = os.path.abspath(os.path.join(path, output_dir))
            path = os.path.join(output_dir, name + ext)
            self.ui.errorsTable.add_message(
                CheckerMessage(
                    'Compilation succeeded (output: %s)' % path,
                    CheckerMessages.INFO, -1,
                    path=filename))
        else:
            for msg in messages:
                msg = CheckerMessage(*msg)
                if msg.status == CheckerMessages.ERROR:
                    self._errors += 1
                self.ui.errorsTable.add_message(msg)

    def _goto_error_msg(self, msg):
        """
        Opens an editor and goes to the error line.
        """
        self.app.file.open_file(msg.path)
        if msg.status:
            TextHelper(self.app.edit.current_editor).goto_line(msg.line)
            self.app.edit.current_editor.setFocus(True)

    def run(self):
        """
        Compiles and run the current editor.
        """
        self.ui.consoleOutput.clear()
        self._run_requested = True
        self.compile()

    def _run_in_external_terminal(self, program, wd, file_type):
        """
        Runs a program in an external terminal.

        :param program: program to run
        :param wd: working directory
        """
        self.ui.consoleOutput.append(
            "Launched in external terminal")
        pyqode_console = system.which('pyqode-console')
        if file_type == FileType.MODULE:
            program = QtCore.QFileInfo(program).baseName()
        if system.windows:
            cmd = [pyqode_console, program]
            if file_type == FileType.MODULE:
                cmd.insert(1, system.which('cobcrun'))
            subprocess.Popen(cmd, cwd=wd,
                             creationflags=subprocess.CREATE_NEW_CONSOLE)
        elif system.darwin:
            cmd = ['open', program]
            if file_type == FileType.MODULE:
                cmd.insert(1, system.which('cobcrun'))
            subprocess.Popen(cmd, cwd=wd)
        else:
            if file_type == FileType.EXECUTABLE:
                cmd = ['"%s %s"' % (pyqode_console, program)]
            else:
                cmd = ['"%s %s %s"' % (pyqode_console, system.which('cobcrun'),
                                       program)]
            cmd = (Settings().external_terminal_command.strip().split(' ') +
                   cmd)
            subprocess.Popen(' '.join(cmd), cwd=wd, shell=True)
        _logger().info('running program in external terminal: %s',
                       ' '.join(cmd))

    def _run(self):
        """
        Runs the current editor program.
        :return:
        """
        # compilation has finished, we can run the program that corresponds
        # to the current editor file
        editor = self.app.edit.current_editor
        file_type = editor.file_type
        self.ui.tabWidgetLogs.setCurrentIndex(LOG_PAGE_RUN)
        self.ui.dockWidgetLogs.show()
        self.ui.consoleOutput.clear()
        output_dir = Settings().output_directory
        if not os.path.isabs(output_dir):
            output_dir = os.path.join(
                os.path.dirname(editor.file.path), output_dir)
        wd = output_dir
        filename = os.path.splitext(editor.file.name)[0] + \
            GnuCobolCompiler().extension_for_type(file_type)
        program = os.path.join(wd, filename)
        if not os.path.exists(program):
            _logger().warning('cannot run %s, file does not exists',
                              program)
            return
        if Settings().external_terminal:
            self._run_in_external_terminal(program, wd, file_type)
            self.enable_run(True)
            self.enable_compile(True)
        else:
            _logger().info('running program')
            self.ui.consoleOutput.setFocus(True)
            for item in self.run_buttons + [self.ui.actionRun]:
                item.setEnabled(False)
            if file_type == FileType.MODULE:
                cobcrun = system.which('cobcrun')
                self.ui.consoleOutput.start_process(
                    cobcrun, [os.path.splitext(editor.file.name)[0]], cwd=wd)
            else:
                self.ui.consoleOutput.start_process(program, cwd=wd)

    def _on_run_finished(self):
        self.enable_compile(True)
        self.enable_run(True)

    def enable_compile(self, enable):
        for item in self.compile_buttons + [self.ui.actionCompile]:
            item.setEnabled(enable)

    def enable_run(self, enable):
        for item in self.run_buttons + [self.ui.actionRun]:
            item.setEnabled(enable)

    def cancel(self):
        """
        Cancels current any ongoing operations (run/compile).
        """
        if self._compilation_thread:
            self._compilation_thread.terminate()
        self.ui.consoleOutput.stop_process()
        self.enable_compile(True)
        self.enable_run(True)
