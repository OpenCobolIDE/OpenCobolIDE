"""
Controls the cobol specific action (compile, run and change program type)

"""
import logging
import os
import subprocess
import sys
from pyqode.core.api import TextHelper
from pyqode.core.modes import CheckerMessage, CheckerMessages
from pyqode.qt import QtCore, QtGui, QtWidgets
from .base import Controller
from ..compiler import FileType, GnuCobolCompiler, get_file_type
from ..view.widgets import TabCornerWidget
from ..settings import Settings


class CompilationThread(QtCore.QThread):
    """
    Compiles a file and all its dependencies in a background thread.

    """
    #: signal emitted when a file has compile. Parameters are the file path,
    #: the compiler exit code and the list of possible errors messages.
    file_compiled = QtCore.Signal(str, int, list)
    #: signal emitted when the thread finished.
    finished = QtCore.Signal()

    #: path of the file to compile
    file_path = ''

    def run(self):
        """
        Compiles the file and all its dependencies.
        """
        compiler = GnuCobolCompiler()
        files = compiler.get_dependencies(self.file_path, recursive=True)
        files.insert(0, self.file_path)
        for f in files:
            status, messages = compiler.compile(f, get_file_type(f))
            self.file_compiled.emit(f, status, messages)
        self.finished.emit()


def _logger():
    return logging.getLogger(__name__)


class CobolController(Controller):
    """
    Controls the cobol menu (compile, run, file type).

    """
    def __init__(self, app):
        super().__init__(app)
        self._compilation_thread = None
        group = QtWidgets.QActionGroup(self.main_window)
        group.addAction(self.ui.actionProgram)
        group.addAction(self.ui.actionSubprogram)
        self.bt_compile = []
        self.bt_run = []
        self.create_bt_compile()
        self.ui.toolBarCode.insertWidget(self.ui.actionRun, self.bt_compile[0])
        group.triggered.connect(self._on_program_type_changed)
        self.ui.actionCompile.triggered.connect(self.compile)
        self.ui.errorsTable.msg_activated.connect(self._goto_error_msg)
        self.ui.actionRun.triggered.connect(self.run)
        self._run_requested = False
        self.ui.consoleOutput.process_finished.connect(self._on_run_finished)
        self.ui.actionCancel.triggered.connect(self.cancel)
        self.corner_widget = TabCornerWidget(
            self.ui.tabWidgetEditors, self.create_bt_compile(),
            self.create_bt_run())
        self.ui.tabWidgetEditors.setCornerWidget(self.corner_widget)

    def create_bt_compile(self):
        bt_compile = QtWidgets.QToolButton()
        bt_compile.setIcon(self.ui.actionCompile.icon())
        bt_compile.setMenu(self.ui.menuProgramType)
        bt_compile.setToolTip(self.ui.actionCompile.toolTip())
        bt_compile.setPopupMode(bt_compile.DelayedPopup)
        bt_compile.clicked.connect(self.compile)
        self.bt_compile.append(bt_compile)
        return bt_compile

    def create_bt_run(self):
        icon = self.ui.actionRun.icon()
        bt = QtWidgets.QToolButton()
        bt.setIcon(icon)
        bt.setToolTip(self.ui.actionRun.toolTip())
        bt.clicked.connect(self.run)
        self.bt_run.append(bt)
        return bt

    def display_file_type(self, editor):
        """
        Displays the current editor file type (changes the checked action in
        the file type drop down menu).

        """
        try:
            ftype = editor.file_type
        except AttributeError:
            pass
        else:
            self.ui.actionProgram.setChecked(ftype == FileType.EXECUTABLE)
            self.ui.actionSubprogram.setChecked(ftype != FileType.EXECUTABLE)
            for item in self.bt_run + [self.ui.actionRun]:
                item.setEnabled(ftype == FileType.EXECUTABLE and
                                not self.ui.consoleOutput.is_running)

    def _on_program_type_changed(self, action):
        """
        Updates current editor file type and enables/disables run action.
        """
        try:
            if action == self.ui.actionProgram:
                self.ui.tabWidgetEditors.currentWidget().file_type = \
                    FileType.EXECUTABLE
                for item in self.bt_run + [self.ui.actionRun]:
                    item.setEnabled(not self.ui.consoleOutput.is_running)
            else:
                self.ui.tabWidgetEditors.currentWidget().file_type = \
                    FileType.MODULE
                for item in self.bt_run + [self.ui.actionRun]:
                    item.setEnabled(False)
        except AttributeError:
            pass

    def compile(self):
        """
        Compiles the current editor
        """
        # ensures all editors are saved before compiling
        self.ui.tabWidgetEditors.save_all()
        # disable actions
        for bt in self.bt_compile + [self.ui.actionCompile]:
            bt.setDisabled(True)
        for item in self.bt_run + [self.ui.actionRun]:
            item.setDisabled(True)
        # reset errors
        self.ui.errorsTable.clear()
        self._errors = 0
        # prepare and start compilation thread
        self._compilation_thread = CompilationThread()
        self._compilation_thread.file_path = \
            self.app.edit.current_editor.file.path
        self._compilation_thread.file_compiled.connect(self._on_file_compiled)
        self._compilation_thread.finished.connect(
            self._on_compilation_finished)
        self._compilation_thread.start()

    def _on_compilation_finished(self):
        """
        Runs the current file if the compilation came from a run request,
        otherwise enables actions/buttons.

        """
        if self._run_requested:
            self._run_requested = False
            if self._errors == 0:
                self._run()
        else:
            for bt in self.bt_compile + [self.ui.actionCompile]:
                bt.setEnabled(True)
            for item in self.bt_run + [self.ui.actionRun]:
                item.setEnabled(self.app.edit.current_editor.file_type ==
                                FileType.EXECUTABLE)

    def _on_file_compiled(self, filename, status, messages):
        """
        Displays compilation status in errors table.
        """
        self.ui.dockWidgetLogs.show()
        self.ui.tabWidgetLogs.setCurrentIndex(0)
        if len(messages) == 0 and status == 0:
            ext = GnuCobolCompiler().extension_for_type(get_file_type(filename))
            name = os.path.splitext(os.path.split(filename)[1])[0]
            path = os.path.join(os.path.dirname(filename), 'bin', name + ext)
            self.ui.errorsTable.add_message(
                CheckerMessage(
                    'Compilation succeeded: %s' % path,
                    CheckerMessages.INFO, -1,
                    path=filename))
        else:
            for msg in messages:
                self._errors += 1
                self.ui.errorsTable.add_message(CheckerMessage(*msg))

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
        self._run_requested = True
        self.compile()

    def _run_in_external_terminal(self, program, wd):
        """
        Runs a program in an external terminal.

        :param program: program to run
        :param wd: working directory
        """
        self.ui.consoleOutput.append(
            "Launched in external terminal")
        if sys.platform == "win32":
            subprocess.Popen(
                program, cwd=wd,
                creationflags=subprocess.CREATE_NEW_CONSOLE)
        else:
            subprocess.Popen(
                Settings().external_terminal_command.split(' ') +
                [program], cwd=wd)

    def _run(self):
        """
        Runs the current editor program.
        :return:
        """
        # compilation has finished, we can run the program that corresponds
        # to the current editor file
        editor = self.app.edit.current_editor
        try:
            file_type = editor.file_type
        except AttributeError:
            pass
        else:
            if file_type == FileType.EXECUTABLE:
                self.ui.tabWidgetLogs.setCurrentIndex(1)
                self.ui.dockWidgetLogs.show()
                self.ui.consoleOutput.clear()
                wd = os.path.join(os.path.dirname(editor.file.path), 'bin')
                program = os.path.join(
                    wd, os.path.splitext(editor.file.name)[0] +
                    GnuCobolCompiler().extension_for_type(file_type))
                if not os.path.exists(program):
                    _logger().warning('cannot run %s, file does not exists',
                                      program)
                    return
                if Settings().external_terminal:
                    self._run_in_external_terminal(program, wd)
                    for item in self.bt_run + [self.ui.actionRun]:
                        item.setEnabled(True)
                else:
                    self.ui.consoleOutput.setFocus(True)
                    for item in self.bt_run + [self.ui.actionRun]:
                        item.setEnabled(False)
                    self.ui.consoleOutput.start_process(program, cwd=wd)

    def _on_run_finished(self):
        for bt in self.bt_compile + [self.ui.actionCompile]:
            bt.setEnabled(True)
        for item in self.bt_run + [self.ui.actionRun]:
            item.setEnabled(True)

    def cancel(self):
        """
        Cancels current any ongoing operations (run/compile).
        """
        if self._compilation_thread:
            self._compilation_thread.terminate()
        self.ui.consoleOutput.stop_process()
        for item in self.bt_compile + [self.ui.actionCompile]:
            item.setEnabled(True)
        for item in self.bt_run + [self.ui.actionRun]:
            item.setEnabled(self.app.edit.current_editor.file_type ==
                            FileType.EXECUTABLE)
