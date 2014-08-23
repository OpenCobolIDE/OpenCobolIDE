"""
Controls the cobol specific action (compile, run and change program type)

"""
import os
from pyqode.core.api import TextHelper
from pyqode.core.modes import CheckerMessage, CheckerMessages
from pyqode.qt import QtCore, QtGui, QtWidgets
import sys
import subprocess
from .base import Controller
from ..compiler import FileType, GnuCobolCompiler, get_file_type
from open_cobol_ide.settings import Settings


class CompilationThread(QtCore.QThread):
    file_compiled = QtCore.Signal(str, int, list)
    finished = QtCore.Signal()

    filename = ''

    def run(self):
        compiler = GnuCobolCompiler()
        files = compiler.get_dependencies(self.filename, recursive=True)
        files.insert(0, self.filename)
        for f in files:
            status, messages = compiler.compile(f, get_file_type(f))
            self.file_compiled.emit(f, status, messages)
        self.finished.emit()


class CobolController(Controller):
    def __init__(self, app):
        super().__init__(app)
        group = QtWidgets.QActionGroup(self.main_window)
        group.addAction(self.ui.actionProgram)
        group.addAction(self.ui.actionSubprogram)
        icon = QtGui.QIcon.fromTheme(
            'application-x-executable',
            QtGui.QIcon(':/ide-icons/rc/application-x-executable.png'))
        self.bt_compile = QtWidgets.QToolButton()
        self.bt_compile.setIcon(icon)
        self.bt_compile.setMenu(self.ui.menuProgramType)
        self.bt_compile.setToolTip(
            'Compile file (F8)\nClick on the arrow to change program type')
        self.bt_compile.setPopupMode(self.bt_compile.MenuButtonPopup)
        self.ui.toolBarCode.insertWidget(self.ui.actionRun, self.bt_compile)
        group.triggered.connect(self._on_program_type_changed)
        self.bt_compile.clicked.connect(self.compile)
        self.ui.actionCompile.triggered.connect(self.compile)
        self.ui.errorsTable.msg_activated.connect(self._goto_error_msg)
        self.ui.actionRun.triggered.connect(self.run)
        self._run_requested = False
        self.ui.consoleOutput.process_finished.connect(self._on_run_finished)

    def display_file_type(self, editor):
        try:
            ftype = editor.file_type
        except AttributeError:
            pass
        else:
            self.ui.actionProgram.setChecked(ftype == FileType.EXECUTABLE)
            self.ui.actionSubprogram.setChecked(ftype != FileType.EXECUTABLE)
            self.ui.actionRun.setEnabled(ftype == FileType.EXECUTABLE)

    def _on_program_type_changed(self, action):
        try:
            if action == self.ui.actionProgram:
                self.ui.tabWidgetEditors.currentWidget().file_type = \
                    FileType.EXECUTABLE
                self.ui.actionRun.setEnabled(True)
            else:
                self.ui.tabWidgetEditors.currentWidget().file_type = \
                    FileType.MODULE
                self.ui.actionRun.setEnabled(False)
        except AttributeError:
            pass

    def compile(self):
        self.bt_compile.setDisabled(True)
        self.ui.actionCompile.setDisabled(True)
        self.ui.actionRun.setDisabled(True)
        self.ui.tabWidgetEditors.save_all()
        self.ui.errorsTable.clear()
        self._errors = 0
        self._compilation_thread = CompilationThread()
        self._compilation_thread.filename = \
            self.app.edit.current_editor.file.path
        self._compilation_thread.file_compiled.connect(self._on_file_compiled)
        self._compilation_thread.finished.connect(
            self._on_compilation_finished)
        self._compilation_thread.start()

    def _on_compilation_finished(self):
        self.bt_compile.setEnabled(True)
        self.ui.actionCompile.setEnabled(True)
        self.ui.actionRun.setEnabled(
            self.app.edit.current_editor.file_type == FileType.EXECUTABLE)
        if self._run_requested:
            self._run_requested = False
            if self._errors == 0:
                self._run()

    def _on_file_compiled(self, filename, status, messages):
        self.ui.dockWidgetLogs.show()
        self.ui.tabWidgetLogs.setCurrentIndex(0)
        if len(messages) == 0 and status == 0:
            ext = GnuCobolCompiler().extension_for_type(get_file_type(
                filename))
            self.ui.errorsTable.add_message(
                CheckerMessage(
                    'Compilation succeeded: %s' %
                    os.path.join(
                        os.path.dirname(filename), 'bin',
                        os.path.splitext(os.path.split(filename)[1])[0] + ext),
                    CheckerMessages.INFO, -1,
                    path=filename))
        else:
            for msg in messages:
                self._errors += 1
                self.ui.errorsTable.add_message(CheckerMessage(*msg))

    def _goto_error_msg(self, msg):
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

    def _run(self):
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
                    return
                if Settings().external_terminal:
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
                else:
                    self.ui.consoleOutput.setFocus(True)
                    self.ui.actionRun.setEnabled(False)
                    self.ui.consoleOutput.start_process(program, cwd=wd)

    def _on_run_finished(self):
        self.ui.actionRun.setEnabled(True)