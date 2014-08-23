"""
Controls the cobol specific action (compile, run and change program type)

"""
import os
from pyqode.core.modes import CheckerMessage, CheckerMessages
from pyqode.qt import QtCore, QtGui, QtWidgets
from .base import Controller
from ..compiler import FileType, GnuCobolCompiler, get_file_type


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

    def display_file_type(self, editor):
        try:
            ftype = editor.file_type
        except AttributeError:
            pass
        else:
            self.ui.actionProgram.setChecked(ftype == FileType.EXECUTABLE)
            self.ui.actionSubprogram.setChecked(ftype != FileType.EXECUTABLE)

    def _on_program_type_changed(self, action):
        try:
            if action == self.ui.actionProgram:
                self.ui.tabWidgetEditors.currentWidget().file_type = \
                    FileType.EXECUTABLE
            else:
                self.ui.tabWidgetEditors.currentWidget().file_type = \
                    FileType.MODULE
        except AttributeError:
            pass

    def compile(self):
        self.ui.tabWidgetEditors.save_all()
        self.ui.errorsTable.clear()
        self._compilation_thread = CompilationThread()
        self._compilation_thread.filename = \
            self.app.edit.current_editor.file.path
        self._compilation_thread.file_compiled.connect(self._on_file_compiled)
        self._compilation_thread.start()

    def _on_file_compiled(self, filename, status, messages):
        self.ui.dockWidgetLogs.show()
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
                self.ui.errorsTable.add_message(CheckerMessage(*msg))
