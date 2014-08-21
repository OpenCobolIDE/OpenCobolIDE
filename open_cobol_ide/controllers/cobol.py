"""
Controls the cobol specific action (compile, run and change program type)

"""
from pyqode.qt import QtWidgets, QtGui
from .base import Controller
from ..compilers import FileType


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
        self.bt_compile.clicked.connect(self.ui.actionCompile.triggered.emit)
        self.bt_compile.setIcon(icon)
        self.bt_compile.setMenu(self.ui.menuProgramType)
        self.bt_compile.setToolTip(
            'Compile file (F8)\nClick on the arrow to change program type')
        self.bt_compile.setPopupMode(self.bt_compile.MenuButtonPopup)
        self.ui.toolBarCode.insertWidget(self.ui.actionRun, self.bt_compile)
        group.triggered.connect(self._on_program_type_changed)

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