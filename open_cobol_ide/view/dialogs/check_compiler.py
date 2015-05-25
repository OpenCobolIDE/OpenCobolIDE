import locale
import os
import tempfile
from open_cobol_ide.compilers import GnuCobolCompiler
from pyqode.qt import QtCore, QtWidgets
from open_cobol_ide.view.forms import dlg_check_compiler_ui
from open_cobol_ide import system


class DlgCheckCompiler(QtWidgets.QDialog):
    def __init__(self, compiler, version, parent):
        super().__init__(parent, QtCore.Qt.WindowSystemMenuHint | QtCore.Qt.WindowTitleHint |
                         QtCore.Qt.WindowCloseButtonHint)
        self._compiler = compiler
        self.ui = dlg_check_compiler_ui.Ui_Dialog()
        self.ui.setupUi(self)
        self.ui.plainTextEdit.setPlainText(version)
        self.ui.buttonBox.button(self.ui.buttonBox.Apply).setText('Check compiler')
        self.ui.buttonBox.button(self.ui.buttonBox.Apply).clicked.connect(self._check_compiler)
        self.ui.buttonBox.button(self.ui.buttonBox.Ok).setEnabled(False)
        self.ui.buttonBox.button(self.ui.buttonBox.Apply).setDisabled(not version)

    def _check_compiler(self):
        output, exit_code = GnuCobolCompiler.check_compiler(self._compiler)
        self.ui.label.setText('Output:')
        self.ui.plainTextEdit.setPlainText(output)
        self.ui.buttonBox.button(self.ui.buttonBox.Ok).setEnabled(exit_code == 0)

    @classmethod
    def check(cls, parent, compiler_path, version):
        dlg = cls(compiler_path, version, parent)
        return dlg.exec_() == dlg.Accepted
