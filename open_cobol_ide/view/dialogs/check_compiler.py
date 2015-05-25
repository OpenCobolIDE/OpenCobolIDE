import locale
import os
import tempfile
from pyqode.qt import QtCore, QtWidgets
from open_cobol_ide.view.forms import dlg_check_compiler_ui
from open_cobol_ide import system


class DlgCheckCompiler(QtWidgets.QDialog):
    def __init__(self, compiler, version, parent):
        super().__init__(parent)
        self._compiler = compiler
        self.ui = dlg_check_compiler_ui.Ui_Dialog()
        self.ui.setupUi(self)
        self.ui.plainTextEdit.setPlainText(version)
        self.ui.buttonBox.button(self.ui.buttonBox.Apply).setText('Check compiler')
        self.ui.buttonBox.button(self.ui.buttonBox.Apply).clicked.connect(self._check_compiler)

    def _check_compiler(self):
        from open_cobol_ide.view.dialogs.preferences import DEFAULT_TEMPLATE

        cbl_path = os.path.join(tempfile.gettempdir(), 'test.cbl')
        with open(cbl_path, 'w') as f:
            f.write(DEFAULT_TEMPLATE)

        output = os.path.join(tempfile.gettempdir(),
            'test' + ('.exe' if system.windows else ''))

        p = QtCore.QProcess()
        print(self._compiler, ['-x', '-o', output, cbl_path])
        p.start(self._compiler, ['-x', '-o', output, cbl_path])
        p.waitForFinished()
        stdout = bytes(p.readAllStandardOutput()).decode(locale.getpreferredencoding())
        stderr = bytes(p.readAllStandardError()).decode(locale.getpreferredencoding())
        self.ui.label.setText('Output')
        output = stderr + stdout
        if p.exitStatus() == 0:
            output = 'Compiler works!\n' + output
        self.ui.plainTextEdit.setPlainText(output)
        self.ui.buttonBox.button(self.ui.buttonBox.Ok).setEnabled(p.exitCode() == 0)

    @classmethod
    def check(cls, parent, compiler_path, version):
        dlg = cls(compiler_path, version, parent)
        return dlg.exec_() == dlg.Accepted
