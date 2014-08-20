import locale
import os
from pyqode.qt import QtCore, QtWidgets
from ..forms import dlg_file_type_ui
from ...settings import Settings
from ...constants import TEMPLATES


class DlgNewFile(QtWidgets.QDialog, dlg_file_type_ui.Ui_Dialog):
    def path(self):
        return os.path.join(
            self.lineEditPath.text(),
            self.lineEditName.text() + self.comboBoxExtension.currentText())

    def template(self):
        """ Gets the file template"""
        return TEMPLATES[self.comboBoxType.currentIndex()]

    def __init__(self, parent):
        super().__init__(parent)
        self.setupUi(self)
        self.enable_ok()
        completer = QtWidgets.QCompleter(self)
        completer.setModel(QtWidgets.QDirModel(completer))
        self.lineEditPath.setCompleter(completer)
        self.lineEditPath.setText(os.path.expanduser("~"))
        self.prev_pth = ""

    @QtCore.Slot(str)
    def on_lineEditName_textChanged(self, txt):
        self.enable_ok()

    @QtCore.Slot(str)
    def on_lineEditPath_textChanged(self, txt):
        self.enable_ok()

    @QtCore.Slot()
    def on_toolButton_clicked(self):
        ret = QtWidgets.QFileDialog.getExistingDirectory(
            self, "Choose the program directory",
            Settings().last_path)
        if ret:
            self.lineEditPath.setText(ret)

    def enable_ok(self):
        pth = str(self.lineEditPath.text())
        bt = self.buttonBox.button(QtWidgets.QDialogButtonBox.Ok)
        name = self.lineEditName.text()
        enable = name != "" and os.path.exists(pth) and os.path.isdir(pth)
        bt.setEnabled(enable)
        self.prev_pth = pth

    @classmethod
    def create_new_file(cls, parent):
        """
        Creates a new file. Shows the new file dialog and creates the file
        on disk if the dialog has been accepted and the destination does not
        overwrite any file (or the user choose to overwrite existing file).

        :param parent: Parent widget
        :return: Path or None if the dialog has been cancelled.

        """
        dlg = cls(parent)
        if dlg.exec_() == dlg.Accepted:
            path = dlg.path()
            if os.path.exists(path):
                answer = QtWidgets.QMessageBox.question(
                    parent, 'Overwrite file',
                    'The file %s already exists. '
                    'Do you want to overwrite it?' % path,
                    QtWidgets.QMessageBox.Yes | QtWidgets.QMessageBox.No,
                    QtWidgets.QMessageBox.No)
                if answer == QtWidgets.QMessageBox.No:
                    return None
            with open(path, 'w', encoding=locale.getpreferredencoding()) as f:
                f.write(dlg.template())
            return path
        return None
