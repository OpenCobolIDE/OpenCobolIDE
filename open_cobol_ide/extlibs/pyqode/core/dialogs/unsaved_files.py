"""
This module contains the unsaved files dialog.
"""
from pyqode.qt.QtWidgets import QDialog
from pyqode.core._forms.dlg_unsaved_files_ui import Ui_Dialog
from pyqode.qt import QtWidgets, QtCore


class DlgUnsavedFiles(QDialog, Ui_Dialog):
    """
    This dialog shows the list of unsaved file in the CodeEditTabWidget.

    Use can choose to:
    - cancel: nothing changed, no tab will be closed
    - save all/save selected: save the selected files or all files
    - discard all changes: nothing will be saved but all tabs will be
    closed.

    """
    def __init__(self, parent, files=None):
        if files is None:
            files = []
        QtWidgets.QDialog.__init__(self, parent)
        Ui_Dialog.__init__(self)
        self.setupUi(self)
        self.bt_save_all = self.buttonBox.button(
            QtWidgets.QDialogButtonBox.SaveAll)
        self.bt_save_all.clicked.connect(self.accept)
        self.discarded = False
        self.bt_discard = self.buttonBox.button(
            QtWidgets.QDialogButtonBox.Discard)
        self.bt_discard.clicked.connect(self._set_discarded)
        self.bt_discard.clicked.connect(self.accept)
        for file in files:
            self._add_file(file)
        self.listWidget.itemSelectionChanged.connect(
            self._on_selection_changed)
        self._on_selection_changed()

    def _add_file(self, path):
        icon = QtWidgets.QFileIconProvider().icon(QtCore.QFileInfo(path))
        item = QtWidgets.QListWidgetItem(icon, path)
        self.listWidget.addItem(item)

    def _set_discarded(self):
        self.discarded = True

    def _on_selection_changed(self):
        nb_items = len(self.listWidget.selectedItems())
        if nb_items == 0:
            self.bt_save_all.setText(_("Save"))
            self.bt_save_all.setEnabled(False)
        else:
            self.bt_save_all.setEnabled(True)
            self.bt_save_all.setText(_("Save selected"))
            if nb_items == self.listWidget.count():
                self.bt_save_all.setText(_("Save all"))
