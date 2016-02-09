"""
This module contains the go to line dialog.
"""
from pyqode.core._forms import dlg_goto_line_ui
from pyqode.qt import QtWidgets


class DlgGotoLine(QtWidgets.QDialog, dlg_goto_line_ui.Ui_Dialog):
    """
    Goto line dialog.
    """
    def __init__(self, parent, current_line, line_count):
        QtWidgets.QDialog.__init__(self, parent)
        dlg_goto_line_ui.Ui_Dialog.__init__(self)
        self.setupUi(self)
        self.buttonBox.accepted.connect(self.accept)
        self.buttonBox.rejected.connect(self.reject)
        self.spinBox.setMaximum(line_count)
        self.spinBox.setValue(current_line)
        self.lblCurrentLine.setText("%d" % current_line)
        self.lblLineCount.setText("%d" % line_count)
        self.buttonBox.button(self.buttonBox.Ok).setText(_("Go"))
        self.buttonBox.button(self.buttonBox.Cancel).setText(
            "I'm going nowhere")
        self.spinBox.setFocus()

    @classmethod
    def get_line(cls, parent, current_line, line_count):
        """
        Gets user selected line.

        :param parent: Parent widget
        :param current_line: Current line number
        :param line_count: Number of lines in the current text document.

        :returns: tuple(line, status) status is False if the dialog has been
            rejected.
        """
        dlg = DlgGotoLine(parent, current_line + 1, line_count)
        if dlg.exec_() == dlg.Accepted:
            return dlg.spinBox.value() - 1, True
        return current_line, False
