"""
This module contains the review dialog.
"""
from qcrash.qt import QtCore, QtGui, QtWidgets
from qcrash._forms import dlg_review_ui


class DlgReview(QtWidgets.QDialog):
    """
    Dialog for reviewing the final report.
    """
    def __init__(self, content, log, parent, window_icon):
        """
        :param content: content of the final report, before review
        :param parent: parent widget
        """
        super(DlgReview, self).__init__(parent)
        self.ui = dlg_review_ui.Ui_Dialog()
        self.ui.setupUi(self)
        self.ui.tabWidget.setCurrentIndex(0)
        self.ui.edit_main.setPlainText(content)
        self.ui.edit_main.installEventFilter(self)
        self.ui.edit_log.installEventFilter(self)
        self.setWindowFlags(self.windowFlags() & ~QtCore.Qt.WindowContextHelpButtonHint)
        self.setWindowIcon(
            QtGui.QIcon.fromTheme('document-edit')
            if window_icon is None else window_icon)
        if log:
            self.ui.edit_log.setPlainText(log)
        else:
            self.ui.tabWidget.tabBar().hide()
        self.ui.edit_main.setFocus()

    def eventFilter(self, obj, event):
        interesting_objects = [self.ui.edit_log, self.ui.edit_main]
        if obj in interesting_objects and event.type() == QtCore.QEvent.KeyPress:
            if event.key() == QtCore.Qt.Key_Return and \
                    event.modifiers() & QtCore.Qt.ControlModifier:
                self.accept()
                return True
        return False

    @classmethod
    def review(cls, content, log, parent, window_icon):  # pragma: no cover
        """
        Reviews the final bug report.

        :param content: content of the final report, before review
        :param parent: parent widget

        :returns: the reviewed report content or None if the review was
                  canceled.
        """
        dlg = DlgReview(content, log, parent, window_icon)
        if dlg.exec_():
            return dlg.ui.edit_main.toPlainText(), \
                dlg.ui.edit_log.toPlainText()
        return None, None
