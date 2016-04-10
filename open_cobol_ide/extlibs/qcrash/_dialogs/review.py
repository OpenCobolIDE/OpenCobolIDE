"""
This module contains the review dialog.
"""
from qcrash.qt import QtWidgets
from qcrash._forms import dlg_review_ui


class DlgReview(QtWidgets.QDialog):
    """
    Dialog for reviewing the final report.
    """
    def __init__(self, content, log, parent):
        """
        :param content: content of the final report, before review
        :param parent: parent widget
        """
        super(DlgReview, self).__init__(parent)
        self.ui = dlg_review_ui.Ui_Dialog()
        self.ui.setupUi(self)
        self.ui.edit_main.setPlainText(content)
        if log:
            self.ui.edit_log.setPlainText(log)
        else:
            self.ui.tabWidget.tabBar().hide()

    @classmethod
    def review(cls, content, log, parent):  # pragma: no cover
        """
        Reviews the final bug report.

        :param content: content of the final report, before review
        :param parent: parent widget

        :returns: the reviewed report content or None if the review was
                  canceled.
        """
        dlg = DlgReview(content, log, parent)
        if dlg.exec_():
            return dlg.ui.edit_main.toPlainText(), dlg.ui.edit_log.toPlainText()
        return None, None
