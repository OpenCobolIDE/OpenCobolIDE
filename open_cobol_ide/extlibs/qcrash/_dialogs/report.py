import logging

from qcrash._forms import dlg_report_bug_ui
from qcrash.qt import QtGui, QtWidgets


_logger = logging.getLogger(__name__)


LOG_LIMIT = 100  # keep only the last 100 lines


class DlgReport(QtWidgets.QDialog):
    """
    A qt dialog for reporting an issue on github or via email.

    The dialog let the user choose whether he wants to report an issue or
    propose an enhancement. Depending on its choice the issue title will be
    prefixed by "[Bug]" or "Enhancement". The user must write a description
    of the issue. User can choose to send the report via email or on github
    (using the user's credentials).

    The client code can specify some additional information to be included in
    the report (but hidden in thr dialog).

        - sys_infos: information about the system
        - traceback: the traceback of an unhandled exception
        - log: the application log (will be truncated).

    """

    def __init__(self, backends, window_title='Report an issue...',
                 window_icon=None, traceback=None, issue_title='',
                 issue_description='', **kwargs):
        """
        """
        super(DlgReport, self).__init__(**kwargs)
        self._traceback = traceback
        self.ui = dlg_report_bug_ui.Ui_Dialog()
        self.ui.setupUi(self)
        self.setWindowTitle(window_title)
        self.setWindowIcon(QtGui.QIcon.fromTheme('tools-report-bug')
                           if window_icon is None else window_icon)
        self.ui.lineEditTitle.setText(issue_title)
        self.ui.plainTextEditDesc.setPlainText(issue_description)

        self.ui.lineEditTitle.textChanged.connect(self._enable_buttons)
        self.ui.plainTextEditDesc.textChanged.connect(self._enable_buttons)

        self.buttons = []
        for backend in backends:
            bt = QtWidgets.QPushButton()
            bt.setText(backend.button_text)
            if backend.button_icon:
                bt.setIcon(backend.button_icon)
            bt.backend = backend
            bt.clicked.connect(self._on_button_clicked)
            self.ui.layout_buttons.addWidget(bt)
            self.buttons.append(bt)
        self._enable_buttons()

    def _enable_buttons(self, *_):
        title = str(self.ui.lineEditTitle.text()).strip()
        desc = str(self.ui.plainTextEditDesc.toPlainText()).strip()
        enable = title != '' and desc != ''
        for bt in self.buttons:
            bt.setEnabled(enable)

    def _on_button_clicked(self):
        from qcrash import api
        bt = self.sender()
        description = self.ui.plainTextEditDesc.toPlainText()
        backend = bt.backend
        title = backend.formatter.format_title(
            str(self.ui.lineEditTitle.text()))
        body = backend.formatter.format_body(
            str(description), api.get_system_information(),
            api.get_application_log(), self._traceback)
        if backend.send_report(title, body):
            self.accept()
