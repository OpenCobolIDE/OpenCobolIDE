import logging

from qcrash._forms import dlg_report_bug_ui
from qcrash.qt import QtCore, QtGui, QtWidgets
from qcrash._dialogs.review import DlgReview


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
                 issue_description='', include_log=True, include_sys_info=True,
                 **kwargs):
        super(DlgReport, self).__init__(**kwargs)
        self._traceback = traceback
        self.window_icon = window_icon
        self.ui = dlg_report_bug_ui.Ui_Dialog()
        self.ui.setupUi(self)
        self.ui.cb_include_sys_info.setChecked(include_sys_info)
        self.ui.cb_include_application_log.setChecked(include_log)
        self.setWindowTitle(window_title)
        self.setWindowIcon(QtGui.QIcon.fromTheme('tools-report-bug')
                           if window_icon is None else window_icon)
        self.ui.lineEditTitle.setText(issue_title)
        self.ui.plainTextEditDesc.setPlainText(issue_description)
        self.setWindowFlags(self.windowFlags() & ~QtCore.Qt.WindowContextHelpButtonHint)

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
        backend.parent_widget = self
        title = backend.formatter.format_title(
            str(self.ui.lineEditTitle.text()))

        sys_info = None
        if self.ui.cb_include_sys_info.isChecked():
            sys_info = api.get_system_information()

        log = None
        if self.ui.cb_include_application_log.isChecked():
            log = api.get_application_log()

        body = backend.formatter.format_body(
            str(description), sys_info, self._traceback)

        if backend.need_review:  # pragma: no cover
            body, log = DlgReview.review(body, log, self, self.window_icon)
            if body is None and log is None:
                return  # user cancelled the review dialog

        try:
            if backend.send_report(title, body, log):
                self.accept()
        except Exception as e:
            QtWidgets.QMessageBox.warning(self, "Failed to send report", "Failed to send report.\n\n%r" % e)
