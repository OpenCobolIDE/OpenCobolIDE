import sys
import pygments
import pyqode.core
import pyqode.cobol
import pyqode.qt
import platform
import logging
from github3 import login
from pyqode.qt import QtWidgets, QtCore, QtGui
from open_cobol_ide import __version__, logger
from open_cobol_ide.view.forms.dlg_report_bug_ui import Ui_Dialog
from open_cobol_ide.view.dialogs.github_login import DlgGithubLogin
from open_cobol_ide.compilers import GnuCobolCompiler
from open_cobol_ide.settings import Settings



BUG_DESCRIPTION = '''%s

## System information

%s

## Application log

```
%s
```
'''


def _logger():
    return logging.getLogger(__name__)


class DlgReportBug(QtWidgets.QDialog):
    def __init__(self, parent):
        super().__init__(parent)
        self.ui = Ui_Dialog()
        self.ui.setupUi(self)
        self.github = None
        # show the login dialog on next frame
        QtCore.QTimer.singleShot(1, self.login)
        self.ui.lineEditTitle.textChanged.connect(self.enable_submit)
        self.ui.plainTextEditDesc.textChanged.connect(self.enable_submit)
        self.ui.pushButtonSubmit.clicked.connect(self.submit)
        self.enable_submit()

    def enable_submit(self, *_):
        self.ui.pushButtonSubmit.setEnabled(
            self.ui.lineEditTitle.text().strip() != '' and
            self.ui.plainTextEditDesc.toPlainText().strip() != '' and
            self.github is not None)

    def login(self):
        if not Settings().github_oauth_token:
            user, token = DlgGithubLogin.login(self)
            if token is None:
                self.reject()
            Settings().github_oauth_token = token
            Settings().github_username = user
        self.github = login(token=Settings().github_oauth_token)
        _logger().info('github login done: %r', self.github)

    def submit(self):
        title = self.ui.lineEditTitle.text().strip()
        description = self.ui.plainTextEditDesc.toPlainText().strip()
        bug = self.ui.radioButtonBug.isChecked()
        if bug:
            title = '[Bug] %s' % title
            labels = ['Bug']
            description = BUG_DESCRIPTION % (description, self.get_system_infos(),
                                             self.get_application_log())
        else:
            title = '[Enhancement] %s' % title
            labels = ['Enhancement']
        usr = 'OpenCobolIDE'
        repo = 'OpenCobolIDE'
        try:
            issue = self.github.create_issue(usr, repo, title, description, labels=labels)
        except Exception:
            _logger().exception('Failed to create issue on github')
            issue = None
        if issue is not None:
            answer = QtWidgets.QMessageBox.question(
                self, 'Open report URL',
                'Report sucessfully submitted. Do you want to see the report '
                'in your web browser?', QtWidgets.QMessageBox.Yes | QtWidgets.QMessageBox.No,
                QtWidgets.QMessageBox.Yes)
            if answer == QtWidgets.QMessageBox.Yes:
                url = 'http://github/%s/%s/issues/%d' % (usr, repo, issue.number)
                QtGui.QDesktopServices.openUrl(QtCore.QUrl(url))
        else:
            QtGui.QClipboard().setText(description)
            QtWidgets.QMessageBox.warning(
                self, 'Failed to submit bug report',
                'An error occurred while submitting the bug report.\n\n'
                'You may report a bug manually here: '
                'https://github.com/OpenCobolIDE/OpenCobolIDE/issues/new\n\n'
                'Note that the complete bug report has been stored'
                'in the clipboard so that you can just press Ctrl+V to copy '
                'it on github!')
        self.accept()

    @classmethod
    def report_bug(cls, parent):
        dlg = cls(parent)
        dlg.exec_()

    def get_system_infos(self):
        try:
            import qdarkstyle
        except ImportError:
            qdarkstyle_version = 'Not installed'
        else:
            qdarkstyle_version = qdarkstyle.__version__

        return '\n'.join([
            '- Operating System: %s' % platform.system(),
            '- OpenCobolIDE: %s' % __version__,
            '- GnuCobol: %s' % GnuCobolCompiler().get_version(),
            '- Python: %s (%dbits)' % (platform.python_version(), 64 if sys.maxsize > 2**32 else 32),
            '- Qt: %s' % QtCore.QT_VERSION_STR,
            '- PyQt: %s' % QtCore.PYQT_VERSION_STR,
            '- pyqode.core: %s' % pyqode.core.__version__,
            '- pyqode.cobol: %s' % pyqode.cobol.__version__,
            '- pyqode.qt: %s' % pyqode.qt.__version__,
            '- pygments: %s' % pygments.__version__,
            '- QDarkStyle: %s' % qdarkstyle_version
        ])

    def get_application_log(self):
        with open(logger.get_path(), 'r') as f:
            return f.read()


if __name__ == '__main__':
    import sys
    app = QtWidgets.QApplication(sys.argv)
    DlgReportBug.report_bug(None)

