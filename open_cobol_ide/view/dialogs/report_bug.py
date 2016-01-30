import github
import keyring
import logging
import pygments
import pyqode.core
import pyqode.cobol
import pyqode.qt
import platform
import sys
import webbrowser
from pyqode.qt import QtWidgets, QtCore, QtGui
from open_cobol_ide import __version__, logger
from open_cobol_ide.settings import Settings
from open_cobol_ide.view.forms.dlg_report_bug_ui import Ui_Dialog
from open_cobol_ide.view.forms import dlg_github_login_ui
from open_cobol_ide.compilers import GnuCobolCompiler


BUG_DESCRIPTION = '''%s

## System information

%s

## Application log

```
%s
```

'''

EMAIL_ADDRESS = 'colin.duquesnoy@gmail.com'


def _logger():
    return logging.getLogger(__name__)


class DlgReportBug(QtWidgets.QDialog):
    def __init__(self, parent, title='', description=''):
        super().__init__(parent)
        self.ui = Ui_Dialog()
        self.ui.setupUi(self)
        self.ui.lineEditTitle.textChanged.connect(self.enable_submit)
        self.ui.plainTextEditDesc.textChanged.connect(self.enable_submit)
        self.ui.pushButtonSubmit.clicked.connect(self.submit)
        self.ui.pushButtonSendEmail.clicked.connect(self.send_email)
        self.ui.lineEditTitle.setText(title)
        self.ui.plainTextEditDesc.setPlainText(description)
        self.enable_submit()

    def enable_submit(self, *_):
        self.ui.pushButtonSubmit.setEnabled(
            self.ui.lineEditTitle.text().strip() != '' and
            self.ui.plainTextEditDesc.toPlainText().strip() != '')
        self.ui.pushButtonSendEmail.setEnabled(
            self.ui.pushButtonSubmit.isEnabled())

    def _get_data(self):
        title = self.ui.lineEditTitle.text().strip()
        description = self.ui.plainTextEditDesc.toPlainText().strip()
        bug = self.ui.radioButtonBug.isChecked()
        if bug:
            title = '[Bug] %s' % title
            description = BUG_DESCRIPTION % (
                description, self.get_system_infos(),
                self.get_application_log())
        else:
            title = '[Enhancement] %s' % title
        return {'title': title, 'body': description}

    def submit(self):
        data = self._get_data()
        username, password, remember = self.get_user_credentials()
        if not username or not password:
            return
        try:
            gh = github.GitHub(username=username, password=password)
            repo = gh.repos('ColinDuquesnoy')('TestBugReport')
            ret = repo.issues.post(title=data['title'], body=data['body'])
        except github.ApiError:
            _logger().exception('failed to send bug report')
            QtWidgets.QMessageBox.warning(
                self, 'Failed to create issue',
                'Failed to create github issue, see the application log for '
                'more information...')
        else:
            issue_nbr = ret['number']
            ret = QtWidgets.QMessageBox.question(
                self, 'Issue created on github',
                'Issue successfully created. Would you like to open the ticket'
                ' in your web browser?')
            if ret == QtWidgets.QMessageBox.Yes:
                webbrowser.open(
                    'https://github.com/ColinDuquesnoy/TestBugReport/issues/'
                    '%d' % issue_nbr)
            self.accept()

    def send_email(self):
        data = self._get_data()
        url = QtCore.QUrl("mailto:%s?subject=%s&body=%s" %
                          (EMAIL_ADDRESS, data['title'], data['body']))
        QtGui.QDesktopServices.openUrl(url)

    def get_user_credentials(self):
        remember = Settings().remember_github_credentials
        username = Settings().github_username
        if remember and username:
            return username, keyring.get_password('github', username), remember
        else:
            username, password, remember = DlgGitHubLogin.login(self)
            if remember:
                Settings().remember_github_credentials = remember
                Settings().github_username = username
                keyring.set_password('github', username, password)
            return username, password, remember

    @classmethod
    def report_bug(cls, parent, title='', description=''):
        dlg = cls(parent, title=title, description=description)
        dlg.exec_()

    def get_system_infos(self):
        try:
            import qdarkstyle
        except ImportError:
            qdarkstyle_version = 'Not installed'
        else:
            qdarkstyle_version = qdarkstyle.__version__

        system = platform.platform()
        if 'linux' in sys.platform.lower():
            system += ' (%s)' % system.linux_distribution()
        elif 'darwin' in sys.platform.lower():
            system += ' (%s)' % platform.mac_ver()[0]
        return '\n'.join([
            '- Operating System: %s' % system,
            '- OpenCobolIDE: %s' % __version__,
            '- GnuCOBOL: %s' % GnuCobolCompiler().get_version(
                include_all=False),
            '- Python: %s (%dbits)' % (platform.python_version(), 64
                                       if sys.maxsize > 2**32 else 32),
            '- Qt: %s' % QtCore.QT_VERSION_STR,
            '- PyQt: %s' % QtCore.PYQT_VERSION_STR,
            '- pyqode.core: %s' % pyqode.core.__version__,
            '- pyqode.cobol: %s' % pyqode.cobol.__version__,
            '- pyqode.qt: %s' % pyqode.qt.__version__,
            '- pygments: %s' % pygments.__version__,
            '- QDarkStyle: %s' % qdarkstyle_version
        ])

    def get_application_log(self):
        try:
            with open(logger.get_path(), 'r') as f:
                content = f.read()
            lines = []
            for l in content.splitlines():
                if l.strip():
                    lines.append(l)
            return '\n'.join(lines[-100:])
        except FileNotFoundError:
            return ''


class DlgGitHubLogin(QtWidgets.QDialog):
    HTML = '<html><head/><body><p align="center"><img src="%s"/></p>' \
        '<p align="center">Sign in to GitHub</p></body></html>'
    GH_MARK_NORMAL = ':/ide-icons/rc/GitHub-Mark.png'
    GH_MARK_LIGHT = ':/ide-icons/rc/GitHub-Mark-Light.png'

    def __init__(self, parent):
        super().__init__(parent)
        self.ui = dlg_github_login_ui.Ui_Dialog()
        self.ui.setupUi(self)

        mark = self.GH_MARK_NORMAL
        if self.palette().base().color().lightness() < 128:
            mark = self.GH_MARK_LIGHT
        html = self.HTML % mark
        self.ui.lbl_html.setText(html)
        self.ui.bt_sign_in.clicked.connect(self.accept)
        self.ui.le_username.textChanged.connect(self.update_btn_state)
        self.ui.le_password.textChanged.connect(self.update_btn_state)
        self.ui.bt_sign_in.setDisabled(True)
        self.ui.le_username.setText(Settings().github_username)

    def update_btn_state(self):
        enable = self.ui.le_username.text().strip() != ''
        enable &= self.ui.le_password.text().strip() != ''
        self.ui.bt_sign_in.setEnabled(enable)

    @classmethod
    def login(cls, parent):
        dlg = DlgGitHubLogin(parent)
        if dlg.exec_() == dlg.Accepted:
            return dlg.ui.le_username.text(), dlg.ui.le_password.text(), \
                dlg.ui.cb_remember.isChecked()
        return None, None, None
