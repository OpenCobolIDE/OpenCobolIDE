from github3 import authorize, models
from pyqode.qt import QtWidgets
from open_cobol_ide.view.forms.dlg_github_login_ui import Ui_Dialog


class DlgGithubLogin(QtWidgets.QDialog):
    @property
    def token(self):
        return self._token

    @property
    def username(self):
        return self.ui.lineEditUser.text().strip()

    def __init__(self, parent):
        super().__init__(parent)
        self._token = ''
        self.ui = Ui_Dialog()
        self.ui.setupUi(self)
        self.ui.pushButton.clicked.connect(self.on_login_clicked)
        self.ui.lineEditUser.textChanged.connect(self.enable_login)
        self.ui.lineEditPassword.textChanged.connect(self.enable_login)
        self.enable_login()

    def enable_login(self, *_):
        self.ui.pushButton.setEnabled(
            self.ui.lineEditUser.text().strip()!= '' and
            self.ui.lineEditPassword.text().strip() != '')

    def on_login_clicked(self):
        usr = self.username
        password = self.ui.lineEditPassword.text().strip()

        note = 'OpenCobolIDE Bug Report Tool'
        note_url = 'https://github.com/OpenCobolIDE'
        scopes = ['user', 'repo']

        try:
            auth = authorize(usr, password, scopes, note, note_url)
        except models.GitHubError as e:
            if e.code == 422:
                QtWidgets.QMessageBox.warning(
                    self, 'Authorization failed',
                    'Authorization failed\n\n'
                    'The authorisation failed because an OAuth token already '
                    'exists.\n\n'
                    'Please, go to your Github account settings '
                    '(Applications->Personal access token), delete '
                    'OpenCobolIDE Bug Report Tool and try again!')
            else:
                QtWidgets.QMessageBox.warning(
                    self, 'Login failed', 'Login failed.\n\n%s' % e.message)
        else:
            self._token = auth.token
            self.accept()

    @classmethod
    def login(cls, parent):
        dlg = cls(parent)
        if dlg.exec_() == dlg.Accepted:
            return dlg.username, dlg.token
        else:
            return None, None


if __name__ == '__main__':
    import sys
    app = QtWidgets.QApplication(sys.argv)
    DlgGithubLogin.login(None)
