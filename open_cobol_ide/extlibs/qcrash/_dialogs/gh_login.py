from qcrash.qt import QtCore, QtWidgets

from qcrash._forms import dlg_github_login_ui


GH_MARK_NORMAL = ':/rc/GitHub-Mark.png'
GH_MARK_LIGHT = ':/rc/GitHub-Mark-Light.png'


class DlgGitHubLogin(QtWidgets.QDialog):
    HTML = '<html><head/><body><p align="center"><img src="%s"/></p>' \
        '<p align="center">Sign in to GitHub</p></body></html>'

    def __init__(self, parent, username, remember, remember_password):
        super(DlgGitHubLogin, self).__init__(parent)
        self.ui = dlg_github_login_ui.Ui_Dialog()
        self.ui.setupUi(self)
        self.setWindowFlags(self.windowFlags() & ~QtCore.Qt.WindowContextHelpButtonHint)
        self.ui.cb_remember.toggled.connect(
            self.ui.cb_remember_password.setEnabled)

        mark = GH_MARK_NORMAL
        if self.palette().base().color().lightness() < 128:
            mark = GH_MARK_LIGHT
        html = self.HTML % mark
        self.ui.lbl_html.setText(html)
        self.ui.bt_sign_in.clicked.connect(self.accept)
        self.ui.le_username.textChanged.connect(self.update_btn_state)
        self.ui.le_password.textChanged.connect(self.update_btn_state)
        self.ui.bt_sign_in.setDisabled(True)
        self.ui.le_username.setText(username)
        self.ui.cb_remember.setChecked(remember)
        self.ui.cb_remember_password.setChecked(remember_password)
        self.ui.cb_remember_password.setEnabled(remember)
        if username:
            self.ui.le_password.setFocus()
        else:
            self.ui.le_username.setFocus()
        self.adjustSize()
        self.setFixedSize(self.width(), self.height())
        self.ui.le_password.installEventFilter(self)
        self.ui.le_username.installEventFilter(self)

    def eventFilter(self, obj, event):
        interesting_objects = [self.ui.le_password, self.ui.le_username]
        if obj in interesting_objects and event.type() == QtCore.QEvent.KeyPress:
            if event.key() == QtCore.Qt.Key_Return and event.modifiers() & QtCore.Qt.ControlModifier and \
                    self.ui.bt_sign_in.isEnabled():
                self.accept()
                return True
        return False

    def update_btn_state(self):
        enable = str(self.ui.le_username.text()).strip() != ''
        enable &= str(self.ui.le_password.text()).strip() != ''
        self.ui.bt_sign_in.setEnabled(enable)

    @classmethod
    def login(cls, parent, username, remember, remember_pswd):  # pragma: no cover
        dlg = DlgGitHubLogin(parent, username, remember, remember_pswd)
        if dlg.exec_() == dlg.Accepted:
            return dlg.ui.le_username.text(), dlg.ui.le_password.text(), \
                dlg.ui.cb_remember.isChecked(), \
                dlg.ui.cb_remember_password.isEnabled() and dlg.ui.cb_remember_password.isChecked()
        return None, None, None, None
