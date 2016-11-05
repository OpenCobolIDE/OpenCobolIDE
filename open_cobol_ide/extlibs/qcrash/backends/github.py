"""
This module contains the github backend.
"""
import logging
import webbrowser

import keyring

from .base import BaseBackend
from ..formatters.markdown import MardownFormatter
from ..qt import QtGui, QtCore, QtWidgets
from .._dialogs.gh_login import DlgGitHubLogin
from .._extlibs import github


GH_MARK_NORMAL = ':/rc/GitHub-Mark.png'
GH_MARK_LIGHT = ':/rc/GitHub-Mark-Light.png'


def _logger():
    return logging.getLogger(__name__)


class GithubBackend(BaseBackend):
    """
    This backend sends the crash report on a github issue tracker::

        https://github.com/gh_owner/gh_repo

    Usage::

        github_backend = qcrash.backends.GithubBackend(
            'ColinDuquesnoy', 'QCrash')
        qcrash.install_backend(github_backend)
    """
    def __init__(self, gh_owner, gh_repo, formatter=MardownFormatter()):
        """
        :param gh_owner: Name of the owner of the github repository.
        :param gh_repo: Name of the repository on github.
        """
        super(GithubBackend, self).__init__(
            formatter, "Submit on github",
            "Submit the issue on our issue tracker on github", None)
        icon = GH_MARK_NORMAL
        if QtWidgets.qApp.palette().base().color().lightness() < 128:
            icon = GH_MARK_LIGHT
        self.button_icon = QtGui.QIcon(icon)
        self.gh_owner = gh_owner
        self.gh_repo = gh_repo
        self._show_msgbox = True  # False when running the test suite

    def send_report(self, title, body, application_log=None):
        _logger().debug('sending bug report on github\ntitle=%s\nbody=%s',
                        title, body)
        username, password, remember, remember_pswd = self.get_user_credentials()
        if not username or not password:
            return False
        _logger().debug('got user credentials')

        # upload log file as a gist
        if application_log:
            url = self.upload_log_file(application_log)
            body += '\nApplication log: %s' % url
        try:
            gh = github.GitHub(username=username, password=password)
            repo = gh.repos(self.gh_owner)(self.gh_repo)
            ret = repo.issues.post(title=title, body=body)
        except github.ApiError as e:
            _logger().warn('failed to send bug report on github. response=%r' %
                           e.response)
            # invalid credentials
            if e.response.code == 401:
                self.qsettings().setValue('github/remember_credentials', 0)
                if self._show_msgbox:
                    QtWidgets.QMessageBox.warning(
                        self.parent_widget, 'Invalid credentials',
                        'Failed to create github issue, invalid credentials...')
            else:
                # other issue
                if self._show_msgbox:
                    QtWidgets.QMessageBox.warning(
                        self.parent_widget,
                        'Failed to create issue',
                        'Failed to create github issue. Error %d' %
                        e.response.code)
            return False
        else:
            issue_nbr = ret['number']
            if self._show_msgbox:
                ret = QtWidgets.QMessageBox.question(
                    self.parent_widget, 'Issue created on github',
                    'Issue successfully created. Would you like to open the '
                    'ticket in your web browser?')
            if ret in [QtWidgets.QMessageBox.Yes, QtWidgets.QMessageBox.Ok]:
                webbrowser.open(
                    'https://github.com/%s/%s/issues/%d' % (
                        self.gh_owner, self.gh_repo, issue_nbr))
            return True

    def _get_credentials_from_qsettings(self):
        remember = self.qsettings().value('github/remember_credentials', "0")
        remember_password = self.qsettings().value('github/remember_password', "0")
        username = self.qsettings().value('github/username', "")
        try:
            # PyQt5 or PyQt4 api v2
            remember = bool(int(remember))
            remember_password = bool(int(remember_password))
        except TypeError:  # pragma: no cover
            # pyside returns QVariants
            remember, _ok = remember.toInt()
            remember_password, _ok = remember_password.toInt()
            username = username.toString()
        if not remember:
            username = ''
        return username, bool(remember), bool(remember_password)

    def _store_credentials(self, username, password, remember, remember_pswd):
        if remember:
            self.qsettings().setValue('github/username', username)
        if remember_pswd:
            try:
                keyring.set_password('github', username, password)
            except RuntimeError:  # pragma: no cover
                _logger().warn('failed to save password in keyring, you '
                               'will be prompted for your credentials '
                               'next time you want to report an issue')
                remember_pswd = False
        self.qsettings().setValue(
            'github/remember_credentials', int(remember))
        self.qsettings().setValue(
            'github/remember_password', int(remember_pswd))

    def get_user_credentials(self):  # pragma: no cover
        # reason: hard to test methods that shows modal dialogs
        username, remember, remember_pswd = self._get_credentials_from_qsettings()

        if remember_pswd and username:
            # get password from keyring
            try:
                password = keyring.get_password('github', username)
            except RuntimeError:
                # no safe keyring backend
                _logger().warn('failed to retrieve password from keyring...')
            else:
                return username, password, remember, remember_pswd

        # ask for credentials
        username, password, remember, remember_pswd = DlgGitHubLogin.login(
            self.parent_widget, username, remember, remember_pswd)

        if remember:
            self._store_credentials(username, password, remember, remember_pswd)

        return username, password, remember, remember_pswd

    def upload_log_file(self, log_content):
        gh = github.GitHub()
        try:
            QtWidgets.qApp.setOverrideCursor(QtCore.Qt.WaitCursor)
            ret = gh.gists.post(
                description="OpenCobolIDE log", public=True,
                files={'OpenCobolIDE.log': {"content": log_content}})
            QtWidgets.qApp.restoreOverrideCursor()
        except github.ApiError:
            _logger().warn('failed to upload log report as a gist')
            return '"failed to upload log file as a gist"'
        else:
            return ret['html_url']
