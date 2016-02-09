"""
This module contains the github backend.
"""
import logging
import webbrowser

import keyring

from .base import BaseBackend
from ..formatters.markdown import MardownFormatter
from ..qt import QtGui, QtWidgets
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

    def send_report(self, title, body):
        _logger().debug('sending bug report on github\ntitle=%s\nbody=%s',
                        title, body)
        username, password, remember = self.get_user_credentials()
        if not username or not password:
            return False
        _logger().debug('got user credentials')
        try:
            gh = github.GitHub(username=username, password=password)
            repo = gh.repos(self.gh_owner)(self.gh_repo)
            ret = repo.issues.post(title=title, body=body)
        except github.ApiError as e:
            _logger().exception('failed to send bug report on github. '
                                'response=%r' % e.response)
            # invalid credentials
            if e.response.code == 401:
                self.qsettings().setValue('github/remember_credentials', 0)
                if self._show_msgbox:
                    QtWidgets.QMessageBox.warning(
                      QtWidgets.qApp.activeWindow(), 'Invalid credentials',
                      'Failed to create github issue, invalid credentials...')
            else:
                # other issue
                if self._show_msgbox:
                    QtWidgets.QMessageBox.warning(
                        QtWidgets.qApp.activeWindow(),
                        'Failed to create issue',
                        'Failed to create github issue. Error %d' %
                        e.response.code)
            return False
        else:
            issue_nbr = ret['number']
            if self._show_msgbox:
                ret = QtWidgets.QMessageBox.question(
                    QtWidgets.qApp.activeWindow(), 'Issue created on github',
                    'Issue successfully created. Would you like to open the '
                    'ticket in your web browser?')
            if ret in [QtWidgets.QMessageBox.Yes, QtWidgets.QMessageBox.Ok]:
                webbrowser.open(
                    'https://github.com/%s/%s/issues/%d' % (
                        self.gh_owner, self.gh_repo, issue_nbr))
            return True

    def get_user_credentials(self):
        remember = self.qsettings().value('github/remember_credentials', "0")
        username = self.qsettings().value('github/username', "")
        try:
            # PyQt5 or PyQt4 api v2
            remember = bool(int(remember))
        except TypeError:
            # pyside returns QVariants
            remember, _ok = remember.toInt()
            username = username.toString()

        if remember and username:
            try:
                return username, keyring.get_password(
                    'github', username), remember
            except RuntimeError:
                _logger().warn('failed to retrieve password from keyring...')
        username, password, remember = DlgGitHubLogin.login(
            QtWidgets.qApp.activeWindow(), username, remember)
        if remember:
            self.qsettings().setValue('github/username', username)
            try:
                keyring.set_password('github', username, password)
            except RuntimeError:
                _logger().warn('failed to save password in keyring, you '
                               'will be prompted for your credentials '
                               'next time you want to report an issue')
                remember = False
            self.qsettings().setValue(
                'github/remember_credentials', int(remember))
        return username, password, remember
