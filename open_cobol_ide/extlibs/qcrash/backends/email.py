"""
This module containes the email backend.
"""
from .base import BaseBackend
from ..formatters.email import EmailFormatter
from ..qt import QtCore, QtGui


class EmailBackend(BaseBackend):
    """
    This backend sends the crash report via email (using mailto).

    Usage::

        email_backend = qcrash.backends.EmailBackend(
            'your_email@provider.com', 'YourAppName')
        qcrash.install_backend(email_backend)

    """
    def __init__(self, email, app_name, formatter=EmailFormatter()):
        """
        :param email: email address to send the bug report to
        :param app_name: application name, will appear in the object of the
                         mail
        :param formatter: the formatter to use to create the final report.
        """
        super(EmailBackend, self).__init__(
            formatter, "Send email", "Send the report via email",
            QtGui.QIcon.fromTheme('mail-send'), need_review=False)
        self.formatter.app_name = app_name
        self.email = email

    def send_report(self, title, body):
        url = QtCore.QUrl(
            "mailto:%s?subject=%s&body=%s" % (self.email, title, body))
        QtGui.QDesktopServices.openUrl(url)
        return False
