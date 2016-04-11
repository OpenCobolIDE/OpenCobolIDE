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

    def send_report(self, title, body, application_log=None):
        base_url = "mailto:%s?subject=%s" % (self.email, title)
        if application_log:
            body += "\nApplication log\n----------------\n\n%s" % application_log
        base_url += '&body=%s' % body
        url = QtCore.QUrl(base_url)
        QtGui.QDesktopServices.openUrl(url)
        return False
