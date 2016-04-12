"""
This module contains the base class for implementing a report backend.
"""


class BaseBackend(object):
    """
    Base class for implementing a backend.

    Subclass must define ``button_text``, ``button_tooltip``and ``button_icon``
    and implement ``send_report(title, description)``.

    The report's title and body will be formatted automatically by the
    associated :attr:`formatter`.
    """
    def __init__(self, formatter, button_text, button_tooltip,
                 button_icon=None, need_review=True):
        """
        :param formatter: the associated formatter (see :meth:`set_formatter`)
        :param button_text: Text of the associated button in the report dialog
        :param button_icon: Icon of the associated button in the report dialog
        :param button_tooltip: Tooltip of the associated button in the report
            dialog
        :param need_review: True to show the review dialog before submitting.
            Some backends (such as the email backend) do not need a review
            dialog as the user can already review it before sending the final
            report
        """
        self.formatter = formatter
        self.button_text = button_text
        self.button_tooltip = button_tooltip
        self.button_icon = button_icon
        self.need_review = need_review
        self.parent_widget = None

    def qsettings(self):
        """
        Gets the qsettings instance that you can use to store various settings
        such as the user credentials (you should use the `keyring` module if
        you want to store user's password).
        """
        from qcrash.api import _qsettings
        return _qsettings

    def set_formatter(self, formatter):
        """
        Sets the formatter associated with the backend.

        The formatter will automatically get called to format the report title
        and body before ``send_report`` is being called.
        """
        self.formatter = formatter

    def send_report(self, title, body, application_log=None):
        """
        Sends the actual bug report.

        :param title: title of the report, already formatted.
        :param body: body of the reporit, already formtatted.
        :param application_log: Content of the application log. Default is None.

        :returns: Whether the dialog should be closed.
        """
        raise NotImplementedError
