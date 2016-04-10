"""
This module contains the Html formatter used by the email backend.
"""
from .base import BaseFormatter


BODY_ITEM_TEMPLATE = '''%(name)s
%(delim)s

%(value)s


'''

NB_LINES_MAX = 50


class EmailFormatter(BaseFormatter):
    """
    Formats the crash report for use in an email (text/plain)
    """
    def __init__(self, app_name=None):
        """
        :param app_name: Name of the application. If set the email subject will
             starts with [app_name]
        """
        self.app_name = app_name

    def format_title(self, title):
        """
        Formats title (add ``[app_name]`` if app_name is not None).
        """
        if self.app_name:
            return '[%s] %s' % (self.app_name, title)
        return title

    def format_body(self, description, sys_info=None, traceback=None):
        """
        Formats the body in plain text. (add a series of '-' under each section
            title).

        :param description: Description of the issue, written by the user.
        :param sys_info: Optional system information string
        :param log: Optional application log
        :param traceback: Optional traceback.
        """
        name = 'Description'
        delim = '-' * 40
        body = BODY_ITEM_TEMPLATE % {
            'name': name, 'value': description, 'delim': delim
        }
        if traceback:
            name = 'Traceback'
            traceback = '\n'.join(traceback.splitlines()[-NB_LINES_MAX:])
            body += BODY_ITEM_TEMPLATE % {
                'name': name, 'value': traceback, 'delim': delim
            }
        if sys_info:
            name = 'System information'
            body += BODY_ITEM_TEMPLATE % {
                'name': name, 'value': sys_info, 'delim': delim
            }
        return body
