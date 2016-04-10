"""
A PyQt/PySide framework for reporting application crash (unhandled exception)
and let the user report an issue/feature request.
"""
from .base import BaseFormatter


BODY_ITEM_TEMPLATE = '''### %(name)s

%(value)s

'''

NB_LINES_MAX = 50


class MardownFormatter(BaseFormatter):
    """
    Formats the issue report using Markdown.
    """
    def format_body(self, description, sys_info=None, traceback=None):
        """
        Formats the body using markdown.

        :param description: Description of the issue, written by the user.
        :param sys_info: Optional system information string
        :param log: Optional application log
        :param traceback: Optional traceback.
        """
        body = BODY_ITEM_TEMPLATE % {
            'name': 'Description', 'value': description
        }
        if traceback:
            traceback = '\n'.join(traceback.splitlines()[-NB_LINES_MAX:])
            body += BODY_ITEM_TEMPLATE % {
                'name': 'Traceback', 'value': '```\n%s\n```' % traceback
            }
        if sys_info:
            sys_info = '- %s' % '\n- '.join(sys_info.splitlines())
            body += BODY_ITEM_TEMPLATE % {
                'name': 'System information', 'value': sys_info
            }
        return body
