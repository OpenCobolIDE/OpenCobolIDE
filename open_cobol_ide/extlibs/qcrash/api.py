"""
This module contains the top level API functions.
"""
from . import _hooks
from .qt import QtCore

from . import backends


def install_backend(*args):
    """
    Install one or more backends.

    Usage::

        qcrash.install_backend(backend1)
        qcrash.install_backend(backend2, backend3)

    :param args: the backends to install. Each backend must be a subclass
        of :class:`qcrash.backends.BaseBackend` (e.g.::
        :class:`qcrash.backends.EmailBackend` or
        :class:`qcrash.backends.GithubBackend`)
    """
    global _backends
    for b in args:
        _backends.append(b)


def get_backends():
    """
    Gets the list of installed backends.
    """
    return _backends


def install_except_hook(except_hook=_hooks.except_hook):
    """
    Install an except hook that will show the crash report dialog when an
    unhandled exception has occured.

    :param except_hook: except_hook function that will be called on the main
        thread whenever an unhandled exception occured. The function takes
        two parameters: the exception object and the traceback string.
    """
    if not _backends:
        raise ValueError('no backends found, you must at least install one '
                         'backend before calling this function')
    global _except_hook
    _except_hook = _hooks.QtExceptHook(except_hook)


def set_qsettings(qsettings):
    """
    Sets the qsettings used by the backends to cache some information such
    as the user credentials. If no custom qsettings is defined, qcrash will
    use its own settings (QSettings('QCrash'))

    :param qsettings: QtCore.QSettings instance
    """
    global _qsettings
    _qsettings = qsettings


def show_report_dialog(window_title='Report an issue...',
                       window_icon=None, traceback=None, issue_title='',
                       issue_description='', parent=None,
                       modal=None, include_log=True, include_sys_info=True):
    """
    Show the issue report dialog manually.

    :param window_title: Title of dialog window
    :param window_icon: the icon to use for the dialog window
    :param traceback: optional traceback string to include in the report.
    :param issue_title: optional issue title
    :param issue_description: optional issue description
    :param parent: parent widget
    :param include_log: Initial state of the include log check box
    :param include_sys_info: Initial state of the include system info check box
    """
    if not _backends:
        raise ValueError('no backends found, you must at least install one '
                         'backend before calling this function')
    from ._dialogs.report import DlgReport
    dlg = DlgReport(_backends, window_title=window_title,
                    window_icon=window_icon, traceback=traceback,
                    issue_title=issue_title,
                    issue_description=issue_description, parent=parent,
                    include_log=include_log, include_sys_info=include_sys_info)
    if modal:
        dlg.show()
        return dlg
    else:
        dlg.exec_()


def _return_empty_string():
    return ''


#: Reference to the function to use to collect system information. Client code
#: should redefine it.
get_system_information = _return_empty_string

#: Reference to the function to use to collect the application's log.
#: Client code should redefine it.
get_application_log = _return_empty_string


_backends = []
_qsettings = QtCore.QSettings('QCrash')


__all__ = [
    'backends',
    'install_backend',
    'get_backends',
    'install_except_hook',
    'set_qsettings',
    'show_report_dialog',
    'get_application_log',
    'get_system_information',
]
