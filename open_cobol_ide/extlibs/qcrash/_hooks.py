"""
This package two hooks functions:

- a hook for pyqt distutis to replace PyQt5 imports by our own (qcrash.qt).
- a sys.excepthook is installed by :meth:`qcrash.api.install_except_hook`
"""
import logging
import sys
import traceback

from .qt import QtCore, QtWidgets


def _logger():
    return logging.getLogger(__name__)


try:
    Signal = QtCore.pyqtSignal
except AttributeError:
    Signal = QtCore.Signal


def fix_qt_imports(path):
    with open(path, 'r') as f_script:
        lines = f_script.read().splitlines()
    new_lines = []
    for l in lines:
        if l.startswith("import "):
            l = "from . " + l
        if "from PyQt5 import" in l:
            l = l.replace("from PyQt5 import", "from qcrash.qt import")
        new_lines.append(l)
    with open(path, 'w') as f_script:
        f_script.write("\n".join(new_lines))


def except_hook(exc, tb):
    from qcrash.api import show_report_dialog
    title = '[Unhandled exception]  %s: %s' % (
        exc.__class__.__name__, str(exc))
    msg_box = QtWidgets.QMessageBox()
    msg_box.setWindowTitle('Unhandled exception')
    msg_box.setText('An unhandled exception has occured...')
    msg_box.setInformativeText(
        'Would you like to report the bug to the developers?')
    msg_box.setIcon(msg_box.Critical)
    msg_box.setDetailedText(tb)
    msg_box.setStandardButtons(QtWidgets.QMessageBox.Ok |
                               QtWidgets.QMessageBox.Cancel)
    msg_box.button(msg_box.Ok).setText('Report')
    msg_box.button(msg_box.Cancel).setText('Close')
    if msg_box.exec_() == msg_box.Ok:
        show_report_dialog(window_title='Report unhandled exception',
                           issue_title=title, traceback=tb)


class QtExceptHook(QtCore.QObject):
    _report_exception_requested = Signal(object, object)

    def __init__(self, except_hook, *args, **kwargs):
        super(QtExceptHook, self).__init__(*args, **kwargs)
        sys.excepthook = self._except_hook
        self._report_exception_requested.connect(except_hook)

    def _except_hook(self, exc_type, exc_val, tb):
        tb = '\n'.join([''.join(traceback.format_tb(tb)),
                        '{0}: {1}'.format(exc_type.__name__, exc_val)])
        _logger().critical('unhandled exception:\n%s', tb)
        # exception might come from another thread, use a signal
        # so that we can be sure we will show the bug report dialog from
        # the main gui thread.
        self._report_exception_requested.emit(exc_val, tb)
