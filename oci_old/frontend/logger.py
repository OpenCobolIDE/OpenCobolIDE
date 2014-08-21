"""
This module contains all the classes related to logging the
application messages.
"""
import logging

from pyqode.core.qt.QtCore import QObject, Signal
from pyqode.core.qt.QtGui import QTextCursor

import oci_old

from oci_old import __version__
from oci_old.settings import Settings


class TextEditWriter(QObject):
    """
    Writes to the text edit in a thread safe way

    (using an internal signal to ensure QTextEdit.append will always be
    called from the main gui thread)
    """
    _appendMsgRequested = Signal(str)

    def __init__(self, text_edit):
        super().__init__()
        self.text_edit = text_edit
        self._appendMsgRequested.connect(self._append)

    def _append(self, message):
        self.text_edit.append(message)
        self.text_edit.moveCursor(QTextCursor.End)

    def requestAppend(self, message):
        self._appendMsgRequested.emit(message)


class TextEditHandler(logging.Handler):
    def __init__(self, text_edit):
        super().__init__()
        self.writer = TextEditWriter(text_edit)

    def emit(self, record):
        assert isinstance(record, logging.LogRecord)
        self.writer.requestAppend(self.formatter.format(record))


def setup(text_edit):
    """
    Sets up the root logger with a custom formatter and 2 handlers:
        - a QTextEdit handler
        - a StreamHandler

    The default log level is info and can be changed through the gui.

    :param text_edit: QTextEdit where to log messages
    """
    level = logging.DEBUG if Settings().debugLog else logging.INFO
    loggers = [logging.getLogger(oci_old.__name__), logging.getLogger('pyqode')]
    formatter = logging.Formatter(
        '%(levelname)s::%(name)s::%(message)s',
        "%Y-%m-%d %H:%M:%S")
    handlers = [logging.StreamHandler(), TextEditHandler(text_edit)]
    loggers[0].setLevel(level)
    loggers[1].setLevel(level)
    for handler in handlers:
        handler.setFormatter(formatter)
    for logger in loggers:
        for handler in handlers:
            logger.addHandler(handler)
        logger.info('OpenCobolIDE %s' % __version__)


def close():
    logging.getLogger(oci_old.__name__).handlers[:] = [logging.StreamHandler()]
    logging.getLogger('pyqode').handlers[:] = [logging.StreamHandler()]
