"""
This module contains all the classes related to logging the
application messages.
"""
import logging

from PyQt4.QtCore import QObject, pyqtSignal
from PyQt4.QtGui import QTextEdit, QTextCursor

import oci

from oci import __version__
from oci.settings import Settings


class TextEditWriter(QObject):
    """
    Writes to the text edit in a thread safe way

    (using an internal signal to ensure QTextEdit.append will always be
    called from the main gui thread)
    """
    _appendMsgRequested = pyqtSignal(str)

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
    logger = logging.getLogger(oci.__name__)
    formatter = logging.Formatter(
        '%(levelname)s::%(name)s::%(message)s',
        "%Y-%m-%d %H:%M:%S")
    handler = logging.StreamHandler()
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    handler = TextEditHandler(text_edit)
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    logger.setLevel(level)
    logger.info('OpenCobolIDE %s' % __version__)
