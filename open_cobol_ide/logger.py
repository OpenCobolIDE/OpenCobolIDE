"""
This module contains all the classes related to logging the
application messages.
"""
import logging
from pyqode.core.qt.QtCore import QObject, Signal
from pyqode.core.qt.QtGui import QTextCursor


class TextEditWriter(QObject):
    """
    Writes to the text edit in a thread safe way

    (using an internal signal to ensure QTextEdit.append will always be
    called from the main gui thread)
    """
    _append_msg_requested = Signal(str)

    def __init__(self, text_edit):
        super().__init__()
        self.text_edit = text_edit
        self._append_msg_requested.connect(self._append)

    def _append(self, message):
        self.text_edit.append(message)
        self.text_edit.moveCursor(QTextCursor.End)

    def request_append(self, message):
        self._append_msg_requested.emit(message)


class TextEditHandler(logging.Handler):
    def __init__(self, text_edit):
        super().__init__()
        self.writer = TextEditWriter(text_edit)

    def emit(self, record):
        assert isinstance(record, logging.LogRecord)
        self.writer.request_append(self.formatter.format(record))


def setup(version, debug):
    """
    Configures the logger
    """
    level = logging.DEBUG if debug else logging.INFO
    logger = logging.getLogger()
    formatter = logging.Formatter(
        '%(levelname)s::%(name)s::%(message)s',
        "%Y-%m-%d %H:%M:%S")
    handlers = [logging.StreamHandler() ]
    logger.setLevel(level)
    for handler in handlers:
        handler.setFormatter(formatter)
    for handler in handlers:
        logger.addHandler(handler)

    logging.getLogger('open_cobol_ide').info('version: %s' % version)
