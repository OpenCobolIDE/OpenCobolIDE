"""
Contains the COBOL code editor widget.

"""
import logging
import os
import sys

from pyqode.core.api import ColorScheme
from pyqode.cobol.widgets import CobolCodeEdit as CodeEditBase

from open_cobol_ide import backend
from open_cobol_ide.compilers import get_file_type
from open_cobol_ide.linter import CobolLinterMode
from open_cobol_ide.settings import Settings


def _logger():
    return logging.getLogger(__name__)


class CobolCodeEdit(CodeEditBase):
    """
    Cobol code editor. We specialise the pyqode.cobol code edit to add support
    for our settings system and for some custom properties (such as the
    file type).
    """

    def __init__(self, parent=None):
        super().__init__(parent, free_format=Settings().free_format)
        self.syntax_highlighter.color_scheme = ColorScheme(
            Settings().color_scheme)
        self.linter_mode = self.modes.append(CobolLinterMode())
        self.app = None

    def _start_server(self):
        if hasattr(sys, "frozen"):
            cwd = os.path.dirname(sys.executable)
            base = 'cobol-backend'
            srv = base + '.exe' if sys.platform == 'win32' else base
            srv = os.path.join(cwd, srv)
            self.backend.start(srv)
        else:
            self.backend.start(backend.__file__)

    def close(self, clear=True):
        self.linter_mode = None
        super().close(clear=clear)
        self.app = None

    def setPlainText(self, txt, mime_type, encoding):
        super().setPlainText(txt, mime_type, encoding)

    @property
    def file_type(self):
        return get_file_type(self.file.path)

    @file_type.setter
    def file_type(self, ftype):
        Settings().set_file_type(self.file.path, ftype)

    def clone(self):
        clone = self.__class__(parent=self.parent())
        clone.app = self.app
        return clone
