import os
import sys
from pyqode.core import widgets
from pyqode.core.backend import server


class TextEdit(widgets.TextCodeEdit):
    """
    This editor is used for non cobol documents.
    """
    def __init__(self, parent=None):
        cwd = os.path.dirname(sys.executable)
        base_backend = 'core-backend'
        if sys.platform == 'win32':
            base_backend += '.exe'
        super().__init__(
            parent, server_script=os.path.join(cwd, base_backend)
            if hasattr(sys, 'frozen') else server.__file__)
