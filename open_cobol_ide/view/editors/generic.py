import os
import sys
from pyqode.core import widgets
from pyqode.core.backend import server


class GenericCodeEdit(widgets.GenericCodeEdit):
    """
    This editor is used for non cobol documents.
    """
    def __init__(self, parent=None):
        super().__init__(
            parent, server_script=os.path.join(os.getcwd(), 'server.exe')
            if hasattr(sys, 'frozen') else server.__file__)
