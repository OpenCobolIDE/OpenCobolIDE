"""
This package contains all the pyqode specific dialogs.

"""
from .goto import DlgGotoLine
from .encodings import DlgPreferredEncodingsEditor
from .encodings import DlgEncodingsChoice
from .unsaved_files import DlgUnsavedFiles

__all__ = [
    'DlgPreferredEncodingsEditor',
    'DlgGotoLine',
    'DlgUnsavedFiles',
    'DlgEncodingsChoice'
]
