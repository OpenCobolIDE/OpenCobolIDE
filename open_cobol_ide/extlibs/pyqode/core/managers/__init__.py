"""
The managers package contains a series of managers classes for CodeEdit.

A manager is class that takes care of a specific aspect of CodeEdit:

    - FileManager: open, save, encoding detection
    - BackendManager: manage the backend process (start the process and
      handle communication through sockets).
    - ModesManager: manage the list of modes of an editor
    - PanelsManager: manage the list of panels and draw them into the editor
      margins.
    - DecorationManager: manage text decorations

"""
from .backend import BackendManager
from .decorations import TextDecorationsManager
from .file import FileManager
from .modes import ModesManager
from .panels import PanelsManager


__all__ = [
    'BackendManager',
    'FileManager',
    'ModesManager',
    'PanelsManager',
    'TextDecorationsManager',
]
