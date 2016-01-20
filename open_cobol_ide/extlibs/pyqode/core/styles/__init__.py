"""
This packages contains some pyqode specific pygments color schemes:
    - qt (inpired by the QtCreator color scheme)
    - darcula (inspired from the darcula them of PyCharm)
"""
from .qt import QtStyle
from .darcula import DarculaStyle

__all__ = [
    'QtStyle',
    'DarculaStyle'
]
