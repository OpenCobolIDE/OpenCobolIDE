import logging
import sys

_logger = logging.getLogger(__name__)

try:
    from PyQt5 import QtWidgets, QtGui, QtCore
except (ImportError, RuntimeError):
    _logger.warning('failed to import PyQt5, going to try PyQt4')
    try:
        from PyQt4 import QtGui, QtGui as QtWidgets, QtCore
    except (ImportError, RuntimeError):
        _logger.warning('failed to import PyQt4, going to try PySide')
        try:
            from PySide import QtGui, QtCore, QtGui as QtWidgets
        except (ImportError, RuntimeError):
            _logger.warning('failed to import PySide')
            _logger.critical('No Qt bindings found, aborting...')
            try:
                from unittest.mock import MagicMock
                QtCore = MagicMock()
                QtGui = MagicMock()
                QtWidgets = MagicMock()
            except ImportError:
                sys.exit(1)

__all__ = ['QtCore', 'QtGui', 'QtWidgets']
