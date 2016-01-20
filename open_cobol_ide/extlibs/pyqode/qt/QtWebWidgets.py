"""
This shiw provides a QWebView either from QtWebKit or from QtWebEngine.
"""
import os
from pyqode.qt import QT_API
from pyqode.qt import PYQT5_API
from pyqode.qt import PYQT4_API
from pyqode.qt import PYSIDE_API

if os.environ[QT_API] in PYQT5_API:
    try:
        from PyQt5.QtWebKitWidgets import QWebView
        from PyQt5.QtWebKitWidgets import QWebPage
    except ImportError:
        try:
            from PyQt5.QtWebEngineWidgets import QWebEngineView as QWebView
            from PyQt5.QtWebEngineWidgets import QWebEnginePage as QWebPage
        except ImportError:
            # neither QtWebKit nor QtWebEngine installed
            QWebPage = None
            QWebView = None
elif os.environ[QT_API] in PYQT4_API:
    from PyQt4.QtWebKit import QWebView, QWebPage
elif os.environ[QT_API] in PYSIDE_API:
    from PySide.QtWebKit import QWebView, QWebPage
else:
    QWebPage = None
    QWebView = None
