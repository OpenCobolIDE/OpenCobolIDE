"""
This is the application entry. This is where we create and run the
Application.
"""
from pyqode.qt import QtGui
from open_cobol_ide.app import Application
from open_cobol_ide.settings import Settings


def main():
    """
    Application entry point.
    """
    app = Application()
    QtGui.QIcon.setThemeName(Settings().icon_theme)
    ret_code = app.run()
    app.close()
    return ret_code
