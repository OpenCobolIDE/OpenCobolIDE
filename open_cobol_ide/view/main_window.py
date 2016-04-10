"""
This module contains the main window implementation.

Most of the code for controlling the gui can be found in the controllers
package, the main window here just sets up its ui and implement the save/restor
state logic as well as the close event handling (give user a chance to save its
work or not).

"""
import os
import logging
from pyqode.qt import QtCore, QtWidgets
from open_cobol_ide import system
from open_cobol_ide.view.forms.ide_ui import Ui_MainWindow
from open_cobol_ide.settings import Settings


def _logger():
    return logging.getLogger(__name__)


class MainWindow(QtWidgets.QMainWindow):
    def __init__(self):
        super().__init__()
        self.ui = Ui_MainWindow()
        self.ui.setupUi(self)
        self.ui.dockWidgetFileSystem.setWindowTitle('File system')
        self.ui.dockWidgetLogs.setWindowTitle('Logs')
        self.ui.dockWidgetNavPanel.setWindowTitle('Navigation')
        self.ui.dockWidgetOffsets.setWindowTitle('Offset calculator')
        self.setAcceptDrops(True)
        self.restore_state()
        Qt = QtCore.Qt
        self.setCorner(Qt.TopLeftCorner, Qt.LeftDockWidgetArea)
        self.setCorner(Qt.TopRightCorner, Qt.RightDockWidgetArea)
        self.setCorner(Qt.BottomLeftCorner, Qt.LeftDockWidgetArea)
        self.setCorner(Qt.BottomRightCorner, Qt.RightDockWidgetArea)

    def restore_state(self):
        """
        Restores the main window state from the saved state (in the settings).
        """
        s = Settings()
        if s.geometry:
            self.restoreGeometry(s.geometry)
        if s.state:
            self.restoreState(s.state)
        self.wasMaximised = s.maximised
        self.prevSize = s.size
        self.ui.actionFullscreen.setChecked(s.fullscreen)

    def save_state(self):
        """
        Saves the window state and geometry to the settings.
        """
        s = Settings()
        s.geometry = self.saveGeometry()
        s.state = self.saveState()
        s.maximised = self.isMaximized()
        s.size = self.size()
        if self.ui.stackedWidget.currentIndex() == 1:
            s.outline_visible = self.ui.dockWidgetNavPanel.isVisible()
            s.fullscreen = self.isFullScreen()

    def closeEvent(self, event):
        """
        Handle close event, gives the user a chance to save its work or not.
        """
        _logger().debug('CLOSING')
        self.ui.tabWidgetEditors.closeEvent(event)
        if event.isAccepted():
            _logger().debug('CLOSED')

    def dragEnterEvent(self, event):
        mime = event.mimeData()
        if mime is None or not mime.hasUrls():
            return
        event.accept()

    def dropEvent(self, event):
        mime = event.mimeData()
        if mime is None or not mime.hasUrls():
            return
        for url in mime.urls():
            path = url.path()
            if system.windows and path.startswith('/'):
                path = path[1:]
            if os.path.isfile(path):
                # open a new editor
                self.app.file.open_file(path)
