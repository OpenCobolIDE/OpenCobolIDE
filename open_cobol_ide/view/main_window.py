"""
This module contains the main window implementation.

Most of the code for controlling the gui can be found in the controllers
package, the main window here just sets up its ui and implement the save/restor
state logic as well as the close event handling (give user a chance to save its
work or not).

"""
import logging
from pyqode.qt import QtWidgets
from open_cobol_ide.view.forms.ide_ui import Ui_MainWindow
from open_cobol_ide.settings import Settings


def _logger():
    return logging.getLogger(__name__)


class MainWindow(QtWidgets.QMainWindow):
    def __init__(self):
        super().__init__()
        self.ui = Ui_MainWindow()
        self.ui.setupUi(self)
        self.restore_state()

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
            self.save_state()
            _logger().debug('CLOSED')
