"""
This module contains the code of the main window, which basically connects
slots to signals and delegate the logic to the various managers of the
application.

"""
import logging
from pyqode.qt import QtWidgets
from .forms.ide_ui import Ui_MainWindow
from ..settings import Settings


def _logger():
    return logging.getLogger(__name__)


class MainWindow(QtWidgets.QMainWindow):
    def restore_state(self):
        s = Settings()
        if s.geometry:
            self.restoreGeometry(s.geometry)
        if s.state:
            self.restoreState(s.state)
        self.wasMaximised = s.maximised
        self.prevSize = s.size
        self.ui.actionFullscreen.setChecked(s.fullscreen)

    def __init__(self):
        super().__init__()
        self.ui = Ui_MainWindow()
        self.ui.setupUi(self)
        self.restore_state()

    def __del__(self):
        _logger().debug("del main window")

    def save_state(self):
        s = Settings()
        s.geometry = self.saveGeometry()
        s.state = self.saveState()
        s.maximised = self.isMaximized()
        s.size = self.size()
        if self.ui.stackedWidget.currentIndex() == 1:
            s.outline_visible = self.ui.dockWidgetNavPanel.isVisible()
            s.fullscreen = self.isFullScreen()

    def closeEvent(self, event):
        self.ui.tabWidgetEditors.closeEvent(event)
        if event.isAccepted():
            self.save_state()
