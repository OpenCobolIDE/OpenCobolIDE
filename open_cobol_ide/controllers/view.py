"""
Contains the view manager.

"""
import logging
from enum import IntEnum
from .base import Controller
from ..settings import Settings


class Page(IntEnum):
    HOME = 0
    EDIT = 1


def _logger():
    return logging.getLogger(__name__)


class ViewController(Controller):
    """
    The view controller manage the view of the main window depending on the
    current page and the current perpsective (making up what we call the view
    context).

    It's main job is to show the proper widgets depending on the view context
    and control the view menu.

    """

    def __init__(self, app):
        super().__init__(app)
        self._page = -1
        self._perspective = 'default'
        self._nav_was_visible = True
        self._offset_was_visible = True

    def show_perspective(self, perspective):
        self._perspective = perspective
        self._apply_perspective()

    def show_home(self):
        self.show_page(Page.HOME)

    def show_editors(self):
        self.show_page(Page.EDIT)

    def show_page(self, page):
        """
        Changes the active page.

        :param page: page
        :type page: open_cobol_ide.core.constants.Page
        """
        _logger().debug('showing page %r' % page)
        self.ui.stackedWidget.setCurrentIndex(int(page))
        if page == Page.HOME:
            if self._page != -1:
                s = Settings()
                s.outline_visible = self.ui.dockWidgetNavPanel.isVisible()
            self.ui.menuBar.hide()
            self.ui.statusbar.hide()
            self.ui.toolBarCode.hide()
            self.ui.toolBarFile.hide()
            self.ui.dockWidgetLogs.hide()
            self.ui.dockWidgetNavPanel.hide()
            self.ui.dockWidgetOffsets.hide()
        else:
            self._apply_perspective()
            self.ui.dockWidgetNavPanel.setVisible(Settings().outline_visible)

    def _apply_perspective(self):
        if self._perspective == 'default':
            self.ui.menuBar.show()
            self.ui.statusbar.show()
            self.ui.toolBarFile.show()
            self.ui.toolBarCode.show()
        else:
            self.ui.menuBar.hide()
            self.ui.statusbar.hide()
            self.ui.toolBarCode.hide()
            self.ui.toolBarFile.hide()
