"""
Contains the view manager.

"""
import logging
from enum import IntEnum
from pyqode.qt import QtGui, QtWidgets
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
        self.setupIcons()

    def setupIcons(self):
        iopen = QtGui.QIcon.fromTheme(
            'document-open', QtGui.QIcon(':/ide-icons/rc/document-open.png'))
        isave = QtGui.QIcon.fromTheme(
            'document-save', QtGui.QIcon(':/ide-icons/rc/document-save.png'))
        isave_as = QtGui.QIcon.fromTheme(
            'document-save-as', QtGui.QIcon(':/ide-icons/rc/document-save-as.png'))
        inew = QtGui.QIcon.fromTheme(
            'document-new',
            QtGui.QIcon(':/ide-icons/rc/document-new.png'))
        iissue = QtGui.QIcon.fromTheme('important', QtGui.QIcon(
            ':/ide-icons/rc/emblem-important.png'))
        icompile = QtGui.QIcon.fromTheme(
            'application-x-executable', QtGui.QIcon(
                ':/ide-icons/rc/application-x-executable.png'))
        irun = QtGui.QIcon.fromTheme(
            'media-playback-start', QtGui.QIcon(
                ':/ide-icons/rc/media-playback-start.png'))
        icancel = QtGui.QIcon.fromTheme('process-stop')
        ifullscreen = QtGui.QIcon.fromTheme(
            'view-fullscreen', QtGui.QIcon(
                ':/ide-icons/rc/view-fullscreen.png'))
        iquit = QtGui.QIcon.fromTheme(
            'window-close', QtGui.QIcon(':/ide-icons/rc/system-log-out.png'))
        iclear = QtGui.QIcon.fromTheme(
            'edit-clear', QtGui.QIcon(':/ide-icons/rc/edit-clear.png'))
        ihelp = QtGui.QIcon.fromTheme(
            'help-contents', QtGui.QIcon(':/ide-icons/rc/help.png'))
        ipreferences = QtGui.QIcon.fromTheme(
            'preferences-system',
            QtGui.QIcon(':/ide-icons/rc/Preferences-system.png'))
        iabout = QtGui.QIcon.fromTheme(
            'help-about', QtGui.QIcon(':/ide-icons/rc/dialog-information.png'))

        if Settings().dark_style:
            iopen = QtGui.QIcon(':/ide-icons/rc/document-open.png')
            isave = QtGui.QIcon(':/ide-icons/rc/document-save.png')
            isave_as = QtGui.QIcon(':/ide-icons/rc/document-save-as.png')
            inew = QtGui.QIcon(':/ide-icons/rc/document-new.png')
            icompile = QtGui.QIcon(
                ':/ide-icons/rc/application-x-executable.png')
            irun = QtGui.QIcon(
                ':/ide-icons/rc/media-playback-start.png')
            ifullscreen = QtGui.QIcon(
                ':/ide-icons/rc/view-fullscreen.png')
            iquit = QtGui.QIcon(':/ide-icons/rc/system-log-out.png')
            iclear = QtGui.QIcon(':/ide-icons/rc/edit-clear.png')
            ihelp = QtGui.QIcon(':/ide-icons/rc/help.png')
            ipreferences = QtGui.QIcon(
                ':/ide-icons/rc/Preferences-system.png')
            iabout = QtGui.QIcon(':/ide-icons/rc/dialog-information.png')

        self.ui.actionPreferences.setIcon(ipreferences)
        self.ui.actionHelp.setIcon(ihelp)
        self.ui.actionClear.setIcon(iclear)
        self.ui.actionQuit.setIcon(iquit)
        self.ui.actionFullscreen.setIcon(ifullscreen)
        self.ui.actionOpen.setIcon(iopen)
        self.ui.btOpenFile.setIcon(iopen)
        self.ui.actionNew.setIcon(inew)
        self.ui.btNewFile.setIcon(inew)
        self.ui.actionSave.setIcon(isave)
        self.ui.actionSaveAs.setIcon(isave_as)
        self.ui.actionRun.setIcon(irun)
        self.ui.actionCancel.setIcon(icancel)
        self.ui.actionCompile.setIcon(icompile)
        self.ui.actionAbout.setIcon(iabout)
        self.ui.tabWidgetLogs.setTabIcon(0, iissue)
        self.ui.tabWidgetLogs.setTabIcon(1, irun)

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
