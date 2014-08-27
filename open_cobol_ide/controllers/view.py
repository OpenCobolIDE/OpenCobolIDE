"""
Contains the view manager.

"""
import logging
from enum import IntEnum
from pyqode.qt import QtCore, QtGui, QtWidgets
from .base import Controller
from .. import system
from ..view.widgets import TabCornerWidget
from ..settings import Settings


class Page(IntEnum):
    """
    Enumerates the different pages of the application (corresponding to the
    main stacked widget indices).
    """
    #: Home page
    HOME = 0
    #: Edit page
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
        self.setup_icons()
        self.ui.tabWidgetEditors.setContextMenuPolicy(
            QtCore.Qt.CustomContextMenu)
        self.ui.tabWidgetEditors.customContextMenuRequested.connect(
            self.show_main_menu_as_context_menu)
        self.ui.tabWidgetEditors.tab_bar.double_clicked.connect(
            self.toggle_perspective)
        self.bt_mnu = QtWidgets.QToolButton()
        self.main_window.addActions(self.make_main_menu().actions())
        window_mnu = self.main_window.createPopupMenu()
        self.ui.menuWindows.addActions(window_mnu.actions())
        self._widget = None
        if system.darwin:
            self.ui.toolBarCode.setIconSize(QtCore.QSize(20, 20))
            self.ui.toolBarFile.setIconSize(QtCore.QSize(20, 20))

    def toggle_perspective(self):
        self.show_perspective(
            'minimal' if Settings().perspective == 'default' else 'default')

    def make_main_menu(self):
        mnu = QtWidgets.QMenu(self.main_window)
        mnu.addMenu(self.ui.menuFile)
        mnu.addMenu(self.ui.menuEdit)
        mnu.addMenu(self.ui.menuView)
        mnu.addMenu(self.ui.menuCobol)
        mnu.addMenu(self.ui.menu)
        return mnu

    def show_main_menu_as_context_menu(self, pos):
        if Settings().perspective == 'minimal':
            self.make_main_menu().exec_(
                self.ui.tabWidgetEditors.mapToGlobal(pos))

    def setup_icons(self):
        """
        Setup actions/buttons icons, loads them from the system icon theme on
        linux.
        """
        iopen = QtGui.QIcon.fromTheme(
            'document-open', QtGui.QIcon(':/ide-icons/rc/document-open.png'))
        isave = QtGui.QIcon.fromTheme(
            'document-save', QtGui.QIcon(':/ide-icons/rc/document-save.png'))
        isave_as = QtGui.QIcon.fromTheme(
            'document-save-as', QtGui.QIcon(
                ':/ide-icons/rc/document-save-as.png'))
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

        self.ui.actionAbout.setMenuRole(QtWidgets.QAction.AboutRole)
        self.ui.actionPreferences.setMenuRole(
            QtWidgets.QAction.PreferencesRole)
        self.ui.actionQuit.setMenuRole(QtWidgets.QAction.QuitRole)

    def show_perspective(self, perspective):
        """
        TODO
        """
        self._perspective = perspective
        self._apply_perspective()
        Settings().perspective = perspective

    def show_home_page(self):
        """
        Shows the home page.
        """
        self.show_page(Page.HOME)

    def show_edit_page(self):
        """
        Show the edit page.
        """
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
            if self.ui.consoleOutput.is_running:
                self.ui.consoleOutput.stop_process()
        else:
            self._apply_perspective()
            self.ui.dockWidgetNavPanel.setVisible(Settings().outline_visible)

    def _apply_perspective(self):
        """
        TODO
        """
        if self._perspective == 'default':
            self.ui.menuBar.show()
            self.ui.statusbar.show()
            self.ui.toolBarFile.show()
            self.ui.toolBarCode.show()
            self.ui.tabWidgetEditors.setCornerWidget(None)
            self._widget = None
        else:
            self.ui.menuBar.hide()
            self.ui.statusbar.hide()
            self.ui.toolBarCode.hide()
            self.ui.toolBarFile.hide()
            if not system.darwin:
                if self._widget is None:
                    self._widget = TabCornerWidget(
                        self.ui.tabWidgetEditors,
                        self.app.cobol.create_bt_compile(),
                        self.app.cobol.create_bt_run())
                    self._widget.show()
                    self.ui.tabWidgetEditors.setCornerWidget(self._widget)
            else:
                self.ui.tabWidgetEditors.setCornerWidget(None)
                self._widget = None
