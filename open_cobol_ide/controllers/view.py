"""
Contains the view manager.

"""
import logging
from enum import IntEnum
from pyqode.qt import QtCore, QtGui, QtWidgets
from .base import Controller
from open_cobol_ide import system
from open_cobol_ide.settings import Settings


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
        self._flg_fs_visible = False
        self._flg_nav_visible = False
        self.setup_icons()
        self.ui.tabWidgetEditors.setContextMenuPolicy(
            QtCore.Qt.CustomContextMenu)
        self.ui.tabWidgetEditors.customContextMenuRequested.connect(
            self.show_main_menu_as_context_menu)
        self.ui.tabWidgetEditors.tab_bar_double_clicked.connect(
            self.toggle_perspective)
        self.ui.actionFullscreen.triggered.connect(self.toggle_fullscreen)
        self.bt_mnu = QtWidgets.QToolButton()
        self.main_window.addActions(self.make_main_menu().actions())
        window_mnu = self.main_window.createPopupMenu()
        self.ui.menuWindows.addActions(window_mnu.actions())
        if system.darwin:
            self.ui.toolBarCOBOL.setIconSize(QtCore.QSize(20, 20))
            self.ui.toolBarFile.setIconSize(QtCore.QSize(20, 20))
            self.ui.toolBarFile.setStyleSheet(
                '''QToolButton
                {
                     background-color: rgba(255, 255,255,0);
                }
                ''')
            self.ui.toolBarCOBOL.setStyleSheet(
                '''QToolButton
                {
                     background-color: rgba(255, 255,255,0);
                }
                ''')
        self.ui.actionFullscreen.setChecked(Settings().fullscreen)

    def restore_state(self):
        self.main_window.restore_state()
        self._flg_fs_visible = self.ui.dockWidgetFileSystem.isVisible()
        self._flg_nav_visible = self.ui.dockWidgetNavPanel.isVisible()

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
        QIcon = QtGui.QIcon
        iopen = QIcon.fromTheme(
            'document-open', QtGui.QIcon(':/ide-icons/rc/document-open.png'))
        isave = QIcon.fromTheme(
            'document-save', QtGui.QIcon(':/ide-icons/rc/document-save.png'))
        isave_as = QIcon.fromTheme(
            'document-save-as', QtGui.QIcon(
                ':/ide-icons/rc/document-save-as.png'))
        inew = QIcon.fromTheme(
            'document-new',
            QtGui.QIcon(':/ide-icons/rc/document-new.png'))
        issue = QIcon.fromTheme('important', QtGui.QIcon(
            ':/ide-icons/rc/emblem-important.png'))

        if QIcon.hasThemeIcon('run-build'):
            icompile = QIcon.fromTheme('run-build')
        else:
            icompile = QIcon.fromTheme(
                'application-x-executable', QtGui.QIcon(
                    ':/ide-icons/rc/application-x-executable.png'))
        irun = QIcon.fromTheme(
            'media-playback-start', QtGui.QIcon(
                ':/ide-icons/rc/media-playback-start.png'))
        icancel = QIcon.fromTheme('process-stop')
        ifullscreen = QIcon.fromTheme(
            'view-fullscreen', QtGui.QIcon(
                ':/ide-icons/rc/view-fullscreen.png'))
        iquit = QIcon.fromTheme(
            'window-close', QtGui.QIcon(':/ide-icons/rc/system-log-out.png'))
        iclear = QIcon.fromTheme(
            'edit-clear', QtGui.QIcon(':/ide-icons/rc/edit-clear.png'))
        ihelp = QIcon.fromTheme(
            'help-contents', QtGui.QIcon(':/ide-icons/rc/help.png'))
        ipreferences = QIcon.fromTheme(
            'preferences-system',
            QtGui.QIcon(':/ide-icons/rc/Preferences-system.png'))
        iabout = QIcon.fromTheme(
            'help-about', QtGui.QIcon(':/ide-icons/rc/dialog-information.png'))
        icon_report_bug = QIcon.fromTheme('tools-report-bug')
        icon_lock = QIcon.fromTheme('system-lock-screen', QtGui.QIcon(
            ':/ide-icons/rc/lock.png'))
        if QIcon.hasThemeIcon('run-build-clean'):
            icon_clean = QIcon.fromTheme('run-build-clean')
        else:
            icon_clean = QIcon.fromTheme('edit-clear', QtGui.QIcon(
                ':/ide-icons/rc/edit-clear.png'))
        icon_rebuild = QIcon.fromTheme('view-refresh', QtGui.QIcon(
            ':/ide-icons/rc/view-refresh.png'))

        self.ui.btFSLock.setIcon(icon_lock)
        self.ui.btNavLock.setIcon(icon_lock)
        self.ui.actionPreferences.setIcon(ipreferences)
        self.ui.btPreferences.setIcon(ipreferences)
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
        self.ui.btAbout.setIcon(iabout)
        self.ui.tabWidgetLogs.setTabIcon(0, icompile)
        self.ui.tabWidgetLogs.setTabIcon(1, issue)
        self.ui.tabWidgetLogs.setTabIcon(2, irun)
        self.ui.actionAbout.setMenuRole(QtWidgets.QAction.AboutRole)
        self.ui.actionPreferences.setMenuRole(
            QtWidgets.QAction.PreferencesRole)
        self.ui.actionQuit.setMenuRole(QtWidgets.QAction.QuitRole)
        self.ui.actionReport_a_bug.setIcon(icon_report_bug)
        self.ui.actionClean.setIcon(icon_clean)
        self.ui.actionRebuild.setIcon(icon_rebuild)

        if system.darwin:
            self.ui.menu.setTitle('Help')

        self.ui.actionEnableLinter.setIcon(QIcon.fromTheme(
            'dialog-error',
            QtGui.QIcon(':/ide-icons/rc/emblem-important.png')))

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
        self.ui.tableWidgetOffsets.set_editor(None)

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
        current_page = self.ui.stackedWidget.currentIndex()
        self.ui.stackedWidget.setCurrentIndex(int(page))
        if page == Page.HOME:
            self.main_window.save_state()
            if not system.ubuntu:
                self.ui.menuBar.hide()
            self.ui.statusbar.hide()
            self.ui.toolBarCOBOL.hide()
            self.ui.toolBarFile.hide()
            self._flg_fs_visible = self.ui.dockWidgetFileSystem.isVisible()
            self._flg_nav_visible = self.ui.dockWidgetNavPanel.isVisible()
            self.ui.dockWidgetLogs.hide()
            self.ui.dockWidgetNavPanel.hide()
            self.ui.dockWidgetOffsets.hide()
            self.ui.dockWidgetFileSystem.hide()
            if self.ui.consoleOutput.is_running:
                self.ui.consoleOutput.stop_process()
            self.ui.widgetHome.show()
            self.ui.btOpenFile.setFocus()
        else:
            if page != current_page:
                self._apply_perspective()
                self.ui.widgetHome.hide()
                self.ui.dockWidgetFileSystem.setVisible(self._flg_fs_visible)
                self.ui.dockWidgetNavPanel.setVisible(self._flg_nav_visible)
                self.ui.dockWidgetOffsets.hide()
                self.ui.dockWidgetLogs.hide()

    def _apply_perspective(self):
        """
        TODO
        """
        if self._perspective == 'default':
            if not system.ubuntu:
                self.ui.menuBar.show()
            self.ui.statusbar.show()
            self.ui.toolBarFile.show()
            self.ui.toolBarCOBOL.show()
        else:
            if not system.ubuntu:
                self.ui.menuBar.hide()
            self.ui.statusbar.hide()
            self.ui.toolBarCOBOL.hide()
            self.ui.toolBarFile.hide()

    def toggle_fullscreen(self):
        if self.ui.actionFullscreen.isChecked():
            self.main_window.showFullScreen()
        else:
            self.main_window.showNormal()
        Settings().fullscreen = self.ui.actionFullscreen.isChecked()
