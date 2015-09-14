"""
Controls the home page.

"""
from open_cobol_ide.view.widgets import FileIconProvider
from pyqode.cobol.api import icons
from pyqode.cobol.widgets import CobolCodeEdit
from pyqode.qt import QtCore, QtGui, QtWidgets
from open_cobol_ide.settings import Settings
from .base import Controller


class HomePageWhite:
    """
    Contains the stylesheets for a white (default) theme.
    """
    label_recent = """
    border: none;
    border-top-left-radius: 3px;
    border-top-right-radius: 3px;
    border-bottom-left-radius: 0px;
    border-bottom-right-radius: 0px;
    padding: 5px;
    """


class HomePageDark:
    """
    Contains the stylesheets for a dark theme.
    """
    label_recent = """border: none;
    border-top-left-radius: 3px;
    border-top-right-radius: 3px;
    border-bottom-left-radius: 0px;
    border-bottom-right-radius: 0px;
    padding: 5px;
    """


class HomeController(Controller):
    """
    The home controller manage the home page and the list of recent files.
    """
    def __init__(self, app):
        super().__init__(app)
        self.update_style()
        self._update_recents()
        self.app.file.recent_files_manager.updated.connect(
            self._update_recents)
        self.ui.btOpenFile.clicked.connect(self.app.file.request_open)
        self.ui.btNewFile.clicked.connect(self.app.file.request_new)
        self.ui.listWidgetRecents.itemClicked.connect(
            self._on_recent_item_clicked)
        self.ui.listWidgetRecents.clear_requested.connect(self._clear_recents)
        self.ui.listWidgetRecents.remove_current_requested.connect(
            self._remove_current_recent_file)
        self.ui.pageHome.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        self.ui.pageHome.customContextMenuRequested.connect(
            self._show_main_menu)

    def _show_main_menu(self, pos):
        self.app.view.make_main_menu().exec_(self.ui.pageHome.mapToGlobal(pos))

    def update_style(self):
        """
        Applies the theme as defined in the Settings.
        """
        if Settings().dark_style:
            style = HomePageDark
        else:
            style = HomePageWhite
        # self.ui.frameRecents.setStyleSheet(style.frame_recent)
        self.ui.labelRecents.setStyleSheet(style.label_recent)
        # self.ui.listWidgetRecents.setStyleSheet(style.list_recent)

    def _clear_recents(self):
        """
        Clears the recent files menu and list.
        """
        self.app.file.recent_files_manager.clear()

    def _remove_current_recent_file(self):
        """
        Removes the current item in the recent files list (both from the list
        and the menu).
        """
        filename = self.ui.listWidgetRecents.currentItem().data(
            QtCore.Qt.UserRole)
        files = self.app.file.recent_files_manager.get_recent_files()
        files.remove(filename)
        self.app.file.recent_files_manager.remove(filename)
        self.app.file.menu_recents.update_actions()

    def _update_recents(self):
        """
        Updates the recent files list.
        """
        self.ui.listWidgetRecents.clear()
        for file in self.app.file.recent_files_manager.get_recent_files():
            item = QtWidgets.QListWidgetItem()
            fi = QtCore.QFileInfo(file)
            icon = FileIconProvider().icon(fi)
            item.setText(fi.fileName())
            item.setToolTip(file)
            item.setIcon(icon)
            item.setData(QtCore.Qt.UserRole, file)
            self.ui.listWidgetRecents.addItem(item)

    def _on_recent_item_clicked(self, item):
        """
        Opens the file that was clicked.
        :param item: clicked item.
        """
        filename = item.data(QtCore.Qt.UserRole)
        self.app.file.open_file(filename)
