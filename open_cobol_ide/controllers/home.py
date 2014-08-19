"""
Controls the home page.

"""
from pyqode.cobol.widgets import CobolCodeEdit
from pyqode.qt import QtCore, QtGui, QtWidgets
from .base import Controller
from ..constants import HomePageWhite, HomePageDark
from ..settings import Settings


class HomeController(Controller):
    """
    The home controller manage the home page and the list of recent files.
    """
    def __init__(self, app):
        super().__init__(app)
        self._apply_style()
        self._update_recents()
        self.app.file.recent_files_manager.updated.connect(
            self._update_recents)
        self.ui.btOpenFile.clicked.connect(self.app.file.request_open)
        self.ui.listWidgetRecents.itemClicked.connect(
            self._on_recent_item_clicked)
        self.ui.listWidgetRecents.clear_requested.connect(self._clear_recents)
        self.ui.listWidgetRecents.remove_current_requested.connect(
            self._remove_current_recent_file)

    def _apply_style(self):
        if Settings().dark_style:
            style = HomePageDark
        else:
            style = HomePageWhite
        self.ui.frameRecents.setStyleSheet(style.frame_recent)
        self.ui.labelRecents.setStyleSheet(style.label_recent)
        self.ui.listWidgetRecents.setStyleSheet(style.list_recent)

    def _clear_recents(self):
        self.app.file.recent_files_manager.clear()

    def _remove_current_recent_file(self):
        filename = self.ui.listWidgetRecents.currentItem().data(
            QtCore.Qt.UserRole)
        files = self.app.file.recent_files_manager.get_recent_files()
        files.remove(filename)
        self.app.file.recent_files_manager.remove(filename)
        self.app.file.menu_recents.update_actions()

    def _update_recents(self):
        self.ui.listWidgetRecents.clear()
        for file in self.app.file.recent_files_manager.get_recent_files():
            item = QtWidgets.QListWidgetItem()
            if ('.' + QtCore.QFileInfo(file).suffix().upper() in
                    CobolCodeEdit.extensions):
                icon = QtGui.QIcon(":/ide-icons/rc/silex-32x32.png")
            else:
                icon = QtGui.QIcon(":/ide-icons/rc/text-x-generic.png")
            item.setText(QtCore.QFileInfo(file).fileName())
            item.setToolTip(file)
            item.setIcon(icon)
            item.setData(QtCore.Qt.UserRole, file)
            self.ui.listWidgetRecents.addItem(item)

    def _on_recent_item_clicked(self, item):
        filename = item.data(QtCore.Qt.UserRole)
        self.app.file.open_file(filename)

