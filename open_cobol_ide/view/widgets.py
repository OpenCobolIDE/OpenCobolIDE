"""
Widgets in this module are used as promoted widgets in Qt Designer
"""
from pyqode.cobol.api import icons
from pyqode.qt import QtCore, QtGui, QtWidgets
from pyqode.core.widgets import SplittableCodeEditTabWidget
from pyqode.core.widgets import FileSystemContextMenu, FileIconProvider as PyQodeIconProvider
import sys
from open_cobol_ide.settings import Settings


class RecentFilesListWidget(QtWidgets.QListWidget):
    """
    A custom list widget which keeps track of the mouse button that is
    pressed. It also changes the mouse cursor to a pointing hand cursor
    when the mouse cursor is over a list item.

    This class is used in QHomeWidget to ignore itemClicked when the right
    button is pressed.
    """
    remove_current_requested = QtCore.Signal()
    clear_requested = QtCore.Signal()

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.setMouseTracking(True)
        self.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(self.show_context_menu)

    def show_context_menu(self, pt):
        """
        Shows the recent files list context menu which allow to remove an item
        from the list or to clear the entire list.
        """
        actionRemove = QtWidgets.QAction('Remove from recent files list', self)
        actionClear = QtWidgets.QAction('Clear recent files list', self)
        actionRemove.triggered.connect(self.remove_current_requested)
        actionClear.triggered.connect(self.clear_requested)
        actionClear.setIcon(QtGui.QIcon.fromTheme(
            'edit-clear', QtGui.QIcon(':/ide-icons/rc/edit-clear.png')))
        menu = QtWidgets.QMenu()
        menu.addAction(actionRemove)
        menu.addAction(actionClear)
        menu.exec_(self.mapToGlobal(pt))

    def mousePressEvent(self, e):
        """
        Keeps track of the pressed button
        """
        self.mouseButton = e.button()
        super().mousePressEvent(e)

    def mouseMoveEvent(self, e):
        """
        Display a pointing hand cursor when over an item. The cursor is
        reset to an ArrowCursor if there are no item under the cursor.
        """
        item = self.itemAt(e.pos())
        super().mouseMoveEvent(e)
        self.setCursor(QtCore.Qt.PointingHandCursor if item else
                       QtCore.Qt.ArrowCursor)
        if item:
            self.setCurrentRow(self.row(item))


class TabWidget(SplittableCodeEditTabWidget):
    pass


class FSContextMenu(FileSystemContextMenu):
    def __init__(self, app):
        super().__init__()
        self.app = app

    def get_new_user_actions(self):
        self.action_new_file = QtWidgets.QAction('COBOL file', self)
        self.action_new_file.setIcon(QtGui.QIcon(
            ':/ide-icons/rc/cobol-mimetype.png'))
        self.action_new_file.triggered.connect(self._on_new_file_triggered)
        return [self.action_new_file]

    def _on_new_file_triggered(self):
        self.app.file.request_new(
            self.tree_view.filePath(self.tree_view.currentIndex()))


class FileIconProvider(PyQodeIconProvider):
    def icon(self, type_or_info):
        if isinstance(type_or_info, QtCore.QFileInfo):
            # COBOL file
            if ('.' + type_or_info.suffix().lower() in
                    Settings().all_extensions):
                return QtGui.QIcon(icons.ICON_MIMETYPE)
            if 'linux' not in sys.platform:
                # use hardcoded icon on windows/OSX
                if type_or_info.isDir():
                    return QtGui.QIcon(':/ide-icons/rc/folder.png')
                else:
                    return QtGui.QIcon(':/ide-icons/rc/file.png')
        else:
            if 'linux' not in sys.platform:
                if type_or_info == self.Folder:
                    return QtGui.QIcon(':/ide-icons/rc/file.png')
                elif type_or_info == self.File:
                    return QtGui.QIcon('')
        return super().icon(type_or_info)
