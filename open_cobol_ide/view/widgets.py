"""
Widgets in this module are used as promoted widgets in Qt Designer
"""
import os
from pyqode.cobol.api import icons
from pyqode.qt import QtCore, QtGui, QtWidgets
from pyqode.core.widgets import SplittableCodeEditTabWidget, OutputWindow
from pyqode.core.widgets import FileSystemContextMenu, FileIconProvider as \
    PyQodeIconProvider
import sys
from open_cobol_ide import backend, system
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


class PathLineEdit(QtWidgets.QLineEdit):
    """
    Line edit specialised for choosing a path.

    Features:
        - use QCompleter with a QDirModel to automatically complete paths.
        - allow user to drop files and folders to set url text
    """
    class Completer(QtWidgets.QCompleter):
        def splitPath(self, path):
            path = os.path.split(os.pathsep)[-1]
            return super().splitPath(path)

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        completer = self.Completer()
        model = QtWidgets.QDirModel(completer)
        model.setIconProvider(FileIconProvider())
        completer.setModel(model)
        self.setCompleter(completer)
        self.setDragEnabled(True)

    def dragEnterEvent(self, event):
        data = event.mimeData()
        urls = data.urls()
        if urls and urls[0].scheme() == 'file':
            event.acceptProposedAction()

    def dragMoveEvent(self, event):
        data = event.mimeData()
        urls = data.urls()
        if urls and urls[0].scheme() == 'file':
            event.acceptProposedAction()

    def dropEvent(self, event):
        data = event.mimeData()
        urls = data.urls()
        if urls and urls[0].scheme() == 'file':
            # for some reason, this doubles up the intro slash
            filepath = urls[0].path()
            if system.windows and filepath.startswith('/'):
                filepath = filepath[1:]
                filepath = os.path.normpath(filepath)
            print(filepath, urls)
            self.setText(filepath)
            self.setFocus()


class ColorPicker(QtWidgets.QPushButton):
    """
    Custom widget to pick a color.
    """
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._color = None
        self.color = QtGui.QColor('red')
        self.setMaximumWidth(32)
        self.pressed.connect(self.pick_color)

    @property
    def color(self):
        return self._color

    @color.setter
    def color(self, color):
        self._color = color
        self.setStyleSheet("ColorPicker { background-color: %s;}" %
                           self._color.name())

    def pick_color(self):
        """
        Show color-picker dialog to select color.

        Qt will use the native dialog by default.
        """
        dlg = QtWidgets.QColorDialog(self)
        dlg.setCurrentColor(QtGui.QColor(self._color))
        dlg.exec_()
        self.color = dlg.currentColor()


class MyOutputWindow(OutputWindow):
    def __init__(self, parent=None):
        cwd = os.path.dirname(sys.executable)
        base_backend = 'core-backend'
        if sys.platform == 'win32':
            base_backend += '.exe'
        backend_path = os.path.join(cwd, base_backend) if hasattr(sys, 'frozen') else backend.__file__
        super(MyOutputWindow, self).__init__(parent=parent, backend=backend_path)
