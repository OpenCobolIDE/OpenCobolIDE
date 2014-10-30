"""
Widgets in this module are used as promoted widgets in Qt Designer
"""
import os
from pyqode.core.qt import QtCore, QtGui, QtWidgets
from pyqode.core.widgets import SplittableCodeEditTabWidget
from pyqode.core.widgets import FileSystemContextMenu


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


class TabCornerWidget(QtWidgets.QWidget):
    dropbtn_stylesheet = """

    QToolButton  { /* all types of tool button */
        background-color: transparent;
        border: 1px solid transparent;
        border-radius: 11px;
        padding: 2px;
        margin-bottom: 1px;
    }

    QToolButton:hover {
        background-color: rgba(128, 128, 128, 20);
        border: 1px solid transparent;
    }

    QToolButton:pressed  {
        background-color: rgba(128, 128, 128, 40);
        border: 1px solid transparent;
    }

    /* the subcontrols below are used only in the MenuButtonPopup mode */
    QToolButton::menu-button  {
        background-color: transparent;
        border: 1px transparent black;
        border-top-right-radius: 3px;
        border-bottom-right-radius: 3px;
        /* 16px width + 4px for border = 20px allocated above */
        width: 16px;
    }

    QToolButton::menu-arrow  {
        image: url(":/pyqode-icons/rc/arrow_down_off.png");
    }

    QToolButton::menu-arrow:open  {
        top: 1px; left: 1px; /* shift it a bit */
    }

    """

    def __init__(self, parent, bt_compile, bt_run):
        super().__init__(parent)
        assert isinstance(bt_compile, QtWidgets.QToolButton)
        assert isinstance(bt_run, QtWidgets.QToolButton)
        bt_compile.setPopupMode(bt_compile.DelayedPopup)
        bt_compile.setStyleSheet(self.dropbtn_stylesheet + """
        QToolButton{
        padding-right: 10px; /* make way for the popup button */
        }""")
        bt_run.setStyleSheet(self.dropbtn_stylesheet)
        layout = QtWidgets.QHBoxLayout()
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(bt_compile)
        layout.addWidget(bt_run)
        self.setLayout(layout)


class TabWidget(SplittableCodeEditTabWidget):
    tab_bar_double_clicked = QtCore.Signal()

    def __init__(self, parent, root=True):
        super().__init__(parent, root)
        self.main_tab_widget.tabBar().double_clicked.connect(
            self.tab_bar_double_clicked.emit)

    def split(self, widget, orientation):
        splitter = super().split(widget, orientation)
        splitter.tab_bar_double_clicked.connect(
            self.tab_bar_double_clicked.emit)


class FSContextMenu(FileSystemContextMenu):
    def __init__(self, app):
        super().__init__()
        self.app = app

    def _init_actions(self):
        self.new_menu = QtWidgets.QMenu('New', self)
        self.new_menu.setIcon(QtGui.QIcon.fromTheme('document-new'))
        self.action_new_file = self.new_menu.addAction('Cobol File')
        self.action_new_file.setIcon(QtGui.QIcon(
            ':/ide-icons/rc/cobol-mimetype.png'))
        self.action_new_file.triggered.connect(self._on_new_file_triggered)
        self.action_mkdir = self.new_menu.addAction('Directory')
        self.action_mkdir.triggered.connect(self._on_mkdir_triggered)
        self.action_mkdir.setIcon(QtGui.QIcon.fromTheme('folder'))
        self.addMenu(self.new_menu)
        self.addSeparator()
        super()._init_actions()

    def _on_new_file_triggered(self):
        self.app.file.request_new(
            self.tree_view.filePath(self.tree_view.currentIndex()))

    def _on_mkdir_triggered(self):
        name, status = QtWidgets.QInputDialog.getText(
            self.tree_view, 'New directory', 'New directory name: ')
        if status:
            path = self.tree_view.filePath(self.tree_view.currentIndex())
            if os.path.isfile(path):
                path = os.path.abspath(os.path.join(path, os.pardir))
            dest = os.path.join(path, name)
            os.mkdir(dest)
