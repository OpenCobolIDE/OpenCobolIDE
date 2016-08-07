"""
This module contains the splittable tab widget API
"""
import inspect
import logging
import mimetypes
import os
import io
import sys
import uuid
import weakref

from pyqode.qt import QtCore, QtWidgets, QtGui
from pyqode.core.api import utils
from pyqode.core.dialogs import DlgUnsavedFiles
from pyqode.core._forms import popup_open_files_ui
from .tab_bar import TabBar
from .code_edits import GenericCodeEdit, TextCodeEdit

from pyqode.core._forms import pyqode_core_rc
assert pyqode_core_rc


def _logger():
    return logging.getLogger(__name__)


class DraggableTabBar(TabBar):
    """
    A draggable tab bar that allow to drag & drop tabs.

    Implementation is based on this qt article:
    http://www.qtcentre.org/wiki/index.php?title=Movable_Tabs
    """
    #: Signal emitted when a tab must be moved to the specified
    #: index (the tab might come from another tab bar (split)).
    tab_move_request = QtCore.Signal(QtWidgets.QWidget, int)

    def __init__(self, parent):
        super(DraggableTabBar, self).__init__(parent)
        self._pos = QtCore.QPoint()
        self.setAcceptDrops(True)
        self.setMouseTracking(True)
        self.setElideMode(QtCore.Qt.ElideNone)

    def mousePressEvent(self, event):
        if event.button() == QtCore.Qt.LeftButton:
            self._pos = event.pos()  # _pos is a QPoint defined in the header
        super(DraggableTabBar, self).mousePressEvent(event)

    def widget_under_mouse(self, event):
        index = self.tabAt(event.pos())
        tab = self.parent().widget(index)
        return tab

    def mouseMoveEvent(self, event):
        # update tooltip with the tooltip of the tab under mouse cursor.
        tab = self.widget_under_mouse(event)
        if tab is not None:
            tooltip = tab.toolTip()
            if not tooltip:
                try:
                    tooltip = tab.file.path
                except AttributeError:
                    pass
            self.setToolTip(tooltip)

        # If the distance is too small then return
        if (event.pos() - self._pos).manhattanLength() < \
                QtWidgets.QApplication.startDragDistance():
            return

        # If the left button isn't pressed anymore then return
        if not event.buttons() & QtCore.Qt.LeftButton:
            return

        drag = QtGui.QDrag(self)
        data = QtCore.QMimeData()
        data.tab = tab
        data.widget = self
        # a crude way to distinguish tab-reodering drags from other drags
        data.setData("action", b"tab-reordering")
        drag.setMimeData(data)
        drag.setPixmap(self.tabIcon(self.tabAt(event.pos())).pixmap(32, 32))
        drag.exec_()

    def dragEnterEvent(self, event):
        # Only accept if it's an tab-reordering request
        m = event.mimeData()
        formats = m.formats()
        if "action" in formats and m.data("action") == "tab-reordering":
            event.acceptProposedAction()

    def dropEvent(self, event):
        # drop a tab in a split (may be the same split or another one).
        m = event.mimeData()
        index = self.tabAt(event.pos())
        # Tell interested objects that a tab should be moved.
        if m.tab != self.parent().widget(index):
            self.tab_move_request.emit(m.tab, index)
        event.acceptProposedAction()


class BaseTabWidget(QtWidgets.QTabWidget):
    """
    Base tab widget class used by SplittableTabWidget. This tab widget adds a
    context menu to the tab bar that allow the user to:
        - split the current tab (horizontally or vertically)
        - close the current tab
        - close all tabs
        - close all other tabs
    """
    #: Signal emitted when the last tab has been closed
    last_tab_closed = QtCore.Signal()

    #: Signal emitted when a tab has been closed
    tab_closed = QtCore.Signal(QtWidgets.QWidget)

    #: Signal emitted when the user clicked on split vertical or split
    #: horizontal
    #: **Parameters**:
    #: - widget: the widget to split
    #: - orientation: split orientation (horizontal/vertical)
    split_requested = QtCore.Signal(QtWidgets.QWidget, int)

    #: Signal emitted when a tab got detached from the TabWidget
    #: **Parameters**:
    #: - old_tab: the old tab instance (before it get closed)
    #: - new_tab: the new tab instance (the one that is detached)
    tab_detached = QtCore.Signal(QtWidgets.QWidget, QtWidgets.QWidget)

    _detached_window_class = None

    def __init__(self, parent):
        super(BaseTabWidget, self).__init__(parent)
        self._current = None
        self.currentChanged.connect(self._on_current_changed)
        self.tabCloseRequested.connect(self._on_tab_close_requested)

        tab_bar = DraggableTabBar(self)
        tab_bar.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        tab_bar.customContextMenuRequested.connect(self._show_tab_context_menu)
        tab_bar.tab_move_request.connect(self._on_tab_move_request)
        self.setTabBar(tab_bar)
        self.setAcceptDrops(True)
        self.setUsesScrollButtons(True)

        #: A list of additional context menu actions
        self.context_actions = []
        self.a_close = None
        self.a_close_all = None
        self._menu_pos = None
        self._create_tab_bar_menu()

        self.detached_tabs = []

    def tab_under_menu(self):
        """
        Returns the tab that sits under the context menu.
        :return: QWidget
        """
        if self._menu_pos:
            return self.tabBar().tabAt(self._menu_pos)
        else:
            return self.currentIndex()

    def close(self):
        """
        Closes the active editor
        """
        self.tabCloseRequested.emit(self.tab_under_menu())

    def close_others(self):
        """
        Closes every editors tabs except the current one.
        """
        current_widget = self.widget(self.tab_under_menu())
        if self._try_close_dirty_tabs(exept=current_widget):
            i = 0
            while self.count() > 1:
                widget = self.widget(i)
                if widget != current_widget:
                    self.remove_tab(i)
                else:
                    i = 1

    def close_left(self):
        """
        Closes every editors tabs on the left of the current one.
        """
        current_widget = self.widget(self.tab_under_menu())
        index = self.indexOf(current_widget)
        if self._try_close_dirty_tabs(tab_range=range(index)):
            while True:
                widget = self.widget(0)
                if widget != current_widget:
                    self.remove_tab(0)
                else:
                    break

    def close_right(self):
        """
        Closes every editors tabs on the left of the current one.
        """
        current_widget = self.widget(self.tab_under_menu())
        index = self.indexOf(current_widget)
        if self._try_close_dirty_tabs(tab_range=range(index + 1, self.count())):
            while True:
                widget = self.widget(self.count() - 1)
                if widget != current_widget:
                    self.remove_tab(self.count() - 1)
                else:
                    break

    def close_all(self):
        """
        Closes all editors
        """
        if self._try_close_dirty_tabs():
            while self.count():
                widget = self.widget(0)
                self.remove_tab(0)
                self.tab_closed.emit(widget)
            return True
        return False

    def detach_tab(self):
        tab_index = self.tab_under_menu()
        tab = self.widget(tab_index)
        try:
            open_parameters = tab.open_parameters
        except AttributeError:
            open_parameters = {
                'encoding': None,
                'replace_tabs_by_spaces': True,
                'clean_trailing_whitespaces': True,
                'safe_save': True,
                'restore_cursor_position': True,
                'preferred_eol': 0,
                'autodetect_eol': True,
                'show_whitespaces': False,
                'kwargs': {}
            }

        path = tab.file.path
        self.tabCloseRequested.emit(tab_index)

        # create a new top level widget and add the tab
        new_tab_widget = self.parent().__class__()
        # reopen document with same open settings.
        new_tab = new_tab_widget.open_document(
            path, encoding=open_parameters['encoding'],
            replace_tabs_by_spaces=open_parameters['replace_tabs_by_spaces'],
            clean_trailing_whitespaces=open_parameters[
                'clean_trailing_whitespaces'],
            safe_save=open_parameters['safe_save'],
            restore_cursor_position=open_parameters['restore_cursor_position'],
            preferred_eol=open_parameters['preferred_eol'],
            autodetect_eol=open_parameters['autodetect_eol'],
            show_whitespaces=open_parameters['show_whitespaces'],
            **open_parameters['kwargs'])

        if self._detached_window_class is None:
            win = new_tab_widget
        else:
            win = self._detached_window_class()
            #: detached window must be an instance of QMainWindow
            win.setCentralWidget(new_tab_widget)

        self.detached_tabs.append(win)
        win.resize(800, 600)
        win.show()

        self.tab_detached.emit(tab, new_tab)

        # if the user has two monitor, move the window to the second monitor
        desktop = QtWidgets.qApp.desktop()
        if desktop.screenCount() > 1:
            primary_screen = desktop.screenNumber(self)
            other_screen = {0: 1, 1: 0}[primary_screen]
            l = desktop.screenGeometry(other_screen).left()
            new_tab_widget.move(l, 0)
            new_tab_widget.showMaximized()

        new_tab_widget.last_tab_closed.connect(self._remove_detached_tab)

    def _remove_detached_tab(self):
        self.detached_tabs.remove(self.sender())
        self.sender().close()

    def save_widget(self, editor):
        """
        Saves the widget. The base implementation does nothing.

        The function must return a bool that tells whether the save succeeded
        or not.

        :param editor: editor widget to save.
        """
        return True

    def _create_tab_bar_menu(self):
        context_mnu = QtWidgets.QMenu()
        for name, slot, icon in [
                (_('Close tab'), self.close, 'document-close'),
                (_('Close tabs to the left'), self.close_left, 'tab-close-other'),
                (_('Close tabs to the right'), self.close_right, 'tab-close-other'),
                (_('Close others tabs'), self.close_others, 'tab-close-other'),
                (_('Close all tabs'), self.close_all,
                 'project-development-close-all'),
                (None, None, None),
                (_('Detach tab'), self.detach_tab, 'tab-detach')]:
            if name is None and slot is None:
                qaction = QtWidgets.QAction(self)
                qaction.setSeparator(True)
            else:
                qaction = QtWidgets.QAction(name, self)
                qaction.triggered.connect(slot)
                if icon:
                    qaction.setIcon(QtGui.QIcon.fromTheme(icon))
            if slot == self.close and self.a_close is None:
                self.a_close = qaction
                self.addAction(self.a_close)
            elif slot == self.close_left:
                self.a_close_left = qaction
            elif slot == self.close_right:
                self.a_close_right = qaction
            elif slot == self.close_others:
                self.a_close_others = qaction
            elif slot == self.close_all:
                self.a_close_all = qaction
                self.addAction(self.a_close_all)
            context_mnu.addAction(qaction)
            # self.addAction(qaction)
        context_mnu.addSeparator()
        menu = QtWidgets.QMenu(_('Split'), context_mnu)
        menu.setIcon(QtGui.QIcon.fromTheme('split'))
        a = menu.addAction(_('Split horizontally'))
        a.triggered.connect(self._on_split_requested)
        a.setIcon(QtGui.QIcon.fromTheme('view-split-left-right'))
        a = menu.addAction(_('Split vertically'))
        a.setIcon(QtGui.QIcon.fromTheme('view-split-top-bottom'))
        a.triggered.connect(self._on_split_requested)
        context_mnu.addMenu(menu)
        context_mnu.addSeparator()
        if self.context_actions:
            context_mnu.addSeparator()
        for action in self.context_actions:
            context_mnu.addAction(action)

        tab = self.widget(self.tab_under_menu())
        index = self.indexOf(tab)
        self.a_close_right.setVisible(0 <= index < self.count() - 1)
        self.a_close_left.setVisible(0 < index <= self.count() - 1)
        self.a_close_others.setVisible(self.count() > 1)
        self.a_close_all.setVisible(self.count() > 1)

        self.a_close.setShortcut('Ctrl+W')
        self.a_close_all.setShortcut('Ctrl+Shift+W')

        self._context_mnu = context_mnu
        return context_mnu

    def _show_tab_context_menu(self, position):
        if self.count():
            self._menu_pos = position
            SplittableTabWidget.tab_under_menu = self.widget(
                self.tab_under_menu())
            mnu = self._create_tab_bar_menu()
            mnu.exec_(self.tabBar().mapToGlobal(position))
            self._menu_pos = None

    def _collect_dirty_tabs(self, skip=None, tab_range=None):
        """
        Collects the list of dirty tabs

        :param skip: Tab to skip (used for close_others).
        """
        widgets = []
        filenames = []
        if tab_range is None:
            tab_range = range(self.count())
        for i in tab_range:
            widget = self.widget(i)
            try:
                if widget.dirty and widget != skip:
                    widgets.append(widget)
                    if widget.file.path:
                        filenames.append(widget.file.path)
                    else:
                        filenames.append(widget.documentTitle())
            except AttributeError:
                pass
        return widgets, filenames

    def _try_close_dirty_tabs(self, exept=None, tab_range=None):
        """
        Tries to close dirty tabs. Uses DlgUnsavedFiles to ask the user
        what he wants to do.
        """
        widgets, filenames = self._collect_dirty_tabs(skip=exept, tab_range=tab_range)
        if not len(filenames):
            return True
        dlg = DlgUnsavedFiles(self, files=filenames)
        if dlg.exec_() == dlg.Accepted:
            if not dlg.discarded:
                for item in dlg.listWidget.selectedItems():
                    filename = item.text()
                    widget = None
                    for widget in widgets:
                        if widget.file.path == filename:
                            break
                    if widget != exept:
                        self.save_widget(widget)
                        self.remove_tab(self.indexOf(widget))
            return True
        return False

    def _get_widget_path(self, widget):
        try:
            return widget.file.path
        except AttributeError:
            return ''

    def _on_tab_close_requested(self, index):
        widget = self.widget(index)
        dirty = False
        try:
            if widget.original is None:
                dirty = widget.dirty
        except AttributeError:
            pass
        if not dirty:
            self.remove_tab(index)
        else:
            # unsaved widget
            path = self._get_widget_path(widget)
            if not path:
                path = self.tabText(self.indexOf(widget))
            dlg = DlgUnsavedFiles(
                self, files=[path])
            if dlg.exec_() == dlg.Accepted:
                rm = True
                if not dlg.discarded:
                    try:
                        rm = self.save_widget(widget)
                    except OSError:
                        pass
                if rm:
                    self.remove_tab(index)

    @staticmethod
    def _close_widget(widget):
        """
        Closes the given widgets and handles cases where the widget has been
        clone or is a clone of another widget
        """
        if widget is None:
            return
        try:
            widget.document().setParent(None)
            widget.syntax_highlighter.setParent(None)
        except AttributeError:
            pass  # not a QPlainTextEdit subclass
        # handled cloned widgets
        clones = []
        if hasattr(widget, 'original') and widget.original:
            # cloned widget needs to be removed from the original
            widget.original.clones.remove(widget)
            try:
                widget.setDocument(None)
            except AttributeError:
                # not a QTextEdit/QPlainTextEdit
                pass
        elif hasattr(widget, 'clones'):
            clones = widget.clones
        try:
            # only clear current editor if it does not have any other clones
            widget.close(clear=len(clones) == 0)
        except (AttributeError, TypeError):
            # not a CodeEdit
            widget.close()
        return clones

    def _restore_original(self, clones):
        try:
            first = clones[0]
        except (IndexError, TypeError):
            # empty or None
            pass
        else:
            first.clones = clones[1:]
            first.original = None
            for c in first.clones:
                c.original = first

    def remove_tab(self, index):
        """
        Overrides removeTab to emit tab_closed and last_tab_closed signals.

        :param index: index of the tab to remove.
        """
        widget = self.widget(index)
        try:
            document = widget.document()
        except AttributeError:
            document = None  # not a QPlainTextEdit
        clones = self._close_widget(widget)
        self.tab_closed.emit(widget)
        self.removeTab(index)
        self._restore_original(clones)
        widget._original_tab_widget._tabs.remove(widget)
        if self.count() == 0:
            self.last_tab_closed.emit()
        if SplittableTabWidget.tab_under_menu == widget:
            SplittableTabWidget.tab_under_menu = None
        if not clones:
            widget.setParent(None)
        else:
            try:
                clones[0].syntax_highlighter.setDocument(document)
            except AttributeError:
                pass  # not a QPlainTextEdit

    def _on_split_requested(self):
        """
        Emits the split requested signal with the desired orientation.
        """
        orientation = self.sender().text()
        widget = self.widget(self.tab_under_menu())
        if 'horizontally' in orientation:
            self.split_requested.emit(
                widget, QtCore.Qt.Horizontal)
        else:
            self.split_requested.emit(
                widget, QtCore.Qt.Vertical)

    def _on_current_changed(self, index):
        tab = self.widget(index)
        if tab:
            tab.setFocus()

    def _on_tab_move_request(self, widget, new_index):
        parent = widget.parent_tab_widget
        index = parent.indexOf(widget)
        text = parent.tabText(index)
        icon = parent.tabIcon(index)
        parent.removeTab(index)
        widget.parent_tab_widget = self
        self.insertTab(new_index, widget, icon, text)
        self.setCurrentIndex(new_index)
        widget.setFocus()
        if parent.count() == 0:
            parent.last_tab_closed.emit()

    def dragEnterEvent(self, event):
        # Only accept if it's an tab-reordering request
        m = event.mimeData()
        formats = m.formats()
        if "action" in formats and m.data("action") == "tab-reordering":
            event.acceptProposedAction()

    def dropEvent(self, event):
        m = event.mimeData()
        index = self.tabBar().tabAt(event.pos())
        # Tell interested objects that a tab should be moved.
        if m.tab != self.widget(index):
            self._on_tab_move_request(m.tab, index)
            event.acceptProposedAction()

    def addTab(self, tab, *args):
        """
        Adds a tab to the tab widget, this function set the parent_tab_widget
        attribute on the tab instance.
        """
        tab.parent_tab_widget = self
        super(BaseTabWidget, self).addTab(tab, *args)


class OpenFilesPopup(QtWidgets.QDialog):
    triggered = QtCore.Signal(str)

    def __init__(self, parent=None, qsettings=None):
        super(OpenFilesPopup, self).__init__(parent)
        self.ui = popup_open_files_ui.Ui_Dialog()
        self.ui.setupUi(self)
        self.ui.tableWidget.itemActivated.connect(self._on_item_activated)
        self.ui.tableWidget.itemDoubleClicked.connect(self._on_item_activated)
        if qsettings is None:
            self.settings = QtCore.QSettings('pyQode', 'pyqode.core')
        else:
            self.settings = qsettings
        self.sort_enabled = bool(self.settings.value(
            'sortOpenFilesAlphabetically', False))
        self.ui.checkBox.setChecked(self.sort_enabled)
        self.ui.checkBox.stateChanged.connect(self._on_sort_changed)

    def set_filenames(self, filenames):
        def clean(filenames):
            ret_val = []
            new_count = 0
            for filename in filenames:
                if not filename:
                    filename = 'New document %d.txt' % (new_count + 1)
                    new_count += 1
                ret_val.append(filename)
            return ret_val

        self._filenames = filenames
        filenames = clean(filenames)
        if self.sort_enabled:
            filenames = sorted(filenames, key=lambda x:
                               QtCore.QFileInfo(x).fileName().lower())
        self.ui.tableWidget.clearContents()
        icon_provider = SplittableCodeEditTabWidget.icon_provider_klass()
        self.ui.tableWidget.setRowCount(len(filenames))
        self.ui.tableWidget.horizontalHeader().setSectionResizeMode(
            QtWidgets.QHeaderView.ResizeToContents)
        for row, path in enumerate(filenames):
            finfo = QtCore.QFileInfo(path)
            filename = finfo.fileName()
            if finfo.exists():
                icon = icon_provider.icon(finfo)
            else:
                icon = icon_provider.icon(icon_provider.File)
            # file name
            item = QtWidgets.QTableWidgetItem()
            item.setText(filename)
            item.setIcon(icon)
            item.setToolTip(path)
            item.setData(QtCore.Qt.UserRole, bytes(path, 'utf-8'))
            self.ui.tableWidget.setItem(row, 0, item)

            # path
            item = QtWidgets.QTableWidgetItem()
            item.setText(path)
            item.setToolTip(path)
            item.setData(QtCore.Qt.UserRole, bytes(path, 'utf-8'))
            self.ui.tableWidget.setItem(row, 1, item)

    def _on_sort_changed(self, *_):
        self.sort_enabled = self.ui.checkBox.isChecked()
        self.settings.setValue(
            'sortOpenFilesAlphabetically', self.sort_enabled)
        self.set_filenames(self._filenames)

    def _on_item_activated(self, item):
        self.hide()
        self.triggered.emit(item.data(QtCore.Qt.UserRole).decode('utf-8'))

    def show(self):
        super(OpenFilesPopup, self).show()
        self.ui.tableWidget.setFocus()
        self.ui.tableWidget.selectRow(0)


class SplittableTabWidget(QtWidgets.QSplitter):
    """
    A splittable tab widget. The widget is implemented as a splitter which
    contains a main tab widget and a collection of child SplittableTabWidget.

    Widgets added to the the tab widget **must** have a ``split`` method which
    returns a clone of the widget instance.

    You can add new tabs to the main tab widget by using the ``add_tab``
    method. Tabs are always closable.

    To change the underlying tab widget class, just set the
    ``tab_widget_klass`` class attribute.

    The splittable tab widget works with any kind of widget. There is a
    specialisation made specifically for managing a collection code editor
    widgets: SplittableCodeEditTabWidget.

    The implementation uses duck typing and will automatically show a dialog
    when closing an editor which has a ``dirty`` property. To actually save the
    widget, you must reimplement :meth:`SplittableTabWidget.save_widget``.
    """
    #: Signal emitted when the last tab has been closed.
    last_tab_closed = QtCore.Signal(QtWidgets.QSplitter)

    #: Signal emitted when the active tab changed (takes child tab widgets
    #: into account). Parameter is the new tab widget.
    current_changed = QtCore.Signal(QtWidgets.QWidget)

    #: Signal emitted when a tab got detached from the TabWidget
    #: **Parameters**:
    #: - old_tab: the old tab instance (before it get closed)
    #: - new_tab: the new tab instance (the one that is detached)
    tab_detached = QtCore.Signal(QtWidgets.QWidget, QtWidgets.QWidget)

    #: The window to use when a type is detached. If None, the detached tab
    #: widget will be shown directly.
    detached_window_klass = None

    #: underlying tab widget class
    tab_widget_klass = BaseTabWidget

    #: Reference to the widget under the tab bar menu
    tab_under_menu = None

    @property
    def popup_shortcut(self):
        """
        Gets/sets the open files popup shortcut (ctrl+t by default).
        """
        if hasattr(self, '_action_popup'):
            return self._shortcut
        return None

    @popup_shortcut.setter
    def popup_shortcut(self, value):
        if hasattr(self, '_action_popup'):
            self._shortcut = value
            self._action_popup.setShortcut(self._shortcut)

    def __init__(self, parent=None, root=True, create_popup=True,
                 qsettings=None):
        super(SplittableTabWidget, self).__init__(parent)
        SplittableTabWidget.tab_widget_klass._detached_window_class = \
            SplittableTabWidget.detached_window_klass
        if root:
            self._action_popup = QtWidgets.QAction(self)
            self._action_popup.setShortcutContext(QtCore.Qt.WindowShortcut)
            self._shortcut = 'Ctrl+T'
            self._action_popup.setShortcut(self._shortcut)
            self._action_popup.triggered.connect(self._show_popup)
            self.addAction(self._action_popup)
            self.popup = OpenFilesPopup(qsettings=qsettings)
            self.popup.setWindowFlags(
                QtCore.Qt.Popup | QtCore.Qt.FramelessWindowHint)
            self.popup.triggered.connect(self._on_popup_triggered)
        self.child_splitters = []
        self.main_tab_widget = self.tab_widget_klass(self)
        self.main_tab_widget.last_tab_closed.connect(
            self._on_last_tab_closed)
        self.main_tab_widget.tab_detached.connect(self.tab_detached.emit)
        self.main_tab_widget.split_requested.connect(self.split)
        self.addWidget(self.main_tab_widget)
        self._parent_splitter = None
        self._current = None
        self.root = root
        if root:
            QtWidgets.QApplication.instance().focusChanged.connect(
                self._on_focus_changed)
        self._uuid = uuid.uuid1()
        self._tabs = []

    def add_context_action(self, action):
        """
        Adds a custom context menu action

        :param action: action to add.
        """
        self.main_tab_widget.context_actions.append(action)
        for child_splitter in self.child_splitters:
            child_splitter.add_context_action(action)

    def add_tab(self, tab, title='', icon=None):
        """
        Adds a tab to main tab widget.

        :param tab: Widget to add as a new tab of the main tab widget.
        :param title: Tab title
        :param icon: Tab icon
        """
        if icon:
            tab._icon = icon
        if not hasattr(tab, 'clones'):
            tab.clones = []
        if not hasattr(tab, 'original'):
            tab.original = None
        if icon:
            self.main_tab_widget.addTab(tab, icon, title)
        else:
            self.main_tab_widget.addTab(tab, title)
        self.main_tab_widget.setCurrentIndex(
            self.main_tab_widget.indexOf(tab))
        self.main_tab_widget.show()
        tab._uuid = self._uuid
        try:
            scroll_bar = tab.horizontalScrollBar()
        except AttributeError:
            # not a QPlainTextEdit class
            pass
        else:
            scroll_bar.setValue(0)
        tab.setFocus()
        tab._original_tab_widget = self
        self._tabs.append(tab)
        self._on_focus_changed(None, tab)

    def _on_popup_triggered(self, path):
        new_count = 0
        for w in self.widgets():
            if w.file.path == path:
                index = w.parent_tab_widget.indexOf(w)
                w.parent_tab_widget.setCurrentIndex(index)
                break
            elif w.file.path == '':
                # New document
                fpath = 'New document %d.txt' % (new_count + 1)
                if fpath == path:
                    index = w.parent_tab_widget.indexOf(w)
                    w.parent_tab_widget.setCurrentIndex(index)
                    break
                new_count += 1

    def _show_popup(self):
        parent_pos = self.main_tab_widget.pos()
        parent_size = self.main_tab_widget.size()
        size = self.popup.size()
        x, y = parent_pos.x(), parent_pos.y()
        pw, ph = parent_size.width(), parent_size.height()
        w = size.width()
        x += pw / 2 - w / 2
        y += ph / 10
        self.popup.move(self.mapToGlobal(QtCore.QPoint(x, y)))
        self.popup.set_filenames(
            [editor.file.path for editor in self.widgets()])
        self.popup.show()

    def _make_splitter(self):
        splitter = None
        for widget in reversed(self.child_splitters):
            if widget.parent() is None:
                widget.setParent(self)
                splitter = widget
                break
        if splitter is None:
            splitter = self.__class__(self, root=False)
            for action in self.main_tab_widget.context_actions:
                splitter.add_context_action(action)
        return splitter

    def split(self, widget, orientation):
        """
        Split the the current widget in new SplittableTabWidget.

        :param widget: widget to split
        :param orientation: orientation of the splitter
        :return: the new splitter
        """
        if widget.original:
            base = widget.original
        else:
            base = widget
        clone = base.split()
        if not clone:
            return
        if orientation == int(QtCore.Qt.Horizontal):
            orientation = QtCore.Qt.Horizontal
        else:
            orientation = QtCore.Qt.Vertical
        self.setOrientation(orientation)
        splitter = self._make_splitter()
        splitter.show()
        self.addWidget(splitter)
        self.child_splitters.append(splitter)
        if clone not in base.clones:
            # code editors maintain the list of clones internally but some
            # other widgets (user widgets) might not.
            base.clones.append(clone)
        clone.original = base
        splitter._parent_splitter = self
        splitter.last_tab_closed.connect(self._on_last_child_tab_closed)
        splitter.tab_detached.connect(self.tab_detached.emit)
        if hasattr(base, '_icon'):
            icon = base._icon
        else:
            icon = None
        # same group of tab splitter (user might have a group for editors and
        # another group for consoles or whatever).
        splitter._uuid = self._uuid
        splitter.add_tab(clone, title=self.main_tab_widget.tabText(
            self.main_tab_widget.indexOf(widget)), icon=icon)
        self.setSizes([1 for i in range(self.count())])
        return splitter

    def has_children(self):
        """
        Checks if there are children tab widgets.
        :return: True if there is at least one tab in the children tab widget.
        """
        for splitter in self.child_splitters:
            if splitter.has_children():
                return splitter
        return self.main_tab_widget.count() != 0

    def current_widget(self):
        """
        Returns a reference to the current widget, i.e. the last widget that
        got the focus.
        :return: QWidget
        """
        if self._current:
            return self._current()
        return None

    def widgets(self, include_clones=False):
        """
        Recursively gets the list of widgets.

        :param include_clones: True to retrieve all tabs, including clones,
            otherwise only original widgets are returned.
        """
        widgets = []
        for i in range(self.main_tab_widget.count()):
            widget = self.main_tab_widget.widget(i)
            try:
                if widget.original is None or include_clones:
                    widgets.append(widget)
            except AttributeError:
                pass
        for child in self.child_splitters:
            widgets += child.widgets(include_clones=include_clones)
        return widgets

    def _on_last_tab_closed(self, *args):
        has_children = self.has_children()
        if has_children:
            # hide the tab widget if there is not tabs
            if not self.main_tab_widget.count():
                self.main_tab_widget.hide()
        else:
            if self.root:
                # ensure root is visible when there are no children
                self.show()
                self.main_tab_widget.show()
            else:
                # hide ourselves (we don't have any other tabs or children)
                self._remove_from_parent()
        if not self.has_children():
            self.last_tab_closed.emit(self)

    def _on_focus_changed(self, old, new):
        try:
            result = new._uuid == self._uuid
        except (AttributeError, TypeError):
            pass
        else:
            if result:
                if new != self.current_widget():
                    self._on_current_changed(new)

    def _on_current_changed(self, new):
        old = self.current_widget()
        self._current = weakref.ref(new)
        _logger().debug(
            'current tab changed (old=%r, new=%r)', old, new)
        self.current_changed.emit(new)
        return old, new

    def _remove_from_parent(self):
        self.hide()
        self.setParent(None)
        self.main_tab_widget.hide()
        if not self.root:
            self._parent_splitter.child_splitters.remove(self)
            self._parent_splitter = None

    def _on_last_child_tab_closed(self):
        if not self.has_children():
            self.last_tab_closed.emit(self)
            if self.root:
                self.show()
                self.main_tab_widget.show()
            else:
                self._remove_from_parent()

    def count(self):
        """
        Returns the number of widgets currently displayed (takes child splits
        into account).
        """
        c = self.main_tab_widget.count()
        for child in self.child_splitters:
            c += child.count()
        return c


class CodeEditTabWidget(BaseTabWidget):
    """
    Tab widget specialised to hold pyqode's code editor widgets.

    It will manage the saving of editors
    """
    default_directory = os.path.expanduser('~')
    dirty_changed = QtCore.Signal(bool)

    @classmethod
    @utils.memoized
    def get_filter(cls, mimetype):
        """
        Returns a filter string for the file dialog. The filter is based
        on the mime type.

        :param mimetype: path from which the filter must be derived.
        :return: Filter string
        """
        filters = ' '.join(
            ['*%s' % ext for ext in mimetypes.guess_all_extensions(mimetype)])
        return '%s (%s)' % (mimetype, filters)

    def addTab(self, widget, *args):
        """
        Re-implements addTab to connect to the dirty changed signal and setup
        some helper attributes.

        :param widget: widget to add
        :param args: optional addtional arguments (name and/or icon).
        """
        widget.dirty_changed.connect(self._on_dirty_changed)
        super(CodeEditTabWidget, self).addTab(widget, *args)

    def _on_dirty_changed(self, dirty):
        """
        Adds a star in front of a dirtt tab and emits dirty_changed.
        """
        widget = self.sender()
        if isinstance(widget, DraggableTabBar):
            return
        parent = widget.parent_tab_widget
        index = parent.indexOf(widget)
        title = parent.tabText(index)
        title = title.replace('* ', '')
        if dirty:
            parent.setTabText(index, "* " + title)
        else:
            parent.setTabText(index, title)
        parent.dirty_changed.emit(dirty)

    @classmethod
    def _ask_path(cls, editor):
        """
        Shows a QFileDialog and ask for a save filename.

        :return: save filename
        """
        try:
            filter = cls.get_filter(editor.mimetypes[0])
        except IndexError:
            filter = _('All files (*)')
        return QtWidgets.QFileDialog.getSaveFileName(
            editor, _('Save file as'), cls.default_directory, filter)

    @classmethod
    def save_widget(cls, editor):
        """
        Implements SplittableTabWidget.save_widget to actually save the
        code editor widget.

        If the editor.file.path is None or empty or the file does not exist,
        a save as dialog is shown (save as).

        :param editor: editor widget to save.
        :return: False if there was a problem saving the editor (e.g. the save
        as dialog has been canceled by the user, or a permission error,...)
        """
        if editor.original:
            editor = editor.original
        if editor.file.path is None or not os.path.exists(editor.file.path):
            # save as
            path, filter = cls._ask_path(editor)
            if not path:
                return False
            if not os.path.splitext(path)[1]:
                if len(editor.mimetypes):
                    path += mimetypes.guess_extension(editor.mimetypes[0])
            try:
                _logger().debug('saving %r as %r', editor.file._old_path, path)
            except AttributeError:
                _logger().debug('saving %r as %r', editor.file.path, path)
            editor.file._path = path
        else:
            path = editor.file.path
        try:
            editor.file.save(path)
        except Exception as e:
            QtWidgets.QMessageBox.warning(editor, "Failed to save file", 'Failed to save %r.\n\nError="%s"' %
                                          (path, e))
        else:
            tw = editor.parent_tab_widget
            text = tw.tabText(tw.indexOf(editor)).replace('*', '')
            tw.setTabText(tw.indexOf(editor), text)
            for clone in [editor] + editor.clones:
                if clone != editor:
                    tw = clone.parent_tab_widget
                    tw.setTabText(tw.indexOf(clone), text)
        return True

    def _get_widget_path(self, editor):
        return editor.file.path


class DetachedEditorWindow(QtWidgets.QMainWindow):
    def __init__(self):
        super(DetachedEditorWindow, self).__init__()
        tb = QtWidgets.QToolBar('File')
        action = tb.addAction(QtGui.QIcon.fromTheme('document-save'),
                              _('Save'))
        action.triggered.connect(self._save)
        action.setShortcut('Ctrl+S')
        self.addToolBar(tb)

    def _save(self):
        self.centralWidget().save_current()


class SplittableCodeEditTabWidget(SplittableTabWidget):
    """
    SplittableTabWidget specialised for CodeEdit and subclasses.

    Offers some convenience function for opening/saving files.

    The widget supports multiple type of code editors. Each editor type must
    be explicitly registered using ``register_editor``. If there is no
    registered editor for the given mime-type, ``fallback_editor`` is used.
    """
    #: Signal emitted when a tab bar is double clicked, this should work
    #: even with child tab bars
    tab_bar_double_clicked = QtCore.Signal()

    #: Signal emitted when a document has been saved.
    #: Parameters:
    #     - save_file_path
    #     - old_content
    document_saved = QtCore.Signal(str, str)

    #: uses a CodeEditTabWidget which is able to save code editor widgets.
    tab_widget_klass = CodeEditTabWidget

    #: the icon provider class to use when creating new document. Must be
    #: a subclass of QtWidgets.QFileIconProvider. By default, QFileIconProvider
    #: is used.
    icon_provider_klass = QtWidgets.QFileIconProvider

    #: Maps a mime-type with an editor type.
    #: This map is used to instantiate the proper editor type when
    #: opening/creating a document.
    editors = {mimetype: TextCodeEdit for mimetype in TextCodeEdit.mimetypes}

    #: Fallback editor is used in case not editors matching the requested
    #: mime-type could not be found in the editors map.
    #: By default the fallback_editor is a
    #: :class:`pyqode.core.widgets.GenericCodeEdit`
    fallback_editor = GenericCodeEdit

    #: signal emitted when the dirty_changed signal of the current editor
    #: has been emitted.
    dirty_changed = QtCore.Signal(bool)

    #: signal emitted when an editor has been created but just before the file
    #: is open. This give you a chance to change some editor settings that
    #: influence file opening.
    editor_created = QtCore.Signal(object)

    #: signal emitted when en editor has been created and the document has
    #: been sucessfully open
    document_opened = QtCore.Signal(object)

    #: Store the number of new documents created, for internal use.
    _new_count = 0

    CLOSED_TABS_HISTORY_LIMIT = 10

    def __init__(self, parent=None, root=True, qsettings=None):
        SplittableTabWidget.detached_window_klass = DetachedEditorWindow
        super(SplittableCodeEditTabWidget, self).__init__(
            parent, root, qsettings=qsettings)
        self.main_tab_widget.tabBar().double_clicked.connect(
            self.tab_bar_double_clicked.emit)
        if root:
            self.closed_tabs_history_btn = QtWidgets.QToolButton()
            self.closed_tabs_history_btn.setAutoRaise(True)
            self.closed_tabs_history_btn.setIcon(QtGui.QIcon.fromTheme(
                'user-trash', QtGui.QIcon(':/pyqode-icons/rc/edit-trash.png')))
            self.closed_tabs_history_btn.setPopupMode(
                QtWidgets.QToolButton.InstantPopup)
            self.closed_tabs_menu = QtWidgets.QMenu()
            self.closed_tabs_history_btn.setMenu(self.closed_tabs_menu)
            self.closed_tabs_history_btn.setDisabled(True)
            self.main_tab_widget.setCornerWidget(self.closed_tabs_history_btn)
            self.main_tab_widget.tab_closed.connect(self._on_tab_closed)

    @classmethod
    def register_code_edit(cls, code_edit_class):
        """
        Register an additional code edit **class**

        .. warning: This method expect a class, not an instance!

        :param code_edit_class: code edit class to register.
        """
        if not inspect.isclass(code_edit_class):
            raise TypeError('must be a class, not an instance.')
        for mimetype in code_edit_class.mimetypes:
            if mimetype in cls.editors:
                _logger().warn('editor for mimetype already registered, '
                               'skipping')
            cls.editors[mimetype] = code_edit_class
        _logger().log(5, 'registered editors: %r', cls.editors)

    def save_current_as(self):
        """
        Save current widget as.
        """
        if not self.current_widget():
            return
        mem = self.current_widget().file.path
        self.current_widget().file._path = None
        self.current_widget().file._old_path = mem
        CodeEditTabWidget.default_directory = os.path.dirname(mem)
        widget = self.current_widget()
        try:
            success = self.main_tab_widget.save_widget(widget)
        except Exception as e:
            QtWidgets.QMessageBox.warning(
                self, _('Failed to save file as'),
                _('Failed to save file as %s\nError=%s') % (
                    widget.file.path, str(e)))
            widget.file._path = mem
        else:
            if not success:
                widget.file._path = mem
            else:
                CodeEditTabWidget.default_directory = os.path.expanduser('~')
                self.document_saved.emit(widget.file.path, '')

                # rename tab
                tw = widget.parent_tab_widget
                tw.setTabText(tw.indexOf(widget),
                              os.path.split(widget.file.path)[1])

        return self.current_widget().file.path

    def save_current(self):
        """
        Save current editor. If the editor.file.path is None, a save as dialog
        will be shown.
        """
        if self.current_widget() is not None:
            editor = self.current_widget()
            self._save(editor)

    def _save(self, widget):
        path = widget.file.path
        try:
            encoding = widget.file.encoding
        except AttributeError:
            # not a code edit
            old_content = ''
        else:
            try:
                with io.open(path, encoding=encoding) as f:
                    old_content = f.read()
            except OSError:
                old_content = ''
        if widget.dirty:
            try:
                self.main_tab_widget.save_widget(widget)
            except Exception as e:
                QtWidgets.QMessageBox.warning(
                    self, 'Failed to save file',
                    'Failed to save file: %s\nError=%s' % (
                        widget.file.path, str(e)))
            else:
                self.document_saved.emit(path, old_content)

    def save_all(self):
        """
        Save all editors.
        """
        for w in self.widgets():
            try:
                self._save(w)
            except OSError:
                _logger().exception('failed to save %s', w.file.path)

    def _create_code_edit(self, mimetype, *args, **kwargs):
        """
        Create a code edit instance based on the mimetype of the file to
        open/create.

        :type mimetype: mime type
        :param args: Positional arguments that must be forwarded to the editor
            widget constructor.
        :param kwargs: Keyworded arguments that must be forwarded to the editor
            widget constructor.
        :return: Code editor widget instance.
        """
        if mimetype in self.editors.keys():
            return self.editors[mimetype](
                *args, parent=self.main_tab_widget, **kwargs)
        editor = self.fallback_editor(*args, parent=self.main_tab_widget,
                                      **kwargs)
        return editor

    def create_new_document(self, base_name='New Document',
                            extension='.txt', preferred_eol=0,
                            autodetect_eol=True, **kwargs):
        """
        Creates a new document.

        The document name will be ``base_name + count + extension``

        :param base_name: Base name of the document. An int will be appended.
        :param extension: Document extension (dotted)
        :param args: Positional arguments that must be forwarded to the editor
            widget constructor.
        :param preferred_eol: Preferred EOL convention. This setting will be
            used for saving the document unless autodetect_eol is True.
        :param autodetect_eol: If true, automatically detects file EOL and
            use it instead of the preferred EOL when saving files.
        :param kwargs: Keyworded arguments that must be forwarded to the editor
            widget constructor.
        :return: Code editor widget instance.
        """
        SplittableCodeEditTabWidget._new_count += 1
        name = '%s%d%s' % (base_name, self._new_count, extension)
        tab = self._create_code_edit(
            self.guess_mimetype(name), **kwargs)
        self.editor_created.emit(tab)
        tab.file.autodetect_eol = autodetect_eol
        tab.file.preferred_eol = preferred_eol
        tab.setDocumentTitle(name)
        self.add_tab(tab, title=name, icon=self._icon(name))
        self.document_opened.emit(tab)
        return tab

    def guess_mimetype(self, path):
        if 'CMakeLists.txt' in path:
            return 'text/x-cmake-project'
        else:
            return mimetypes.guess_type(path)[0]

    @utils.with_wait_cursor
    def open_document(self, path, encoding=None, replace_tabs_by_spaces=True,
                      clean_trailing_whitespaces=True, safe_save=True,
                      restore_cursor_position=True, preferred_eol=0,
                      autodetect_eol=True, show_whitespaces=False, **kwargs):
        """
        Opens a document.

        :param path: Path of the document to open
        :param encoding: The encoding to use to open the file. Default is
            locale.getpreferredencoding().
        :param replace_tabs_by_spaces: Enable/Disable replace tabs by spaces.
            Default is true.
        :param clean_trailing_whitespaces: Enable/Disable clean trailing
            whitespaces (on save). Default is True.
        :param safe_save: If True, the file is saved to a temporary file first.
            If the save went fine, the temporary file is renamed to the final
            filename.
        :param restore_cursor_position: If true, last cursor position will be
            restored. Default is True.
        :param preferred_eol: Preferred EOL convention. This setting will be
            used for saving the document unless autodetect_eol is True.
        :param autodetect_eol: If true, automatically detects file EOL and
            use it instead of the preferred EOL when saving files.
        :param show_whitespaces: True to show white spaces.
        :param kwargs: addtional keyword args to pass to the widget
                       constructor.
        :return: The created code editor
        """
        original_path = os.path.normpath(path)
        path = os.path.normcase(original_path)
        paths = []
        widgets = []
        for w in self.widgets(include_clones=False):
            if os.path.exists(w.file.path):
                # skip new docs
                widgets.append(w)
                paths.append(os.path.normcase(w.file.path))
        if path in paths:
            i = paths.index(path)
            w = widgets[i]
            tw = w.parent_tab_widget
            tw.setCurrentIndex(tw.indexOf(w))
            return w
        else:
            assert os.path.exists(original_path)
            name = os.path.split(original_path)[1]

            use_parent_dir = False
            for tab in self.widgets():
                title = QtCore.QFileInfo(tab.file.path).fileName()
                if title == name:
                    tw = tab.parent_tab_widget
                    new_name = os.path.join(os.path.split(os.path.dirname(
                        tab.file.path))[1], title)
                    tw.setTabText(tw.indexOf(tab), new_name)
                    use_parent_dir = True

            if use_parent_dir:
                name = os.path.join(
                    os.path.split(os.path.dirname(path))[1], name)
                use_parent_dir = False

            tab = self._create_code_edit(self.guess_mimetype(path), **kwargs)
            self.editor_created.emit(tab)

            tab.open_parameters = {
                'encoding': encoding,
                'replace_tabs_by_spaces': replace_tabs_by_spaces,
                'clean_trailing_whitespaces': clean_trailing_whitespaces,
                'safe_save': safe_save,
                'restore_cursor_position': restore_cursor_position,
                'preferred_eol': preferred_eol,
                'autodetect_eol': autodetect_eol,
                'show_whitespaces': show_whitespaces,
                'kwargs': kwargs
            }
            tab.file.clean_trailing_whitespaces = clean_trailing_whitespaces
            tab.file.safe_save = safe_save
            tab.file.restore_cursor = restore_cursor_position
            tab.file.replace_tabs_by_spaces = replace_tabs_by_spaces
            tab.file.autodetect_eol = autodetect_eol
            tab.file.preferred_eol = preferred_eol
            tab.show_whitespaces = show_whitespaces
            try:
                tab.file.open(original_path, encoding=encoding)
            except Exception as e:
                _logger().exception('exception while opening file')
                tab.close()
                tab.setParent(None)
                tab.deleteLater()
                raise e
            else:
                tab.setDocumentTitle(name)
                tab.file._path = original_path
                icon = self._icon(path)
                self.add_tab(tab, title=name, icon=icon)
                self.document_opened.emit(tab)

                for action in self.closed_tabs_menu.actions():
                    if action.toolTip() == original_path:
                        self.closed_tabs_menu.removeAction(action)
                        break
                self.closed_tabs_history_btn.setEnabled(
                    len(self.closed_tabs_menu.actions()) > 0)

                return tab

    def close_document(self, path):
        """
        Closes a text document.
        :param path: Path of the document to close.
        """
        to_close = []
        for widget in self.widgets(include_clones=True):
            p = os.path.normpath(os.path.normcase(widget.file.path))
            path = os.path.normpath(os.path.normcase(path))
            if p == path:
                to_close.append(widget)
        for widget in to_close:
            tw = widget.parent_tab_widget
            tw.remove_tab(tw.indexOf(widget))

    def rename_document(self, old_path, new_path):
        """
        Renames an already opened document (this will not rename the file,
        just update the file path and tab title).

        Use that function to update a file that has been renamed externally.

        :param old_path: old path (path of the widget to rename with
            ``new_path``
        :param new_path: new path that will be used to rename the tab.
        """
        to_rename = []
        title = os.path.split(new_path)[1]
        for widget in self.widgets(include_clones=True):
            p = os.path.normpath(os.path.normcase(widget.file.path))
            old_path = os.path.normpath(os.path.normcase(old_path))
            if p == old_path:
                to_rename.append(widget)
        for widget in to_rename:
            tw = widget.parent_tab_widget
            widget.file._path = new_path
            tw.setTabText(tw.indexOf(widget), title)

    def closeEvent(self, event):
        """
        Saves dirty editors on close and cancel the event if the user choosed
        to continue to work.

        :param event: close event
        """
        dirty_widgets = []
        for w in self.widgets(include_clones=False):
            if w.dirty:
                dirty_widgets.append(w)
        filenames = []
        for w in dirty_widgets:
            if os.path.exists(w.file.path):
                filenames.append(w.file.path)
            else:
                filenames.append(w.documentTitle())
        if len(filenames) == 0:
            self.close_all()
            return
        dlg = DlgUnsavedFiles(self, files=filenames)
        if dlg.exec_() == dlg.Accepted:
            if not dlg.discarded:
                for item in dlg.listWidget.selectedItems():
                    filename = item.text()
                    widget = None
                    for widget in dirty_widgets:
                        if widget.file.path == filename or \
                                widget.documentTitle() == filename:
                            break
                    tw = widget.parent_tab_widget
                    tw.save_widget(widget)
                    tw.remove_tab(tw.indexOf(widget))
            self.close_all()
        else:
            event.ignore()

    def close_all(self):
        for w in self.widgets(include_clones=True):
            tw = w.parent_tab_widget
            tw.remove_tab(tw.indexOf(w))

    def _icon(self, path):
        provider = self.icon_provider_klass()
        if not os.path.exists(path):
            return provider.icon(provider.File)
        return provider.icon(QtCore.QFileInfo(path))

    def _on_current_changed(self, new):
        old, new = super(
            SplittableCodeEditTabWidget, self)._on_current_changed(new)
        if new:
            new.dirty_changed.connect(self.dirty_changed.emit)
        self.dirty_changed.emit(new.dirty)
        return old, new

    def split(self, widget, orientation):
        splitter = super(SplittableCodeEditTabWidget, self).split(
            widget, orientation)
        if splitter:
            splitter.tab_bar_double_clicked.connect(
                self.tab_bar_double_clicked.emit)

    def _on_tab_closed(self, tab):
        try:
            path = tab.file.path
            open_params = tab.open_parameters
        except AttributeError:
            pass
        else:
            for i, action in enumerate(self.closed_tabs_menu.actions()):
                if action.toolTip() == path:
                    # already in menu, just move it at the top
                    if i:
                        before = self.closed_tabs_menu.actions()[0]
                        self.closed_tabs_menu.removeAction(action)
                        self.closed_tabs_menu.insertAction(before, action)
                    break
            else:
                filename = QtCore.QFileInfo(path).fileName()
                try:
                    before = self.closed_tabs_menu.actions()[0]
                except IndexError:
                    action = self.closed_tabs_menu.addAction(
                        self._icon(path), filename)
                else:
                    action = QtWidgets.QAction(self._icon(path), filename,
                                               self.closed_tabs_menu)
                    self.closed_tabs_menu.insertAction(before, action)
                action.setToolTip(path)
                action.triggered.connect(self._open_closed_path)
                action.setData(open_params)
                self.closed_tabs_history_btn.setEnabled(True)
                nb_actions = len(self.closed_tabs_menu.actions())
                while nb_actions > self.CLOSED_TABS_HISTORY_LIMIT:
                    self.closed_tabs_menu.removeAction(
                        self.closed_tabs_menu.actions()[-1])
                    nb_actions = len(self.closed_tabs_menu.actions())

    def _open_closed_path(self):
        action = self.sender()
        path = action.toolTip()
        open_parameters = action.data()
        self.open_document(
            path, encoding=open_parameters['encoding'],
            replace_tabs_by_spaces=open_parameters['replace_tabs_by_spaces'],
            clean_trailing_whitespaces=open_parameters[
                'clean_trailing_whitespaces'],
            safe_save=open_parameters['safe_save'],
            restore_cursor_position=open_parameters['restore_cursor_position'],
            preferred_eol=open_parameters['preferred_eol'],
            autodetect_eol=open_parameters['autodetect_eol'],
            show_whitespaces=open_parameters['show_whitespaces'],
            **open_parameters['kwargs'])
        self.closed_tabs_menu.removeAction(action)
        self.closed_tabs_history_btn.setEnabled(
            len(self.closed_tabs_menu.actions()) > 0)
