#!/usr/bin/env python
# This file is part of cobcide.
# 
# cobcide is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# cobcide is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with cobcide.  If not, see <http://www.gnu.org/licenses/>.
"""
Contains classes and functions to manage the tab widget
"""
import pcef

from PySide.QtCore import QFileInfo, QObject, Signal
from PySide.QtGui import QTabWidget, QIcon, QWidget, QMessageBox

from pcef.editors.generic import GenericEditor

from cobcide import FileType
from cobcide.editor import CobolEditor


class TabManager(QObject):
    """
    Manages the IDE tab widget:
        - add tab
        - close tab
        - notify when active tab changed
        - ...
    """
    #: Signal emitted when the current tab has changed
    tabChanged = Signal(QWidget, str)
    #: Signal emitted when the text cursor of the active tab moved.
    cursorPosChanged = Signal(int, int)

    def __init__(self, tabWidget):
        """
        :param tabWidget: The controlled tab widget
        :type tabWidget: QTabWidget
        """
        QObject.__init__(self)
        self.__tabWidget = tabWidget
        self.__tabWidget.tabCloseRequested.connect(self.__close_tab)
        self.__tabWidget.currentChanged.connect(self.__on_current_changed)
        self.__current_index = -1

    def has_open_tabs(self):
        """
        Tells if there are any open tabs

        :return: True if at least one tab is open, else False
        """
        return self.__tabWidget.count() != 0

    @property
    def active_tab(self):
        """
        :rtype: pcef.core.CodeEditorWidget
        """
        return self.__tabWidget.currentWidget()

    @property
    def active_tab_type(self):
        """
        Return the active tab type (program, subprogram,...)

        :return: cobcide.FileType
        """
        return self.active_tab.fileType

    @property
    def active_tab_filename(self):
        """
        Retursn the active tab filename
        :return:
        """
        return self.active_tab.codeEdit.tagFilename

    @property
    def active_tab_file_dir(self):
        """
        Returns the pant directory of the file shown in the active tab
        """
        return QFileInfo(self.active_tab_filename).dir().path()

    @property
    def is_clean(self):
        """
        Checks if all open tabs are clean (not modified)

        :return: True if all open tabs are clean
        """
        ret_val = True
        for i in range(self.__tabWidget.count()):
            tab = self.__tabWidget.widget(i)
            if tab.codeEdit.dirty:
                ret_val = False
                break
        return ret_val

    def cleanup(self):
        """
        Try to perform cleanup, request a tab close on every open tab.

        :return: True if all tabs are closed.
        """
        count = self.__tabWidget.count()
        while count:
            self.__tabWidget.tabCloseRequested.emit(0)
            count -= 1
        return self.__tabWidget.count() == 0

    def open_tab(self, filepath, choice, encoding):
        """
        Open a new file tab

        :param filename: File path, supposed to be valid and already checked.
        """
        # create a new tab
        if choice == FileType.Program or \
                choice == FileType.Subprogram:
            icon = QIcon(":/ide-icons/rc/cobol-mimetype.png")
            tab = CobolEditor(self.__tabWidget)
            tab.fileType = choice
        else:
            icon = QIcon(":/ide-icons/rc/text-x-generic.png")
            tab = GenericEditor(self.__tabWidget)
            tab.fileType = choice
        pcef.openFileInEditor(tab, filepath, encoding=encoding)
        index = self.__tabWidget.addTab(
            tab, icon, QFileInfo(filepath).fileName())
        self.__tabWidget.setCurrentIndex(index)
        tab.setFocus()
        return tab

    def __close_tab(self, index):
        """
        Close a tab, only close if clean or user agree to close a dirty editor.

        :param index: The tab index to close
        """
        if index != -1:
            tab = self.__tabWidget.widget(index)
            if tab is not None and tab.codeEdit.dirty:
                filename = tab.codeEdit.tagFilename
                filename = QFileInfo(filename).fileName()
                if QMessageBox.warning(
                        self.__tabWidget, "Close document %s" %
                        filename,"Are you sure you want to close %s without "
                        "saving?" % filename, QMessageBox.Yes | QMessageBox.No,
                        QMessageBox.No) == QMessageBox.Yes:
                    tab.codeEdit.dirty = False
                    self.__tabWidget.removeTab(index)
            else:
                self.__tabWidget.removeTab(index)

    def __on_current_changed(self, index):
        """
        Slot called when the current tab changed. Emit the tab changed signal
        which is more convenient for the client code.

        :param index:The new tab index
        """
        txt = ""
        if self.__current_index != -1:
            tab = self.__tabWidget.widget(self.__current_index)
            if tab:
                try:
                    tab.codeEdit.dirtyChanged.disconnect(
                        self.__on_dirty_changed)
                    tab.codeEdit.cursorPositionChanged.disconnect(
                    self.__on_cursor_pos_changed)
                except RuntimeError:
                    pass
        tab = self.__tabWidget.widget(index)
        if tab:
            tab.codeEdit.dirtyChanged.connect(self.__on_dirty_changed)
            tab.codeEdit.cursorPositionChanged.connect(
                    self.__on_cursor_pos_changed)
            txt = self.__tabWidget.tabText(index)
            self.__current_index = index
        self.tabChanged.emit(tab, txt)

    def __on_dirty_changed(self, dirty):
        """
        Changes tab text when its status changed

        :param dirty: Dirty flag - bool
        """
        finfo = QFileInfo(self.active_tab.codeEdit.tagFilename)
        txt = finfo.fileName()
        if dirty:
            txt += "*"
        self.__tabWidget.setTabText(self.__current_index, txt)

    def get_cursor_pos(self):
        """
        Returns the active tab cursor position expressed in lines and columns.

        :return: A tuple of int (line, column)
        """
        if self.has_open_tabs():
            tc = self.active_tab.codeEdit.textCursor()
            l = tc.blockNumber() + 1
            c = tc.columnNumber() + 1
        else:
            l = 0
            c = 0
        return l, c

    def __on_cursor_pos_changed(self):
        """
        Slots called when the active tab's text cursor position changed.

        Emits a more convenient signal: cursorPosChanged(line, column)
        """
        l, c = self.get_cursor_pos()
        self.cursorPosChanged.emit(l, c)
