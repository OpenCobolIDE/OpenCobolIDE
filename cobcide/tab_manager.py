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
from PySide.QtCore import QFileInfo, QObject, Signal
import pcef
from pcef.editors.generic import GenericEditor
from PySide.QtGui import QTabWidget, QIcon, QWidget
from cobcide import FileType
from cobcide.tabs import CobolEditor


class TabManager(QObject):
    """
    Manages the IDE tab widget:
        - add tab
        - close tab
        - notify when active tab changed
        - ...
    """
    tabChanged = Signal(QWidget, str)

    def __init__(self, tabWidget):
        """
        :param tabWidget: The controlled tab widget
        :type tabWidget: QTabWidget
        """
        QObject.__init__(self)
        self.__tabWidget = tabWidget
        self.__tabWidget.tabCloseRequested.connect(self.__close_tab)
        self.__tabWidget.currentChanged.connect(self.__on_current_changed)

    def has_open_tabs(self):
        return self.__tabWidget.count() != 0

    @property
    def active_tab(self):
        return self.__tabWidget.currentWidget()

    @property
    def active_tab_type(self):
        return self.active_tab.fileType

    @property
    def active_tab_filename(self):
        return self.active_tab.codeEdit.tagFilename

    def open_tab(self, filepath, choice):
        """
        Open a new file tab

        :param filename: File path, supposed to be valid and already checked.
        """
        # create a new tab
        if choice == FileType.Program or \
                choice == FileType.Subprogram:
            tab = CobolEditor()
            tab.fileType = choice
            pcef.openFileInEditor(tab, filepath)
            icon = QIcon(":/ide-icons/rc/cobol-mimetype.png")
        else:
            tab = GenericEditor()
            icon = QIcon(":/ide-icons/rc/text-x-generic.png")
            tab.fileType = choice
        pcef.openFileInEditor(tab, filepath)
        index = self.__tabWidget.addTab(tab, icon,
                                         QFileInfo(filepath).fileName())
        self.__tabWidget.setCurrentIndex(index)
        tab.setFocus()
        return tab

    def __close_tab(self, index):
        if index != -1:
            self.__tabWidget.removeTab(index)

    def __on_current_changed(self, index):
        tab = self.__tabWidget.widget(index)
        txt = self.__tabWidget.tabText(index)
        self.tabChanged.emit(tab, txt)
