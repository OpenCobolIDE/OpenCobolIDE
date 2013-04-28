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
Contains a class that manage the errors lists and update the ui accordingly.

An error manager is attached to an open tab.
"""
from PySide.QtCore import QFileInfo
from PySide.QtGui import QColor
from PySide.QtGui import QIcon
from PySide.QtGui import QListWidgetItem

from pcef.code_edit import cursorForPosition
from pcef.code_edit import TextDecoration
from pcef.modes.checker import ERROR_TYPE_SYNTAX
from pcef.modes.checker import ERROR_TYPE_WARNING


class ErrorsManager(object):
    def __init__(self, listWidgetErrors, editor):
        """
        :param listWidgetErrors: QListWidget

        :param editor: CobolEditor
        """
        self.__listWidget = listWidgetErrors
        self.__editor = editor
        self.__checkerPanel = editor.checkerPanel
        self.__markers = []
        self.__decorations = []
        self.colors = {ERROR_TYPE_WARNING: "#FFFF00",
                       ERROR_TYPE_SYNTAX:  "#FF0000"}
        self.__cache_errors = []

    def __clear_decorations(self):
        """ Clear all decorations """
        for deco in self.__decorations:
            self.__editor.codeEdit.removeDecoration(deco)
        self.__decorations[:] = []

    def __clear_markers(self):
        """ Clear all markers """
        for marker in self.__markers:
            if self.__checkerPanel:
                try:
                    self.__checkerPanel.removeMarker(marker)
                except ValueError:
                    pass
        self.__markers[:] = []

    def updateErrors(self):
        """Update errors slot"""
        self.set_errors(self.__cache_errors)

    def clear_errors(self):
        self.__clear_markers()
        self.__clear_decorations()
        self.__cache_errors = []

    def set_errors(self, errors):
        """
        Sets the list of errors (decoration/markers)

        :param errors: The list of errors tuple (filename, type, line, msg str)
        """
        self.__cache_errors += errors
        # if not len(errors) and output_filename:
        #     icon = QIcon(":/ide-icons/rc/accept.png")
        #     item = QListWidgetItem(icon, "Compilation succeeded: %s" %
        #                                  output_filename)
        #     self.__listWidget.addItem(item)
        #     return
        current_cursor = self.__editor.codeEdit.textCursor()
        hbar_pos = self.__editor.codeEdit.horizontalScrollBar().sliderPosition()
        vbar_pos = self.__editor.codeEdit.verticalScrollBar().sliderPosition()
        for error in errors:
            filename = error[0]
            type = error[1]
            line = error[2]
            message = error[3]
            if type == "Success":
                # nothing to do in case of success
                return
            elif type == "Error":
                type = ERROR_TYPE_SYNTAX
            else:
                type = ERROR_TYPE_WARNING
            message = "{0} : {1}".format(QFileInfo(filename).fileName(),
                                         message)
            self.addError(type, line, message)
        # restore context
        self.__editor.codeEdit.setTextCursor(current_cursor)
        self.__editor.codeEdit.horizontalScrollBar().setSliderPosition(hbar_pos)
        self.__editor.codeEdit.verticalScrollBar().setSliderPosition(vbar_pos)

    def addError(self, error_type, line, message=None):
        """
        Add an error marker and decoration

        :param error_type: Type of error

        :param line: Error line

        :param message: Error message (tooltip)
        """
        assert error_type in self.colors and self.colors[error_type] is not None
        c = cursorForPosition(self.__editor.codeEdit, line, 1,
                              selectEndOfLine=True)
        deco = TextDecoration(c, draw_order=error_type + 1, tooltip=message)
        deco.setSpellchecking(color=QColor(self.colors[error_type]))
        self.__decorations.append(deco)
        self.__editor.codeEdit.addDecoration(deco)
        self.__markers.append(
            self.__checkerPanel.addCheckerMarker(error_type, line, message))
        icon = QIcon(self.__checkerPanel.icons[error_type])
        # item = QListWidgetItem(icon, "{0}: {1}".format(line, message))
        # self.__listWidget.addItem(item)
