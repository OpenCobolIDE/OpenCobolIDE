# Copyright (c) <2013-2014> Colin Duquesnoy
#
# This file is part of OpenCobolIDE.
#
# OpenCobolIDE is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# OpenCobolIDE is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# OpenCobolIDE. If not, see http://www.gnu.org/licenses/.
"""
Gives an easy and safe access to the app settings
"""
import os
import sys
from pyqode.qt import QtCore, QtGui
from pyqode.qt.QtCore import QSettings


class Settings(object):

    def __init__(self):
        self._settings = QSettings("OpenCobolIDE", "OpenCobolIDE")

    def clear(self):
        self._settings.clear()

    # Geometry and state (visible windows, ...) + working settings (last path)
    # ------------------------------------------------------------------------
    @property
    def geometry(self):
        v = self._settings.value("mainWindowGeometry")
        if v:
            return bytes(v)
        return None

    @geometry.setter
    def geometry(self, geometry):
        self._settings.setValue("mainWindowGeometry", geometry)

    @property
    def state(self):
        v = self._settings.value("mainWindowState")
        if v:
            return bytes(v)
        return None

    @state.setter
    def state(self, state):
        self._settings.setValue("mainWindowState", state)

    @property
    def maximised(self):
        return bool(int(self._settings.value("maximised", "0")))

    @maximised.setter
    def maximised(self, value):
        self._settings.setValue("maximised", int(value))

    @property
    def fullscreen(self):
        return bool(int(self._settings.value("fullscreen", "0")))

    @fullscreen.setter
    def fullscreen(self, value):
        self._settings.setValue("fullscreen", int(value))

    @property
    def size(self):
        return self._settings.value("size", QtCore.QSize(1200, 800))

    @size.setter
    def size(self, value):
        self._settings.setValue("size", value)

    @property
    def navigationPanelVisible(self):
        return bool(int(self._settings.value("navigationPanelVisible", "1")))

    @navigationPanelVisible.setter
    def navigationPanelVisible(self, value):
        self._settings.setValue("navigationPanelVisible", int(value))

    @property
    def logPanelVisible(self):
        return bool(int(self._settings.value("logPanelVisible", "0")))

    @logPanelVisible.setter
    def logPanelVisible(self, value):
        self._settings.setValue("logPanelVisible", int(value))

    @property
    def lastFilePath(self):
        """
        Returns the last used open/save path
        """
        default_value = ""
        if sys.platform == "win32":
            default_value = "c:\\"
        return self._settings.value("lastUsedPath", default_value)

    @lastFilePath.setter
    def lastFilePath(self, path):
        """
        Sets the last used path (save or open path)

        :param path: path string
        :type path: str or unicode
        """
        self._settings.setValue("lastUsedPath", os.path.dirname(path))

    # Logging settings
    # ----------------
    @property
    def appLogVisible(self):
        return bool(int(self._settings.value('appLogVisible', '0')))

    @appLogVisible.setter
    def appLogVisible(self, value):
        self._settings.setValue('appLogVisible', int(value))

    @property
    def debugLog(self):
        return bool(int(self._settings.value('debugLog', "0")))

    @debugLog.setter
    def debugLog(self, value):
        self._settings.setValue('debugLog', int(value))

    # View settings
    # -------------
    @property
    def displayLineNumbers(self):
        return bool(int(self._settings.value('displayLineNumbers', '1')))

    @displayLineNumbers.setter
    def displayLineNumbers(self, value):
        self._settings.setValue('displayLineNumbers', int(value))

    @property
    def displayMargins(self):
        return bool(int(self._settings.value('displayMargins', '1')))

    @displayMargins.setter
    def displayMargins(self, value):
        self._settings.setValue('displayMargins', int(value))

    @property
    def displayMenuBar(self):
        return bool(int(self._settings.value('displayMenuBar', '0')))

    @displayMenuBar.setter
    def displayMenuBar(self, value):
        self._settings.setValue('displayMenuBar', int(value))

    @property
    def displayStatusBar(self):
        return bool(int(self._settings.value('displayStatusBar', '1')))

    @displayStatusBar.setter
    def displayStatusBar(self, value):
        self._settings.setValue('displayStatusBar', int(value))

    @property
    def displayToolBar(self):
        return bool(int(self._settings.value('displayToolBar', '0')))

    @displayToolBar.setter
    def displayToolBar(self, value):
        self._settings.setValue('displayToolBar', int(value))

    @property
    def displayControlPanel(self):
        return bool(int(self._settings.value('displayControlPanel', '1')))

    @displayControlPanel.setter
    def displayControlPanel(self, value):
        self._settings.setValue('displayControlPanel', int(value))

    @property
    def highlightCurrentLine(self):
        return bool(int(self._settings.value('highlightCurrentLine', '1')))

    @highlightCurrentLine.setter
    def highlightCurrentLine(self, value):
        self._settings.setValue('highlightCurrentLine', int(value))

    @property
    def highlightMatchingBraces(self):
        return bool(int(self._settings.value('highlightMatchingBraces', '1')))

    @highlightMatchingBraces.setter
    def highlightMatchingBraces(self, value):
        self._settings.setValue('highlightMatchingBraces', int(value))

    @property
    def highlightWhitespaces(self):
        return bool(int(self._settings.value('highlightWhitespaces', '0')))

    @highlightWhitespaces.setter
    def highlightWhitespaces(self, value):
        self._settings.setValue('highlightWhitespaces', int(value))

    @property
    def tabWidth(self):
        return int(self._settings.value('tabWidth', '4'))

    @tabWidth.setter
    def tabWidth(self, value):
        self._settings.setValue('tabWidth', int(value))

    # Editor settings
    # ---------------
    @property
    def enableAutoIndent(self):
        return bool(int(self._settings.value('enableAutoIndent', '1')))

    @enableAutoIndent.setter
    def enableAutoIndent(self, value):
        self._settings.setValue('enableAutoIndent', int(value))

    @property
    def saveOnFocusOut(self):
        return bool(int(self._settings.value('saveOnFocusOut', '1')))

    @saveOnFocusOut.setter
    def saveOnFocusOut(self, value):
        self._settings.setValue('saveOnFocusOut', int(value))

    @property
    def ccTriggerLen(self):
        return int(self._settings.value('ccTriggerLen', '1'))

    @ccTriggerLen.setter
    def ccTriggerLen(self, value):
        self._settings.setValue('ccTriggerLen', value)

    # Font & Color settings
    # ---------------------
    @property
    def consoleBackground(self):
        return QtGui.QColor(
            self._settings.value("consoleBackground", "#FFFFFF"))

    @consoleBackground.setter
    def consoleBackground(self, value):
        if isinstance(value, QtGui.QColor):
            value = value.name()
        self._settings.setValue("consoleBackground", value)

    @property
    def consoleForeground(self):
        return QtGui.QColor(
            self._settings.value("consoleForeground", "#404040"))

    @consoleForeground.setter
    def consoleForeground(self, value):
        if isinstance(value, QtGui.QColor):
            value = value.name()
        self._settings.setValue("consoleForeground", value)

    @property
    def consoleUserInput(self):
        return QtGui.QColor(self._settings.value("consoleUserInput",
                                                 "#22AA22"))

    @consoleUserInput.setter
    def consoleUserInput(self, value):
        if isinstance(value, QtGui.QColor):
            value = value.name()
        self._settings.setValue("consoleUserInput", value)

    @property
    def consoleAppOutput(self):
        return QtGui.QColor(self._settings.value("consoleAppOutput",
                                                 "#4040FF"))

    @consoleAppOutput.setter
    def consoleAppOutput(self, value):
        if isinstance(value, QtGui.QColor):
            value = value.name()
        self._settings.setValue("consoleAppOutput", value)

    @property
    def globalStyle(self):
        return self._settings.value("globalStyle", "white")

    @globalStyle.setter
    def globalStyle(self, value):
        self._settings.setValue("globalStyle", value)

    @property
    def fontName(self):
        font = self._settings.value("fontName")
        if not font:
            font = "monospace"
            if sys.platform == "win32":
                font = "Consolas"
            elif sys.platform == "darwin":
                font = 'Monaco'
        return font

    @fontName.setter
    def fontName(self, font):
        print(font)
        if not font:
            font = "monospace"
            if sys.platform == "win32":
                font = "Consolas"
            elif sys.platform == "darwin":
                font = 'Monaco'
        self._settings.setValue("fontName", font)

    @property
    def fontSize(self):
        return int(self._settings.value('fontSize', '10'))

    @fontSize.setter
    def fontSize(self, value):
        self._settings.setValue('fontSize', value)

    @property
    def colorScheme(self):
        return self._settings.value('colorScheme', 'default')

    @colorScheme.setter
    def colorScheme(self, value):
        self._settings.setValue('colorScheme', value)

    # Build and Run settings
    # ----------------------
    @property
    def runInShell(self):
        return bool(int(self._settings.value('runInShell', "0")))

    @runInShell.setter
    def runInShell(self, value):
        self._settings.setValue('runInShell', int(value))

    @property
    def shellCommand(self):
        # works on gnome, what about KDE and how do I detect that?
        # at the moment just go with gnome, user can change that in the
        # settings dialog anyway
        default_shell_cmd = "gnome-terminal -e"
        return str(self._settings.value("shell", default_shell_cmd))

    @shellCommand.setter
    def shellCommand(self, cmd):
        self._settings.setValue("shell", cmd)
