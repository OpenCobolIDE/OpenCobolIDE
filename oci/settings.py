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
import pyqode.core
from pyqode.qt import QtCore, QtGui
from pyqode.qt.QtCore import QSettings


DEFAULT_EDITOR_SETTINGS = """{
    "General": {
        "leftMarginPos": "7",
        "rightMarginPos": "72",
        "showWhiteSpaces": "False",
        "tabLength": "4",
        "useSpacesInsteadOfTab": "True"
    },
    "codeCompletion": {
        "caseSensitive": "False",
        "showTooltips": "True",
        "triggerKey": "32",
        "triggerKeys": "[46]",
        "triggerLength": "1",
        "triggerSymbols": "[]"
    }
}
"""

DEFAULT_EDITOR_STYLE = """{
    "General": {
        "background": "#f8f8f8",
        "caretLineBackground": "#efefef",
        "foldIndicatorBackground": "#c1d1b0",
        "font": "monospace",
        "fontSize": "10",
        "foreground": "#000000",
        "margin": "#FF0000",
        "matchedBraceBackground": "#b4eeb4",
        "matchedBraceForeground": "#ff0000",
        "nativeFoldingIndicator": "True",
        "pygmentsStyle": "default",
        "searchOccurrenceBackground": "#ffff00",
        "searchOccurrenceForeground": "#000000",
        "selectionBackground": "#accd8a",
        "selectionForeground": "#ffffff",
        "whiteSpaceForeground": "#dddddd"
    }
}
"""

class Settings(object):

    def __init__(self):
        self._settings = QSettings("OpenCobolIDE", "OpenCobolIDE")

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
        return self._settings.value("size", QtCore.QSize(900, 700))

    @property
    def navigationPanelVisible(self):
        return bool(int(self._settings.value("navigationPanelVisible", "1")))

    @navigationPanelVisible.setter
    def navigationPanelVisible(self, value):
        return self._settings.setValue("navigationPanelVisible", int(value))

    @property
    def logPanelVisible(self):
        return bool(int(self._settings.value("logPanelVisible", "0")))

    @logPanelVisible.setter
    def logPanelVisible(self, value):
        return self._settings.setValue("logPanelVisible", int(value))

    @size.setter
    def size(self, value):
        self._settings.setValue("size", value)

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
        assert isinstance(path, str) or isinstance(path, unicode)
        self._settings.setValue("lastUsedPath", os.path.dirname(path))

    @property
    def show_line_numbers(self):
        return bool(int(self._settings.value("showLineNumbers", True)))

    @show_line_numbers.setter
    def show_line_numbers(self, state):
        self._settings.setValue("showLineNumbers", int(state))

    @property
    def show_whitespaces(self):
        return bool(int(self._settings.value("showWhitespaces", False)))

    @show_whitespaces.setter
    def show_whitespaces(self, state):
        self._settings.setValue("showWhitespaces", int(state))

    @property
    def enable_cc(self):
        return bool(int(self._settings.value("enableCC", True)))

    @enable_cc.setter
    def enable_cc(self, state):
        self._settings.setValue("enableCC", int(state))

    @property
    def useExternalShell(self):
        return bool(int(self._settings.value("useExternalShell", False)))

    @useExternalShell.setter
    def useExternalShell(self, state):
        self._settings.setValue("useExternalShell", int(state))

    @property
    def shellCommand(self):
        # works on gnome, what about KDE and how do I detect that?
        # at the moment just go with gnome, user can change that in the settings
        # dialog anyway
        default_shell_cmd = "gnome-terminal -e"
        return str(self._settings.value("shell", default_shell_cmd))

    @shellCommand.setter
    def shellCommand(self, cmd):
        self._settings.setValue("shell", cmd)

    @property
    def editorSettings(self):
        json_data = self._settings.value("editorSettings",
                                         DEFAULT_EDITOR_SETTINGS)
        r = pyqode.core.PropertyRegistry()
        r.load(json_data)
        return r

    @editorSettings.setter
    def editorSettings(self, propertyRegistry):
        self._settings.setValue("editorSettings", propertyRegistry.dump())

    @property
    def editorStyle(self):
        json_data = self._settings.value("editorStyle",
                                         DEFAULT_EDITOR_STYLE)
        r = pyqode.core.PropertyRegistry()
        r.load(json_data)
        return r

    @editorStyle.setter
    def editorStyle(self, propertyRegistry):
        self._settings.setValue("editorStyle", propertyRegistry.dump())

    @property
    def consoleBackground(self):
        return QtGui.QColor(self._settings.value("consoleBackground",
                                                 "#FFFFFF"))

    @consoleBackground.setter
    def consoleBackground(self, value):
        if isinstance(value, QtGui.QColor):
            value = value.name()
        self._settings.setValue("consoleBackground", value)

    @property
    def consoleForeground(self):
        return QtGui.QColor(self._settings.value("consoleForeground",
                                                 "#404040"))

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
    def homePageColorScheme(self):
        return int(self._settings.value("homePageStyle", "0"))

    @homePageColorScheme.setter
    def homePageColorScheme(self, value):
        self._settings.setValue("homePageStyle", str(value))

    @property
    def appStyle(self):
        return int(self._settings.value("style", "0"))

    @appStyle.setter
    def appStyle(self, value):
        self._settings.setValue("style", value)

    @property
    def runInExternalTerminal(self):
        return bool(int(self._settings.value('runInExternalTerminal', "0")))

    @runInExternalTerminal.setter
    def runInExternalTerminal(self, value):
        self._settings.setValue('runInExternalTerminal', int(value))

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
