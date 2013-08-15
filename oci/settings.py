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
Gives an easy and safe access to the app settings
"""
import os
import sys

from PySide.QtCore import QSettings

from pcef import style
from pcef import styles
from pcef.styles.white import WhiteStyle


class Settings(object):

    def __init__(self):
        self._settings = QSettings("CD", "OpenCobolIDE")

    @property
    def geometry(self):
        return self._settings.value("mainWindowGeometry")

    @geometry.setter
    def geometry(self, geometry):
        self._settings.setValue("mainWindowGeometry", geometry)

    @property
    def state(self):
        return self._settings.value("mainWindowState")

    @state.setter
    def state(self, state):
        self._settings.setValue("mainWindowState", state)

    @property
    def maximised(self):
        return bool(int(self._settings.value("maximised", 0)))

    @maximised.setter
    def maximised(self, value):
        self._settings.setValue("maximised", int(value))

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
        return str(self._settings.value("shell", "gnome-terminal -e"))

    @shellCommand.setter
    def shellCommand(self, cmd):
        self._settings.setValue("shell", cmd)
