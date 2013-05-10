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
import sys

from PySide.QtCore import QSettings

from pcef import style
from pcef import styles
from pcef.styles.white import WhiteStyle


class Settings(object):

    def __init__(self):
        self._settings = QSettings("CD", "cobcide")
        # self.initialized = False
        if not self.initialized:
            self.init_style_settings()
        try:
            styles.getStyle("cobcide")
        except KeyError:
            styles.addStyle(self.style)

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
        return bool(int(self._settings.value("maximised", False)))

    @maximised.setter
    def maximised(self, value):
        self._settings.setValue("maximised", int(value))

    @property
    def last_used_path(self):
        """
        Returns the last used open/save path
        """
        default_value = ""
        if sys.platform == "win32":
            default_value = "c:\\"
        return self._settings.value("lastUsedPath", default_value)

    @last_used_path.setter
    def last_used_path(self, path):
        """
        Sets the last used path (save or open path)

        :param path: path string
        :type path: str or unicode
        """
        assert isinstance(path, str) or isinstance(path, unicode)
        self._settings.setValue("lastUsedPath", path)

    @property
    def recent_files(self):
        """
        Returns the list of recent files
        :return: a list of strings
        """
        ret_val = self._settings.value('recentFileList', [])
        if ret_val is None:
            ret_val = []
        if isinstance(ret_val, unicode):
            ret_val = [ret_val]
        return ret_val

    @recent_files.setter
    def recent_files(self, files):
        """
        Save the list of rece,t files
        :param files: list of strings
        """
        self._settings.setValue('recentFileList', files)

    def clear_recent_files(self):
        """
        Clear the list of recent files
        :return:
        """
        self.recent_files = []

    @property
    def create_desktop_entry(self):
        """
        Returns the createDesktopEntry flag. True by default.

        :return: flag
        """
        return bool(int(self._settings.value("createDesktopEntry", True)))

    @create_desktop_entry.setter
    def create_desktop_entry(self, create):
        """
        Sets the desktop entry flag

        :param create: flag
        """
        self._settings.setValue("createDesktopEntry", int(create))

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
    def use_external_shell(self):
        return bool(int(self._settings.value("useExternalShell", False)))

    @use_external_shell.setter
    def use_external_shell(self, state):
        self._settings.setValue("useExternalShell", int(state))

    @property
    def shell_cmd(self):
        return str(self._settings.value("shell", "gnome-terminal -e"))

    @shell_cmd.setter
    def shell_cmd(self, cmd):
        self._settings.setValue("shell", cmd)

    @property
    def font_name(self):
        return str(self._settings.value("font", str(style.DEFAULT_FONT)))

    @font_name.setter
    def font_name(self, name):
        self._settings.setValue("font", str(name))

    @property
    def font_size(self):
        return int(self._settings.value("fontSize", style.DEFAULT_FONT_SIZE))

    @font_size.setter
    def font_size(self, size):
        self._settings.setValue("fontSize", int(size))

    @property
    def pygments_style(self):
        return str(self._settings.value("pygmentsStyle", "default"))

    @pygments_style.setter
    def pygments_style(self, style):
        self._settings.setValue("pygmentsStyle", str(style))

    def get_style_color(self, key):
        return str(self._settings.value("styleColor%s" % key, "#FFFFFF"))

    def set_style_color(self, key, color_str):
        self._settings.setValue("styleColor%s" % key, str(color_str))

    @property
    def initialized(self):
        return bool(self._settings.value("initialized", False))

    @initialized.setter
    def initialized(self, state):
        self._settings.setValue("initialized", state)

    def set_settings_from_style(
            self, new_style, show_line_nbrs=False, enable_cc=True):
        s = self
        s.enable_cc = enable_cc
        s.font_name = new_style.fontName
        s.font_size = new_style.fontSize
        s.pygments_style = new_style.pygmentsStyle
        s.show_whitespaces = new_style.showWhitespaces
        s.show_line_numbers = show_line_nbrs
        s.set_style_color("Margin", new_style.marginColor)
        s.set_style_color("ActiveLine", new_style.activeLineColor)
        s.set_style_color("Selection", new_style.selectionBackgroundColor)
        s.set_style_color("SelectedText", new_style.selectionTextColor)
        s.set_style_color("LineNumber", new_style.lineNbrColor)
        s.set_style_color("PanelBackground", new_style.panelsBackgroundColor)
        s.set_style_color("PanelBackground2", new_style.panelSeparatorColor)
        s.set_style_color("TextOccurences", new_style.searchColor)
        s.set_style_color("SearchResults", new_style.searchBackgroundColor)
        s.set_style_color("Warning", new_style.warningColor)
        s.set_style_color("Error", new_style.errorColor)

    def get_default_style(self):
        new_style = WhiteStyle()
        new_style.name = "cobcide"
        new_style.pygmentsStyle = "default"
        new_style.showWhitespaces = False
        enable_cc = True
        show_line_nbrs = True
        return enable_cc, new_style, show_line_nbrs

    def init_style_settings(self):
        enable_cc, new_style, show_line_nbrs = self.get_default_style()
        self.set_settings_from_style(new_style, enable_cc, show_line_nbrs)
        self.initialized = False

    @property
    def style(self):
        s = self.get_default_style()[1]
        assert isinstance(s, style.Style)
        s.fontName = self.font_name
        s.fontSize = self.font_size
        s.pygmentsStyle = self.pygments_style
        s.showWhitespaces = self.show_whitespaces
        s.marginColor = self.get_style_color("Margin")
        s.activeLineColor = self.get_style_color("ActiveLine")
        s.selectionBackgroundColor = self.get_style_color("Selection")
        s.selectionTextColor = self.get_style_color("SelectedText")
        s.lineNbrColor = self.get_style_color("LineNumber")
        s.panelsBackgroundColor = self.get_style_color(
            "PanelBackground")
        s.panelSeparatorColor = self.get_style_color(
            "PanelBackground2")
        s.searchColor = self.get_style_color("TextOccurences")
        s.searchBackgroundColor = self.get_style_color("SearchResults")
        s.warningColor = self.get_style_color("Warning")
        s.errorColor = self.get_style_color("Error")
        return s

