# This file is part of OCIDE.
# 
# OCIDE is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# OCIDE is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with OCIDE.  If not, see <http://www.gnu.org/licenses/>.
"""
Gives an easy and safe access to the app settings
"""
import sys

from PySide.QtCore import QSettings



class Settings(object):

    def __init__(self):
        self.__settings = QSettings("CD", "cobcide")

    @property
    def last_used_path(self):
        """
        Returns the last used open/save path
        """
        default_value = ""
        if sys.platform == "win32":
            default_value = "c:\\"
        return self.__settings.value("lastUsedPath", default_value)

    @last_used_path.setter
    def last_used_path(self, path):
        """
        Sets the last used path (save or open path)

        :param path: path string
        :type path: str or unicode
        """
        assert isinstance(path, str) or isinstance(path, unicode)
        self.__settings.setValue("lastUsedPath", path)

    @property
    def recent_files(self):
        """
        Returns the list of recent files
        :return: a list of strings
        """
        ret_val = self.__settings.value('recentFileList', [])
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
        self.__settings.setValue('recentFileList', files)

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
        return bool(self.__settings.value("createDesktopEntry", True))

    @create_desktop_entry.setter
    def create_desktop_entry(self, create):
        """
        Sets the desktop entry flag

        :param create: flag
        """
        self.__settings.setValue("createDesktopEntry", bool(create))
