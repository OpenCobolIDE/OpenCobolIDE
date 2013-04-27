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
This module contains the IDE application entry point.
"""
import os
import sys
import logging
import token

logging.basicConfig()
import subprocess

from PySide.QtGui import QApplication
from PySide.QtGui import QMessageBox

# allow launching the ide without installing
sys.path.insert(0, os.path.abspath(os.getcwd() + "/../"))
from cobcide import desktop_entry
from cobcide.settings import Settings
from cobcide.window import MainWindow


def windows_init():
    """
    Windows specific initialisation:

    - set env var to embedded OpenCobol variable
    - set PATH to cobol library path only (erase previous values)
    """
    cwd = os.getcwd()
    oc_root_pth = os.path.join(cwd, "OpenCobol")
    os.environ["COB_CONFIG_DIR"] = os.path.join(oc_root_pth, "config")
    os.environ["COB_COPY_DIR"] = os.path.join(oc_root_pth, "copy")
    os.environ["COB_LIBRARY_PATH"] = os.path.join(oc_root_pth, "bin")
    os.environ["COB_INCLUDE_PATH"] = os.path.join(oc_root_pth, "include")
    os.environ["COB_LIB_PATH"] = os.path.join(oc_root_pth, "lib")
    os.environ["PATH"] = os.environ["COB_LIBRARY_PATH"]


def cmd_exists(cmd):
    """
    Checks if a cmd exists (used to find the sudo tool)

    :param cmd: The cmd/binary to check
    :return:
    """
    p = subprocess.Popen(['whereis', '-b', cmd], stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE, stdin=subprocess.PIPE)
    output = p.stdout.read()
    return output != ("%s:\n" % cmd)


def get_sudo_tool():
    """
    Return an appropriate sudo tool or None
    """
    tools = ["gksudo", "kdesudo", "gnomesu", "kdesu"]
    for t in tools:
        if cmd_exists(t):
            return t
    return None


def linux_init():
    """
    GNU/Linux specific init: create a desktop entry for the app if the entry
    does not already exists.
    """
    settings = Settings()
    if settings.create_desktop_entry:
        if desktop_entry.check():
            tool = get_sudo_tool()
            if QMessageBox.question(
                    None, "Create desktop entry?",
                    "Would you like to create a desktop entry for "
                    "OpenCobolIDE?",
                    QMessageBox.Yes | QMessageBox.No,
                    QMessageBox.Yes) == QMessageBox.Yes:
                try:
                    print subprocess.Popen(
                        [tool, "python", desktop_entry.__file__],
                        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                        stdin=subprocess.PIPE).communicate()
                except:
                    QMessageBox.warning(None, "Failed to create desktop entry",
                                        "Could not create desktop entry.\n\nEnsure you have %s installed then restart "
                                        "the ide" % tool)
            else:
                # prevent mbox to appear again
                settings.create_desktop_entry = False
            if tool:
                pass


def main():
    """
    App main entry point when run as a script.

    Setup the Qt gui application, create the ide window and run the qt main
    loop.
    """
    app = QApplication(sys.argv)
    if sys.platform == "win32":
        windows_init()
    else:
        linux_init()
    win = MainWindow()
    win.showNormal()
    return app.exec_()


if __name__ == "__main__":
    sys.exit(main())
