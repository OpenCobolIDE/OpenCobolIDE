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
import logging
logging.basicConfig()
import os
import sys
sys.path.append(os.path.abspath(os.getcwd() + "/../"))
from PySide.QtGui import QApplication
from cobcide.window import MainWindow


def windows_init():
    """
    Windows specific initialisation:

    - set env var to embedded OpenCobol variable
    - remove any other mingw from path
    """
    cwd = os.getcwd()
    oc_root_pth = os.path.join(cwd, "OpenCobol")
    os.environ["COB_CONFIG_DIR"] = os.path.join(oc_root_pth, "config")
    os.environ["COB_COPY_DIR"] = os.path.join(oc_root_pth, "copy")
    os.environ["COB_LIBRARY_PATH"] = os.path.join(oc_root_pth, "bin")
    os.environ["COB_INCLUDE_PATH"] = os.path.join(oc_root_pth, "include")
    os.environ["COB_LIB_PATH"] = os.path.join(oc_root_pth, "lib")
    os.environ["PATH"] = os.environ["COB_LIBRARY_PATH"]


def main():
    """
    App main entry point when run as a script.

    Setup the Qt gui application, create the ide window and run the qt main
    loop.
    """
    if sys.platform == "win32":
        windows_init()
    app = QApplication(sys.argv)
    win = MainWindow()
    win.showNormal()
    return app.exec_()


if __name__ == "__main__":
    sys.exit(main())
