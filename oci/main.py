# Copyright 2013 Colin Duquesnoy
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
This module contains the application main function.
"""
import logging
logging.basicConfig(level=logging.WARNING)
import os
import sys
# setup environement to force pyqode to use PyQt4
os.environ['QT_API'] = 'PyQt'
import pyqode.core
from PyQt4 import QtGui
from oci.main_window import MainWindow
from oci.settings import Settings


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


def main():
    try:
        import faulthandler
        faulthandler.enable()
    except ImportError:
        pass
    app = QtGui.QApplication(sys.argv)
    if sys.platform == "win32":
        windows_init()
    # open main window
    win = MainWindow()
    if Settings().fullscreen:
        win.showFullScreen()
    else:
        win.showCentered()
    app.exec_()
    
    
if __name__ == "__main__":
    main()
