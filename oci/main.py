#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# OpenCobolIDE
#
# Copyright 2013, Colin Duquesnoy <colin.duquesnoy@gmail.com>
#
# This software is released under the GPLv3 license.
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#
"""
This is module contains the application main function.
"""
import os
import sys
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
