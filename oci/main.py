#!/usr/bin/env python
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
This module contains the application main function.
"""
import os
import sys
# setup environement to force pyqode to use PyQt4
os.environ['QT_API'] = 'PyQt'
from PyQt4 import QtGui
from oci.main_window import MainWindow
from oci.settings import Settings



def main():
    app = QtGui.QApplication(sys.argv)
    # open main window
    win = MainWindow()
    app.exec_()
    
    
if __name__ == "__main__":
    main()
