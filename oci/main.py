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
import logging
logging.basicConfig(level=logging.INFO)
import sys
from pyqode.core.qt import QtWidgets
from oci.utils import init_env
from oci.frontend.main_window import MainWindow


def main():
    app = QtWidgets.QApplication(sys.argv)
    init_env()
    win = MainWindow()
    app.exec_()


if __name__ == "__main__":
    main()
