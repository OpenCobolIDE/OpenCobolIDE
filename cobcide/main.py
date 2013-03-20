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
sys.path.append(os.path.abspath(os.getcwd() + "/../"))
from PySide.QtGui import QApplication
import cobcide
from cobcide.window import MainWindow


def main():
    """
    App main entry point when run as a script.

    Setup the Qt gui application, create the ide window and run the qt main
    loop.
    """
    print cobcide.__version__
    app = QApplication(sys.argv)
    # app.setStyleSheet(qdarkstyle.load_stylesheet())
    win = MainWindow()
    win.showNormal()
    return app.exec_()


if __name__ == "__main__":
    sys.exit(main())
