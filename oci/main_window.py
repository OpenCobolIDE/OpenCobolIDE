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
Contains the main window implementation
"""
from oci.ui.ide_ui import Ui_MainWindow
from PyQt4 import QtGui
from pyqode.widgets import QHomeWidget


class MainWindow(QtGui.QMainWindow, Ui_MainWindow):
    def __init__(self):
        QtGui.QMainWindow.__init__(self)
        Ui_MainWindow.__init__(self)
        self.setupUi(self)

        # unable to set icon from them with a fallback in Qt Designer
        self.tabWidgetLogs.setTabIcon(0, QtGui.QIcon.fromTheme(
            "applications-system",
            QtGui.QIcon(":/ide-icons/rc/applications-system.png")))
        self.tabWidgetLogs.setTabIcon(1, QtGui.QIcon.fromTheme(
            "media-playback-start",
            QtGui.QIcon(":/ide-icons/rc/media-playback-start.png")))
        self.QHomeWidget.setupRecentFiles(organization="CD",
                           menuRecentFiles=self.menuRecent_files,
                           actionClearMnuRecentFiles=self.actionClear)
        self.QHomeWidget.addAction(self.actionNew)
        self.QHomeWidget.addAction(self.actionOpen)
        self.QHomeWidget.addAction(self.actionAbout)
        self.QHomeWidget.addAction(self.actionQuit)
