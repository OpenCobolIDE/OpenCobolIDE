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
from oci.ui import loadUi
from PyQt4 import QtGui
from pyqode.widgets import QHomeWidget


class MainWindow(QtGui.QMainWindow):

    def __init__(self):
        QtGui.QMainWindow.__init__(self)
        loadUi("ide.ui", self, "ide.qrc")
        # unable to set icon from them with a fallback in Qt Designer
        self.setupIcons()
        self.QHomeWidget.setupRecentFiles(organization="CD",
                           menuRecentFiles=self.menuRecent_files,
                           actionClearMnuRecentFiles=self.actionClear)
        self.setupQuickStartActions()
        self.setPage(home=True)

    def setupIcons(self):
        docOpenIcon = QtGui.QIcon.fromTheme(
            "document-open", QtGui.QIcon(":/ide-icons/rc/document-open.png"))
        docSaveIcon = QtGui.QIcon.fromTheme(
            "document-new", QtGui.QIcon(":/ide-icons/rc/document-new.png"))
        docSaveAsIcon = QtGui.QIcon.fromTheme(
            "document-save", QtGui.QIcon(":/ide-icons/rc/document-save.png"))
        docNewIcon = QtGui.QIcon.fromTheme(
            "document-save-as", QtGui.QIcon(":/ide-icons/rc/document-save-as.png"))
        compileIcon = QtGui.QIcon.fromTheme(
            "application-x-executable", QtGui.QIcon(
                ":/ide-icons/rc/application-x-executable.png"))
        runIcon = QtGui.QIcon.fromTheme(
            "media-playback-start", QtGui.QIcon(
                ":/ide-icons/rc/media-playback-start.png"))
        fullscreenIcon = QtGui.QIcon.fromTheme(
            "view-fullscreen", QtGui.QIcon(
                ":/ide-icons/rc/view-fullscreen.png"))
        quitIcon = QtGui.QIcon.fromTheme(
            "window-close", QtGui.QIcon(":/ide-icons/rc/system-log-out.png"))
        clearIcon = QtGui.QIcon.fromTheme(
            "edit-clear", QtGui.QIcon(":/ide-icons/rc/edit-clear.png"))
        helpIcon = QtGui.QIcon.fromTheme(
            "help", QtGui.QIcon(":/ide-icons/rc/help.png"))
        preferencesIcon = QtGui.QIcon.fromTheme(
            "preferences-system", QtGui.QIcon(":/ide-icons/rc/Preferences-system.png"))
        self.actionPreferences.setIcon(preferencesIcon)
        self.actionHelp.setIcon(helpIcon)
        self.actionClear.setIcon(clearIcon)
        self.actionQuit.setIcon(quitIcon)
        self.actionFullscreen.setIcon(fullscreenIcon)
        self.actionOpen.setIcon(docOpenIcon)
        self.actionNew.setIcon(docNewIcon)
        self.actionSave.setIcon(docSaveIcon)
        self.actionSaveAs.setIcon(docSaveAsIcon)
        self.actionRun.setIcon(runIcon)
        self.actionCompile.setIcon(compileIcon)
        self.tabWidgetLogs.setTabIcon(0, compileIcon)
        self.tabWidgetLogs.setTabIcon(1, runIcon)

    def setupQuickStartActions(self):
        self.QHomeWidget.addAction(self.actionNew)
        self.QHomeWidget.addAction(self.actionOpen)
        self.QHomeWidget.addAction(self.actionPreferences)
        self.QHomeWidget.addAction(self.actionQuit)

    def setPage(self, home=False):
        if home:
            self.stackedWidget.setCurrentIndex(0)
            self.menuBar.hide()
            self.toolBarFile.hide()
            self.toolBarCode.hide()
            self.dockWidgetLogs.hide()
            self.dockWidgetNavPanel.hide()