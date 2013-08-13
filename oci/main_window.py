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
import pyqode.core
from oci import __version__
from oci.ui import loadUi
from PyQt4 import QtCore, QtGui


class MainWindow(QtGui.QMainWindow):

    def __init__(self):
        QtGui.QMainWindow.__init__(self)
        loadUi("ide.ui", self, "ide.qrc")
        self.setupIcons()
        self.QHomeWidget.setupRecentFiles(
            organization="ColinDuquesnoy",
            menuRecentFiles=self.menuRecent_files,
            actionClearMnuRecentFiles=self.actionClear)
        self.setupQuickStartActions()
        self.setPage(home=True)
        self.tabWidgetEditors.lastTabClosed.connect(self.setPage)

    @QtCore.pyqtSlot()
    def on_actionNew_triggered(self):
        print("New")

    @QtCore.pyqtSlot()
    def on_actionOpen_triggered(self):
        # todo se souvenir du path
        fn = QtGui.QFileDialog.getOpenFileName(self, "Open a file", "")
        if fn:
            tab = pyqode.core.QGenericCodeEdit(self.tabWidgetEditors)
            tab.openFile(fn)
            self.tabWidgetEditors.addEditorTab(tab)
            self.setPage(False)
            self.QHomeWidget.setCurrentFile(fn)

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
        self.QHomeWidget.addAction(self.actionHelp)
        self.QHomeWidget.addAction(self.actionAbout)
        self.QHomeWidget.addAction(self.actionQuit)

    def showCentered(self):
        screenGeometry = QtGui.QApplication.desktop().screenGeometry()
        x = (screenGeometry.width() - self.width()) / 2
        y = (screenGeometry.height() - self.height()) / 2
        print(self.pos().x(), self.pos().y(), x, y)
        self.move(x, y)
        self.show()

    def setPage(self, home=True):
        if home:
            self.stackedWidget.setCurrentIndex(0)
            self.menuBar.hide()
            self.toolBarFile.hide()
            self.toolBarCode.hide()
            self.dockWidgetLogs.hide()
            self.dockWidgetNavPanel.hide()
            self.setMinimumWidth(800)
            self.setMaximumHeight(500)
            self.resize(800, 500)
            self.showNormal()
            self.statusBar().showMessage("OpenCobolIDE v.%s" % __version__)
        else:
            self.statusBar().clearMessage()
            self.menuBar.show()
            self.toolBarFile.show()
            self.toolBarCode.show()
            self.dockWidgetLogs.show()
            self.dockWidgetNavPanel.show()
            self.stackedWidget.setCurrentIndex(1)
            self.showMaximized()