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
Contains the main window implementation
"""
import os
import sys

import qdarkstyle

import pyqode.core
import pyqode.widgets

from PyQt4 import QtCore, QtGui
from PyQt4.QtGui import QToolButton, QActionGroup, QListWidgetItem, QTreeWidgetItem, QInputDialog

from oci import __version__, constants, cobol
from oci.dialogs import DlgNewFile, DlgAbout, DlgPreferences
from oci.editor import QCobolCodeEdit
from oci import settings
from oci.settings import Settings
from oci.ui import ide_ui


class MainWindow(QtGui.QMainWindow, ide_ui.Ui_MainWindow):

    compilerMsgReady = QtCore.pyqtSignal(pyqode.core.CheckerMessage)
    compilationFinished = QtCore.pyqtSignal(bool)

    def __init__(self):
        QtGui.QMainWindow.__init__(self)
        ide_ui.Ui_MainWindow.__init__(self)
        self.setupUi(self)

        # status bar
        self.lblFilename = QtGui.QLabel()
        self.lblEncoding = QtGui.QLabel()
        self.lblCursorPos = QtGui.QLabel()
        self.statusbar.addPermanentWidget(self.lblFilename, 200)
        self.statusbar.addPermanentWidget(self.lblEncoding, 20)
        self.statusbar.addPermanentWidget(self.lblCursorPos, 20)

        self.initDefaultSettings()
        self.stackedWidget.setCurrentIndex(0)
        self.__prevRootNode = None
        s = Settings()
        if s.geometry:
            self.restoreGeometry(s.geometry)
        if s.state:
            self.restoreState(s.state)
        self.dockWidgetNavPanel.setFloating(False)
        self.dockWidgetLogs.setFloating(False)
        self.wasMaximised = s.maximised
        self.prevSize = s.size
        self.actionFullscreen.setChecked(s.fullscreen)

        self.consoleOutput.backgroundColor = s.consoleBackground
        self.consoleOutput.processOutputColor = s.consoleForeground
        self.consoleOutput.usrInputColor = s.consoleUserInput
        self.consoleOutput.appMessageColor = s.consoleAppOutput

        self.setHomePageColorScheme(s.homePageColorScheme)

        if s.appStyle == constants.DARK_STYLE:
            QtGui.QApplication.instance().setStyleSheet(
                qdarkstyle.load_stylesheet(pyside=False))

        self.setupIcons()
        self.QHomeWidget.setupRecentFiles(
            organization="OpenCobolIDE",
            app="OpenCobolIDE",
            menuRecentFiles=self.menuRecent_files,
            actionClearMnuRecentFiles=self.actionClear)
        self.QHomeWidget.fileOpenRequested.connect(self.openFile)
        self.setupQuickStartActions()
        self.showHomePage(True)
        self.tabWidgetEditors.lastTabClosed.connect(self.showHomePage)
        self.tabWidgetEditors.dirtyChanged.connect(
            self.actionSave.setEnabled)
        self.tabWidgetEditors.currentChanged.connect(
            self.onCurrentEditorChanged)
        self.tabWidgetEditors.dirtyChanged.emit(False)
        self.setupToolbar()
        self.errorsTable.messageActivated.connect(
            self.onCompilerMessageActivated)
        self.jobRunner = pyqode.core.JobRunner(self, nbThreadsMax=1)
        self.compilerMsgReady.connect(self.addCompilerMsg)
        self.compilationFinished.connect(self.onCompilationFinished)

        # View actions
        # toolbars
        self.toolBarFile.visibilityChanged.connect(
            self.updateViewToolbarMenu)
        self.toolBarCode.visibilityChanged.connect(
            self.updateViewToolbarMenu)
        self.aShowCodeToolbar.toggled.connect(
            self.toolBarCode.setVisible)
        self.aShowFilesToolbar.toggled.connect(
            self.toolBarFile.setVisible)
        # dock windows
        self.dockWidgetLogs.visibilityChanged.connect(
            self.updateViewWindowMenu)
        self.dockWidgetNavPanel.visibilityChanged.connect(
            self.updateViewWindowMenu)
        self.aShowNavWin.toggled.connect(
            self.dockWidgetNavPanel.setVisible)
        self.aShowLogsWin.toggled.connect(
            self.dockWidgetLogs.setVisible)

    @staticmethod
    def initDefaultSettings():
        e = QCobolCodeEdit()
        settings.DEFAULT_EDITOR_STYLE = e.style.dump()
        settings.DEFAULT_EDITOR_SETTINGS = e.settings.dump()
        del e

    def updateViewToolbarMenu(self):
        """
        Updates the View>Toolbars menu
        """
        v = self.toolBarFile.isVisible()
        self.aShowFilesToolbar.setChecked(v)
        v = self.toolBarCode.isVisible()
        self.aShowCodeToolbar.setChecked(v)

    def updateViewWindowMenu(self):
        """
        Updates the View>Windows menu
        """
        self.aShowLogsWin.setChecked(
            self.dockWidgetLogs.isVisible())
        self.aShowNavWin.setChecked(
            self.dockWidgetNavPanel.isVisible())

    def addCompilerMsg(self, message):
        self.errorsTable.addMessage(message)

    def onCompilationFinished(self, status):
        self.actionCompile.setEnabled(True)
        self.onCurrentEditorChanged(self.tabWidgetEditors.currentIndex())
        self.errorsTable.setSortingEnabled(True)
        self.errorsTable.sortItems(0)

    def updateNavigationPanel(self, rootNode):
        """
        Updates the navigation panel using the DocumentAnalyserMode infos.
        """
        if self.__prevRootNode != rootNode:
            self.twNavigation.clear()
            #rootNode.setExpanded(True)
            self.twNavigation.addTopLevelItem(rootNode)
            self.twNavigation.expandItem(rootNode)
            for i in range(rootNode.childCount()):
                self.twNavigation.expandItem(rootNode.child(i))
            #self.twNavigation.expandAll()
            #self.twNavigation.expandChildren()
            self.__prevRootNode = rootNode

    def setupToolbar(self):
        """
        Setup the toolbar (adds a drop-down button for program types)
        """
        # create program type group
        ag = QActionGroup(self)
        ag.addAction(self.actionProgram)
        ag.addAction(self.actionSubprogram)
        ag.triggered.connect(self.on_programType_triggered)
        self.programActionGroup = ag
        self.tb = QToolButton()
        self.tb.setToolTip("Select the cobol program type to make")
        self.tb.setMenu(self.menuProgramType)
        self.tb.setPopupMode(QToolButton.InstantPopup)
        self.tb.setText("Executable")
        self.toolBarCode.insertWidget(self.actionCompile, self.tb)
        self.toolBarCode.insertSeparator(self.actionCompile)

    def on_programType_triggered(self, action):
        self.tb.setText(action.text())
        editor = self.tabWidgetEditors.currentWidget()
        if action.text() == "Executable":
            editor.programType = constants.ProgramType.Executable
        else:
            editor.programType = constants.ProgramType.Module

    def setHomePageColorScheme(self, schemeNbr):
        schemes = [pyqode.widgets.ColorScheme, constants.DarkColorScheme]
        self.QHomeWidget.setColorScheme(schemes[schemeNbr]())

    @QtCore.pyqtSlot()
    def on_actionNew_triggered(self):
        dlg = DlgNewFile(self)
        if dlg.exec_() == DlgNewFile.Accepted:
            pth = dlg.path()
            content = dlg.template()
            with open(pth, 'wb') as f:
                if sys.version_info[0] == 3:
                    content = bytes(content, "utf-8")
                f.write(content)
            self.openFile(pth)

    @QtCore.pyqtSlot()
    def on_actionOpen_triggered(self):
        filter = "%s%s%s" % (constants.COBOL_FILES_FILTER,
                            constants.FILTER_SEPARATOR,
                            constants.OTHER_FILES_FILTER)
        self.openFile(QtGui.QFileDialog.getOpenFileName(
            self, "Open a file", Settings().lastFilePath, filter))

    @QtCore.pyqtSlot()
    def on_actionSave_triggered(self):
        self.tabWidgetEditors.saveCurrent()

    @QtCore.pyqtSlot()
    def on_actionSaveAs_triggered(self):
        filter = "%s%s%s" % (constants.COBOL_FILES_FILTER,
                            constants.FILTER_SEPARATOR,
                            constants.OTHER_FILES_FILTER)
        fn, filter = QtGui.QFileDialog.getSaveFileNameAndFilter(
            self, "Save file as...", Settings().lastFilePath, filter)
        if os.path.splitext(fn)[1] == "":
            if filter == constants.COBOL_FILES_FILTER:
                fn += ".cob"
        if fn:
            self.tabWidgetEditors.currentEditor.dirty = True
            self.tabWidgetEditors.saveCurrent(filePath=fn)
            self.QHomeWidget.setCurrentFile(fn)

    @QtCore.pyqtSlot()
    def on_actionQuit_triggered(self):
        if QtGui.QMessageBox.question(
                self, "Quit OpenCobolIDE?",
                "Are you sure you want to quit OpenCobolIDE?",
                QtGui.QMessageBox.Yes | QtGui.QMessageBox.No,
                QtGui.QMessageBox.No) == QtGui.QMessageBox.Yes:
            self.saveSettings()
            QtGui.QApplication.instance().aboutToQuit.emit()
            sys.exit(0)

    @QtCore.pyqtSlot()
    def on_actionCompile_triggered(self):
        self.tabWidgetEditors.saveAll()
        self.errorsTable.clear()
        self.errorsTable.setSortingEnabled(False)
        self.actionCompile.setEnabled(False)
        self.actionRun.setEnabled(False)
        self.dockWidgetLogs.show()
        self.tabWidgetLogs.setCurrentIndex(0)
        editor = self.tabWidgetEditors.currentWidget()
        self.jobRunner.startJob(self.compileCurrent, False,
                                editor.filePath, editor.programType)

    @QtCore.pyqtSlot()
    def on_actionAbout_triggered(self):
        dlg = DlgAbout(self)
        dlg.exec_()

    @QtCore.pyqtSlot()
    def on_actionRun_triggered(self):
        self.tabWidgetLogs.setCurrentIndex(1)
        self.dockWidgetLogs.show()
        self.consoleOutput.setFocus(True)
        source_fn = self.tabWidgetEditors.currentWidget().filePath
        source_fn = os.path.join(os.path.dirname(source_fn), "bin",
                                 os.path.basename(source_fn))
        target = cobol.makeOutputFilePath(
            source_fn, self.tabWidgetEditors.currentWidget().programType)
        cwd = os.path.join(os.path.dirname(target))
        self.actionRun.setEnabled(False)
        self.consoleOutput.processFinished.connect(self.onProgramFinished)
        self.consoleOutput.runProcess(target, cwd=cwd)

    @QtCore.pyqtSlot()
    def on_actionPreferences_triggered(self):
        s = Settings()
        dlg = DlgPreferences(self)
        dlg.homePageColorScheme = s.homePageColorScheme
        dlg.editorSettings = s.editorSettings
        dlg.editorStyle = s.editorStyle
        dlg.consoleBackground = s.consoleBackground
        dlg.consoleUserInput = s.consoleUserInput
        dlg.consoleForeground = s.consoleForeground
        dlg.consoleAppOutput = s.consoleAppOutput
        dlg.console.runProcess("python")
        if dlg.exec_() == dlg.Accepted:
            s.editorSettings = dlg.editorSettings
            s.editorStyle = dlg.editorStyle
            s.consoleBackground = dlg.consoleBackground
            s.consoleForeground = dlg.consoleForeground
            s.consoleUserInput = dlg.consoleUserInput
            s.consoleAppOutput = dlg.consoleAppOutput
            s.homePageColorScheme = dlg.homePageColorScheme
            self.setHomePageColorScheme(s.homePageColorScheme)
            self.tabWidgetEditors.resetSettings(dlg.editorSettings)
            self.tabWidgetEditors.resetStyle(dlg.editorStyle)
            self.tabWidgetEditors.refreshIcons(useTheme=dlg.rbLightStyle.isChecked())
            self.consoleOutput.backgroundColor = dlg.consoleBackground
            self.consoleOutput.processOutputColor = dlg.consoleForeground
            self.consoleOutput.usrInputColor = dlg.consoleUserInput
            self.consoleOutput.appMessageColor = dlg.consoleAppOutput
            self.setupIcons()

    @QtCore.pyqtSlot()
    def on_actionHelp_triggered(self):
        QtGui.QDesktopServices.openUrl(
            QtCore.QUrl("http://opencobolide.readthedocs.org/en/latest/"))

    def onProgramFinished(self, status):
        self.actionRun.setEnabled(True)

    def compileCurrent(self, filePath, programType):
        """
        Compiles the current file and its dependencies (executed in a background
        thread
        """
        dependencies = cobol.parseDependencies(filePath)
        globalStatus = True
        for path, pgmType in dependencies:
            status, messags = cobol.compile(path, pgmType)
            globalStatus &= status
            for msg in messags:
                self.compilerMsgReady.emit(msg)
            if status == 0:
                msg = pyqode.core.CheckerMessage(
                        "Compilation succeeded", pyqode.core.MSG_STATUS_INFO, -1,
                        icon=":/ide-icons/rc/accept.png", filename=path)
                msg.filename =path
            else:
                msg = pyqode.core.CheckerMessage(
                    "Compilation failed", pyqode.core.MSG_STATUS_ERROR, -1, filename=path)
            msg.filename = path
            self.compilerMsgReady.emit(msg)
        status, messages = cobol.compile(filePath, programType)
        for msg in messages:
            self.compilerMsgReady.emit(msg)
        if status == 0:
            msg = pyqode.core.CheckerMessage(
                "Compilation succeeded", pyqode.core.MSG_STATUS_INFO, -1,
                icon=":/ide-icons/rc/accept.png", filename=filePath)
        else:
            msg = pyqode.core.CheckerMessage(
                "Compilation failed", pyqode.core.MSG_STATUS_ERROR, -1,
                filename=filePath)
        msg.filename = filePath
        self.compilerMsgReady.emit(msg)
        self.compilationFinished.emit(globalStatus)

    def onCurrentEditorChanged(self, index):
        w = self.tabWidgetEditors.widget(index)
        if w:
            try:
                if w.programType[0] == constants.ProgramType.Executable[0]:
                    self.programActionGroup.triggered.emit(self.actionProgram)
                    self.actionProgram.setChecked(True)
                    self.actionRun.setEnabled(True)
                    self.actionCompile.setEnabled(True)
                else:
                    self.programActionGroup.triggered.emit(self.actionSubprogram)
                    self.actionSubprogram.setChecked(True)
                    self.actionRun.setEnabled(False)
                    self.actionCompile.setEnabled(True)
                w.analyserMode.parse()
                rn = w.analyserMode.root_node
                if rn:
                    self.updateNavigationPanel(rn)
                self.tb.setEnabled(True)
                self.updateStatusBar(w)
            except AttributeError:
                self.tb.setEnabled(False)
                self.actionRun.setEnabled(False)
                self.actionCompile.setEnabled(False)
            self.menuEdit.clear()
            self.menuEdit.addActions(w.actions())
            self.menuEdit.addSeparator()
        else:
            self.showHomePage(True)
        self.menuEdit.addAction(self.actionPreferences)

    def updateStatusBar(self, editor=None):
        if editor is None:
            editor = self.tabWidgetEditors.currentWidget()
        self.lblEncoding.setText(editor.fileEncoding)
        self.lblFilename.setText(editor.filePath)
        self.lblCursorPos.setText("%d:%d" % editor.cursorPosition)

    def saveSettings(self):
        if self.stackedWidget.currentIndex() == 1:
            s = Settings()
            s.geometry = self.saveGeometry()
            s.state = self.saveState()
            s.maximised = self.isMaximized()
            s.size = self.size()
            s.navigationPanelVisible = self.dockWidgetNavPanel.isVisible()
            s.logPanelVisible = self.dockWidgetLogs.isVisible()
            s.fullscreen = self.isFullScreen()

    def closeEvent(self, QCloseEvent):
        self.tabWidgetEditors.closeEvent(QCloseEvent)
        self.saveSettings()

    def onCompilerMessageActivated(self, message):
        index = self.tabWidgetEditors.isFileOpen(message.filename)
        if index == -1:
            self.openFile(message.filename)
        else:
            self.tabWidgetEditors.setCurrentIndex(index)
        self.tabWidgetEditors.currentWidget().gotoLine(message.line, move=True)

    @QtCore.pyqtSlot()
    def on_actionFullscreen_triggered(self):
        if self.actionFullscreen.isChecked():
            self.showFullScreen()
        else:
            self.showNormal()

    @QtCore.pyqtSlot(QTreeWidgetItem, int)
    def on_twNavigation_itemActivated(self, item, column):
        """
        Moves the text cursor on the selected document node position

        :param item: oci.cobol.DocumentNode
        """
        w = self.tabWidgetEditors.currentWidget()
        w.gotoLine(item.line, move=True, column=item.column)

    def openFile(self, fn):
        try:
            if fn:
                extension = os.path.splitext(fn)[1]
                icon = None
                Settings().lastFilePath = fn
                if extension.lower() in constants.ALL_COBOL_EXTENSIONS:
                    tab = QCobolCodeEdit(self.tabWidgetEditors)
                    icon = QtGui.QIcon(tab.icon)
                    tab.analyserMode.documentLayoutChanged.connect(
                        self.updateNavigationPanel)
                    tab.settings = Settings().editorSettings
                    tab.settings.setValue("triggerKeys", [],
                                          section="Code completion")
                    tab.style = Settings().editorStyle
                else:
                    tab = pyqode.core.QGenericCodeEdit(self.tabWidgetEditors)
                    tab.settings = Settings().editorSettings
                    tab.settings.setValue("minIndentColumn", 0)
                    tab.style = Settings().editorStyle
                tab.openFile(fn, detectEncoding=True)
                tab.refreshIcons(useTheme=Settings().appStyle == constants.WHITE_STYLE)
                self.tabWidgetEditors.addEditorTab(tab, icon=icon)
                self.showHomePage(False)
                self.QHomeWidget.setCurrentFile(fn)
                self.updateStatusBar(tab)
                tab.cursorPositionChanged.connect(self.updateStatusBar)
        except IOError:
            QtGui.QMessageBox.warning(
                self, "File does not exist",
                "Cannot open file %s, the file does not exists." % fn)

    def setupIcons(self):
        if Settings().appStyle == constants.WHITE_STYLE:
            docOpenIcon = QtGui.QIcon.fromTheme(
                "document-open", QtGui.QIcon(":/ide-icons/rc/document-open.png"))
            docSaveIcon = QtGui.QIcon.fromTheme(
                "document-save", QtGui.QIcon(":/ide-icons/rc/document-save.png"))
            docSaveAsIcon = QtGui.QIcon.fromTheme(
                "document-save-as", QtGui.QIcon(":/ide-icons/rc/document-save-as.png"))
            docNewIcon = QtGui.QIcon.fromTheme(
                "document-new",
                QtGui.QIcon(":/ide-icons/rc/document-new.png"))
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
                "preferences-system",
                QtGui.QIcon(":/ide-icons/rc/Preferences-system.png"))
        else:
            docOpenIcon = QtGui.QIcon(":/ide-icons/rc/document-open.png")
            docSaveIcon = QtGui.QIcon(":/ide-icons/rc/document-save.png")
            docSaveAsIcon = QtGui.QIcon(":/ide-icons/rc/document-save-as.png")
            docNewIcon = QtGui.QIcon(":/ide-icons/rc/document-new.png")
            compileIcon = QtGui.QIcon(
                ":/ide-icons/rc/application-x-executable.png")
            runIcon = QtGui.QIcon(
                ":/ide-icons/rc/media-playback-start.png")
            fullscreenIcon = QtGui.QIcon(
                ":/ide-icons/rc/view-fullscreen.png")
            quitIcon = QtGui.QIcon(":/ide-icons/rc/system-log-out.png")
            clearIcon = QtGui.QIcon(":/ide-icons/rc/edit-clear.png")
            helpIcon = QtGui.QIcon(":/ide-icons/rc/help.png")
            preferencesIcon = QtGui.QIcon(
                ":/ide-icons/rc/Preferences-system.png")
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

        self.QHomeWidget.setActionIcon(self.actionNew, docNewIcon)
        self.QHomeWidget.setActionIcon(self.actionOpen, docOpenIcon)
        self.QHomeWidget.setActionIcon(self.actionPreferences, preferencesIcon)
        self.QHomeWidget.setActionIcon(self.actionHelp, helpIcon)
        self.QHomeWidget.setActionIcon(self.actionQuit, quitIcon)

    def setupQuickStartActions(self):
        self.QHomeWidget.addAction(self.actionNew)
        self.QHomeWidget.addAction(self.actionOpen)
        self.QHomeWidget.addAction(self.actionPreferences)
        self.QHomeWidget.addAction(self.actionHelp)
        self.QHomeWidget.addAction(self.actionAbout)
        self.QHomeWidget.addAction(self.actionQuit)
        self.addAction(self.actionFullscreen)

    def showCentered(self):
        screenGeometry = QtGui.QApplication.desktop().screenGeometry()
        x = (screenGeometry.width() - self.width()) / 2
        y = (screenGeometry.height() - self.height()) / 2
        self.move(x, y)
        self.showNormal()

    def showHomePage(self, home=True):
        if home:
            if self.stackedWidget.currentIndex() == 1:
                self.prevSize = self.size()
                self.wasMaximised = self.isMaximized()
                s = Settings()
                s.navigationPanelVisible = self.dockWidgetNavPanel.isVisible()
                s.logPanelVisible = self.dockWidgetLogs.isVisible()
                self.setMinimumWidth(700)
                self.setMinimumHeight(400)
                self.resize(700, 400)
                if not self.isFullScreen():
                    self.showCentered()
            self.consoleOutput.clear()
            self.setMinimumWidth(700)
            self.setMinimumHeight(400)
            self.resize(700, 400)
            self.stackedWidget.setCurrentIndex(0)
            self.menuBar.hide()
            self.toolBarFile.hide()
            self.toolBarCode.hide()
            self.dockWidgetLogs.hide()
            self.dockWidgetNavPanel.hide()
            self.lblEncoding.setText("")
            self.lblFilename.setText("OpenCobolIDE v.%s" % __version__)
            self.lblCursorPos.setText("")
        else:
            if self.stackedWidget.currentIndex() == 0:
                self.stackedWidget.setCurrentIndex(1)
                self.menuBar.show()
                self.toolBarFile.show()
                self.toolBarCode.show()
                self.dockWidgetNavPanel.show()
                self.setMinimumWidth(900)
                self.setMinimumHeight(700)
                if not self.isFullScreen():
                    if self.wasMaximised:
                        self.showMaximized()
                    else:
                        if self.prevSize.width() < 900:
                            self.prevSize.setWidth(900)
                        if self.prevSize.height() < 700:
                            self.prevSize.setHeight(700)
                        self.resize(self.prevSize)
                        self.showCentered()
                s = Settings()
                self.dockWidgetNavPanel.setVisible(s.navigationPanelVisible)
                self.dockWidgetLogs.setVisible(s.logPanelVisible)
