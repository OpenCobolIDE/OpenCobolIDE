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
import logging
import os
import sys
import subprocess

import qdarkstyle
from pyqode.core import modes
from pyqode.core.api import utils as pyqode_utils, TextHelper
from pyqode.core.qt import QtCore, QtGui, QtWidgets
from pyqode.core.qt.QtWidgets import QTreeWidgetItem
from pyqode.core.qt.QtWidgets import QMessageBox

import oci
from oci import __version__, constants, utils
from oci.settings import Settings
from oci.backend import compiler
from oci.backend.pic_parser import PicFieldInfo
from oci.frontend import home, dialogs, editors, services, logger
from oci.frontend import compiler as compiler_frontend
from oci.frontend.ui import ide_ui


def _logger():
    return logging.getLogger(__name__)


class MainWindow(QtWidgets.QMainWindow, ide_ui.Ui_MainWindow):

    compilerMsgReady = QtCore.Signal(modes.CheckerMessage)
    compilationFinished = QtCore.Signal(bool)

    def __init__(self):
        super().__init__()
        self._run_request = None
        services._set_main_window(self)
        self.compilation_manager = compiler_frontend.CompilationManager()
        self.setupUi(self)
        self.applySettings()
        self.__prevRootNode = None
        logger.setup(self.textEditLogs)
        _logger().info('Environment: %r' % os.environ)
        if Settings().fullscreen:
            self.showFullScreen()
        else:
            self.show()
        self.init_compiler()

    @QtCore.Slot()
    def updateViewToolbarMenu(self):
        """
        Updates the View>Toolbars menu
        """
        v = self.toolBarFile.isVisible()
        self.aShowFilesToolbar.setChecked(v)
        v = self.toolBarCode.isVisible()
        self.aShowCodeToolbar.setChecked(v)

    @QtCore.Slot()
    def updateViewWindowMenu(self):
        """
        Updates the View>Windows menu
        """
        self.aShowLogsWin.setChecked(
            self.dockWidgetLogs.isVisible())
        self.aShowNavWin.setChecked(
            self.dockWidgetNavPanel.isVisible())

    @QtCore.Slot(object)
    def addCompilerMsg(self, message):
        self.errorsTable.add_message(message)
        self.tabWidgetLogs.setCurrentIndex(1)

    @QtCore.Slot(int)
    def onCompilerProcessError(self, error):
        from pyqode.core.api.client import PROCESS_ERROR_STRING
        QMessageBox.warning(self, 'Compiler process error',
                            'An error occured with the cobc process: %s' %
                            PROCESS_ERROR_STRING[error])

    @QtCore.Slot(bool)
    def onCompilationFinished(self, failure):
        _logger().info('compilation finished')
        self.compilerOutputEdit.setTextColor(self.palette().text().color())
        self.compilerOutputEdit.append('Compilation finished')
        self.actionCompile.setEnabled(True)
        self.onCurrentEditorChanged(self.tabWidgetEditors.currentIndex())
        self.errorsTable.setSortingEnabled(True)
        self.errorsTable.sortItems(0)
        if self._run_request:
            target, wd = self._run_request
            self._run_request = None
            if not failure:
                self.run_target(target, wd)

    @QtCore.Slot(QtWidgets.QAction)
    def on_programType_triggered(self, action):
        # self.tb.setText(action.text())
        editor = self.tabWidgetEditors.currentWidget()
        if action.text() == "Executable":
            editor.programType = constants.ProgramType.Executable
        else:
            editor.programType = constants.ProgramType.Module

    @QtCore.Slot(QtWidgets.QListWidgetItem)
    def on_listWidgetRecents_itemClicked(self):
        self.openFile(self.listWidgetRecents.currentItem().data(32))

    @QtCore.Slot()
    def on_btNewFile_clicked(self):
        self.on_actionNew_triggered()

    @QtCore.Slot()
    def on_actionNew_triggered(self):
        _logger().debug('action new triggered')
        dialogs.Dlg = dialogs.DlgNewFile(self)
        if dialogs.Dlg.exec_() == dialogs.DlgNewFile.Accepted:
            pth = dialogs.Dlg.path()
            content = dialogs.Dlg.template()
            with open(pth, 'wb') as f:
                if sys.version_info[0] == 3:
                    content = bytes(content, "utf-8")
                f.write(content)
            self.openFile(pth)

    @QtCore.Slot()
    def on_btOpenFile_clicked(self):
        self.on_actionOpen_triggered()

    @QtCore.Slot()
    def on_actionOpen_triggered(self):
        _logger().debug('action open triggered')
        filter = "%s%s%s" % (constants.COBOL_FILES_FILTER,
                            constants.FILTER_SEPARATOR,
                            constants.OTHER_FILES_FILTER)
        self.openFile(QtWidgets.QFileDialog.getOpenFileName(
            self, "Open a file", Settings().lastFilePath, filter)[0])

    @QtCore.Slot()
    def on_actionSave_triggered(self):
        self.tabWidgetEditors.save_current()

    @QtCore.Slot()
    def on_actionSaveAs_triggered(self):
        _logger().debug('action save as triggered')
        filter = "%s%s%s" % (constants.COBOL_FILES_FILTER,
                            constants.FILTER_SEPARATOR,
                            constants.OTHER_FILES_FILTER)
        fn, filter = QtWidgets.QFileDialog.getSaveFileName(
            self, "Save file as...", Settings().lastFilePath, filter)
        if os.path.splitext(fn)[1] == "":
            if filter == constants.COBOL_FILES_FILTER:
                fn += ".cob"
        if fn:
            _logger().debug('new path = %s' % fn)
            self.tabWidgetEditors.currentWidget().dirty = True
            self.tabWidgetEditors.save_current(path=fn)
            self.recent_files_manager.open_file(fn)
            self.menu_recents.update_actions()
            home.updateRecents()

    @QtCore.Slot()
    def on_actionQuit_triggered(self):
        if QtWidgets.QMessageBox.question(
                self, "Quit OpenCobolIDE?",
                "Are you sure you want to quit OpenCobolIDE?",
                QtWidgets.QMessageBox.Yes | QtWidgets.QMessageBox.No,
                QtWidgets.QMessageBox.No) == QtWidgets.QMessageBox.Yes:
            self.saveSettings()
            QtWidgets.QApplication.instance().aboutToQuit.emit()
            sys.exit(0)

    @QtCore.Slot()
    def on_actionCompile_triggered(self):
        _logger().debug('compile action triggered')
        self.tabWidgetEditors.save_all()
        self.errorsTable.clear()
        _logger().debug('error table cleared')
        self.errorsTable.setSortingEnabled(False)
        self.actionCompile.setEnabled(False)
        self.actionRun.setEnabled(False)
        self.dockWidgetLogs.show()
        self.tabWidgetLogs.setCurrentIndex(0)
        editor = self.tabWidgetEditors.currentWidget()
        self.compileCurrent(editor.file.path, editor.file.encoding,
                            editor.programType)

    @QtCore.Slot()
    def on_actionAbout_triggered(self):
        dialogs.Dlg = dialogs.DlgAbout(self)
        dialogs.Dlg.exec_()

    def run_target(self, target, wd):
        self.tabWidgetLogs.setCurrentIndex(2)
        self.dockWidgetLogs.show()
        self.consoleOutput.clear()
        if not Settings().runInShell:
            _logger().info('running %s in embedded terminal' % target)
            self.consoleOutput.setFocus(True)
            self.actionRun.setEnabled(False)
            self.consoleOutput.start_process(target, cwd=wd)
        else:
            _logger().info('running %s in external terminal' % target)
            self.consoleOutput.append("Launched in external terminal")
            if sys.platform == "win32":
                subprocess.Popen(
                    target, cwd=wd,
                    creationflags=subprocess.CREATE_NEW_CONSOLE)
            else:
                subprocess.Popen(Settings().shellCommand.split(' ') + [target],
                                 cwd=wd)

    @QtCore.Slot()
    def on_actionRun_triggered(self):
        source_fn = self.tabWidgetEditors.currentWidget().file.path
        source_fn = os.path.join(os.path.dirname(source_fn), "bin",
                                 os.path.basename(source_fn))
        target = compiler.makeOutputFilePath(
            source_fn, self.tabWidgetEditors.currentWidget().programType)
        wd = os.path.join(os.path.dirname(target))
        self._run_request = (target, wd)
        self.on_actionCompile_triggered()

    @QtCore.Slot()
    def on_actionPreferences_triggered(self):
        try:
            dialogs.DlgPreferences.editSettings(self)
        except dialogs.DlgRejectedError:
            pass
        else:
            self.applySettings()

    @QtCore.Slot()
    def on_actionHelp_triggered(self):
        QtGui.QDesktopServices.openUrl(
            QtCore.QUrl("http://opencobolide.readthedocs.org/en/latest/"))

    @QtCore.pyqtSlot()
    def on_actionShowAppLog_triggered(self):
        state = self.actionShowAppLog.isChecked()
        Settings().appLogVisible = state
        if state:
            self.tabWidgetLogs.addTab(
                self.tabAppLog,
                QtGui.QIcon(':/ide-icons/rc/silex-32x32.png'),
                'Application log')
            self.tabWidgetLogs.setCurrentIndex(3)
        else:
            self.tabWidgetLogs.removeTab(3)

    @QtCore.pyqtSlot()
    def on_actionDebug_level_triggered(self):
        state = self.actionDebug_level.isChecked()
        Settings().debugLog = state
        l = logging.getLogger(oci.__name__)
        l.setLevel(logging.DEBUG if state else logging.INFO)

    @QtCore.Slot(int)
    def onProgramFinished(self, status):
        _logger().info('program finished %s' % status)
        self.actionRun.setEnabled(True)

    @QtCore.Slot(object)
    def onCompilerMessageActivated(self, message):
        index = self.tabWidgetEditors.index_from_filename(message.path)
        if index == -1:
            self.openFile(message.path)
        else:
            self.tabWidgetEditors.setCurrentIndex(index)
        editor = self.tabWidgetEditors.currentWidget()
        TextHelper(editor).goto_line(message.line, move=True)
        self.tabWidgetEditors.currentWidget().setFocus()

    @QtCore.Slot()
    def on_actionFullscreen_triggered(self):
        if self.actionFullscreen.isChecked():
            self.showFullScreen()
        else:
            self.showNormal()

    @QtCore.Slot(QTreeWidgetItem, int)
    def on_twNavigation_itemActivated(self, item, column):
        """
        Moves the text cursor on the selected document node position

        :param item: oci.parser.DocumentNode
        """
        w = self.tabWidgetEditors.currentWidget()
        statement = item.data(0, QtCore.Qt.UserRole)
        TextHelper(w).goto_line(statement.line, move=True,
                                column=statement.column)
        self.tabWidgetEditors.currentWidget().setFocus()

    @QtCore.Slot(int)
    def onCurrentEditorChanged(self, index):
        w = self.tabWidgetEditors.widget(index)
        if w:
            try:
                if w.programType[0] == constants.ProgramType.Executable[0]:
                    # self.programActionGroup.triggered.emit(self.actionProgram)
                    self.actionProgram.setChecked(True)
                    self.actionRun.setEnabled(True)
                    self.actionCompile.setEnabled(True)
                else:
                    # self.programActionGroup.triggered.emit(self.actionSubprogram)
                    self.actionSubprogram.setChecked(True)
                    self.actionRun.setEnabled(False)
                    self.actionCompile.setEnabled(True)
                w.analyserMode.parse()
                rn = w.analyserMode.root_node
                if rn:
                    self.updateNavigationPanel(rn)
                self.updateStatusBar(w)
            except (AttributeError, TypeError):
                self.actionRun.setEnabled(False)
                self.actionCompile.setEnabled(False)
            self.menuEdit.clear()
            self.menuEdit.addActions(w.actions())
            self.menuEdit.addSeparator()
        else:
            self.showHomePage(True)
        self.menuEdit.addAction(self.actionPreferences)

    @QtCore.Slot(object)
    def displayPICInfos(self, infos):
        _logger().debug('display PIC infos requested')
        self.tableWidgetOffsets.clear()
        self.tableWidgetOffsets.setRowCount(len(infos))
        self.tableWidgetOffsets.setColumnCount(4)
        self.tableWidgetOffsets.setHorizontalHeaderLabels(
            ['Level', 'Name', 'Offset', 'PIC'])
        try:
            # PyQt4
            self.tableWidgetOffsets.horizontalHeader().setResizeMode(
                QtWidgets.QHeaderView.ResizeToContents)
            self.tableWidgetOffsets.horizontalHeader().setResizeMode(
                1, QtWidgets.QHeaderView.Stretch)
        except AttributeError:
            # PyQt5
            self.tableWidgetOffsets.horizontalHeader().setSectionResizeMode(
                QtWidgets.QHeaderView.ResizeToContents)
            self.tableWidgetOffsets.horizontalHeader().setSectionResizeMode(
                QtWidgets.QHeaderView.Stretch)
        # process each info in a separate row
        for i, info in enumerate(infos):
            assert isinstance(info, PicFieldInfo)
            self.tableWidgetOffsets.setItem(
                i, 0, QtWidgets.QTableWidgetItem("%s" % info.level))
            self.tableWidgetOffsets.setItem(
                i, 1, QtWidgets.QTableWidgetItem(info.name))
            self.tableWidgetOffsets.setItem(
                i, 2, QtWidgets.QTableWidgetItem("%s" % info.offset))
            self.tableWidgetOffsets.setItem(
                i, 3, QtWidgets.QTableWidgetItem(info.pic))
        self.dockWidgetOffsets.show()
        self.dockWidgetOffsets.adjustSize()

    def applySettings(self):
        if Settings().globalStyle == 'white':
            QtWidgets.QApplication.instance().setStyleSheet("")
            self.frameRecents.setStyleSheet(home.frame_recent_white)
            self.labelRecents.setStyleSheet(home.label_recent_white)
            self.listWidgetRecents.setStyleSheet(home.list_recent_white)
        else:
            self.frameRecents.setStyleSheet(home.frame_recent_dark)
            self.labelRecents.setStyleSheet(home.label_recent_dark)
            self.listWidgetRecents.setStyleSheet(home.list_recent_dark)
            if '5' in os.environ['QT_API']:
                QtWidgets.QApplication.instance().setStyleSheet(
                    qdarkstyle.load_stylesheet_pyqt5())
            else:
                QtWidgets.QApplication.instance().setStyleSheet(
                    qdarkstyle.load_stylesheet(pyside=False))
        self.statusbar.setVisible(Settings().displayStatusBar)
        for i in range(self.tabWidgetEditors.count()):
            self.tabWidgetEditors.widget(i).updateSettings()
        self.setupIcons()
        self.menuBar.setVisible(Settings().displayMenuBar)
        if self.stackedWidget.currentIndex():
            self.toolBarCode.setVisible(Settings().displayToolBar)
            self.toolBarFile.setVisible(Settings().displayToolBar)
        self.lblFormat.setText('Free format' if Settings().free_format else 'Fixed format')

    def updateNavigationPanel(self, rootNode):
        """
        Updates the navigation panel using the DocumentAnalyserMode infos.
        """
        if self.__prevRootNode != rootNode:
            _logger().debug('updating navigation panel')
            self.twNavigation.clear()
            tiRoot = utils.ast_to_qtree(rootNode)
            self.twNavigation.addTopLevelItem(tiRoot)
            self.twNavigation.expandItem(tiRoot)
            for i in range(tiRoot.childCount()):
                self.twNavigation.expandItem(tiRoot.child(i))
            self.__prevRootNode = rootNode

    def compileCurrent(self, file_path, file_encoding, programType):
        """
        Compiles the current file and its dependencies (
        executed in a background thread)
        """
        self.compilerOutputEdit.clear()
        self.compilerOutputEdit.setTextColor(self.palette().text().color())
        self.compilerOutputEdit.append('Compilation started')
        commands = compiler.get_all_commands(
            file_path, file_encoding, programType)
        self.compilation_manager.run_compilation_commands(commands)

    def updateStatusBar(self, editor=None):
        if editor is None:
            editor = self.tabWidgetEditors.currentWidget()
        self.lblEncoding.setText(editor.file.encoding)
        self.lblFilename.setText(editor.file.path)
        self.lblCursorPos.setText(
            "%d:%d" % TextHelper(editor).cursor_position())

    def saveSettings(self):
        s = Settings()
        s.geometry = self.saveGeometry()
        s.state = self.saveState()
        s.maximised = self.isMaximized()
        s.size = self.size()
        if self.stackedWidget.currentIndex() == 1:
            s.navigationPanelVisible = self.dockWidgetNavPanel.isVisible()
            s.logPanelVisible = self.dockWidgetLogs.isVisible()
            s.fullscreen = self.isFullScreen()

    def closeEvent(self, QCloseEvent):
        self.tabWidgetEditors.closeEvent(QCloseEvent)
        self.saveSettings()
        logger.close()

    def openFile(self, fn):
        if fn and self.tabWidgetEditors.index_from_filename(fn) == -1:
            _logger().info('opening file: %s' % fn)
            extension = os.path.splitext(fn)[1]
            Settings().lastFilePath = fn
            if extension.lower() in constants.ALL_COBOL_EXTENSIONS:
                tab = editors.make_cobol_editor()
            else:
                tab = editors.GenericCodeEdit(self.tabWidgetEditors)
            try:
                tab.file.open(fn)
            except IOError:
                QtWidgets.QMessageBox.warning(
                    self, "File does not exist",
                    "Cannot open file %s, the file does not exists." % fn)
            index = self.tabWidgetEditors.add_code_edit(tab)
            self.showHomePage(False)
            self.updateStatusBar(tab)
            tab.cursorPositionChanged.connect(self.updateStatusBar)
            self.recent_files_manager.open_file(fn)
            self.menu_recents.update_actions()
            home.updateRecents()

            self.onCurrentEditorChanged(index)

    def showHomePage(self, home=True):
        self.menuBar.setVisible(Settings().displayMenuBar)
        if home:
            _logger().debug('going to home page')
            if self.stackedWidget.currentIndex() == 1:
                s = Settings()
                s.navigationPanelVisible = self.dockWidgetNavPanel.isVisible()
                s.logPanelVisible = self.dockWidgetLogs.isVisible()
            self.consoleOutput.clear()
            self.stackedWidget.setCurrentIndex(0)
            self.toolBarFile.hide()
            self.toolBarCode.hide()
            self.dockWidgetLogs.hide()
            self.statusBar().setVisible(Settings().displayStatusBar)
            self.dockWidgetNavPanel.hide()
            self.dockWidgetOffsets.hide()
            self.lblEncoding.setText('')
            self.lblFilename.setText('OpenCobolIDE v.%s' % __version__)
            self.lblCursorPos.setText('')
        else:
            _logger().debug('going to editor page')
            if self.stackedWidget.currentIndex() == 0:
                self.stackedWidget.setCurrentIndex(1)
                self.dockWidgetNavPanel.show()
                s = Settings()
                self.statusBar().setVisible(Settings().displayStatusBar)
                self.dockWidgetNavPanel.setVisible(s.navigationPanelVisible)
                self.dockWidgetLogs.setVisible(s.logPanelVisible)
                if s.displayToolBar:
                    self.toolBarFile.setVisible(True)
                    self.toolBarCode.setVisible(True)

    def init_compiler(self):
        try:
            compiler.initialize()
        except compiler.InitializationFailed as e:
            QMessageBox.warning(
                self, 'Compiler configuration error',
                'Failed to find OpenCobol compiler.\n%s' % e)

    def setupStatusBar(self):
        self.lblFilename = QtWidgets.QLabel()
        self.lblEncoding = QtWidgets.QLabel()
        self.lblFormat = QtWidgets.QLabel()
        self.lblFormat.setAlignment(QtCore.Qt.AlignHCenter |
                                    QtCore.Qt.AlignVCenter)
        self.lblFormat.setText('Free format' if Settings().free_format else 'Fixed format')
        self.lblEncoding.setAlignment(QtCore.Qt.AlignHCenter |
                                      QtCore.Qt.AlignVCenter)
        self.lblEncoding.setText('')
        self.lblCursorPos = QtWidgets.QLabel()
        self.lblCursorPos.setAlignment(QtCore.Qt.AlignHCenter |
                                       QtCore.Qt.AlignVCenter)
        self.lblCursorPos.setText('')
        if not sys.platform == 'darwin':
            btMenu = QtWidgets.QToolButton(self)
            btMenu.setPopupMode(btMenu.MenuButtonPopup)
            mnu = QtWidgets.QMenu(self)
            mnu.addAction(self.actionNew)
            mnu.addAction(self.actionOpen)
            mnu.addMenu(self.menu_recents)
            mnu.addSeparator()
            mnu.addMenu(self.menuView)
            mnu.addSeparator()
            mnu.addAction(self.actionHelp)
            mnu.addAction(self.actionAbout)
            mnu.addSeparator()
            mnu.addAction(self.actionQuit)

            btMenu.setMenu(mnu)
            btMenu.setToolTip('Preferences')
            btMenu.clicked.connect(self.actionPreferences.trigger)
            btMenu.setIcon(QtGui.QIcon.fromTheme(
                "preferences-system",
                QtGui.QIcon(":/ide-icons/rc/Preferences-system.png")))
            self.statusbar.addPermanentWidget(btMenu)
        self.statusbar.addPermanentWidget(self.lblFilename, 200)
        self.statusbar.addPermanentWidget(self.lblFormat, 20)
        self.statusbar.addPermanentWidget(self.lblEncoding, 10)
        self.statusbar.addPermanentWidget(self.lblCursorPos, 5)
        self.stackedWidget.setCurrentIndex(0)

    def setupAppLog(self, s):
        if not s.appLogVisible:
            self.tabWidgetLogs.removeTab(3)
        self.tabWidgetLogs.setCurrentIndex(0)
        self.actionClearLog.triggered.connect(self.textEditLogs.clear)
        self.actionDebug_level.setChecked(s.debugLog)
        self.actionShowAppLog.setChecked(s.appLogVisible)

    def setupGeometryAndState(self, s):
        if s.geometry:
            self.restoreGeometry(s.geometry)
        if s.state:
            self.restoreState(s.state)
        self.dockWidgetNavPanel.setFloating(False)
        self.dockWidgetLogs.setFloating(False)
        self.wasMaximised = s.maximised
        self.prevSize = s.size
        self.actionFullscreen.setChecked(s.fullscreen)

    def setupConsoleOutput(self, s):
        self.consoleOutput.background_color = s.consoleBackground
        self.consoleOutput.stdout_color = s.consoleForeground
        self.consoleOutput.stderr_color = s.consoleForeground
        self.consoleOutput.stdin_color = s.consoleUserInput
        self.consoleOutput.app_msg_color = s.consoleAppOutput

    def setupStyle(self, s):
        if s.global_style == constants.DARK_STYLE:
            QtWidgets.QApplication.instance().setStyleSheet(
                qdarkstyle.load_stylesheet(pyside=False))
        self.setupIcons()

    def setupActions(self):
        self.tabWidgetEditors.last_tab_closed.connect(self.showHomePage)
        self.tabWidgetEditors.currentChanged.connect(
            self.onCurrentEditorChanged)
        self.tabWidgetEditors.dirty_changed.emit(False)
        self.setupToolbar()
        self.errorsTable.msg_activated.connect(
            self.onCompilerMessageActivated)
        self.jobRunner = pyqode_utils.DelayJobRunner(delay=0)
        self.compilerMsgReady.connect(self.addCompilerMsg)
        self.consoleOutput.process_finished.connect(self.onProgramFinished)
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
        # compilation
        self.compilation_manager.compilationFinished.connect(
            self.onCompilationFinished)
        self.compilation_manager.compilerMessageAvailable.connect(
            self.addCompilerMsg)
        self.compilation_manager.processError.connect(
            self.onCompilerProcessError)

    def setupIcons(self):

        docOpenIcon = QtGui.QIcon.fromTheme(
            "document-open", QtGui.QIcon(":/ide-icons/rc/document-open.png"))
        docSaveIcon = QtGui.QIcon.fromTheme(
            "document-save", QtGui.QIcon(":/ide-icons/rc/document-save.png"))
        docSaveAsIcon = QtGui.QIcon.fromTheme(
            "document-save-as", QtGui.QIcon(":/ide-icons/rc/document-save-as.png"))
        docNewIcon = QtGui.QIcon.fromTheme(
            "document-new",
            QtGui.QIcon(":/ide-icons/rc/document-new.png"))
        issueIcon = QtGui.QIcon.fromTheme('important', QtGui.QIcon(
            ':/ide-icons/rc/emblem-important.png'))
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
        aboutIcon = QtGui.QIcon.fromTheme(
            'info', QtGui.QIcon(':/ide-icons/rc/dialog-information.png'))
        if Settings().globalStyle != 'white':
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
            aboutIcon = QtGui.QIcon(':/ide-icons/rc/dialog-information.png')
        self.actionPreferences.setIcon(preferencesIcon)
        self.actionHelp.setIcon(helpIcon)
        self.actionClear.setIcon(clearIcon)
        self.actionQuit.setIcon(quitIcon)
        self.actionFullscreen.setIcon(fullscreenIcon)
        self.actionOpen.setIcon(docOpenIcon)
        self.btOpenFile.setIcon(docOpenIcon)
        self.actionNew.setIcon(docNewIcon)
        self.btNewFile.setIcon(docNewIcon)
        self.actionSave.setIcon(docSaveIcon)
        self.actionSaveAs.setIcon(docSaveAsIcon)
        self.actionRun.setIcon(runIcon)
        self.actionCompile.setIcon(compileIcon)
        self.actionAbout.setIcon(aboutIcon)
        self.tabWidgetLogs.setTabIcon(0, compileIcon)
        self.tabWidgetLogs.setTabIcon(1, issueIcon)
        self.tabWidgetLogs.setTabIcon(2, runIcon)

    def setupToolbar(self):
            """
            Setup the toolbar (adds a drop-down button for program types)
            """
            # create program type group
            ag = QtWidgets.QActionGroup(self)
            ag.addAction(self.actionProgram)
            ag.addAction(self.actionSubprogram)
            ag.triggered.connect(self.on_programType_triggered)

    def setupUi(self, MainWindow):
        s = Settings()
        super().setupUi(MainWindow)
        self.compilerOutputEdit = compiler_frontend.CompilerOutputEdit()
        self.tabWidgetLogs.insertTab(0, self.compilerOutputEdit,
                                     'Compilation output')
        self.compilation_manager.stdoutAvailable.connect(
            self.compilerOutputEdit.writeStdout)
        self.compilation_manager.stderrAvailable.connect(
            self.compilerOutputEdit.writeStderr)
        self.compilation_manager.cmdStarted.connect(
            self.compilerOutputEdit.writeCmd)
        self.toolBarCode.setIconSize(QtCore.QSize(22, 22))
        self.toolBarFile.setIconSize(QtCore.QSize(22, 22))
        self.actionPreferences.setShortcut('F2')
        self.addActions(self.menuBar.actions())
        home.setup_recent_files()
        self.setupAppLog(s)
        self.setupStatusBar()
        self.setupGeometryAndState(s)
        self.setupConsoleOutput(s)
        self.setupStyle(s)
        self.showHomePage(True)
        self.setupActions()