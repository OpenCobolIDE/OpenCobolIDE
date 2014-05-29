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
from pyqode.core import frontend
from pyqode.core.frontend import modes, widgets
from pyqode.core.frontend import utils as pyqode_utils
from pyqode.qt import QtCore, QtGui, QtWidgets
from pyqode.qt.QtWidgets import QTreeWidgetItem
from pyqode.qt.QtWidgets import QMessageBox

import oci
from oci import __version__, constants, logger, utils, settings
from oci.settings import Settings
from oci.backend import compiler
from oci.backend.parser import parse_dependencies
from oci.backend.pic_parser import PicFieldInfo
from oci.frontend.dialogs import DlgNewFile, DlgAbout, DlgPreferences, \
    DialogRejected
from oci.frontend.editors import CobolCodeEdit, GenericCodeEdit
from oci.frontend.ui import ide_ui


def _logger():
    return logging.getLogger(__name__)


# Homepage stylesheets
frame_recent_white = """
QFrame
{
border: 1px solid rgb(150, 150, 150);
border-radius: 5px;
background-color: rgb(255, 255, 255);
}
"""

list_recent_white = """
QListWidget
{
border: none;
background-color: transparent;
}
"""
label_recent_white = """border: none;
border-top-left-radius: 3px;
border-top-right-radius: 3px;
border-bottom-left-radius: 0px;
border-bottom-right-radius: 0px;
background-color: rgb(206, 206, 206);
padding: 5px;
border-bottom: 1px solid rgb(150, 150, 150);
"""

frame_recent_dark = """border: 1px solid rgb(80, 80, 80);
border-radius: 5px;
"""

list_recent_dark = """border: none;
background-color: transparent;
"""

label_recent_dark = """border: none;
border-top-left-radius: 3px;
border-top-right-radius: 3px;
border-bottom-left-radius: 0px;
border-bottom-right-radius: 0px;
background-color: rgb(64, 64, 64);
padding: 5px;
border-bottom: 1px solid rgb(80, 80, 80);
"""


class MainWindow(QtWidgets.QMainWindow, ide_ui.Ui_MainWindow):

    compilerMsgReady = QtCore.Signal(modes.CheckerMessage)
    compilationFinished = QtCore.Signal(bool)

    def __init__(self):
        super().__init__()
        self.setupUi(self)
        self.applySettings()
        logger.setup(self.textEditLogs)
        _logger().info('Environment: %r' % os.environ)
        if Settings().fullscreen:
            self.showFullScreen()
        else:
            self.show()
        compiler.init_env()
        if not compiler.check_env():
            if sys.platform == 'win32':
                msg = 'OpenCobol is bundled with the IDE. Ensure that ' \
                      'the IDE is installed in a path without spaces and ' \
                      'that the OpenCobol folder sits next to the executable.'
            else:
                msg = 'You have to install the package open-cobol to use the ' \
                      'IDE.'
            QMessageBox.warning(self, 'Compiler configuration error',
                                'Failed to find the OpenCobol compiler.\n%s' %
                                msg)

    def setupUi(self, MainWindow):
        super().setupUi(MainWindow)
        # Home page stylesheet
        self.frameRecents.setStyleSheet(frame_recent_white)
        self.labelRecents.setStyleSheet(label_recent_white)
        self.listWidgetRecents.setStyleSheet(list_recent_white)
        self.actionPreferences.setShortcut('F2')
        self.addActions(self.menuBar.actions())
        self.listWidgetRecents.clear_requested.connect(self._clear_recents)
        self.listWidgetRecents.remove_current_requested.connect(
            self._remove_current_recent_file)
        self.toolBarCode.setIconSize(QtCore.QSize(22, 22))
        self.toolBarFile.setIconSize(QtCore.QSize(22, 22))
        s = Settings()
        # application log
        if not s.appLogVisible:
            self.tabWidgetLogs.removeTab(2)
        self.tabWidgetLogs.setCurrentIndex(0)
        self.actionClearLog.triggered.connect(self.textEditLogs.clear)
        self.actionDebug_level.setChecked(s.debugLog)
        self.actionShowAppLog.setChecked(s.appLogVisible)

        # recent files menu + manager
        self.setup_recent_files_menu()

        # status bar
        self.lblFilename = QtWidgets.QLabel()
        self.lblEncoding = QtWidgets.QLabel()
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
        self.statusbar.addPermanentWidget(self.lblEncoding, 10)
        self.statusbar.addPermanentWidget(self.lblCursorPos, 1)
        self.stackedWidget.setCurrentIndex(0)
        self.__prevRootNode = None
        if s.geometry:
            self.restoreGeometry(s.geometry)
        if s.state:
            self.restoreState(s.state)
        self.dockWidgetNavPanel.setFloating(False)
        self.dockWidgetLogs.setFloating(False)
        self.wasMaximised = s.maximised
        self.prevSize = s.size
        self.actionFullscreen.setChecked(s.fullscreen)

        self.consoleOutput.background_color = s.consoleBackground
        self.consoleOutput.stdout_color = s.consoleForeground
        self.consoleOutput.stderr_color = s.consoleForeground
        self.consoleOutput.stdin_color = s.consoleUserInput
        self.consoleOutput.app_msg_color = s.consoleAppOutput

        if s.globalStyle == constants.DARK_STYLE:
            QtWidgets.QApplication.instance().setStyleSheet(
                qdarkstyle.load_stylesheet(pyside=False))

        self.setupIcons()
        self.showHomePage(True)
        self.tabWidgetEditors.last_tab_closed.connect(self.showHomePage)
        self.tabWidgetEditors.currentChanged.connect(
            self.onCurrentEditorChanged)
        self.tabWidgetEditors.dirty_changed.emit(False)
        self.setupToolbar()
        self.errorsTable.msg_activated.connect(
            self.onCompilerMessageActivated)
        self.jobRunner = pyqode_utils.DelayJobRunner(delay=0)
        self.compilerMsgReady.connect(self.addCompilerMsg)
        self.compilationFinished.connect(self.onCompilationFinished)
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

    def _clear_recents(self):
        self.recent_files_manager.clear()
        self.updateRecents()

    def _remove_current_recent_file(self):
        filename = self.listWidgetRecents.currentItem().data(32)
        files = self.recent_files_manager.get_recent_files()
        files.remove(filename)
        self.recent_files_manager.remove(filename)
        self.updateRecents()

    def setup_recent_files_menu(self):
        """ Setup the recent files menu and manager """
        self.recent_files_manager = widgets.RecentFilesManager(
            'OpenCobolIDE', 'OpenCobolIDE')
        self.menu_recents = widgets.MenuRecentFiles(
            self.menuFile, title='Recents',
            recent_files_manager=self.recent_files_manager)
        self.menu_recents.open_requested.connect(self.openFile)
        self.menu_recents.clear_requested.connect(self.listWidgetRecents.clear)
        self.menu_recents.clear()
        self.menuFile.insertMenu(self.actionQuit, self.menu_recents)
        self.menuFile.insertSeparator(self.actionQuit)
        self.updateRecents()

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
        self.errorsTable.add_message(message)

    def onCompilationFinished(self, status):
        _logger().info('compilation finished')
        self.actionCompile.setEnabled(True)
        self.onCurrentEditorChanged(self.tabWidgetEditors.currentIndex())
        self.errorsTable.setSortingEnabled(True)
        self.errorsTable.sortItems(0)

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
            #self.twNavigation.expandAll()
            #self.twNavigation.expandChildren()
            self.__prevRootNode = rootNode

    def setupToolbar(self):
        """
        Setup the toolbar (adds a drop-down button for program types)
        """
        # create program type group
        ag = QtWidgets.QActionGroup(self)
        ag.addAction(self.actionProgram)
        ag.addAction(self.actionSubprogram)
        ag.triggered.connect(self.on_programType_triggered)

    def on_programType_triggered(self, action):
        # self.tb.setText(action.text())
        editor = self.tabWidgetEditors.currentWidget()
        if action.text() == "Executable":
            editor.programType = constants.ProgramType.Executable
        else:
            editor.programType = constants.ProgramType.Module

    def on_listWidgetRecents_itemClicked(self):
        self.openFile(self.listWidgetRecents.currentItem().data(32))

    @QtCore.Slot()
    def on_btNewFile_clicked(self):
        self.on_actionNew_triggered()

    @QtCore.Slot()
    def on_actionNew_triggered(self):
        _logger().debug('action new triggered')
        dlg = DlgNewFile(self)
        if dlg.exec_() == DlgNewFile.Accepted:
            pth = dlg.path()
            content = dlg.template()
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
            self.updateRecents()

    def updateRecents(self):
        self.menu_recents.update_actions()
        self.listWidgetRecents.clear()
        for file in self.recent_files_manager.get_recent_files():
            item = QtWidgets.QListWidgetItem()
            if ('.' + QtCore.QFileInfo(file).suffix().upper() in
                constants.COBOL_EXTENSIONS):
                icon = QtGui.QIcon(":/ide-icons/rc/silex-32x32.png")
            else:
                icon = QtGui.QIcon(":/ide-icons/rc/text-x-generic.png")
            item.setText(QtCore.QFileInfo(file).fileName())
            item.setToolTip(file)
            item.setIcon(icon)
            item.setData(32, file)
            self.listWidgetRecents.addItem(item)

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
        _logger().info('compiling file: %s' % editor.file_path)
        self.jobRunner.request_job(self.compileCurrent,
                                   editor.file_path, editor.file_encoding,
                                   editor.programType)

    @QtCore.Slot()
    def on_actionAbout_triggered(self):
        dlg = DlgAbout(self)
        dlg.exec_()

    @QtCore.Slot()
    def on_actionRun_triggered(self):
        source_fn = self.tabWidgetEditors.currentWidget().file_path
        source_fn = os.path.join(os.path.dirname(source_fn), "bin",
                                 os.path.basename(source_fn))
        target = compiler.makeOutputFilePath(
            source_fn, self.tabWidgetEditors.currentWidget().programType)
        wd = os.path.join(os.path.dirname(target))
        if not Settings().runInShell:
            _logger().info('running %s in embedded terminal' % target)
            self.tabWidgetLogs.setCurrentIndex(1)
            self.dockWidgetLogs.show()
            self.consoleOutput.setFocus(True)
            self.actionRun.setEnabled(False)
            self.consoleOutput.start_process(target, cwd=wd)
        else:
            _logger().info('running %s in external terminal' % target)
            if sys.platform == "win32":
                subprocess.Popen(
                    target, cwd=wd,
                    creationflags=subprocess.CREATE_NEW_CONSOLE)
            else:
                subprocess.Popen(Settings().shellCommand.split(' ') + [target],
                                 cwd=wd)

    def applySettings(self):
        if Settings().globalStyle == 'white':
            QtWidgets.QApplication.instance().setStyleSheet("")
            self.frameRecents.setStyleSheet(frame_recent_white)
            self.labelRecents.setStyleSheet(label_recent_white)
            self.listWidgetRecents.setStyleSheet(list_recent_white)
        else:
            self.frameRecents.setStyleSheet(frame_recent_dark)
            self.labelRecents.setStyleSheet(label_recent_dark)
            self.listWidgetRecents.setStyleSheet(list_recent_dark)
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

    @QtCore.Slot()
    def on_actionPreferences_triggered(self):
        try:
            DlgPreferences.editSettings(self)
        except DialogRejected:
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
            self.tabWidgetLogs.setCurrentIndex(2)
        else:
            self.tabWidgetLogs.removeTab(2)

    @QtCore.pyqtSlot()
    def on_actionDebug_level_triggered(self):
        state = self.actionDebug_level.isChecked()
        Settings().debugLog = state
        l = logging.getLogger(oci.__name__)
        l.setLevel(logging.DEBUG if state else logging.INFO)

    def onProgramFinished(self, status):
        _logger().info('program finished')
        self.actionRun.setEnabled(True)

    def compileCurrent(self, file_path, file_encoding, programType):
        """
        Compiles the current file and its dependencies (executed in a background
        thread
        """
        dependencies = parse_dependencies(file_path, file_encoding)
        globalStatus = True
        for path, pgmType in dependencies:
            status, messags = compiler.compile(path, pgmType)
            globalStatus &= status
            for msg in messags:
                self.compilerMsgReady.emit(msg)
        status, messages = compiler.compile(file_path, programType)
        messages = [modes.CheckerMessage(*msg) for msg in messages]
        for msg in messages:
            self.compilerMsgReady.emit(msg)
        if status == 0:
            msg = modes.CheckerMessage("Compilation succeeded",
                                       modes.CheckerMessages.INFO, 1,
                                       icon=":/ide-icons/rc/accept.png",
                                       path=file_path)
            msg.filename = file_path
            self.compilerMsgReady.emit(msg)
        else:
            msg = modes.CheckerMessage(
                "Compilation failed", modes.CheckerMessages.ERROR, -1,
                path=file_path)
            self.compilerMsgReady.emit(msg)
        self.compilationFinished.emit(globalStatus)

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

    def updateStatusBar(self, editor=None):
        if editor is None:
            editor = self.tabWidgetEditors.currentWidget()
        self.lblEncoding.setText(editor.file_encoding)
        self.lblFilename.setText(editor.file_path)
        self.lblCursorPos.setText("%d:%d" % frontend.cursor_position(editor))

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

    def onCompilerMessageActivated(self, message):
        index = self.tabWidgetEditors.index_from_filename(message.path)
        if index == -1:
            self.openFile(message.path)
        else:
            self.tabWidgetEditors.setCurrentIndex(index)
        frontend.goto_line(self.tabWidgetEditors.currentWidget(),
                           message.line, move=True)
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
        frontend.goto_line(w, statement.line, move=True,
                           column=statement.column)
        self.tabWidgetEditors.currentWidget().setFocus()

    def openFile(self, fn):
        try:
            if fn and self.tabWidgetEditors.index_from_filename(fn) == -1:
                _logger().info('opening file: %s' % fn)
                extension = os.path.splitext(fn)[1]
                icon = None
                Settings().lastFilePath = fn
                if extension.lower() in constants.ALL_COBOL_EXTENSIONS:
                    tab = CobolCodeEdit(self.tabWidgetEditors)
                    icon = QtGui.QIcon(tab.icon)
                    tab.analyserMode.documentLayoutChanged.connect(
                        self.updateNavigationPanel)
                    tab.picInfosAvailable.connect(self.displayPICInfos)
                    tab.compilationRequested.connect(
                        self.on_actionCompile_triggered)
                    tab.runRequested.connect(
                        self.on_actionRun_triggered)
                    tab.pgmTypeChangeRequested.connect(
                        self.on_programType_triggered)
                else:
                    tab = GenericCodeEdit(self.tabWidgetEditors)
                tab.file_path = fn
                index = self.tabWidgetEditors.add_code_edit(tab, icon=icon)
                self.showHomePage(False)
                self.updateStatusBar(tab)
                tab.cursorPositionChanged.connect(self.updateStatusBar)
                self.recent_files_manager.open_file(fn)
                self.menu_recents.update_actions()
                self.updateRecents()
                tab.openFile(fn)
                self.onCurrentEditorChanged(index)
        except IOError:
            QtWidgets.QMessageBox.warning(
                self, "File does not exist",
                "Cannot open file %s, the file does not exists." % fn)

    def setupIcons(self):
        if Settings().globalStyle == 'white':
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
            aboutIcon = QtGui.QIcon.fromTheme(
                'info', QtGui.QIcon(':/ide-icons/rc/dialog-information.png'))
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
        self.tabWidgetLogs.setTabIcon(1, runIcon)

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

    def displayPICInfos(self, infos):
        _logger().debug('display PIC infos requested')
        self.tableWidgetOffsets.clear()
        self.tableWidgetOffsets.setRowCount(len(infos))
        self.tableWidgetOffsets.setHorizontalHeaderLabels(
            ['Level', 'Name', 'Offset', 'PIC'])
        try:
            self.tableWidgetOffsets.horizontalHeader().setResizeMode(
                QtWidgets.QHeaderView.ResizeToContents)
            self.tableWidgetOffsets.horizontalHeader().setResizeMode(
                1, QtWidgets.QHeaderView.Stretch)
        except AttributeError:
            self.tableWidgetOffsets.horizontalHeader().setSectionResizeMode(
                QtWidgets.QHeaderView.ResizeToContents)
            self.tableWidgetOffsets.horizontalHeader().setSectionResizeMode(
                1, QtWidgets.QHeaderView.Stretch)

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