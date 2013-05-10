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
Contains the IDE main window.
"""
import chardet
import os
import sys

from PySide.QtCore import QFileInfo, Qt
from PySide.QtCore import QThreadPool
from PySide.QtCore import Slot
from PySide.QtGui import QAction, QIcon
from PySide.QtGui import QActionGroup
from PySide.QtGui import QFileDialog
from PySide.QtGui import QLabel
from PySide.QtGui import QListWidgetItem
from PySide.QtGui import QMainWindow
from PySide.QtGui import QMessageBox
from PySide.QtGui import QTreeWidgetItem
from PySide.QtGui import QWidget

from pcef import saveFileFromEditor
from pcef.code_edit import cursorForPosition

from cobcide import FileType
from cobcide import cobol
from cobcide.dialogs import DlgAbout
from cobcide.dialogs import DlgFileType
from cobcide.dialogs import DlgPreferences
from cobcide.editor import CobolEditor
from cobcide.errors_manager import ErrorsManager
from cobcide.settings import Settings
from cobcide.tab_manager import TabManager
from cobcide.ui import ide_ui


class MainWindow(QMainWindow):
    """
    The IDE main window
    """
    #: Home page index
    PAGE_HOME = 0
    #: The editor page index
    PAGE_EDITOR = 1

    def __init__(self):
        QMainWindow.__init__(self)
        # Create our thread pool (used to launch compilation and run command)
        self.__threadPool = QThreadPool()
        self.__threadPool.setMaxThreadCount(1)

        # setup ui
        self.__ui = ide_ui.Ui_MainWindow()
        self.__ui.setupUi(self)
        s = Settings()
        self.restoreGeometry(s.geometry)
        self.restoreState(s.state)
        print s.maximised
        if s.maximised:
            self.setWindowState(Qt.WindowMaximized)
        self.__ui.listWidgetErrors.itemDoubleClicked.connect(
            self.__on_error_double_clicked)
        self.__ui.mnuActiveEditor.setEnabled(False)

        # setup home page
        self.__ui.wHomePage.set_internal_data(self.__ui.menuRecent_files,
                                              self.__ui.actionClear)

        # setup tab manager
        self.__tab_manager = TabManager(self.__ui.tabWidget)
        self.__tab_manager.tabChanged.connect(self.__on_current_tab_changed)
        self.__tab_manager.cursorPosChanged.connect(
            self.__on_cursor_pos_changed)
        self._last_program = None

        # setup program/subprogram action group
        ag = QActionGroup(self)
        ag.addAction(self.__ui.actionProgram)
        ag.addAction(self.__ui.actionSubprogram)
        ag.triggered.connect(self.__change_current_file_type)
        self.__update_toolbar()

        # view menu
        # toolbars
        self.__ui.toolBarFile.visibilityChanged.connect(
            self.__update_view_toolbar_menu)
        self.__ui.toolBarCode.visibilityChanged.connect(
            self.__update_view_toolbar_menu)
        self.__ui.aShowCodeToolbar.toggled.connect(
            self.__ui.toolBarCode.setVisible)
        self.__ui.aShowFilesToolbar.toggled.connect(
            self.__ui.toolBarFile.setVisible)
        # dock windows
        self.__ui.dockWidgetLogs.visibilityChanged.connect(
            self.__update_view_window_menu)
        self.__ui.dockWidgetNavPanel.visibilityChanged.connect(
            self.__update_view_window_menu)
        self.__ui.aShowNavWin.toggled.connect(
            self.__ui.dockWidgetNavPanel.setVisible)
        self.__ui.aShowLogsWin.toggled.connect(
            self.__ui.dockWidgetLogs.setVisible)


        # setup status bar
        self.lblFilename = QLabel()
        self.lblEncoding = QLabel()
        self.lblCursorPos = QLabel()
        self.__ui.statusbar.addPermanentWidget(self.lblFilename, 200)
        self.__ui.statusbar.addPermanentWidget(self.lblEncoding, 20)
        self.__ui.statusbar.addPermanentWidget(self.lblCursorPos, 20)

        # show the home page
        self.__on_current_tab_changed(None, "")

        # compilation status
        self._compilation_error = False

    def __update_view_toolbar_menu(self):
        """
        Updates the View>Toolbars menu
        """
        v = self.__ui.toolBarFile.isVisible()
        self.__ui.aShowFilesToolbar.setChecked(v)
        v = self.__ui.toolBarCode.isVisible()
        self.__ui.aShowCodeToolbar.setChecked(v)

    def __update_view_window_menu(self):
        """
        Updates the View>Windows menu
        """
        self.__ui.aShowLogsWin.setChecked(
            self.__ui.dockWidgetLogs.isVisible())
        self.__ui.aShowNavWin.setChecked(
            self.__ui.dockWidgetNavPanel.isVisible())

    def __update_toolbar(self):
        """
        Update toolbar buttons states depending on the context (whether there
        is an opened file,... )
        """
        # no file open, disable all buttons
        if not self.__tab_manager.has_open_tabs():
            self.__ui.actionSave_as.setEnabled(False)
            self.__ui.actionSave.setEnabled(False)
            self.__ui.actionCompile.setEnabled(False)
            self.__ui.actionRun.setEnabled(False)
            self.__ui.actionProgram.setEnabled(False)
            self.__ui.actionProgram.setChecked(False)
            self.__ui.actionSubprogram.setEnabled(False)
            self.__ui.actionSubprogram.setChecked(False)
        else:
            # a file is open, at least we can save it
            self.__ui.actionSave_as.setEnabled(True)
            self.__ui.actionSave.setEnabled(True)
            # this is a cobol file, we can enable compile and run
            if isinstance(self.__tab_manager.active_tab, CobolEditor):
                self.__ui.actionCompile.setEnabled(True)
                self.__ui.actionRun.setEnabled(True)
                self.__ui.actionProgram.setEnabled(True)
                self.__ui.actionSubprogram.setEnabled(True)
                # check the correct button for the file type
                if self.__tab_manager.active_tab_type == FileType.Program:
                    self.__ui.actionProgram.setChecked(True)
                    self.__ui.actionSubprogram.setChecked(False)
                elif self.__tab_manager.active_tab_type == FileType.Subprogram:
                    self.__ui.actionProgram.setChecked(False)
                    self.__ui.actionSubprogram.setChecked(True)
                    self.__ui.actionRun.setEnabled(self._last_program is not None)
            else:
                # this is a regular text file, we can only save it everything
                # else is disabled
                self.__ui.actionCompile.setEnabled(False)
                self.__ui.actionRun.setEnabled(False)
                self.__ui.actionProgram.setEnabled(False)
                self.__ui.actionSubprogram.setEnabled(False)
                self.__ui.actionProgram.setChecked(False)
                self.__ui.actionSubprogram.setChecked(False)

    def __update_status_bar_infos(self, widget):
        """
        Updates the status bar infos (widgets)

        :param widget: current editor widget
        """
        if widget:
            self.lblFilename.setText(widget.codeEdit.tagFilename)
            self.lblEncoding.setText(widget.codeEdit.tagEncoding)
            l, c = self.__tab_manager.get_cursor_pos()
            self.__on_cursor_pos_changed(l, c)
        else:
            self.lblFilename.setText("")
            self.lblEncoding.setText("")
            self.lblCursorPos.setText("")

    def __update_navigation_panel(self):
        """
        Updates the navigation panel using the DocumentAnalyserMode infos.
        """
        self.__ui.twNavigation.clear()
        if(self.__tab_manager.active_tab and
               isinstance(self.__tab_manager.active_tab, CobolEditor)):
            self.__ui.twNavigation.addTopLevelItem(
                self.__tab_manager.active_tab.documentAnalyserMode.root_node)
        self.__ui.twNavigation.expandAll()

    def __detect_encoding(self, filename):
        """
        Detect file encoding using chardet.

        :param filename: Filename to check

        :return: encoding - str
        """
        try:
            with open(filename, "r") as f:
                encoding = chardet.detect(f.read())['encoding']
                if not encoding:
                    raise Exception()
        except:
            encoding = sys.getfilesystemencoding()
        return encoding

    def __open_file(self, filename, filetype=None):
        """
        Open a file in a new editor tab

        :param filename: Filename

        :param filetype: Filetype - optional. A null filetype will trigger the
                         auto dectection
        """
        app_settings = Settings()
        if filename != "" and os.path.exists(filename):
            filename = os.path.normpath(filename)
            try:
                # detect file type if file type is None
                if not filetype:
                    filetype = cobol.detect_file_type(filename)
                # detect encoding
                encoding = self.__detect_encoding(filename)
                # open a tab
                tab = self.__tab_manager.open_tab(filename, filetype, encoding)
                # save last used path
                app_settings.last_used_path = \
                    self.__tab_manager.active_tab_file_dir
                # add an error manager (to remember the list of errors for the
                # compiler log list widget
                if isinstance(tab, CobolEditor):
                    error_manager = ErrorsManager(
                        self.__ui.listWidgetErrors, tab)
                    tab.errors_manager = error_manager
                    # also connect to the document layout changed signal
                    tab.documentAnalyserMode.documentLayoutChanged.connect(
                        self.__update_navigation_panel)
                # update ui
                self.__ui.stackedWidget.setCurrentIndex(
                    self.PAGE_EDITOR)
                self.__update_toolbar()
                self.__ui.wHomePage.setCurrentFile(filename)
            except UnicodeDecodeError:
                QMessageBox.critical(
                    self, "Bad encoding",
                    "Failed to open %s, bad encoding.\n\nChardet could not "
                    "detect a usable encoding, please encode your file with"
                    "a standard encoding (utf-8 for example)")

    def closeEvent(self, event):
        """
        On close event, performs cleanup
        """
        event.ignore()
        if self.__tab_manager.is_clean or self.__tab_manager.cleanup():
            event.accept()
            s = Settings()
            s.geometry = self.saveGeometry()
            s.state = self.saveState()
            s.maximised = self.isMaximized()
            print s.maximised


    @Slot()
    def on_actionNew_triggered(self):
        """
        Creates a new file
        """
        # ask file type
        dlg = DlgFileType(
            parent=self,
            label="<p>What kind of file do you want to <b>create</b>?</p>")
        if dlg.exec_() == DlgFileType.Accepted:
            extensions = "Cobol files *.cbl (*.cbl)"
            default_ext = ".cbl"
            if dlg.choice == FileType.Text:
                extensions = "Text files *.txt (*.txt *.dat)"
                default_ext = ".txt"
            # ask the save filename
            filename = QFileDialog.getSaveFileName(
                self, "Choose the save filename", Settings().last_used_path,
                extensions)[0]
            if filename != "":
                if QFileInfo(filename).suffix() == "":
                    filename += default_ext
                try:
                    with open(filename, "w") as f:
                        f.write("")
                    self.__open_file(filename, dlg.choice)
                except IOError or OSError:
                    QMessageBox.warning(self, "Failed to create file",
                                        "Failed to create file {0}.\n"
                                        "Check that you have the access rights "
                                        "on this folder and retry." % filename)

    @Slot()
    def on_actionOpen_triggered(self):
        """
        Opens a file
        """
         # ask file type
        app_settings = Settings()
        filename = QFileDialog.getOpenFileName(
            self, "Choose a file to open", app_settings.last_used_path,
            "Cobol files (*.cbl);; Text files (*.txt *.dat)")[0]
        self.__open_file(filename)

    @Slot(bool)
    def on_actionFullscreen_toggled(self, fullscreen):
        """
        Toggle fullscreen

        :param fullscreen: Fullscreen state
        """
        if fullscreen:
            self.showFullScreen()
        else:
            self.showNormal()

    @Slot()
    def on_actionQuit_triggered(self):
        """
        Exits the application
        """
        self.close()

    @Slot()
    def on_actionSave_triggered(self):
        """ Saves the current file """
        editor = self.__tab_manager.active_tab
        try:
            saveFileFromEditor(editor, encoding=editor.codeEdit.tagEncoding)
        except UnicodeEncodeError:
            # fallback to utf-8
            saveFileFromEditor(editor, encoding='utf-8')
        self.__ui.wHomePage.setCurrentFile(
            self.__tab_manager.active_tab_filename)
        self.__update_status_bar_infos(editor)

    @Slot()
    def on_actionSave_as_triggered(self):
        """ Saves the current file as"""
        editor = self.__tab_manager.active_tab
        s = Settings()
        extensions = "Cobol files *.cbl (*.cbl);;Text files *.txt (*.txt *.dat)"
        filename = QFileDialog.getSaveFileName(
            self, "Choose a save filename", s.last_used_path, extensions,
            ".cbl")[0]
        s = Settings()
        if filename != "":
            filename = os.path.normpath(filename)
            try:
                saveFileFromEditor(editor, filename,
                                   encoding=editor.codeEdit.tagEncoding)
            except UnicodeEncodeError:
                saveFileFromEditor(editor, filename,
                                   encoding='utf-8')
            self.__ui.wHomePage.setCurrentFile(filename)
            s.last_used_path = self.__tab_manager.active_tab_file_dir
        self.__update_status_bar_infos(editor)

    @Slot()
    def on_actionAbout_triggered(self):
        """
        Shows the about dialog
        :return:
        """
        dlg = DlgAbout(self)
        dlg.exec_()

    @Slot()
    def on_actionCompile_triggered(self):
        """
        Compiles current file
        """
        self.compile(self.__tab_manager.active_tab_filename,
                     self.__tab_manager.active_tab_type, True)

    def compile(self, filename, filetype, background=True):
        self.__ui.dockWidgetLogs.show()
        # clear open tab errors
        self._compilation_error = False
        dependencies = cobol.parse_dependencies(filename)
        for fn, ft in dependencies:
            tab = self.__tab_manager.get_tab_by_filename(fn)
            if tab:
                tab.errors_manager.clear_errors()
        self.__ui.tabWidgetLogs.setCurrentIndex(0)
        self.__ui.listWidgetErrors.clear()
        self.__tab_manager.active_tab.errors_manager.clear_errors()
        self.on_actionSave_triggered()
        runnable = cobol.CompilerThread(filename, filetype)
        runnable.setAutoDelete(True)
        runnable.events.finished.connect(self.__ui.actionCompile.setEnabled)
        runnable.events.msgReady.connect(self.on_compiler_msg_ready)
        if background:
            self.__threadPool.start(runnable)
        else:
            runnable.run()

    def on_compiler_msg_ready(self, filename, type, line, msg):
        tab = self.__tab_manager.get_tab_by_filename(filename)
        if tab:
            tab.errors_manager.set_errors([(filename, type, line, msg)])
        if type == "Success":
            icon = QIcon(":/ide-icons/rc/accept.png")
            if tab:
                tab.errors_manager.clear_errors()
        elif type == "Error":
            icon = QIcon(":/icons/rc/marker_error.png")
            self._compilation_error = True
        else:
            icon = QIcon(":/icons/rc/marker_warning.png")
        if line != -1:
            item = QListWidgetItem(icon, u"{0}:{1}: {2}".format(filename, line, msg))
        else:
            item = QListWidgetItem(icon, u"{0}: {1}".format(filename, msg))
        self.__ui.listWidgetErrors.addItem(item)

    @Slot()
    def on_actionRun_triggered(self):
        """
        Run current file executable
        """
        self.__ui.plainTextEditOutput.clear()
        filename = self.__tab_manager.active_tab_filename
        if self.__tab_manager.active_tab_type == FileType.Subprogram:
            filename = self._last_program
        self.compile(filename, FileType.Program, background=False)
        if not self._compilation_error:
            self.__ui.tabWidgetLogs.setCurrentIndex(1)
            runner = cobol.Runner(filename)
            runner.setAutoDelete(True)
            runner.events.finished.connect(self.__ui.actionRun.setEnabled)
            runner.events.lineAvailable.connect(
                self.__ui.plainTextEditOutput.appendPlainText)
            runner.events.error.connect(self.__on_run_error)
            self.__threadPool.start(runner)
            self._last_program = filename

    @Slot(str)
    def __on_run_error(self, msg):
        """
        Slot called when an error occured when running an executable

        :param msg: Error message
        """
        self.__ui.plainTextEditOutput.appendPlainText(msg)
        QMessageBox.critical(self, "Error executing program",
                             "An error occured while running a cobol program:"
                             "\n\n"
                             "%s" % msg)

    @Slot(QListWidgetItem)
    def __on_error_double_clicked(self, item):
        """
        Moves the text cursor to the line of the error

        :param item: QListWidgetItem
        """
        try:
            tokens = item.text().split(':')
            if sys.platform == "win32":
                filename = tokens[0] + ":" + tokens[1]
                line = int(tokens[2])
            else:
                filename = tokens[0]
                line = int(tokens[1])
            tab = self.__tab_manager.get_tab_by_filename(filename)
            if tab:
                self.__tab_manager.activate_tab_by_filename(filename)
                c = cursorForPosition(self.__tab_manager.active_tab.codeEdit, line,
                              column=1)
                tab.codeEdit.setTextCursor(c)
        except ValueError:
            pass

    @Slot(QWidget, str)
    def __on_current_tab_changed(self, widget, txt):
        """
        Updates ui when current file changed

        :param widget: The new editor widget

        :param txt: The new tab text
        """
        if widget:
            self.setWindowTitle("OpenCobolIDE - %s" % txt)
            self.__update_toolbar()
            if isinstance(widget, CobolEditor):
                if widget.errors_manager:
                    widget.errors_manager.updateErrors()
                widget.documentAnalyserMode.parse()
                if self.__ui.stackedWidget.currentIndex() == 0:
                    self.__ui.dockWidgetNavPanel.show()
            else:
                self.__ui.listWidgetErrors.clear()
                self.__ui.dockWidgetNavPanel.hide()
            self.__update_navigation_panel()
            self.__ui.tabWidgetLogs.setCurrentIndex(0)
            self.__ui.mnuActiveEditor.setEnabled(True)
            self.__ui.mnuActiveEditor.clear()
            self.__ui.mnuActiveEditor.addActions(
                widget.codeEdit.contextMenu.actions())
            if self.__ui.stackedWidget.currentIndex() == 0:
                self.__ui.dockWidgetLogs.show()
            self.__ui.menuBar.show()
            self.__ui.toolBarCode.show()
            self.__ui.toolBarFile.show()
            self.__ui.statusbar.show()
        else:
            self.setWindowTitle("OpenCobolIDE")
            self.__update_toolbar()
            self.__ui.plainTextEditOutput.clear()
            self.__ui.listWidgetErrors.clear()
            self.__ui.mnuActiveEditor.clear()
            self.__ui.mnuActiveEditor.setEnabled(False)
            self.__ui.stackedWidget.setCurrentIndex(self.PAGE_HOME)
            self.__ui.dockWidgetNavPanel.hide()
            self.__ui.dockWidgetLogs.hide()
            self.__ui.twNavigation.clear()
            self.__ui.menuBar.hide()
            self.__ui.toolBarCode.hide()
            self.__ui.toolBarFile.hide()
            self.__ui.statusbar.hide()
        self.__update_status_bar_infos(widget)

    @Slot(QAction)
    def __change_current_file_type(self, action):
        """
        Changes the current file type

        :param action: QAction - program or subprogram
        """
        if self.__tab_manager.active_tab_type != FileType.Text:
            if action == self.__ui.actionProgram:
                self.__tab_manager.active_tab.fileType = FileType.Program
            else:
                self.__tab_manager.active_tab.fileType = FileType.Subprogram
        self.__update_toolbar()

    @Slot(int, int)
    def __on_cursor_pos_changed(self, l, c):
        """
        Update cursor position label

        :param l: Line number
        :param c: Columns number
        """
        self.lblCursorPos.setText("{0}:{1}".format(l, c))

    @Slot(str)
    def on_wHomePage_quick_start_action_triggered(self, text):
        """
        Executes the quick start action

        :param text: Selected action text
        """
        if text == "Create a new file":
            self.on_actionNew_triggered()
        elif text == "Open a file":
            self.on_actionOpen_triggered()
        elif text == "Preferences":
            self.on_aPreferences_triggered()
        elif text == "About":
            self.on_actionAbout_triggered()

    @Slot(str, str)
    def on_wHomePage_recent_action_triggered(self, text, filename):
        """
        Open a recent file (from the recent file menu or from the recent action
        lists of the home page

        :param text: action text
        :param filename: recent filename
        """
        try:
            self.__open_file(filename)
        except IOError or OSError:
            pass

    @Slot(QTreeWidgetItem, int)
    def on_twNavigation_itemActivated(self, item, column):
        """
        Moves the text cursor on the selected document node position

        :param item: cobcide.cobol.DocumentNode
        :param column:
        """
        from pcef.code_edit import cursorForPosition
        tc = cursorForPosition(
            self.__tab_manager.active_tab.codeEdit, item.line + 1, 0)
        self.__tab_manager.active_tab.codeEdit.setTextCursor(tc)

    @Slot()
    def on_aPreferences_triggered(self):
        """
        Shows the preferences dialog
        """
        dlg = DlgPreferences(self)
        if dlg.exec_() == DlgPreferences.Accepted:
            self.__tab_manager.refresh_editor_styles()
