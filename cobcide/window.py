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
import os
import chardet
import sys

from PySide.QtCore import Slot, QThreadPool, QFileInfo
from PySide.QtGui import QMainWindow, QActionGroup, QDialog, QLabel
from PySide.QtGui import QFileDialog
from PySide.QtGui import QMessageBox, QListWidgetItem
from pcef import saveFileFromEditor
from pcef.code_edit import cursorForPosition

from cobcide import __version__, FileType, cobol
from cobcide.dialogs import DlgFileType
from cobcide.errors_manager import ErrorsManager
from cobcide.tab_manager import TabManager
from cobcide.tabs import CobolEditor
from cobcide.ui import ide_ui, dlg_about_ui
from cobcide.settings import Settings


class MainWindow(QMainWindow):
    PAGE_HOME = 0
    PAGE_EDITOR = 1
    def __init__(self):
        QMainWindow.__init__(self)
        self.__ui = ide_ui.Ui_MainWindow()
        self.__ui.setupUi(self)
        self.__ui.listWidgetErrors.itemDoubleClicked.connect(
            self.__on_error_double_clicked)
        self.__tab_manager = TabManager(self.__ui.tabWidget)
        self.__tab_manager.tabChanged.connect(self.__on_current_tab_changed)
        self.__tab_manager.cursorPosChanged.connect(
            self.__on_cursor_pos_changed)
        ag = QActionGroup(self)
        ag.addAction(self.__ui.actionProgram)
        ag.addAction(self.__ui.actionSubprogram)
        ag.triggered.connect(self.__change_current_file_type)
        self.__update_toolbar()
        self.__threadPool = QThreadPool()
        self.__threadPool.setMaxThreadCount(1)
        self.__ui.stackedWidget.setCurrentIndex(self.PAGE_HOME)
        self.lblFilename = QLabel()
        self.lblEncoding = QLabel()
        self.lblCursorPos = QLabel()
        self.__ui.statusbar.addPermanentWidget(self.lblFilename, 200)
        self.__ui.statusbar.addPermanentWidget(self.lblEncoding, 20)
        self.__ui.statusbar.addPermanentWidget(self.lblCursorPos, 20)
        self.__ui.wHomePage.set_internal_data(self.__ui.menuRecent_files,
                                              self.__ui.actionClear)

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
                elif self.__tab_manager.active_tab_type == \
                        FileType.Subprogram:
                    self.__ui.actionProgram.setChecked(False)
                    self.__ui.actionSubprogram.setChecked(True)
                    self.__ui.actionRun.setEnabled(False)
            else:
                # this is a regular text file, we can only save it everything
                # else is disabled
                self.__ui.actionCompile.setEnabled(False)
                self.__ui.actionRun.setEnabled(False)
                self.__ui.actionProgram.setEnabled(False)
                self.__ui.actionSubprogram.setEnabled(False)
                self.__ui.actionProgram.setChecked(False)
                self.__ui.actionSubprogram.setChecked(False)


    @Slot()
    def on_actionNew_triggered(self):
        # ask file type
        dlg = DlgFileType(
            parent=self,
            label="<p>What kind of file do you want to <b>create</b>?</p>")
        if dlg.exec_() == DlgFileType.Accepted:
            extension = "Cobol files (*.cbl)"
            if dlg.choice == FileType.Text:
                extension = "Text files (*.txt *.dat)"
            # ask the save filename
            filename = QFileDialog.getSaveFileName(
                self, "Choose the save filename", "", extension)[0]
            self._open_file(dlg.choice, filename)

    def detect_encoding(self, filename):
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

    def _open_file(self, choice, filename):
        app_settings = Settings()
        if filename != "":
            filename = os.path.normpath(filename)
            try:
                self.__ui.stackedWidget.setCurrentIndex(
                    self.PAGE_EDITOR)
                encoding = self.detect_encoding(filename)
                tab = self.__tab_manager.open_tab(filename, choice,
                                                  encoding)
                app_settings.last_used_path = \
                    self.__tab_manager.active_tab_file_dir
                if isinstance(tab, CobolEditor):
                    error_manager = ErrorsManager(
                        self.__ui.listWidgetErrors, tab)
                    tab.errors_manager = error_manager
                self.__update_toolbar()
                self.__ui.wHomePage.setCurrentFile(filename)
            except UnicodeDecodeError:
                QMessageBox.critical(
                    self, "Bad encoding",
                    "Failed to open %s, bad encoding.\n\nChardet could not "
                    "detect a usable encoding, please encode your file with"
                    "a standard encoding (utf-8 for example)")

    @Slot()
    def on_actionOpen_triggered(self):
         # ask file type
        dlg = DlgFileType(parent=self)
        if dlg.exec_() == DlgFileType.Accepted:
            if dlg.choice == FileType.Text:
                extension = "Text files (*.dat *.txt)"
            else:
                extension = "Cobol files (*.cbl)"
            app_settings = Settings()
            filename = QFileDialog.getOpenFileName(
                self, "Choose a file to open", app_settings.last_used_path,
                extension)[0]
            self._open_file(dlg.choice, filename)

    @Slot(bool)
    def on_actionFullscreen_toggled(self, fullscreen):
        if fullscreen:
            self.showFullScreen()
        else:
            self.showNormal()

    @Slot()
    def on_actionQuit_triggered(self):
        self.close()

    @Slot()
    def on_actionSave_triggered(self):
        """ Save the current file """
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
        """ Save the current file as"""
        editor = self.__tab_manager.active_tab
        s = Settings()
        filename = QFileDialog.getSaveFileName(
            self, "Choose a save filename", s.last_used_path)[0]
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
        dlg = QDialog(self)
        ui = dlg_about_ui.Ui_Dialog()
        ui.setupUi(dlg)
        ui.labelMain.setText(ui.labelMain.text() % __version__)
        dlg.exec_()

    @Slot()
    def on_actionCompile_triggered(self):
        self.__ui.tabWidgetLogs.setCurrentIndex(0)
        self.on_actionSave_triggered()
        filename = self.__tab_manager.active_tab_filename
        errors, output_filename = cobol.compile(
            filename, self.__tab_manager.active_tab_type)
        self.__tab_manager.active_tab.errors_manager.set_errors(errors,
                                                                output_filename)

    @Slot()
    def on_actionRun_triggered(self):
        self.__ui.tabWidgetLogs.setCurrentIndex(1)
        self.__ui.plainTextEditOutput.clear()
        filename = self.__tab_manager.active_tab_filename
        runner = cobol.Runner(filename)
        runner.setAutoDelete(True)
        runner.events.finished.connect(self.__ui.actionRun.setEnabled)
        runner.events.lineAvailable.connect(
            self.__ui.plainTextEditOutput.appendPlainText)
        runner.events.error.connect(self.__on_run_error)
        self.__threadPool.start(runner)

    def __on_run_error(self, msg):
        self.__ui.plainTextEditOutput.appendPlainText(msg)
        QMessageBox.critical(self, "Error executing program",
                             "An error occured while running a cobol program:"
                             "\n\n"
                             "%s" % msg)

    @Slot(QListWidgetItem)
    def __on_error_double_clicked(self, item):
        """
        :param item: QListWidgetItem
        """
        try:
            line = int(item.text().split(':')[0])
            c = cursorForPosition(self.__tab_manager.active_tab.codeEdit, line,
                          column=1)
            self.__tab_manager.active_tab.codeEdit.setTextCursor(c)
        except ValueError:
            pass

    def __update_status_bar_infos(self, widget):
        if widget:
            self.lblFilename.setText(widget.codeEdit.tagFilename)
            self.lblEncoding.setText(widget.codeEdit.tagEncoding)
            l, c = self.__tab_manager.get_cursor_pos()
            self.__on_cursor_pos_changed(l, c)
        else:
            self.lblFilename.setText("")
            self.lblEncoding.setText("")
            self.lblCursorPos.setText("")

    def __on_current_tab_changed(self, widget, txt):
        if widget:
            self.setWindowTitle("OpenCobol IDE - %s" % txt)
            self.__update_toolbar()
            if isinstance(widget, CobolEditor) and widget.errors_manager:
                widget.errors_manager.updateErrors()
            else:
                self.__ui.listWidgetErrors.clear()
            self.__ui.tabWidgetLogs.setCurrentIndex(0)
            self.__ui.menuEdit.clear()
            self.__ui.menuEdit.addActions(
                self.__tab_manager.active_tab.codeEdit.contextMenu.actions())
        else:
            self.setWindowTitle("OpenCobol IDE")
            self.__update_toolbar()
            self.__ui.plainTextEditOutput.clear()
            self.__ui.listWidgetErrors.clear()
            self.__ui.menuEdit.clear()
            self.__ui.stackedWidget.setCurrentIndex(self.PAGE_HOME)
        self.__update_status_bar_infos(widget)

    def __change_current_file_type(self, action):
        if self.__tab_manager.active_tab_type != FileType.Text:
            if action == self.__ui.actionProgram:
                self.__tab_manager.active_tab.fileType = FileType.Program
            else:
                self.__tab_manager.active_tab.fileType = FileType.Subprogram
        self.__update_toolbar()

    def __on_cursor_pos_changed(self, l, c):
        self.lblCursorPos.setText("{0}:{1}".format(l, c))

    @Slot(str)
    def on_wHomePage_quick_start_action_triggered(self, text):
        if text == "Create a new file":
            self.on_actionNew_triggered()
        elif text == "Open a file":
            self.on_actionOpen_triggered()
        elif text == "About":
            self.on_actionAbout_triggered()

    @Slot(str, str)
    def on_wHomePage_recent_action_triggered(self, text, filename):
        dlg = DlgFileType(parent=self)
        if dlg.exec_() == DlgFileType.Accepted and QFileInfo(filename).exists():
            self._open_file(dlg.choice, filename)