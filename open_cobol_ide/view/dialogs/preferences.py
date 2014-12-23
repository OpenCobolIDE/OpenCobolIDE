import os
import sys
from pyqode.core.api.syntax_highlighter import PYGMENTS_STYLES, ColorScheme
from pyqode.qt import QtCore, QtGui, QtWidgets
from open_cobol_ide import system
from open_cobol_ide.enums import GnuCobolStandard
from open_cobol_ide.settings import Settings
from open_cobol_ide.view.forms import dlg_preferences_ui


class DlgPreferences(QtWidgets.QDialog, dlg_preferences_ui.Ui_Dialog):
    flags_in_checkbox = ['-g', '-ftrace', '-ftraceall', '-fdebugging-line',
                         '-static']
    def __init__(self, parent):
        super().__init__(parent)
        self.setupUi(self)
        themes = system.icon_themes()
        if themes:
            self.comboBoxIconTheme.addItems(themes)
        else:
            self.comboBoxIconTheme.hide()
            self.lblIconTheme.hide()
        self.tabWidget.setTabIcon(2, QtGui.QIcon.fromTheme(
            'application-x-executable',
            QtGui.QIcon(':/ide-icons/rc/application-x-executable.png')))
        self.tabWidget.setTabIcon(1, QtGui.QIcon.fromTheme(
            'applications-graphics',
            QtGui.QIcon(':/ide-icons/rc/applications-graphics.png')))
        self.tabWidget.setTabIcon(3, QtGui.QIcon.fromTheme(
            'media-playback-start', QtGui.QIcon(':/ide-icons/rc/media-playback-start.png')))
        self.buttonBox.button(self.buttonBox.Reset).clicked.connect(self.reset)
        self.buttonBox.button(self.buttonBox.RestoreDefaults).clicked.connect(
            self.restore_defaults)
        self.checkBoxRunExtTerm.stateChanged.connect(
            self.lineEditRunTerm.setEnabled)
        self.checkBoxCustomPath.stateChanged.connect(
            self.lineEditCompilerPath.setEnabled)
        self.listWidgetColorSchemes.currentItemChanged.connect(
            self.update_color_scheme_preview)
        self.plainTextEdit.setPlainText('''      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
            DISPLAY "Hello world"
            STOP RUN.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.

        ''', '', '')
        self.lineEditDbpre.setReadOnly(True)
        self.lineEditDbpreFramework.setReadOnly(True)
        self.lineEditCobmysqlapi.setReadOnly(True)
        self.toolButtonDbpre.clicked.connect(self._select_dbpre)
        self.toolButtonDbpreFramework.clicked.connect(self._select_dbpre_framework)
        self.toolButtonCobMySqlApiPath.clicked.connect(self._select_cobmysqlapi)
        self.reset(all_tabs=True)

    def _select_dbpre(self):
        path, _ = QtWidgets.QFileDialog.getOpenFileName(
            self, 'Select dbpre executable')
        if path:
            self.lineEditDbpre.setText(path)

    def _select_cobmysqlapi(self):
        path, _ = QtWidgets.QFileDialog.getOpenFileName(
            self, 'Select dbpre framework directory')
        if path:
            self.lineEditCobmysqlapi.setText(path)

    def _select_dbpre_framework(self):
        def bool_to_string(value):
            if value:
                return 'Found'
            else:
                return 'Missing'

        path = QtWidgets.QFileDialog.getExistingDirectory(
            self, 'Select cobmysqlapi object file')
        if path:
            pgctbbat = os.path.exists(os.path.join(path, 'PGCTBBAT'))
            pgctbbatws = os.path.exists(os.path.join(path, 'PGCTBBATWS'))
            sqlca = os.path.exists(os.path.join(path, 'SQLCA'))
            if pgctbbat and pgctbbatws and sqlca:
                self.lineEditDbpreFramework.setText(path)
            else:
                QtWidgets.QMessageBox.warning(self, 'Invalid dpre framework directory',
                                              'Missing one of the following files: \n'
                                              'PGCTBBAT: %s\n'
                                              'PGCTBBATWS: %s\n'
                                              'SQLCA: %s\n' % (
                                                  bool_to_string(pgctbbat),
                                                  bool_to_string(pgctbbatws),
                                                  bool_to_string(sqlca)))

    def stop_backend(self):
        self.plainTextEdit.backend.stop()

    def _update_icon_theme(self, c):
        index = self.comboBoxIconTheme.findText(c)
        self.comboBoxIconTheme.setCurrentIndex(index)

    @QtCore.Slot(bool)
    def on_radioButtonColorWhite_toggled(self, native):
        # choose an icon them that goes well with the selected style
        if not native:
            candidates = ['Faenza-Darkest', 'nouveGnomeGray', 'matefaenzadark',
                          'gnome', 'oxygen']
            available = system.icon_themes()
            for c in candidates:
                if c in available:
                    self._update_icon_theme(c)
                    break
        else:
            self._update_icon_theme('default')
        # choose a a color scheme that goes well with the selected style
        for i in range(self.listWidgetColorSchemes.count()):
            if (native and self.listWidgetColorSchemes.item(i).text() ==
                    'qt'):
                self.listWidgetColorSchemes.setCurrentRow(i)
                break
            elif (not native and
                    self.listWidgetColorSchemes.item(i).text() == 'darcula'):
                self.listWidgetColorSchemes.setCurrentRow(i)
                break

    def update_color_scheme_preview(self, item):
        if item:
            self.plainTextEdit.syntax_highlighter.color_scheme = ColorScheme(
                item.text())

    def setupUi(self, Dialog):
        super().setupUi(Dialog)
        self.setMinimumWidth(450)
        self.tabWidget.setCurrentIndex(0)

    def reset(self, all_tabs=False):
        settings = Settings()
        # Editor
        if self.tabWidget.currentIndex() == 0 or all_tabs:
            self.checkBoxShowErrors.setChecked(settings.show_errors)
            self.checkBoxViewLineNumber.setChecked(settings.display_lines)
            self.checkBoxHighlightCurrentLine.setChecked(
                settings.highlight_caret)
            self.checkBoxHighlightWhitespaces.setChecked(
                settings.show_whitespaces)
            self.spinBoxEditorTabLen.setValue(settings.tab_len)
            self.checkBoxEditorAutoIndent.setChecked(
                settings.enable_autoindent)
            self.spinBoxEditorCCTriggerLen.setValue(
                settings.code_completion_trigger_len)
            self.rbLowerCaseKwds.setChecked(settings.lower_case_keywords)
            self.rbUpperCaseKwds.setChecked(not settings.lower_case_keywords)
            self.lineEditCommentIndicator.setText(settings.comment_indicator)
            self.checkBoxSmartBackspace.setChecked(
                settings.enable_smart_backspace)
        # Style
        if self.tabWidget.currentIndex() == 1 or all_tabs:
            rb = (self.radioButtonColorDark if settings.dark_style else
                  self.radioButtonColorWhite)
            rb.setChecked(True)
            index = self.comboBoxIconTheme.findText(settings.icon_theme)
            if index != -1:
                self.comboBoxIconTheme.setCurrentIndex(index)
            self.fontComboBox.setCurrentFont(QtGui.QFont(settings.font))
            self.spinBoxFontSize.setValue(settings.font_size)
            self.listWidgetColorSchemes.clear()
            current_index = None
            self.listWidgetColorSchemes.clear()
            for style in PYGMENTS_STYLES:
                self.listWidgetColorSchemes.addItem(style)
                if style == settings.color_scheme:
                    current_index = self.listWidgetColorSchemes.count() - 1
            if current_index:
                self.listWidgetColorSchemes.setCurrentRow(current_index)
        # Run
        if self.tabWidget.currentIndex() == 3 or all_tabs:
            self.checkBoxRunExtTerm.setChecked(settings.external_terminal)
            self.lineEditRunTerm.setVisible(sys.platform != 'win32')
            self.lineEditRunTerm.setEnabled(settings.external_terminal)
            self.lineEditRunTerm.setText(settings.external_terminal_command)
        # compiler
        if self.tabWidget.currentIndex() == 2 or all_tabs:
            self.checkBoxFreeFormat.setChecked(settings.free_format)
            self.comboBoxStandard.setCurrentIndex(
                int(settings.cobol_standard))
            self.checkBoxCustomPath.setChecked(
                settings.custom_compiler_path != system.which('cobc'))
            self.lineEditCompilerPath.setText(settings.custom_compiler_path)
            self.lineEditCompilerPath.setEnabled(
                self.checkBoxCustomPath.isChecked())
            flags = Settings().compiler_flags
            self.cb_debugging_line.setChecked(
                self.cb_debugging_line.text() in flags)
            self.cb_ftrace.setChecked(self.cb_ftrace.text() in flags)
            self.cb_ftraceall.setChecked(self.cb_ftraceall.text() in flags)
            self.cb_g.setChecked(self.cb_g.text() in flags)
            self.cb_static.setChecked(self.cb_static.text() in flags)
            for v in self.flags_in_checkbox:
                try:
                    flags.remove(v)
                except ValueError:
                    pass
            self.lineEditLibs.setText(settings.libraries)
            self.lineEditLibSearchPath.setText(settings.library_search_path)
            self.le_compiler_flags.setText(' '.join(flags))
        # SQL Cobol
        if self.tabWidget.currentIndex() == 4 or all_tabs:
            self.lineEditDbpre.setText(settings.dbpre)
            self.lineEditDbpreFramework.setText(settings.dbpre_framework)
            self.lineEditCobmysqlapi.setText(settings.cobmysqlapi)
            self.lineEditDBHOST.setText(settings.dbhost)
            self.lineEditDBUSER.setText(settings.dbuser)
            self.lineEditDBPASSWD.setText(settings.dbpasswd)
            self.lineEditDBNAME.setText(settings.dbname)
            self.lineEditDBPORT.setText(settings.dbport)
            self.lineEditDBSOCKET.setText(settings.dbsocket)

    def restore_defaults(self):
        settings = Settings()
        index = self.tabWidget.currentIndex()
        # Editor
        if index == 0:
            settings.show_error = True
            settings.display_lines = True
            settings.highlight_caret = True
            settings.show_whitespaces = False
            settings.tab_len = 4
            settings.enable_autoindent = True
            settings.code_completion_trigger_len = 1
            settings.comment_indicator = '*> '
            settings.enable_smart_backspace = False
            settings.lower_case_keywords = False
        # Style
        elif index == 1:
            settings.dark_style = False
            settings.icon_theme = 'default'
            settings.font = 'Source Code Pro'
            settings.font_size = 11
            settings.colorScheme = 'qt'
        # run
        elif index == 3:
            settings.external_terminal = False
            settings.external_terminal_command = None
        # compiler
        elif index == 2:
            settings.free_format = False
            settings.cobol_standard = GnuCobolStandard.default
            settings.custom_compiler_path = ''
            settings.compiler_flags = []
            settings.library_search_path = ''
            settings.libraries = ''
        elif index == 4:
            settings.dbpre = ''
            settings.dbpre_framework = ''
            settings.cobmysqlapi = ''
            settings.dbhost = 'localhost'
            settings.dbuser = ''
            settings.dbpasswd = ''
            settings.dbname = ''
            settings.dbport = '03306'
            settings.dbsocket = 'null'
        self.reset()

    @classmethod
    def edit_preferences(cls, parent):
        def restore_state(dlg):
            s = Settings()
            dlg.resize(s.preferences_width, s.preferences_height)

        def save_state(dlg):
            s = Settings()
            s.preferences_width = dlg.width()
            s.preferences_height = dlg.height()

        dlg = cls(parent)
        restore_state(dlg)
        ret_val = dlg.exec_()
        dlg.stop_backend()
        if ret_val != dlg.Accepted:
            raise ValueError()
        save_state(dlg)
        settings = Settings()
        settings.display_lines = dlg.checkBoxViewLineNumber.isChecked()
        settings.highlight_caret = dlg.checkBoxHighlightCurrentLine.isChecked()
        settings.show_whitespaces = \
            dlg.checkBoxHighlightWhitespaces.isChecked()
        settings.tab_len = dlg.spinBoxEditorTabLen.value()
        settings.enable_autoindent = dlg.checkBoxEditorAutoIndent.isChecked()
        settings.code_completion_trigger_len = \
            dlg.spinBoxEditorCCTriggerLen.value()
        settings.dark_style = dlg.radioButtonColorDark.isChecked()
        settings.font = dlg.fontComboBox.currentFont().family()
        settings.font_size = dlg.spinBoxFontSize.value()
        settings.color_scheme = dlg.listWidgetColorSchemes.currentItem().text()
        settings.external_terminal = dlg.checkBoxRunExtTerm.isChecked()
        settings.external_terminal_command = dlg.lineEditRunTerm.text()
        settings.lower_case_keywords = dlg.rbLowerCaseKwds.isChecked()
        if dlg.checkBoxCustomPath.isChecked():
            settings.custom_compiler_path = dlg.lineEditCompilerPath.text()
        else:
            settings.customCompilerPath = ''
        settings.free_format = dlg.checkBoxFreeFormat.isChecked()
        settings.comment_indicator = dlg.lineEditCommentIndicator.text()
        settings.cobol_standard = GnuCobolStandard(
            dlg.comboBoxStandard.currentIndex())
        settings.icon_theme = dlg.comboBoxIconTheme.currentText()
        settings.show_errors = dlg.checkBoxShowErrors.isChecked()
        settings.enable_smart_backspace = \
            dlg.checkBoxSmartBackspace.isChecked()
        settings.library_search_path = dlg.lineEditLibSearchPath.text()
        settings.libraries = dlg.lineEditLibs.text()

        cb_flags = [dlg.cb_g, dlg.cb_ftrace, dlg.cb_ftraceall,
                    dlg.cb_debugging_line, dlg.cb_static]
        flags = [cb.text() for cb in cb_flags if cb.isChecked()]
        flags += dlg.le_compiler_flags.text().split(' ')
        settings.compiler_flags = flags
        # sql
        settings.dbpre = dlg.lineEditDbpre.text()
        settings.dbpre_framework = dlg.lineEditDbpreFramework.text()
        settings.cobmysqlapi = dlg.lineEditCobmysqlapi.text()
        settings.dbhost = dlg.lineEditDBHOST.text()
        settings.dbuser = dlg.lineEditDBUSER.text()
        settings.dbpasswd = dlg.lineEditDBPASSWD.text()
        settings.dbname = dlg.lineEditDBNAME.text()
        settings.dbport = dlg.lineEditDBPORT.text()
        settings.dbsocket = dlg.lineEditDBSOCKET.text()
