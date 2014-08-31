import os
import sys
from pyqode.core.api.syntax_highlighter import PYGMENTS_STYLES, ColorScheme
from pyqode.qt import QtCore, QtGui, QtWidgets, QT_API, PYQT5_API, PYSIDE_API
from ...compiler import GnuCobolStandard
from open_cobol_ide import system
from open_cobol_ide.view.editors import update_editor_settings
from ...settings import Settings
from ...view.forms import dlg_preferences_ui


class DlgPreferences(QtWidgets.QDialog, dlg_preferences_ui.Ui_Dialog):
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
            'exec',
            QtGui.QIcon(':/ide-icons/rc/application-x-executable.png')))
        self.tabWidget.setTabIcon(1, QtGui.QIcon.fromTheme(
            'applications-graphics',
            QtGui.QIcon(':/ide-icons/rc/applications-graphics.png')))
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
        self.reset(all_tabs=True)

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
                    self.listWidgetColorSchemes.item(i).text() ==
                          'darcula'):
                self.listWidgetColorSchemes.setCurrentRow(i)
                break

    def update_color_scheme_preview(self, item):
        self.plainTextEdit.syntax_highlighter.color_scheme = ColorScheme(
            item.text())

    def setupUi(self, Dialog):
        super().setupUi(Dialog)
        self.setMinimumWidth(450)
        self.tabWidget.setCurrentIndex(0)

    def reset(self, all_tabs=False):
        settings = Settings()
        if self.tabWidget.currentIndex() == 0 or all_tabs:
            # Editor Tab
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
        if self.tabWidget.currentIndex() == 1 or all_tabs:
            # Font & Color tab
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
            for style in PYGMENTS_STYLES:
                self.listWidgetColorSchemes.addItem(style)
                if style == settings.color_scheme:
                    current_index = self.listWidgetColorSchemes.count() - 1
            if current_index:
                self.listWidgetColorSchemes.setCurrentRow(current_index)
        # Build & run tab
        if self.tabWidget.currentIndex() == 2 or all_tabs:
            self.checkBoxRunExtTerm.setChecked(settings.external_terminal)
            self.lineEditRunTerm.setVisible(sys.platform != 'win32')
            self.lineEditRunTerm.setEnabled(settings.external_terminal)
            self.lineEditRunTerm.setText(settings.external_terminal_command)
            self.checkBoxCustomPath.setChecked(
                settings.custom_compiler_path != '')
            self.lineEditCompilerPath.setText(settings.custom_compiler_path)
            self.lineEditCompilerPath.setEnabled(self.checkBoxCustomPath.isChecked())
        if self.tabWidget.currentIndex() == 3 or all_tabs:
            self.checkBoxFreeFormat.setChecked(settings.free_format)
            self.lineEditCommentIndicator.setText(settings.comment_indicator)
            self.comboBoxStandard.setCurrentIndex(int(settings.cobol_standard))

    def restore_defaults(self):
        settings = Settings()
        index = self.tabWidget.currentIndex()
        if index == 0:
            settings.show_error = True
            settings.display_lines = True
            settings.highlight_caret = True
            settings.show_whitespaces = False
            settings.tab_len = 4
            settings.enable_autoindent = True
            settings.code_completion_trigger_len = 1
        if index == 1:
            settings.dark_style = False
            settings.icon_theme = 'default'
            settings.font = 'Source Code Pro'
            settings.font_size = 11
            settings.colorScheme = 'qt'
        if index == 2:
            settings.external_terminal = False
            settings.external_terminal_command = None
            settings.custom_compiler_path = ''
        if index == 3:
            settings.free_format = False
            settings.cobol_standard = GnuCobolStandard.default
            settings.comment_indicator = '*>'
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
        if dlg.exec_() != dlg.Accepted:
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
