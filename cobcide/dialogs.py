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
This module contains the application dialogs
"""
import PySide
import pcef
import pygments

from PySide.QtCore import Slot
from PySide.QtGui import QDialog, QButtonGroup, QTableWidgetItem, QFont, \
    QColorDialog, QColor

from pygments.styles import STYLE_MAP

from pcef import styles
import sys

from cobcide import __version__, cobol
from cobcide import FileType
from cobcide.settings import Settings
from cobcide.ui.dlg_file_type_ui import Ui_Dialog as UiFileTypeDialog
from cobcide.ui.dlg_about_ui import Ui_Dialog as UiAboutDialog
from cobcide.ui.dlg_preferences_ui import Ui_Dialog as UiPreferencesDialog


class DlgFileType(QDialog):
    """
    This dialog asks the user to choose a file type:

        - cobcide.FileType.Program
        - cobcide.FileType.SubProgram
        - cobcide.FileType.

    """
    def __init__(self, label=None, parent=None):
        """
        :param label: Dialog label, use default if None
        """
        QDialog.__init__(self, parent)
        self.__ui = UiFileTypeDialog()
        self.__ui.setupUi(self)
        self.__btnGroup = QButtonGroup()
        self.__btnGroup.addButton(self.__ui.radioButtonProgram)
        self.__btnGroup.addButton(self.__ui.radioButtonSubprogram)
        self.__btnGroup.addButton(self.__ui.radioButtonText)
        self.__ui.radioButtonProgram.setChecked(True)
        if label and (isinstance(label, str) or isinstance(label, unicode)):
            self.__ui.label.setText(label)

    @property
    def choice(self):
        """
        Returns the chosen file type
        """
        id = self.__btnGroup.checkedId()
        if id == -2:
            return FileType.Program
        elif id == -3:
            return FileType.Subprogram
        else:
            return FileType.Text


class DlgAbout(QDialog):
    """
    About dialog. Shows the about text and the 3rd party libraries versions.
    """
    def __init__(self, parent=None):
        QDialog.__init__(self, parent)
        self.__ui = UiAboutDialog()
        self.__ui.setupUi(self)
        self.__ui.labelMain.setText(self.__ui.labelMain.text() % __version__)
        # self.__ui.tabWidget.setCurrentIndex(0)
        # print

        versions = [cobol.get_cobc_version(),
                    PySide.QtCore.__version__,
                    PySide.__version__,
                    pcef.pcef_version,
                    pygments.__version__,
                    "1.0",  # there is no __version__ for the QWelcomeWindow
                    ]
        for i, version in enumerate(versions):
            item = QTableWidgetItem(version)
            self.__ui.tbwVersions.setItem(i, 0, item)


CODE_EXAMPLE = \
    """      *******************************************************************
      ** Example taken from http://progopedia.com/version/opencobol-1.0/*
      *******************************************************************
       IDENTIFICATION DIVISION.
      **************************************
       PROGRAM-ID. SAMPLE.
      **
       DATA DIVISION.
      **************************************
       WORKING-STORAGE SECTION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*
       77 FACT      PIC 9(15) comp  .
       77 N         PIC 99          .
       77 I         PIC 99          .
       77 IST       PIC XX          .
       77 FACTTST   PIC X(18)       .
      **
       PROCEDURE DIVISION.
      **************************************
       MAIN-PROCECURE.
           MOVE 16 to N
           MOVE 0 to I
           MOVE 1 to FACT
           PERFORM UNTIL I GREATER THAN N
               MOVE I to IST
               MOVE FACT to FACTTST
               DISPLAY IST "! = " FACTTST
               ADD 1 to I
               MULTIPLY I BY FACT
                    ON SIZE ERROR DISPLAY "value too big"
               END-MULTIPLY
           END-PERFORM
           EXIT PROGRAM.
       END PROGRAM SAMPLE.
      **
"""

COLOR_NAMES = ["Margin", "ActiveLine", "Selection", "SelectedText",
               "LineNumber", "PanelBackground", "PanelBackground2",
               "TextOccurences", "SearchResults", "Warning", "Error"]


class DlgPreferences(QDialog):

    def __init__(self, parent):
        QDialog.__init__(self, parent)
        self.__init = False
        self.ui = UiPreferencesDialog()
        self.ui.setupUi(self)
        self.ui.lwMenu.setCurrentRow(0)
        self.ui.lwColors.setCurrentRow(0)
        self.ui.plainTextEdit.syntaxHighlightingMode.setLexerFromFilename(
            "*.cbl")
        self.ui.plainTextEdit.codeEdit.setPlainText(CODE_EXAMPLE)
        self.ui.plainTextEdit.zoomMode.enabled = False
        s = Settings()
        self.ui.cbCodeCompletion.setChecked(s.enable_cc)
        self.ui.cbLineNbr.setChecked(s.show_line_numbers)
        self.ui.cbUseExtShell.setChecked(s.use_external_shell)
        self.ui.leTerminal.setText(s.shell_cmd)
        self.ui.cbWhitespaces.setChecked(s.show_whitespaces)
        font = QFont(s.font_name)
        font.setFamily(s.font_name)
        self.ui.fcbFont.setCurrentFont(font)
        self.ui.sbFontSize.setValue(s.font_size)
        self.ui.cmbPygmentsStyle.addItems(STYLE_MAP.keys())
        index = self.ui.cmbPygmentsStyle.findText(s.pygments_style)
        if index != -1:
            self.__init = True
            self.ui.cmbPygmentsStyle.setCurrentIndex(index)
        else:
            index = self.ui.cmbPygmentsStyle.findText("default")
            self.ui.cmbPygmentsStyle.setCurrentIndex()

        self.ui.pbColor.setStyleSheet(
            "background-color: %s" % s.get_style_color(
                self.ui.lwColors.currentItem().text()))

        if sys.platform == "win32":
            self.ui.lblExternalTerminal.hide()
            self.ui.leTerminal.hide()
        else:
            self.ui.lblExternalTerminal.setEnabled(self.ui.cbUseExtShell.isChecked())
            self.ui.leTerminal.setEnabled(self.ui.cbUseExtShell.isChecked())

    @Slot(int)
    def on_lwMenu_currentRowChanged(self, row):
        self.ui.swMain.setCurrentIndex(row)

    @Slot(int)
    def on_lwColors_currentRowChanged(self, row):
        s = Settings()
        self.ui.pbColor.setStyleSheet(
            "background-color: %s" % s.get_style_color(
                self.ui.lwColors.currentItem().text()))

    @Slot(int)
    def on_cmbPygmentsStyle_currentIndexChanged(self, index):
        if self.__init:
            style = self.ui.cmbPygmentsStyle.itemText(index)
            s = Settings()
            s.pygments_style = style
            self.refresh_preview()

    @Slot()
    def on_pbColor_clicked(self):
        dlg = QColorDialog()
        color_name = self.ui.lwColors.currentItem().text()
        s = Settings()
        dlg.setCurrentColor(s.get_style_color(color_name))
        if dlg.exec_() == QColorDialog.Accepted:
            color = dlg.currentColor()
            s.set_style_color(color_name, color.name())
            self.ui.pbColor.setStyleSheet(
                "background-color: %s" % s.get_style_color(
                    self.ui.lwColors.currentItem().text()))
            self.refresh_preview()

    @Slot(bool)
    def on_cbWhitespaces_toggled(self, state):
        s = Settings()
        s.show_whitespaces = state
        self.refresh_preview()

    @Slot(bool)
    def on_cbLineNbr_toggled(self, state):
        s = Settings()
        s.show_line_numbers = state
        self.refresh_preview()

    @Slot(bool)
    def on_cbCodeCompletion_toggled(self, state):
        print state
        s = Settings()
        s.enable_cc = state
        self.refresh_preview()

    @Slot(bool)
    def on_cbUseExtShell_toggled(self, state):
        print state
        s = Settings()
        s.use_external_shell = state
        if sys.platform != "win32":
            self.ui.lblExternalTerminal.setEnabled(state)
            self.ui.leTerminal.setEnabled(state)

    @Slot(int)
    def on_sbFontSize_valueChanged(self, value):
        s = Settings()
        s.font_size = value
        self.refresh_preview()

    @Slot(QFont)
    def on_fcbFont_currentFontChanged(self, font):
        assert isinstance(font, QFont)
        s = Settings()
        s.font_name = font.family()
        self.refresh_preview()

    @Slot(unicode)
    def on_leTerminal_textEdited(self, txt):
        s = Settings()
        s.shell_cmd = txt

    def refresh_preview(self):
        s = Settings()
        self.ui.plainTextEdit.currentStyle = s.style
        self.ui.plainTextEdit.lineNumberPanel.enabled = s.show_line_numbers
        self.ui.plainTextEdit.codeCompletionMode.enabled = s.enable_cc
        self.ui.plainTextEdit.syntaxHighlightingMode.highlighter.rehighlight()
