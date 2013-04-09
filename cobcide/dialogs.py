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
import pcef
import pygments
import PySide
import sys

from PySide.QtCore import Slot
from PySide.QtGui import QAbstractButton
from PySide.QtGui import QButtonGroup
from PySide.QtGui import QColorDialog
from PySide.QtGui import QDialog
from PySide.QtGui import QFont
from PySide.QtGui import QTableWidgetItem

from pygments.styles import STYLE_MAP

from cobcide import __version__
from cobcide import cobol
from cobcide import FileType
from cobcide.settings import Settings
from cobcide.ui.dlg_about_ui import Ui_Dialog as UiAboutDialog
from cobcide.ui.dlg_file_type_ui import Ui_Dialog as UiFileTypeDialog
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


#: Example code shown in the preview editor
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

# Enumerates color names
COLOR_NAMES = ["Margin", "ActiveLine", "Selection", "SelectedText",
               "LineNumber", "PanelBackground", "PanelBackground2",
               "TextOccurences", "SearchResults", "Warning", "Error"]


class DlgPreferences(QDialog):
    """
    Preferences dialogs. Shows a sets of options that can be changed by the
    user (mainly editor style )
    """

    def __init__(self, parent):
        QDialog.__init__(self, parent)
        self.__init = False
        self.ui = UiPreferencesDialog()
        self.ui.setupUi(self)
        self.ui.lwMenu.setCurrentRow(0)
        self.ui.lwColors.setCurrentRow(0)
        self.__refresh_ui()

    def __refresh_ui(self):
        """
        Refresh ui; refresh control value from settings values
        """
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
            self.ui.lblExternalTerminal.setEnabled(
                self.ui.cbUseExtShell.isChecked())
            self.ui.leTerminal.setEnabled(
                self.ui.cbUseExtShell.isChecked())

    def __refresh_preview(self):
        """
        Refresh the preview editor (reset style, turn on/off modes/panels)
        """
        s = Settings()
        self.ui.plainTextEdit.currentStyle = s.style
        self.ui.plainTextEdit.lineNumberPanel.enabled = s.show_line_numbers
        self.ui.plainTextEdit.codeCompletionMode.enabled = s.enable_cc
        self.ui.plainTextEdit.syntaxHighlightingMode.highlighter.rehighlight()

    @Slot(int)
    def on_lwMenu_currentRowChanged(self, row):
        """
        Chanages the actibe page

        :param row: The current page/row to set
        """
        self.ui.swMain.setCurrentIndex(row)

    @Slot(int)
    def on_lwColors_currentRowChanged(self, row):
        """
        Changes the currently displayed color

        :param row: Color row
        """
        s = Settings()
        self.ui.pbColor.setStyleSheet(
            "background-color: %s" % s.get_style_color(
                self.ui.lwColors.currentItem().text()))

    @Slot(int)
    def on_cmbPygmentsStyle_currentIndexChanged(self, index):
        """
        Changes the pygments style.

        :param index:Pygment style index
        """
        if self.__init:
            style = self.ui.cmbPygmentsStyle.itemText(index)
            s = Settings()
            s.pygments_style = style
            self.__refresh_preview()

    @Slot()
    def on_pbColor_clicked(self):
        """
        Changes active color
        """
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
            self.__refresh_preview()

    @Slot(bool)
    def on_cbWhitespaces_toggled(self, state):
        """
        Toggle show whitespaces

        :param state: flag - bool
        """
        s = Settings()
        s.show_whitespaces = state
        self.__refresh_preview()

    @Slot(bool)
    def on_cbLineNbr_toggled(self, state):
        """
        Shows/Hides the line number panel

        :param state: True to show the panel
        """
        s = Settings()
        s.show_line_numbers = state
        self.__refresh_preview()

    @Slot(bool)
    def on_cbCodeCompletion_toggled(self, state):
        """
        Enables/Disables code completion

        :param state: True to enable cc
        """
        s = Settings()
        s.enable_cc = state
        self.__refresh_preview()

    @Slot(bool)
    def on_cbUseExtShell_toggled(self, state):
        """
        Enable/Disable run in external terminal

        :param state: State: true = enable
        """
        s = Settings()
        s.use_external_shell = state
        if sys.platform != "win32":
            self.ui.lblExternalTerminal.setEnabled(state)
            self.ui.leTerminal.setEnabled(state)

    @Slot(int)
    def on_sbFontSize_valueChanged(self, value):
        """
        Changes font size

        :param value: New size value
        """
        s = Settings()
        s.font_size = value
        self.__refresh_preview()

    @Slot(QFont)
    def on_fcbFont_currentFontChanged(self, font):
        """
        Change current font

        :param font: The new font to set
        """
        s = Settings()
        s.font_name = font.family()
        self.__refresh_preview()

    @Slot(unicode)
    def on_leTerminal_textEdited(self, txt):
        """
        Change external terminal base command

        :param txt: Base command
        """
        s = Settings()
        s.shell_cmd = txt

    @Slot(QAbstractButton)
    def on_buttonBox_clicked(self, button):
        """
        Reset preferences

        :param button: The clicked button, we only handle the reset button
        """
        assert  isinstance(button, QAbstractButton)
        if button.text() == "Reset":
            s = Settings()
            s.init_style_settings()
            s.shell_cmd = "gnome-terminal -e"
            s.use_external_shell = False
            s.enable_cc = False
            self.__refresh_ui()
            self.__refresh_preview()
