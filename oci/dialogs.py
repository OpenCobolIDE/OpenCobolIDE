# Copyright 2013 Colin Duquesnoy
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
Contains the application dialogs
"""
from PyQt4 import QtGui
from PyQt4 import QtCore
import os
import pygments
import pyqode.core
import pyqode.widgets
import sys
from oci import cobol, __version__
from oci.settings import Settings
from oci.ui import loadUi

EXE_TEMPLATE = """      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
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

"""

MODULE_TEMPLATE = """      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM.
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
       LINKAGE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       01 PARAMETRES.
      **
      * Input/Output parameters from/to the calling PROGRAM
      **
           02 PA-RETURN-CODE PIC 99 VALUE 0.
       PROCEDURE DIVISION USING PARAMETRES.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
        DISPLAY "Hello world"
        MOVE 0 TO PA-RETURN-CODE
        STOP RUN.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM.

"""

TEMPLATES = [EXE_TEMPLATE, MODULE_TEMPLATE, ""]


class DlgNewFile(QtGui.QDialog):
    def path(self):
        return os.path.join(
            self.lineEditPath.text(),
            self.lineEditName.text() + self.comboBoxExtension.currentText())

    def template(self):
        """ Gets the file template"""
        return TEMPLATES[self.comboBoxType.currentIndex()]

    def __init__(self, parent):
        QtGui.QDialog.__init__(self, parent)
        loadUi("dlg_file_type.ui", self)
        self.enableOkButton()
        completer = QtGui.QCompleter(self)
        completer.setModel(QtGui.QDirModel(completer))
        self.lineEditPath.setCompleter(completer)
        self.lineEditPath.setText(os.path.expanduser("~"))
        self.prev_pth = ""

    if sys.version_info[0] == 2:
        @QtCore.pyqtSlot(unicode)
        def on_lineEditName_textChanged(self, txt):
            self.enableOkButton()

        @QtCore.pyqtSlot(unicode)
        def on_lineEditPath_textChanged(self, txt):
            self.enableOkButton()
    else:
        @QtCore.pyqtSlot(str)
        def on_lineEditName_textChanged(self, txt):
            self.enableOkButton()

        @QtCore.pyqtSlot(str)
        def on_lineEditPath_textChanged(self, txt):
            self.enableOkButton()

    @QtCore.pyqtSlot()
    def on_toolButton_clicked(self):
        ret = QtGui.QFileDialog.getExistingDirectory(
            self, "Choose the program directory",
            Settings().lastFilePath)
        if ret:
            self.lineEditPath.setText(ret)

    def enableOkButton(self):
        pth = str(self.lineEditPath.text())
        bt = self.buttonBox.button(QtGui.QDialogButtonBox.Ok)
        name = self.lineEditName.text()
        enable = name != "" and os.path.exists(pth) and os.path.isdir(pth)
        if sys.platform == "win32":
            if " " in pth and pth != self.prev_pth:
                QtGui.QMessageBox.warning(
                    self, "Warning",
                    "Cannot create a new file in a path that contains spaces, "
                    "please choose another directory.")
                enable = False
        bt.setEnabled(enable)
        self.prev_pth = pth


class DlgAbout(QtGui.QDialog):
    """
    About dialog. Shows the about text and the 3rd party libraries versions.
    """
    def __init__(self, parent=None):
        QtGui.QDialog.__init__(self, parent)
        loadUi("dlg_about.ui", self)
        self.labelMain.setText(self.labelMain.text() % __version__)
        versions = [cobol.get_cobc_version(),
                    QtCore.QT_VERSION_STR,
                    QtCore.PYQT_VERSION_STR,
                    pyqode.core.__version__,
                    pyqode.widgets.__version__,
                    pygments.__version__]
        for i, version in enumerate(versions):
            item = QtGui.QTableWidgetItem(version)
            self.tbwVersions.setItem(i, 0, item)


class DlgPreferences(QtGui.QDialog):

    @property
    def editorSettings(self):
        return self.codeEdit.settings

    @editorSettings.setter
    def editorSettings(self, value):
        self.codeEdit.settings = value
        self.propGridSettings.setPropertyRegistry(self.codeEdit.settings)

    @property
    def homePageColorScheme(self):
        return self.__homePageColorScheme

    @homePageColorScheme.setter
    def homePageColorScheme(self, value):
        self.__homePageColorScheme = value
        schemes = [pyqode.widgets.ColorScheme, pyqode.widgets.DarkColorScheme]
        self.homeWidget.setColorScheme(schemes[value]())
        self.radioButtonWhite.setChecked(value == 0)
        self.radioButtonDark.setChecked(value == 1)

    @property
    def editorStyle(self):
        return self.codeEdit.style

    @editorStyle.setter
    def editorStyle(self, value):
        self.codeEdit.style = value
        self.propGridStyle.setPropertyRegistry(self.codeEdit.style)

    @property
    def consoleBackground(self):
        return self.console.backgroundColor

    @consoleBackground.setter
    def consoleBackground(self, value):
        self.console.backgroundColor = value
        self.colorButtonConsoleBck.color = value

    @property
    def consoleForeground(self):
        return self.console.processOutputColor

    @consoleForeground.setter
    def consoleForeground(self, value):
        self.console.processOutputColor = value
        self.colorButtonConsoleFore.color = value

    @property
    def consoleUserInput(self):
        return self.console.userInputOutputColor

    @consoleUserInput.setter
    def consoleUserInput(self, value):
        self.console.userInputOutputColor = value
        self.colorButtonConsoleUsr.color = value

    @property
    def consoleAppOutput(self):
        return self.console.appMessageColor

    @consoleAppOutput.setter
    def consoleAppOutput(self, value):
        self.console.appMessageColor = value
        self.colorButtonConsoleApp.color = value

    def __init__(self, parent=None,
                 editorSettings=None, editorStyle=None):
        QtGui.QDialog.__init__(self, parent)
        loadUi("dlg_preferences.ui", self)
        self.__homePageColorScheme = 0
        self.codeEdit.syntaxHighlighterMode.setLexerFromFilename("file.cbl")
        self.codeEdit.syntaxHighlighterMode.rehighlight()
        lw = self.lwMenu
        assert isinstance(lw, QtGui.QListWidget)
        lw.item(0).setIcon(QtGui.QIcon.fromTheme(
            "preferences-system",
            QtGui.QIcon(":/ide-icons/rc/Preferences-system.png")))
        lw.item(1).setIcon(QtGui.QIcon.fromTheme(
            "applications-graphics",
            QtGui.QIcon(":/ide-icons/rc/Mypaint-icon.png")))
        self.stackedWidget.setCurrentIndex(0)
        self.tabWidgetSettings.setCurrentIndex(0)
        self.tabWidgetStyle.setCurrentIndex(0)
        self.propGridStyle.rehighlightRequested.connect(
            self.codeEdit.syntaxHighlighterMode.rehighlight)
        self.radioButtonWhite.toggled.connect(self.onHomePageStyleChanged)

    @QtCore.pyqtSlot(int)
    def on_lwMenu_currentRowChanged(self, row):
        self.stackedWidget.setCurrentIndex(row)

    @QtCore.pyqtSlot(QtGui.QColor)
    def on_colorButtonConsoleFore_valueChanged(self, color):
        self.consoleForeground = color

    @QtCore.pyqtSlot(QtGui.QColor)
    def on_colorButtonConsoleBck_valueChanged(self, color):
        self.consoleBackground = color

    @QtCore.pyqtSlot(QtGui.QColor)
    def on_colorButtonConsoleUsr_valueChanged(self, color):
        self.consoleUserInput = color

    @QtCore.pyqtSlot(QtGui.QColor)
    def on_colorButtonConsoleApp_valueChanged(self, color):
        self.consoleAppOutput = color

    def keyPressEvent(self, QKeyEvent):
        if QKeyEvent.key() == QtCore.Qt.Key_Return:
            QKeyEvent.accept()

    def onHomePageStyleChanged(self, whiteEnable):
        self.homePageColorScheme = int(not whiteEnable)
