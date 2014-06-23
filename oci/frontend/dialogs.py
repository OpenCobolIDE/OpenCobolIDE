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
Contains the application dialogs
"""
import os

import pygments
from pyqode.core.modes import PYGMENTS_STYLES
from pyqode.core.qt import QtWidgets
from pyqode.core.qt import QtCore
from pyqode.core.qt import QtGui
import qdarkstyle
import pyqode.core
import sys

from oci.backend import compiler
from oci import __version__
from oci.constants import TEMPLATES
from oci.settings import Settings
from oci.frontend.ui import dlg_about_ui, dlg_file_type_ui, dlg_preferences_ui


class DlgNewFile(QtWidgets.QDialog, dlg_file_type_ui.Ui_Dialog):
    def path(self):
        return os.path.join(
            self.lineEditPath.text(),
            self.lineEditName.text() + self.comboBoxExtension.currentText())

    def template(self):
        """ Gets the file template"""
        return TEMPLATES[self.comboBoxType.currentIndex()]

    def __init__(self, parent):
        super().__init__(parent)
        self.setupUi(self)
        self.enableOkButton()
        completer = QtWidgets.QCompleter(self)
        completer.setModel(QtWidgets.QDirModel(completer))
        self.lineEditPath.setCompleter(completer)
        self.lineEditPath.setText(os.path.expanduser("~"))
        self.prev_pth = ""

    @QtCore.Slot(str)
    def on_lineEditName_textChanged(self, txt):
        self.enableOkButton()

    @QtCore.Slot(str)
    def on_lineEditPath_textChanged(self, txt):
        self.enableOkButton()

    @QtCore.Slot()
    def on_toolButton_clicked(self):
        ret = QtWidgets.QFileDialog.getExistingDirectory(
            self, "Choose the program directory",
            Settings().lastFilePath)
        if ret:
            self.lineEditPath.setText(ret)

    def enableOkButton(self):
        pth = str(self.lineEditPath.text())
        bt = self.buttonBox.button(QtWidgets.QDialogButtonBox.Ok)
        name = self.lineEditName.text()
        enable = name != "" and os.path.exists(pth) and os.path.isdir(pth)
        bt.setEnabled(enable)
        self.prev_pth = pth


class DlgAbout(QtWidgets.QDialog, dlg_about_ui.Ui_Dialog):
    """
    About dialog. Shows the about text and the 3rd party libraries versions.
    """
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)
        self.labelMain.setText(self.labelMain.text() % __version__)
        versions = [compiler.get_cobc_version(),
                    QtCore.QT_VERSION_STR,
                    QtCore.PYQT_VERSION_STR,
                    pyqode.core.__version__,
                    pygments.__version__,
                    qdarkstyle.__version__]
        for i, version in enumerate(versions):
            item = QtWidgets.QTableWidgetItem(version)
            self.tbwVersions.setItem(i, 0, item)
        self.textBrowser.setStyleSheet("color: red")


class DlgRejectedError(Exception):
    pass


class DlgPreferences(QtWidgets.QDialog, dlg_preferences_ui.Ui_Dialog):
    TAB_WIDGET_WHITE_CSS = """
QTabWidget::pane { /* The tab widget frame */
     border-top: 1px black;
     position: absolute;
     outline: none;
 }

/* Style the tab using the tab sub-control. Note that
   it reads QTabBar _not_ QTabWidget */
QTabBar::tab {
     border: none;
     padding: 5px;
     color: #7C8080;
     outline: none;
}

QTabBar::tab:selected, QTabBar::tab:hover {
    border-bottom: 3px solid gray; /* same as the pane color */
    color: #2E3436;
}

QTabBar::tab:selected {
    border-bottom: 3px solid #5F96CD; /* same as the pane color */
    color: #2E3436;
    font: bold;
}
"""
    TAB_WIDGET_DARK_CSS = """
QTabWidget::pane { /* The tab widget frame */
     position: absolute;
     outline: none;
     border: none;
 }

/* Style the tab using the tab sub-control. Note that
   it reads QTabBar _not_ QTabWidget */
QTabBar::tab {
     background: #282828;
     border: none;
     border-bottom: 2px solid silver;
     border-top: 2px solid silver;
     border-radius: 0px;
     padding: 5px;
     color: #808080;
     outline: none;
     padding: 5px;
     margin: 0px;
}

QTabBar::tab:selected, QTabBar::tab:hover {
    border-bottom: 3px solid gray; /* same as the pane color */
    color: #D2D2D2;
    padding: 5px;
    margin: 0px;
}

QTabBar::tab:selected {
    border-bottom: 3px solid #5F96CD; /* same as the pane color */
    color: #D2D2D2;
    font: bold;
    padding: 5px;
    margin: 0px;
}
"""

    TAB_BAR_WHITE_CSS = """
background: #E2E2E2;
alignment: left;
border:1px transparent;
outline: none;
border-bottom: 1px solid #A7ABA7;
border-top: 1px solid #A7ABA7
"""

    TAB_BAR_DARK_CSS = """
background: #282828;
alignment: left;
border:1px transparent;
outline: none;
border-bottom: 2px solid silver;
border-top: 2px solid silver;
"""

    def __init__(self, parent):
        super().__init__(parent)
        self.setupUi(self)
        self.buttonBox.button(self.buttonBox.Reset).clicked.connect(self.reset)
        self.buttonBox.button(self.buttonBox.RestoreDefaults).clicked.connect(
            self.restoreDefaults)
        self.checkBoxRunExtTerm.stateChanged.connect(
            self.lineEditRunTerm.setEnabled)
        self.checkBoxCustomPath.stateChanged.connect(
            self.lineEditCompilerPath.setEnabled)
        self.reset(allTabs=True)

    @QtCore.Slot(bool)
    def on_radioButtonColorWhite_toggled(self, state):
        for i in range(self.listWidgetColorSchemes.count()):
            if (state and
                    self.listWidgetColorSchemes.item(i).text() == 'default'):
                self.listWidgetColorSchemes.setCurrentRow(i)
                break
            elif (not state and
                    self.listWidgetColorSchemes.item(i).text() == 'native'):
                self.listWidgetColorSchemes.setCurrentRow(i)
                break

    def setupUi(self, Dialog):
        super().setupUi(Dialog)
        self.setMinimumWidth(450)
        self.tabWidget.setCurrentIndex(0)
        bar = self.tabWidget.tabBar()
        if sys.platform != 'darwin':
            if Settings().globalStyle == 'white':
                bar.setStyleSheet(self.TAB_BAR_WHITE_CSS)
            else:
                bar.setStyleSheet(self.TAB_BAR_DARK_CSS)

    def reset(self, allTabs=False):
        settings = Settings()
        if self.tabWidget.currentIndex() == 0 or allTabs:
            # View tab
            self.checkBoxViewLineNumber.setChecked(settings.displayLineNumbers)
            self.checkBoxViewMargins.setChecked(settings.displayMargins)
            self.checkBoxViewStatus.setChecked(settings.displayStatusBar)
            self.checkBoxViewMenuBar.setChecked(settings.displayMenuBar)
            self.checkBoxHighlightCurrentLine.setChecked(
                settings.highlightCurrentLine)
            self.checkBoxHighlightBraces.setChecked(
                settings.highlightMatchingBraces)
            self.checkBoxHighlightWhitespaces.setChecked(
                settings.highlightWhitespaces)
            self.checkBoxViewToolBar.setChecked(
                settings.displayToolBar)
            self.checkBoxViewControlPanel.setChecked(
                settings.displayControlPanel)
        # Editor Tab
        if self.tabWidget.currentIndex() == 1 or allTabs:
            self.spinBoxEditorTabLen.setValue(settings.tabWidth)
            self.checkBoxEditorAutoIndent.setChecked(
                settings.enableAutoIndent)
            self.checkBoxEditorSaveOnFocusOut.setChecked(
                settings.saveOnFocusOut)
            self.spinBoxEditorCCTriggerLen.setValue(settings.ccTriggerLen)
        if self.tabWidget.currentIndex() == 2 or allTabs:
            # Font & Color tab
            rb = self.radioButtonColorWhite if settings.globalStyle == 'white' \
                else self.radioButtonColorDark
            rb.setChecked(True)
            self.fontComboBox.setCurrentFont(QtGui.QFont(settings.fontName))
            self.spinBoxFontSize.setValue(settings.fontSize)
            self.listWidgetColorSchemes.clear()
            current_index = None
            for style in PYGMENTS_STYLES:
                self.listWidgetColorSchemes.addItem(style)
                if style == settings.colorScheme:
                    current_index = self.listWidgetColorSchemes.count() - 1
            if current_index:
                self.listWidgetColorSchemes.setCurrentRow(current_index)
        # Build & run tab
        if self.tabWidget.currentIndex() == 3 or allTabs:
            self.checkBoxRunExtTerm.setChecked(settings.runInShell)
            self.lineEditRunTerm.setVisible(sys.platform != 'win32')
            self.lineEditRunTerm.setEnabled(settings.runInShell)
            self.lineEditRunTerm.setText(settings.shellCommand)
            self.checkBoxCustomPath.setChecked(
                settings.customCompilerPath != '')
            self.lineEditCompilerPath.setText(settings.customCompilerPath)
        if self.tabWidget.currentIndex() == 4 or allTabs:
            self.checkBoxFreeFormat.setChecked(settings.free_format)
            self.spinBoxLeftMargin.setValue(settings.left_margin)
            self.spinBoxRightMargin.setValue(settings.right_margin)


    def restoreDefaults(self):
        settings = Settings()
        index = self.tabWidget.currentIndex()
        if index == 0:
            settings.displayLineNumbers = True
            settings.displayMargins = True
            settings.displayStatusBar = True
            settings.displayMenuBar = False
            settings.displayControlPanel = True
            settings.displayToolBar = False
            settings.highlightCurrentLine = True
            settings.highlightMatchingBraces = True
            settings.highlightWhitespaces = False
        if index == 1:
            settings.tabWidth = 4
            settings.enableAutoIndent = True
            settings.saveOnFocusOut = True
            settings.ccTriggerLen = 1
        if index == 2:
            settings.globalStyle = 'white'
            settings.fontName = None
            settings.fontSize = 10
            settings.colorScheme = 'default'
        if index == 3:
            settings.runInShell = False
            settings.shellCommand = None
            settings.customCompilerPath = ''
        if index == 4:
            settings.free_format = False
            settings.left_margin = 7
            settings.right_margin = 72
        self.reset()

    def resizeEvent(self, *args, **kwargs):
        # super().resizeEvent(*args, **kwargs)
        if sys.platform != 'darwin':
            self.tabWidget.tabBar().setFixedWidth(self.width())
            css = self.TAB_WIDGET_WHITE_CSS if Settings().globalStyle == 'white' \
                else self.TAB_WIDGET_DARK_CSS
            self.tabWidget.setStyleSheet(
                css + 'QTabBar::tab { width: %dpx};' %
                ((self.width() - self.width() / 8) / self.tabWidget.count()))

    @classmethod
    def editSettings(cls, parent):
        dlg = cls(parent)
        if dlg.exec_() != dlg.Accepted:
            raise DlgRejectedError()
        settings = Settings()
        settings.displayLineNumbers = dlg.checkBoxViewLineNumber.isChecked()
        settings.displayMargins = dlg.checkBoxViewMargins.isChecked()
        settings.displayStatusBar = dlg.checkBoxViewStatus.isChecked()
        settings.highlightCurrentLine = dlg.checkBoxHighlightCurrentLine.isChecked()
        settings.highlightMatchingBraces = dlg.checkBoxHighlightBraces.isChecked()
        settings.highlightWhitespaces = dlg.checkBoxHighlightWhitespaces.isChecked()
        settings.tabWidth = dlg.spinBoxEditorTabLen.value()
        settings.enableAutoIndent = dlg.checkBoxEditorAutoIndent.isChecked()
        settings.saveOnFocusOut = dlg.checkBoxEditorSaveOnFocusOut.isChecked()
        settings.ccTriggerLen = dlg.spinBoxEditorCCTriggerLen.value()
        settings.globalStyle = 'white' if dlg.radioButtonColorWhite.isChecked() else 'dark'
        settings.fontName = dlg.fontComboBox.currentFont().family()
        settings.fontSize = dlg.spinBoxFontSize.value()
        settings.colorScheme = dlg.listWidgetColorSchemes.currentItem().text()
        settings.runInShell = dlg.checkBoxRunExtTerm.isChecked()
        settings.shellCommand = dlg.lineEditRunTerm.text()
        settings.displayControlPanel = dlg.checkBoxViewControlPanel.isChecked()
        settings.displayToolBar = dlg.checkBoxViewToolBar.isChecked()
        settings.displayMenuBar = dlg.checkBoxViewMenuBar.isChecked()
        if dlg.checkBoxCustomPath.isChecked():
            settings.customCompilerPath = dlg.lineEditCompilerPath.text()
        else:
            settings.customCompilerPath = ''
        settings.free_format = dlg.checkBoxFreeFormat.isChecked()
        settings.left_margin = dlg.spinBoxLeftMargin.value()
        settings.right_margin = dlg.spinBoxRightMargin.value()
