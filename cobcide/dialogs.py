# This file is part of OCIDE.
# 
# OCIDE is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# OCIDE is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with OCIDE.  If not, see <http://www.gnu.org/licenses/>.
"""
This module contains the application dialogs
"""
import PySide
from PySide.QtGui import QDialog, QButtonGroup, QTableWidgetItem
import pcef
import pygments
import qwelcomewindow

from cobcide import __version__, cobol
from cobcide import FileType
from cobcide.ui.dlg_file_type_ui import Ui_Dialog as UiFileTypeDialog
from cobcide.ui.dlg_about_ui import Ui_Dialog as UiAboutDialog


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
        self.__ui.tabWidget.setCurrentIndex(0)
        print

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
