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
This module contains the home page widget
"""
import qwelcomewindow
from PySide.QtCore import QFileInfo
from PySide.QtGui import QIcon, QAction
from cobcide.settings import Settings


class HomePageWidget(qwelcomewindow.QWelcomeWidget):
    """
    The home page widget.

    Shows a list of quick start actions and take care of keeping a list
    of recent files
    """
    MaxRecentFiles = 10

    def __init__(self, parent):
        qwelcomewindow.QWelcomeWidget.__init__(
            self, parent=parent, app_name="OpenCobolIDE",
            app_icon=QIcon(":/ide-icons/rc/silex-64x64.png"))
        self.add_action(self.ActionType.QuickStart, "Create a new file",
                        QIcon(":/ide-icons/rc/document-new.png"))
        self.add_action(self.ActionType.QuickStart, "Open a file",
                        QIcon(":/ide-icons/rc/document-open.png"))
        self.add_action(self.ActionType.QuickStart, "About",
                        QIcon(":/ide-icons/rc/dialog-information.png"))
        self.ui.lblRecents.setText("Recent files")
        self.ui.lblQuickStart.setText("Quick start")

    def set_internal_data(self, menuRecentFiles, actionClearList):
        self.menuRecentFiles = menuRecentFiles
        self.actionClearList = actionClearList
        self.actionClearList.triggered.connect(self._clear)
        self.init()

    def _clear(self):
        settings = Settings()
        settings.clear_recent_files()
        self.updateRecentFileActions()

    def createActions(self):
        for i in range(self.MaxRecentFiles):
            self.recentFileActs.append(
                QAction(self, visible=False, triggered=self.openRecentFile))

    def openRecentFile(self):
        action = self.sender()
        if action:
            txt = action.data()
            self.recent_action_triggered.emit(action.text(), txt)

    def createMenus(self):
        self.menuRecentFiles.clear()
        for i in range(self.MaxRecentFiles):
            self.menuRecentFiles.addAction(self.recentFileActs[i])
        self.separatorAct = self.menuRecentFiles.addSeparator()
        self.menuRecentFiles.addAction(self.actionClearList)
        self.updateRecentFileActions()

    def updateRecentFileActions(self):
        settings = Settings()
        files = settings.recent_files
        files_no = 0
        if files:
            files_no = len(files)
        numRecentFiles = min(files_no, self.MaxRecentFiles)
        self.clear_recent_actions()
        for i in range(numRecentFiles):
            text = "%s" % self.strippedName(files[i])
            self.recentFileActs[i].setText(text)
            self.recentFileActs[i].setData(files[i])
            self.recentFileActs[i].setVisible(True)
            self.add_action(0, text, data=files[i])
        for j in range(numRecentFiles, self.MaxRecentFiles):
            self.recentFileActs[j].setVisible(False)
        self.separatorAct.setVisible((numRecentFiles > 0))

    def setCurrentFile(self, fileName):
        self.curFile = fileName
        settings = Settings()
        files = settings.recent_files
        try:
            files.remove(fileName)
        except ValueError:
            pass
        files.insert(0, fileName)
        del files[self.MaxRecentFiles:]
        settings.recent_files = files
        self.updateRecentFileActions()

    def init(self):
        """
        Loads the recent file list, create its recent actions and create the
        recent files menu.
        """
        self.recentFileActs = []
        self.createActions()
        self.createMenus()


    def strippedName(self, fullFileName):
        return QFileInfo(fullFileName).fileName()
