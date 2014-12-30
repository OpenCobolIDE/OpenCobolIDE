"""
This module contains the FileController.

"""
import logging
import os
from pyqode.core import widgets
from pyqode.cobol.api import icons
from pyqode.qt import QtWidgets, QtGui
from open_cobol_ide.view.editors import CobolCodeEdit
from open_cobol_ide.settings import Settings
from open_cobol_ide.view.dialogs.new_file import DlgNewFile
from .base import Controller


def _logger():
    return logging.getLogger(__name__)


#: Cobol files file (for open/save dialogs)
COBOL_FILES_FILTER = 'Cobol files (%s)' % ' '.join(
    [ext.lower() for ext in CobolCodeEdit.extensions] +
    CobolCodeEdit.extensions).replace('.', '*.')
#: Other files file (ALL files)
OTHER_FILES_FILTER = 'Other text files (*)'
#: filter separator
FILTER_SEPARATOR = ';;'
FILTER = FILTER_SEPARATOR.join([COBOL_FILES_FILTER, OTHER_FILES_FILTER])


class FileIconProvider(QtWidgets.QFileIconProvider):
    def icon(self, file_infos):
        try:
            if '.%s' % file_infos.suffix() in CobolCodeEdit.all_extensions():
                return QtGui.QIcon(icons.ICON_MIMETYPE)
        except AttributeError:
            pass
        return super().icon(file_infos)


class FileController(Controller):
    """
    Controls file operations (new file, open, recent files, ...).

    """
    def __init__(self, app):
        super().__init__(app)
        self.recent_files_manager = widgets.RecentFilesManager(
            'OpenCobolIDE', 'OpenCobolIDE4')
        self.menu_recents = widgets.MenuRecentFiles(
            self.ui.menuFile, title='Recents',
            recent_files_manager=self.recent_files_manager,
            icon_provider=FileIconProvider())
        self.menu_recents.open_requested.connect(self.open_file)
        self.menu_recents.clear_requested.connect(
            self.recent_files_manager.clear)
        self.ui.menuFile.clear()
        self.ui.menuFile.addAction(self.ui.actionNew)
        self.ui.menuFile.addAction(self.ui.actionOpen)
        self.ui.menuFile.addSeparator()
        self.ui.menuFile.addMenu(self.menu_recents)
        self.ui.menuFile.addSeparator()
        self.ui.menuFile.addAction(self.ui.actionSave)
        self.ui.menuFile.addAction(self.ui.actionSaveAs)
        self.ui.menuFile.addSeparator()
        self.ui.menuFile.addAction(self.ui.actionQuit)
        self.menu_recents.update_actions()
        self.recent_files_manager.updated.connect(
            self.menu_recents.update_actions)
        self.ui.actionOpen.triggered.connect(self.request_open)
        self.ui.actionNew.triggered.connect(self.request_new)
        self.ui.actionSave.triggered.connect(
            self.ui.tabWidgetEditors.save_current)
        self.ui.actionSaveAs.triggered.connect(self.save_as)
        self.ui.actionQuit.triggered.connect(self.quit)
        self.ui.tabWidgetEditors.register_code_edit(CobolCodeEdit)
        self.ui.tabWidgetEditors.icon_provider_klass = FileIconProvider

    def request_new(self, path=None):
        """
        Requests the creation of a new file, show the new file wizard and open
        if wizard completed sucessfully.

        """
        if path and os.path.isfile(path):
            path = os.path.abspath(os.path.join(path, os.pardir))
        path = DlgNewFile.create_new_file(self.main_window, path=path)
        if path:
            self.open_file(path)

    def request_open(self):
        """
        Prompts the user for a file to open and open it.
        """
        paths, status = QtWidgets.QFileDialog.getOpenFileNames(
            self.main_window, 'Open a file', directory=Settings().last_path,
            filter=FILTER)
        if status:
            for path in paths:
                self.open_file(path)

    def open_file(self, path):
        """
        Open a file in the edit view. If there is an editor already open for
        the requested file, we simply give it the focus.

        :param path: path of the file to open.
        """
        self.app.edit.add_editor(path)
        self.app.view.show_edit_page()
        self.app.file.recent_files_manager.open_file(path)
        Settings().last_path = path

    def save_as(self):
        """
        Saves the currend editor content as.
        """
        fn = self.ui.tabWidgetEditors.save_current_as()
        self.recent_files_manager.open_file(fn)
        Settings().last_path = fn

    def quit(self):
        """
        Quits the application, but contrarily asks the user first.
        """
        if QtWidgets.QMessageBox.question(
                self.main_window, 'Quit OpenCobolIDE?',
                'Are you sure you want to quit OpenCobolIDE?',
                QtWidgets.QMessageBox.Yes | QtWidgets.QMessageBox.No,
                QtWidgets.QMessageBox.No) == QtWidgets.QMessageBox.Yes:
            _logger().debug('quit action triggered')
            self.app.exit()
