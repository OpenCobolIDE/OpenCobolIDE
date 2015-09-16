"""
This module contains the FileController.

"""
import logging
import os
from pyqode.core import widgets
from pyqode.qt import QtWidgets
from open_cobol_ide.view.editors import CobolCodeEdit
from open_cobol_ide.settings import Settings
from open_cobol_ide.view.dialogs.new_file import DlgNewFile
from .base import Controller
from open_cobol_ide.view.widgets import FileIconProvider


def _logger():
    return logging.getLogger(__name__)


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
            self.save_current)
        self.ui.actionSaveAs.triggered.connect(self.save_as)
        self.ui.actionQuit.triggered.connect(self.quit)
        self.ui.tabWidgetEditors.register_code_edit(CobolCodeEdit)
        self.ui.tabWidgetEditors.icon_provider_klass = FileIconProvider
        self.ui.tvFileSystem.set_icon_provider(FileIconProvider())

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
        paths, _ = QtWidgets.QFileDialog.getOpenFileNames(
            self.main_window, 'Open a file', directory=Settings().last_path,
            filter=widgets.splittable_tab_widget.CodeEditTabWidget.get_filter(
                'text/x-cobol') + ';;Other files (*)')
        if paths:
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

    def save_current(self):
        try:
            self.ui.tabWidgetEditors.save_current()
        except PermissionError as e:
            QtWidgets.QMessageBox.warning(
                self.main_window, 'Failed to save file',
                str(e))

    def save_as(self):
        """
        Saves the currend editor content as.
        """
        try:
            fn = self.ui.tabWidgetEditors.save_current_as()
        except PermissionError:
            QtWidgets.QMessageBox.warning(
                self.main_window, 'Failed to save file',
                'Failed to save file, permission error...')
        else:
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
