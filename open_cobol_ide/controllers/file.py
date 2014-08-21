"""
This module contains the FileController.

"""
import logging
import mimetypes
import os

from pyqode.core import widgets
from pyqode.qt import QtWidgets

from .base import Controller
from ..view.editors import CobolCodeEdit
from ..settings import Settings
from ..view.dialogs.new_file import DlgNewFile


def _logger():
    return logging.getLogger(__name__)


COBOL_FILES_FILTER = 'Cobol files (%s)' % ' '.join(
    [ext.lower() for ext in CobolCodeEdit.extensions] +
    CobolCodeEdit.extensions).replace('.', '*.')
OTHER_FILES_FILTER = 'Other text files (*)'
FILTER_SEPARATOR = ';;'


class FileController(Controller):
    """
    Controls file operations (new file, open, recent files, ...).

    """
    def __init__(self, app):
        super().__init__(app)
        self.recent_files_manager = widgets.RecentFilesManager(
            'OpenCobolIDE', 'OpenCobolIDE')
        self.menu_recents = widgets.MenuRecentFiles(
            self.ui.menuFile, title='Recents',
            recent_files_manager=self.recent_files_manager)
        self.menu_recents.open_requested.connect(self.open_file)
        self.menu_recents.clear_requested.connect(
            self.recent_files_manager.clear)
        self.ui.menuFile.insertMenu(self.ui.actionQuit, self.menu_recents)
        self.ui.menuFile.insertSeparator(self.ui.actionQuit)
        self.menu_recents.update_actions()
        self.recent_files_manager.updated.connect(
            self.menu_recents.update_actions)
        self.ui.actionOpen.triggered.connect(self.request_open)
        self.ui.actionNew.triggered.connect(self.request_new)
        self.ui.actionSave.triggered.connect(
            self.ui.tabWidgetEditors.save_current)
        self.ui.actionSaveAs.triggered.connect(self._save_as)
        self.ui.actionQuit.triggered.connect(self._on_quit)

    def request_new(self):
        path = DlgNewFile.create_new_file(self.main_window)
        if path:
            self.open_file(path)

    def request_open(self):
        """
        Prompts the user for a file to open and open it.
        """
        filter = '%s%s%s' % (COBOL_FILES_FILTER,
                             FILTER_SEPARATOR,
                             OTHER_FILES_FILTER)
        path, status = QtWidgets.QFileDialog.getOpenFileName(
            self.main_window, 'Open a file', directory=Settings().last_path,
            filter=filter)
        if status:
            self.open_file(path)

    def open_file(self, path):
        name = os.path.split(path)[1]
        mimetype = mimetypes.guess_type(path)[0]
        index = self.ui.tabWidgetEditors.index_from_filename(path)
        if index != -1:
            # already in tab widget.
            self.ui.tabWidgetEditors.setCurrentIndex(index)
        else:
            _logger().debug('opening file path=%s, name=%s, mimetype=%s' %
                            (path, name, mimetype))
            self.app.edit.add_editor(path, name, mimetype)
        self.app.view.show_editors()
        self.app.file.recent_files_manager.open_file(path)

    def _save_as(self):
        filter = '%s%s%s' % (COBOL_FILES_FILTER, FILTER_SEPARATOR,
                             OTHER_FILES_FILTER)
        fn, filter = QtWidgets.QFileDialog.getSaveFileName(
            self.main_window, 'Save file as...',
            self.ui.tabWidgetEditors.currentWidget().file.path, filter)
        if not fn:
            return
        # ensure correct extension
        if os.path.splitext(fn)[1] == '':
            if filter == COBOL_FILES_FILTER:
                fn += '.cbl'
            else:
                fn += '.txt'
        _logger().info('saving editor content as: %s', fn)
        self.ui.tabWidgetEditors.save_current(path=fn)
        self.recent_files_manager.open_file(fn)
        self.ui.tabWidgetEditors.currentChanged.emit(
            self.ui.tabWidgetEditors.currentIndex())
        Settings().last_path = fn

    def _on_quit(self):
        if QtWidgets.QMessageBox.question(
                self.main_window, 'Quit OpenCobolIDE?',
                'Are you sure you want to quit OpenCobolIDE?',
                QtWidgets.QMessageBox.Yes | QtWidgets.QMessageBox.No,
                QtWidgets.QMessageBox.No) == QtWidgets.QMessageBox.Yes:
            _logger().debug('quit action triggered')
            self.app.exit()

