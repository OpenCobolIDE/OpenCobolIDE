"""
This module contains the FileController.

"""
import logging
import mimetypes
import os
from pyqode.core import widgets
from pyqode.core.widgets import GenericCodeEdit
from pyqode.qt import QtWidgets
from .base import Controller
from .. import constants
from ..settings import Settings
from ..view.editors import CobolCodeEdit


def _logger():
    return logging.getLogger(__name__)


class FileController(Controller):
    """
    Controls file operations (new file, open, recent files, ...).

    """
    #: the list of supported editor types
    editor_types = [CobolCodeEdit, GenericCodeEdit]

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
        # self.ui.actionNew.triggered.connect(self.request_new)

    def _editor_from_mimetype(self, mimetype):
        for klass in self.editor_types:
            if mimetype in klass.mimetypes:
                return klass()
        return self.editor_types[-1]

    def request_open(self):
        """
        Prompts the user for a file to open and open it.
        """
        filter = "%s%s%s" % (constants.COBOL_FILES_FILTER,
                             constants.FILTER_SEPARATOR,
                             constants.OTHER_FILES_FILTER)
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
            # pass
            _logger().debug('opening file path=%s, name=%s, mimetype=%s' %
                            (path, name, mimetype))
            editor = self._editor_from_mimetype(mimetype)
            editor.file.open(path)
            self.ui.tabWidgetEditors.add_code_edit(editor, name)
        self.app.view.show_editors()
        self.app.file.recent_files_manager.open_file(path)

