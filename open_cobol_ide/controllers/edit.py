"""
Contains the EditController.

"""
from pyqode.core.api import TextHelper
from pyqode.qt import QtCore, QtWidgets
from .base import Controller
from ..settings import Settings
from ..view.editors import CobolCodeEdit, GenericCodeEdit


class EditController(Controller):
    """
    Controls the edit view (and the edit menu).

    """
    #: the list of supported editor types
    editor_types = [CobolCodeEdit, GenericCodeEdit]

    @property
    def current_editor(self):
        """
        :rtype: pyqode.cobol.widgets.CobolCodeEdit
        """
        return self.ui.tabWidgetEditors.currentWidget()

    def __init__(self, app):
        super().__init__(app)
        self.ui.tabWidgetEditors.last_tab_closed.connect(
            self.app.view.show_home)
        self.ui.tabWidgetEditors.currentChanged.connect(
            self._current_changed)
        self.ui.tableWidgetOffsets.show_requested.connect(
            self.ui.dockWidgetOffsets.show)

        # encoding
        self._lbl_encoding = QtWidgets.QLabel()
        self._lbl_encoding.setFrameShape(QtWidgets.QFrame.NoFrame)
        self._lbl_encoding.setAlignment(
            QtCore.Qt.AlignHCenter | QtCore.Qt.AlignVCenter)
        self._lbl_encoding.setText('UTF-8')

        # format
        self._lbl_format = QtWidgets.QLabel()
        self._lbl_format.setFrameShape(QtWidgets.QFrame.NoFrame)
        self._lbl_format.setAlignment(
            QtCore.Qt.AlignHCenter | QtCore.Qt.AlignVCenter)
        self._lbl_format.setText(
            'Free format' if Settings().free_format else 'Fixed format')

        # cursor position
        self._lbl_cursor = QtWidgets.QLabel()
        self._lbl_cursor.setFrameShape(QtWidgets.QFrame.NoFrame)
        self._lbl_cursor.setAlignment(QtCore.Qt.AlignHCenter |
                                     QtCore.Qt.AlignVCenter)
        self._lbl_cursor.setText('1:1')

        self.ui.statusbar.addPermanentWidget(self._lbl_format)
        self.ui.statusbar.addPermanentWidget(self._lbl_cursor)
        self.ui.statusbar.addPermanentWidget(self._lbl_encoding)

        self.ui.statusbar.setStyleSheet(
            'QStatusBar::item { border: 0px solid black };')

    def add_editor(self, path, name, mimetype):
        # pass
        editor = self._editor_from_mimetype(mimetype)
        editor.file.open(path)
        editor.cursorPositionChanged.connect(self._update_status_bar_labels)
        index = self.ui.tabWidgetEditors.add_code_edit(editor, name)
        self.app.cobol.display_file_type(editor)
        self.ui.tabWidgetEditors.setTabToolTip(index, path)
        self._update_status_bar_labels()

    def _editor_from_mimetype(self, mimetype):
        for klass in self.editor_types:
            if mimetype in klass.mimetypes:
                return klass()
        return self.editor_types[-1]()

    def _current_changed(self, new_index):
        if new_index == -1:
            self.main_window.setWindowTitle(
                self.app.title)
        else:
            editor = self.ui.tabWidgetEditors.currentWidget()
            self.main_window.setWindowTitle(
                '[%s] - %s' % (editor.file.path, self.app.title))
            # update tools (outline and offsets table)
            self.ui.twNavigation.set_editor(editor)
            self.ui.tableWidgetOffsets.set_editor(editor)
            # update current editor menu
            self.ui.mnuActiveEditor.clear()
            self._mnu = self.current_editor.get_context_menu()
            self.ui.mnuActiveEditor.addActions(
                self._mnu.actions())
            self.app.cobol.display_file_type(editor)

    def _update_status_bar_labels(self):
        if self.current_editor:
            l, c = TextHelper(self.current_editor).cursor_position()
            self._lbl_cursor.setText('%d:%d' % (l + 1, c + 1))
            self._lbl_encoding.setText(self.current_editor.file.encoding)
            self._lbl_format.setText(
                'Free format' if Settings().free_format else 'Fixed format')
