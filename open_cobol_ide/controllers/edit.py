"""
Contains the EditController.

"""
import logging
import os
import weakref
from pyqode.core.api import TextHelper, ColorScheme
from pyqode.qt import QtCore, QtGui, QtWidgets
from .base import Controller
from open_cobol_ide.enums import FileType
from ..view.dialogs.preferences import DlgPreferences
from ..settings import Settings
from ..view.editors import CobolCodeEdit, GenericCodeEdit, TextEdit, \
    update_editor_settings
from ..view.widgets import FSContextMenu


def _logger():
    return logging.getLogger(__name__)


class EditController(Controller):
    """
    Controls the edit view (and the edit menu).
    """
    #: the list of supported editor types
    editor_types = [CobolCodeEdit, GenericCodeEdit, TextEdit]

    @property
    def current_editor(self):
        """
        :rtype: pyqode.cobol.widgets.CobolCodeEdit
        """
        return self.ui.tabWidgetEditors.current_widget()

    def __init__(self, app):
        super().__init__(app)
        self.ui.tabWidgetEditors.editors.clear()
        self.ui.tabWidgetEditors.fallback_editor = GenericCodeEdit
        for editor_type in self.editor_types:
            self.ui.tabWidgetEditors.register_code_edit(editor_type)
        self.ui.tabWidgetEditors.current_changed.connect(
            self._current_changed)
        self.ui.tabWidgetEditors.last_tab_closed.connect(
            self._on_last_tab_closed)
        self.ui.tableWidgetOffsets.show_requested.connect(
            self.ui.dockWidgetOffsets.show)
        self.ui.actionPreferences.triggered.connect(self.edit_preferences)
        self.ui.btPreferences.clicked.connect(self.edit_preferences)
        self.ui.actionPreferences.setShortcut(QtGui.QKeySequence.Preferences)
        if self.ui.actionPreferences.shortcut().toString().strip() == '':
            self.ui.actionPreferences.setShortcut('F2')
        self._setup_status_bar()
        self.ui.tvFileSystem.activated.connect(self._on_tvFileSystem_activated)
        self.ui.tvFileSystem.setHeaderHidden(True)
        self.ui.bt_fs_up.setIcon(QtGui.QIcon.fromTheme(
            'go-up', QtGui.QIcon(':/pyqode-icons/rc/go-up.png')))
        self.ui.bt_fs_up.clicked.connect(self._fs_go_up)
        for i in range(1, 4):
            self.ui.tvFileSystem.hideColumn(i)
        self.ui.tvFileSystem.add_ignore_patterns('bin', '.oci*.cbl')
        mnu = FSContextMenu(self.app)
        self.ui.tvFileSystem.set_context_menu(mnu)
        self.ui.tvFileSystem.file_created.connect(self.app.file.open_file)
        self.ui.tvFileSystem.file_renamed.connect(
            self.ui.tabWidgetEditors.rename_document)
        self.ui.tvFileSystem.file_deleted.connect(
            self.ui.tabWidgetEditors.close_document)
        lock_fs_path = Settings().lock_fs_path
        if lock_fs_path and os.path.exists(lock_fs_path):
            self.ui.tvFileSystem.set_root_path(lock_fs_path)
        else:
            Settings().lock_fs_path = ''
        self.ui.btFSLock.setChecked(lock_fs_path != '')
        self.ui.btFSLock.toggled.connect(self._on_fs_path_lock_toggled)
        self.ui.btNavLock.toggled.connect(self._on_navlock_toggled)
        self.ui.btNavLock.setChecked(not self.ui.twNavigation.sync_with_editor)
        self.ui.twNavigation.sync_with_editor_changed.connect(
            self._on_nav_sync_changed)
        self.ui.actionEnableLinter.toggled.connect(
            self._on_enable_linter_toggled)

    def _on_fs_path_lock_toggled(self, checked):
        if checked:
            Settings().lock_fs_path = \
                self.ui.tabWidgetEditors.current_widget().file.path
        else:
            Settings().lock_fs_path = ''

    def _on_nav_sync_changed(self, sync):
        self.ui.btNavLock.setChecked(not sync)

    def _on_navlock_toggled(self, checked):
        self.ui.btNavLock.blockSignals(True)
        self.ui.twNavigation.sync_with_editor = not checked
        self.ui.btNavLock.blockSignals(False)

    def _on_tvFileSystem_activated(self, index):
        path = self.ui.tvFileSystem.filePath(index)
        if os.path.isfile(path):
            self.app.file.open_file(path)

    def _setup_status_bar(self):
        """
        Adds some labels to the tool bar:
            - format (free or fixed)
            - encoding
            - cursor position (line:column)
        """
        # encoding
        self._lbl_encoding = QtWidgets.QLabel()
        self._lbl_encoding.setFrameShape(QtWidgets.QFrame.NoFrame)
        self._lbl_encoding.setAlignment(
            QtCore.Qt.AlignHCenter | QtCore.Qt.AlignVCenter)
        self._lbl_encoding.setText('UTF-8')
        # format
        self._cb_free_format = QtWidgets.QCheckBox('Free format')
        self._cb_free_format.setChecked(Settings().free_format)
        self._cb_free_format.toggled.connect(self._on_free_format_toggled)
        # cursor position
        self._lbl_cursor = QtWidgets.QLabel()
        self._lbl_cursor.setFrameShape(QtWidgets.QFrame.NoFrame)
        self._lbl_cursor.setAlignment(QtCore.Qt.AlignHCenter |
                                      QtCore.Qt.AlignVCenter)
        self._lbl_cursor.setText('1:1')

        # path
        self._lbl_path = QtWidgets.QLabel()
        self._lbl_path.setFrameShape(QtWidgets.QFrame.NoFrame)
        self._lbl_path.setText('n/a')

        # linter control button
        linter_btn = QtWidgets.QToolButton(self.ui.statusbar)
        linter_btn.setDefaultAction(self.ui.actionEnableLinter)

        self.ui.statusbar.addWidget(self._lbl_path, True)
        self.ui.statusbar.addAction(self.ui.actionEnableLinter)
        self.ui.statusbar.addPermanentWidget(linter_btn)
        self.ui.statusbar.addPermanentWidget(self._cb_free_format)
        self.ui.statusbar.addPermanentWidget(self._lbl_cursor)
        self.ui.statusbar.addPermanentWidget(self._lbl_encoding)
        self.ui.statusbar.setStyleSheet(
            'QStatusBar::item { border: 0px solid black };')

    def add_editor(self, path):
        """
        Adds an editor widget to the main tab widget.

        :param path: path of the file to open
        :param name: name of the tab
        :param mimetype: mimetype of the file to open.
        :return:
        """
        try:
            try:
                editor = self.ui.tabWidgetEditors.open_document(
                    path, preferred_eol=Settings().preferred_eol,
                    autodetect_eol=Settings().autodetect_eol)
            except TypeError:
                editor = self.ui.tabWidgetEditors.open_document(path)
        except Exception as e:
            QtWidgets.QMessageBox.warning(
                self.main_window, 'Failed to open path',
                'Failed to open path: %s' % e)
        else:
            try:
                fw = editor.modes.get('FileWatcherMode')
            except KeyError:
                pass
            else:
                fw.file_deleted.connect(self._on_file_deleted)
            editor.app = weakref.ref(self.app)
            update_editor_settings(editor)
            try:
                self.ui.actionEnableLinter.blockSignals(True)
                self.ui.actionEnableLinter.setChecked(Settings().show_errors)
                self.ui.actionEnableLinter.blockSignals(False)
                editor.set_buttons()
            except AttributeError:
                pass
            self.app.cobol.display_file_type(editor)
            editor.cursorPositionChanged.connect(self._update_status_bar_labels)
            self._update_status_bar_labels()
            return editor

    def _on_last_tab_closed(self):
        self.main_window.save_state()
        self.main_window.setWindowTitle(self.app.title)
        self.ui.twNavigation.set_editor(None)
        self.app.view.show_home_page()

    def _current_changed(self, new_index):
        """
        Updates ui when the current editor changed.

        :param new_index: index of the new current editor.
        """
        if new_index != -1:
            editor = self.ui.tabWidgetEditors.current_widget()
            self.main_window.setWindowTitle(
                '%s [%s] - %s' % (
                    editor.file.name, editor.file.path, self.app.title))
            # update tools (outline and offsets table)
            self.ui.twNavigation.set_editor(editor)
            if self.ui.btFSLock.isChecked() and \
                    not os.path.exists(Settings().lock_fs_path):
                Settings().lock_fs_path = ''
                self.ui.btFSLock.setChecked(False)
            if not self.ui.btFSLock.isChecked():
                new_root = os.path.dirname(editor.file.path)
                if new_root != self.ui.tvFileSystem.root_path:
                    self.ui.tvFileSystem.set_root_path(new_root)
            self.ui.tableWidgetOffsets.set_editor(editor)
            # update linter action
            try:
                self.ui.actionEnableLinter.blockSignals(True)
                self.ui.actionEnableLinter.setChecked(
                    editor.linter_mode.enabled)
                self.ui.actionEnableLinter.blockSignals(False)
            except AttributeError:
                # not a cobol code edit
                self.ui.actionEnableLinter.setEnabled(False)
            else:
                self.ui.actionEnableLinter.setEnabled(True)
            # update current editor menu
            self.ui.mnuActiveEditor.clear()
            self.ui.mnuActiveEditor.addActions(
                [a for a in editor.actions() if not hasattr(
                    a, 'dont_show_in_edit_menu')])
            self.ui.mnuActiveEditor.addSeparator()
            try:
                for m in editor._menus:
                    self.ui.mnuActiveEditor.addMenu(m)
            except AttributeError:
                # not a code edit
                pass

            # update file type menu
            self.app.cobol.display_file_type(editor)
            # update cobol related actions (run/compile
            try:
                is_executable = (self.app.edit.current_editor.file_type ==
                                 FileType.EXECUTABLE)
            except (AttributeError, UnicodeDecodeError):
                # not a cobol editor, or not file not loaded due to an
                # encoding error
                self.ui.menuCobol.setEnabled(False)
                self.app.cobol.enable_compile(False)
                self.app.cobol.enable_run(False)
            else:
                self.ui.menuCobol.setEnabled(True)
                self.app.cobol.enable_compile(
                    not self.ui.consoleOutput.is_running)
                self.app.cobol.enable_run(
                    not self.ui.consoleOutput.is_running)
            self._update_status_bar_labels()
            _logger().debug('current editor changed: %s', editor.file.path)
        else:
            self.ui.actionEnableLinter.setEnabled(False)

    def _get_cursor_pos_in_bytes(self, original, encoding):
        text = TextHelper(self.current_editor).current_line_text()
        text = text[:original]
        try:
            return len(bytes(text, encoding))
        except UnicodeEncodeError:
            return original

    def _update_status_bar_labels(self):
        """
        Updates the status bar labels (format, encoding, cursor position).
        """
        if self.current_editor:
            l, c = TextHelper(self.current_editor).cursor_position()
            encoding = self.current_editor.file.encoding
            if Settings().show_cursor_pos_in_bytes:
                c = self._get_cursor_pos_in_bytes(c, encoding)
            self._lbl_cursor.setText('%d:%d' % (l + 1, c + 1))
            self._lbl_encoding.setText(encoding)
            self._cb_free_format.setEnabled(True)
            self._cb_free_format.setChecked(Settings().free_format)
            self._lbl_path.setText(self.current_editor.file.path)
        else:
            self._lbl_cursor.setText('n/a')
            self._lbl_encoding.setText('n/a')
            self._cb_free_format.setChecked(False)
            self._cb_free_format.setEnabled(False)
            self._lbl_path.setText('n/a')

    def edit_preferences(self):
        try:
            DlgPreferences.edit_preferences(self.main_window)
        except ValueError:
            # dialog canceled
            _logger().info('settings dialog canceled')
        else:
            _logger().info('applying settings')
            self.app.apply_mimetypes_preferences()
            self.app.update_app_style()
            self.app.home.update_style()
            QtGui.QIcon.setThemeName(Settings().icon_theme)
            self._update_status_bar_labels()
            for editor in self.ui.tabWidgetEditors.widgets(
                    include_clones=True):
                update_editor_settings(editor)
                editor.rehighlight()
                try:
                    self.ui.actionEnableLinter.blockSignals(True)
                    self.ui.actionEnableLinter.setChecked(
                        editor.linter_mode.enabled)
                    self.ui.actionEnableLinter.blockSignals(False)
                except AttributeError:
                    # not a cobol code edit
                    self.ui.actionEnableLinter.setEnabled(False)
                else:
                    self.ui.actionEnableLinter.setEnabled(True)

    def _on_file_deleted(self, editor):
        if QtWidgets.QMessageBox.question(
                self.main_window, 'Close editor?',
                '%s has been deleted externally. Do you want to close its '
                'editor?' % editor.file.path,
                QtWidgets.QMessageBox.Yes | QtWidgets.QMessageBox.No,
                QtWidgets.QMessageBox.No) == QtWidgets.QMessageBox.Yes:
            self.ui.tabWidgetEditors.close_document(editor.file.path)

    def _fs_go_up(self):
        """
        Go up in the FileSystem view
        """
        parent_path = os.path.abspath(
            os.path.join(self.ui.tvFileSystem.root_path, os.path.pardir))
        self.ui.tvFileSystem.set_root_path(parent_path)

    def _on_enable_linter_toggled(self, is_checked):
        self.current_editor.linter_mode.enabled = is_checked

    def _on_free_format_toggled(self, state):
        Settings().free_format = state
        for editor in self.ui.tabWidgetEditors.widgets(include_clones=True):
            update_editor_settings(editor)
