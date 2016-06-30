"""
This module contains the file system tree view.
"""
import sys
import fnmatch
import locale
import logging
import os
import platform
import shutil
import subprocess
from pyqode.qt import QtCore, QtGui, QtWidgets
from pyqode.core import icons


def _logger():
    return logging.getLogger(__name__)


def debug(msg, *args):
    return _logger().log(5, msg, *args)


class FileSystemTreeView(QtWidgets.QTreeView):
    """
    Extends QtWidgets.QTreeView with a filterable file system model.

    To exclude directories or extension, just set
    :attr:`FilterProxyModel.ignored_directories` and
    :attr:`FilterProxyModel.ignored_extensions`.

    Provides methods to retrieve file info from model index.

    By default there is no context menu and no file operations are possible. We
    provide a standard context menu with basic file system operation (
    :class:`FileSystemContextMenu`) that you can extend and
    set on the tree view using :meth:`FileSystemTreeView.set_context_menu`

    """
    class FilterProxyModel(QtCore.QSortFilterProxyModel):
        """
        Excludes :attr:`ignored_directories` and :attr:`ignored_extensions`
        from the file system model.
        """
        def __init__(self):
            super(FileSystemTreeView.FilterProxyModel, self).__init__()
            #: The list of file extension to exclude
            self.ignored_patterns = [
                '*.pyc', '*.pyo', '*.coverage', '.DS_Store', '__pycache__']
            self._ignored_unused = []

        def set_root_path(self, path):
            """
            Sets the root path to watch.
            :param path: root path (str).
            """
            self._ignored_unused[:] = []
            self._root = path
            parent_dir = os.path.dirname(path)
            for item in os.listdir(parent_dir):
                item_path = os.path.join(parent_dir, item)
                if item_path != path:
                    self._ignored_unused.append(os.path.normpath(item_path))

        def filterAcceptsRow(self, row, parent):
            index0 = self.sourceModel().index(row, 0, parent)
            finfo = self.sourceModel().fileInfo(index0)
            fn = finfo.fileName()
            fp = os.path.normpath(finfo.filePath())
            if os.path.ismount(self._root):
                return True
            if fp in self._ignored_unused:
                return False
            for ptrn in self.ignored_patterns:
                if fnmatch.fnmatch(fn, ptrn):
                    return False
            debug('accepting %s', finfo.filePath())
            return True

    #: signal emitted when the user deleted a file or a directory
    #: Deprecated, use files_deleted instead.
    #: Parameters:
    #: - path (str): path of the file that got deleted
    #: Note that if the removed path is a directory, this signal will be emitted for every file
    #: found recursively in the parent directory
    file_deleted = QtCore.Signal(str)

    #: Signal emitted when the user deleted a file or a directory,
    #: it is emitted only once with all the files deleted.
    files_deleted = QtCore.Signal(list)

    #: signal emitted when the user renamed a file or a directory
    #: Deprecated, use files_renamed instead.
    #: Parameters:
    #: - old (str): old path
    #: - new (str): new path
    file_renamed = QtCore.Signal(str, str)

    #: Signal emitted when the user renamed a file or a directory,
    #: it is emitted once with all the renamed files (not directgories)
    files_renamed = QtCore.Signal(list)

    #: signal emitted when the user created a file
    #: Parameters:
    #: - path (str): path of the file that got created
    file_created = QtCore.Signal(str)

    #: signal emitted just before the context menu is shown
    #: Parameters:
    #:   - file path: current file path.
    about_to_show_context_menu = QtCore.Signal(str)

    def __init__(self, parent=None):
        super(FileSystemTreeView, self).__init__(parent)
        self._path_to_set = None
        self._path_to_select = None
        self.context_menu = None
        self._root_path = None
        self.root_path = ''
        self.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_context_menu)
        self.helper = FileSystemHelper(self)
        self.setSelectionMode(self.ExtendedSelection)
        self._ignored_patterns = []
        self._icon_provider = QtWidgets.QFileIconProvider()
        self._hide_extra_colums = True
        from pyqode.core.widgets import FileIconProvider
        self.set_icon_provider(FileIconProvider())

    def showEvent(self, event):
        super(FileSystemTreeView, self).showEvent(event)
        if self._path_to_set:
            self.set_root_path(self._path_to_set, self._hide_extra_colums)
            self._path_to_set = None

    def set_icon_provider(self, icon_provider):
        self._icon_provider = icon_provider

    def ignore_directories(self, *directories):
        """
        Adds the specified directories to the list of ignored directories.

        This must be done before calling set_root_path!

        .. deprecated:: 2.6.1
            Use :func:`add_ignore_pattern` instead.

        :param directories: the directories to ignore
        """
        for d in directories:
            self.add_ignore_patterns(d)

    def ignore_extensions(self, *extensions):
        """
        Adds the specified extensions to the list of ignored directories.

        This must be done before calling set_root_path!


        .. deprecated:: 2.6.1
            Use :func:`add_ignore_pattern` instead.

        :param extensions: the extensions to ignore

        .. note:: extension must have the dot: '.py' and not 'py'
        """
        for ext in extensions:
            self.add_ignore_patterns('*%s' % ext)

    def clear_ignore_patterns(self):
        """
        Clears the list of ignore patterns
        """
        self._ignored_patterns[:] = []

    def add_ignore_patterns(self, *patterns):
        """
        Adds an ignore pattern to the list for ignore patterns.

        Ignore patterns are used to filter out unwanted files or directories
        from the file system model.

        A pattern is a Unix shell-style wildcards. See :mod:`fnmatch` for a
        deeper explanation about the shell-style wildcards.
        """
        for ptrn in patterns:
            if isinstance(ptrn, list):
                for p in ptrn:
                    self._ignored_patterns.append(p)
            else:
                self._ignored_patterns.append(ptrn)

    def set_context_menu(self, context_menu):
        """
        Sets the context menu of the tree view.

        :param context_menu: QMenu
        """
        self.context_menu = context_menu
        self.context_menu.tree_view = self
        self.context_menu.init_actions()
        for action in self.context_menu.actions():
            self.addAction(action)

    def set_root_path(self, path, hide_extra_columns=True):
        """
        Sets the root path to watch
        :param path: root path - str
        :param hide_extra_columns: Hide extra column (size, paths,...)
        """
        if not self.isVisible():
            self._path_to_set = path
            self._hide_extra_colums = hide_extra_columns
            return
        if sys.platform == 'win32' and os.path.splitunc(path)[0]:
            mdl = QtGui.QStandardItemModel(1, 1)
            item = QtGui.QStandardItem(
                QtGui.QIcon.fromTheme(
                    'dialog-warning',
                    QtGui.QIcon(':/pyqode-icons/rc/dialog-warning.png')),
                'UNC pathnames not supported.')
            mdl.setItem(0, 0, item)
            self.setModel(mdl)
            self.root_path = None
            return
        self._hide_extra_colums = hide_extra_columns

        if os.path.isfile(path):
            path = os.path.abspath(os.path.join(path, os.pardir))
        self._fs_model_source = QtWidgets.QFileSystemModel()
        self._fs_model_source.setFilter(QtCore.QDir.Dirs | QtCore.QDir.Files |
                                        QtCore.QDir.NoDotAndDotDot |
                                        QtCore.QDir.Hidden)
        self._fs_model_source.setIconProvider(self._icon_provider)
        self._fs_model_proxy = self.FilterProxyModel()
        for item in self._ignored_patterns:
            self._fs_model_proxy.ignored_patterns.append(item)
        self._fs_model_proxy.setSourceModel(self._fs_model_source)
        self._fs_model_proxy.set_root_path(path)
        # takes parent of the root path, filter will keep only `path`, that
        # way `path` appear as the top level node of the tree
        self._root_path = os.path.dirname(path)
        self.root_path = path
        self._fs_model_source.directoryLoaded.connect(self._on_path_loaded)
        self._fs_model_source.setRootPath(self._root_path)

    def _on_path_loaded(self, path):
        if os.path.normpath(path) != self._root_path:
            return
        try:
            self.setModel(self._fs_model_proxy)
            file_root_index = self._fs_model_source.setRootPath(
                self._root_path)
            root_index = self._fs_model_proxy.mapFromSource(file_root_index)
            self.setRootIndex(root_index)
            if not os.path.ismount(self._root_path):
                self.expandToDepth(0)
            if self._hide_extra_colums:
                self.setHeaderHidden(True)
                for i in range(1, 4):
                    self.hideColumn(i)
            if self._path_to_select:
                self.select_path(self._path_to_select)
                self._path_to_select = None
        except RuntimeError:
            # wrapped C/C++ object of type FileSystemTreeView has been deleted
            return

    def filePath(self, index):
        """
        Gets the file path of the item at the specified ``index``.

        :param index: item index - QModelIndex
        :return: str
        """
        return self._fs_model_source.filePath(
            self._fs_model_proxy.mapToSource(index))

    def fileInfo(self, index):
        """
        Gets the file info of the item at the specified ``index``.

        :param index: item index - QModelIndex
        :return: QFileInfo
        """
        return self._fs_model_source.fileInfo(
            self._fs_model_proxy.mapToSource(index))

    def _show_context_menu(self, point):
        if self.context_menu:
            self.about_to_show_context_menu.emit(
                FileSystemHelper(self).get_current_path())
            self.context_menu.exec_(self.mapToGlobal(point))

    def select_path(self, path):
        if not self.isVisible():
            self._path_to_select = path
        else:
            self.setCurrentIndex(self._fs_model_proxy.mapFromSource(
                self._fs_model_source.index(path)))


class FileSystemHelper:
    """
    File system helper. Helps manipulating the clipboard for file operations
    on the tree view (drag & drop, context menu, ...).
    """
    class _UrlListMimeData(QtCore.QMimeData):
        def __init__(self, copy=True):
            super(FileSystemHelper._UrlListMimeData, self).__init__()
            self.copy = copy

        def set_list(self, urls):
            """
            Sets the lis of urls into the mime type data.
            """
            lst = []
            for url in urls:
                lst.append(bytes(url, encoding=locale.getpreferredencoding()))
            self.setData(self.format(self.copy), b'\n'.join(lst))

        @classmethod
        def list_from(cls, mime_data, copy=True):
            """
            Returns a list of url from mimetype data
            :param mime_data: mime data from which we must read the list of
                urls
            :param copy: True to copy, False to cut
            """
            string = bytes(mime_data.data(cls.format(copy))).decode('utf-8')
            lst = string.split('\n')
            urls = []
            for val in lst:
                urls.append(val)
            return urls

        def formats(self):
            return [self.format(self.copy)]

        @classmethod
        def format(cls, copy=True):
            return 'text/tv-copy-url-list' if copy else 'text/tv-cut-url-list'

    def __init__(self, treeview):
        self.tree_view = treeview

    def copy_to_clipboard(self, copy=True):
        """
        Copies the selected items to the clipboard
        :param copy: True to copy, False to cut.
        """
        urls = self.selected_urls()
        if not urls:
            return
        mime = self._UrlListMimeData(copy)
        mime.set_list(urls)
        clipboard = QtWidgets.QApplication.clipboard()
        clipboard.setMimeData(mime)

    def selected_urls(self):
        """
        Gets the list of selected items file path (url)
        """
        urls = []
        debug('gettings urls')
        for proxy_index in self.tree_view.selectedIndexes():
            finfo = self.tree_view.fileInfo(proxy_index)
            urls.append(finfo.canonicalFilePath())
        debug('selected urls %r' % [str(url) for url in urls])
        return urls

    def paste_from_clipboard(self):
        """
        Pastes files from clipboard.
        """
        to = self.get_current_path()
        if os.path.isfile(to):
            to = os.path.abspath(os.path.join(to, os.pardir))
        mime = QtWidgets.QApplication.clipboard().mimeData()

        paste_operation = None
        if mime.hasFormat(self._UrlListMimeData.format(copy=True)):
            paste_operation = True
        elif mime.hasFormat(self._UrlListMimeData.format(copy=False)):
            paste_operation = False
        if paste_operation is not None:
            self._paste(
                self._UrlListMimeData.list_from(mime, copy=paste_operation),
                to, copy=paste_operation)

    def _paste(self, sources, destination, copy):
        """
        Copies the files listed in ``sources`` to destination. Source are
        removed if copy is set to False.
        """
        for src in sources:
            debug('%s <%s> to <%s>' % (
                'copying' if copy else 'cutting', src, destination))
            perform_copy = True
            ext = os.path.splitext(src)[1]
            original = os.path.splitext(os.path.split(src)[1])[0]
            filename, status = QtWidgets.QInputDialog.getText(
                self.tree_view, _('Copy'), _('New name:'),
                QtWidgets.QLineEdit.Normal, original)
            if filename == '' or not status:
                return
            filename = filename + ext
            final_dest = os.path.join(destination, filename)
            if os.path.exists(final_dest):
                rep = QtWidgets.QMessageBox.question(
                    self.tree_view, _('File exists'),
                    _('File <%s> already exists. Do you want to erase it?') %
                    final_dest,
                    QtWidgets.QMessageBox.Yes | QtWidgets.QMessageBox.No,
                    QtWidgets.QMessageBox.No)
                if rep == QtWidgets.QMessageBox.No:
                    perform_copy = False
            if not perform_copy:
                continue
            try:
                if os.path.isfile(src):
                    shutil.copy(src, final_dest)
                else:
                    shutil.copytree(src, final_dest)
            except (IOError, OSError) as e:
                QtWidgets.QMessageBox.warning(
                    self.tree_view, _('Copy failed'), _('Failed to copy "%s" to "%s".\n\n%s' %
                                                        (src, destination, str(e))))
                _logger().exception('failed to copy "%s" to "%s', src,
                                    destination)
            else:
                debug('file copied %s', src)
            if not copy:
                debug('removing source (cut operation)')
                if os.path.isfile(src):
                    os.remove(src)
                else:
                    shutil.rmtree(src)
                self.tree_view.files_renamed.emit([(src, final_dest)])

    @staticmethod
    def _get_files(path):
        """
        Returns the list of files contained in path (recursively).
        """
        ret_val = []
        for root, _, files in os.walk(path):
            for f in files:
                ret_val.append(os.path.join(root, f))
        return ret_val

    def delete(self):
        """
        Deletes the selected items.
        """
        urls = self.selected_urls()
        rep = QtWidgets.QMessageBox.question(
            self.tree_view, _('Confirm delete'),
            _('Are you sure about deleting the selected files/directories?'),
            QtWidgets.QMessageBox.Yes | QtWidgets.QMessageBox.No,
            QtWidgets.QMessageBox.Yes)
        if rep == QtWidgets.QMessageBox.Yes:
            deleted_files = []
            for fn in urls:
                try:
                    if os.path.isfile(fn):
                        os.remove(fn)
                        deleted_files.append(fn)
                    else:
                        files = self._get_files(fn)
                        shutil.rmtree(fn)
                        deleted_files += files
                except OSError as e:
                    QtWidgets.QMessageBox.warning(
                        self.tree_view, _('Delete failed'),
                        _('Failed to remove "%s".\n\n%s') % (fn, str(e)))
                    _logger().exception('failed to remove %s', fn)
            self.tree_view.files_deleted.emit(deleted_files)
            for d in deleted_files:
                debug('%s removed', d)
                self.tree_view.file_deleted.emit(os.path.normpath(d))

    def get_current_path(self):
        """
        Gets the path of the currently selected item.
        """
        path = self.tree_view.fileInfo(
            self.tree_view.currentIndex()).filePath()
        # https://github.com/pyQode/pyQode/issues/6
        if not path:
            path = self.tree_view.root_path
        return path

    def copy_path_to_clipboard(self):
        """
        Copies the file path to the clipboard
        """
        path = self.get_current_path()
        QtWidgets.QApplication.clipboard().setText(path)
        debug('path copied: %s' % path)

    def rename(self):
        """
        Renames the selected item in the tree view
        """
        src = self.get_current_path()
        pardir, name = os.path.split(src)
        new_name, status = QtWidgets.QInputDialog.getText(
            self.tree_view, _('Rename '), _('New name:'),
            QtWidgets.QLineEdit.Normal, name)
        if status:
            dest = os.path.join(pardir, new_name)
            old_files = []
            if os.path.isdir(src):
                old_files = self._get_files(src)
            else:
                old_files = [src]
            try:
                os.rename(src, dest)
            except OSError as e:
                QtWidgets.QMessageBox.warning(
                    self.tree_view, _('Rename failed'),
                    _('Failed to rename "%s" into "%s".\n\n%s') % (src, dest, str(e)))
            else:
                if os.path.isdir(dest):
                    new_files = self._get_files(dest)
                else:
                    new_files = [dest]
                self.tree_view.file_renamed.emit(os.path.normpath(src),
                                                 os.path.normpath(dest))
                renamed_files = []
                for old_f, new_f in zip(old_files, new_files):
                    self.tree_view.file_renamed.emit(old_f, new_f)
                    renamed_files.append((old_f, new_f))
                # emit all changes in one go
                self.tree_view.files_renamed.emit(renamed_files)

    def create_directory(self):
        """
        Creates a directory under the selected directory (if the selected item
        is a file, the parent directory is used).
        """
        src = self.get_current_path()
        name, status = QtWidgets.QInputDialog.getText(
            self.tree_view, _('Create directory'), _('Name:'),
            QtWidgets.QLineEdit.Normal, '')
        if status:
            fatal_names = ['.', '..']
            for i in fatal_names:
                if i == name:
                    QtWidgets.QMessageBox.critical(
                        self.tree_view, _("Error"), _("Wrong directory name"))
                    return

            if os.path.isfile(src):
                src = os.path.dirname(src)
            dir_name = os.path.join(src, name)
            try:
                os.makedirs(dir_name, exist_ok=True)
            except OSError as e:
                QtWidgets.QMessageBox.warning(
                    self.tree_view, _('Failed to create directory'),
                    _('Failed to create directory: "%s".\n\n%s') % (dir_name, str(e)))

    def create_file(self):
        """
        Creates a file under the current directory.
        """
        src = self.get_current_path()
        name, status = QtWidgets.QInputDialog.getText(
            self.tree_view, _('Create new file'), _('File name:'),
            QtWidgets.QLineEdit.Normal, '')
        if status:
            fatal_names = ['.', '..', os.sep]
            for i in fatal_names:
                if i == name:
                    QtWidgets.QMessageBox.critical(
                        self.tree_view, _("Error"), _("Wrong directory name"))
                    return

            if os.path.isfile(src):
                src = os.path.dirname(src)
            path = os.path.join(src, name)
            try:
                with open(path, 'w'):
                    pass
            except OSError as e:
                QtWidgets.QMessageBox.warning(
                    self.tree_view, _('Failed to create new file'),
                    _('Failed to create file: "%s".\n\n%s') % (path, str(e)))
            else:
                self.tree_view.file_created.emit(os.path.normpath(path))


class FileSystemContextMenu(QtWidgets.QMenu):
    """
    Default context menu for the file system treeview.

    This context menu contains the following actions:
        - Copy
        - Cut
        - Paste
        - Delete
        - Copy path

    .. note:: copy/cut/paste action works only from inside the application
        (e.g. you cannot paste what you copied in the app to the explorer)

    """
    _explorer = None
    _command = None

    def __init__(self):
        super(FileSystemContextMenu, self).__init__()
        #: Reference to the tree view
        self.tree_view = None

    def addAction(self, *args):
        action = super(FileSystemContextMenu, self).addAction(*args)
        if action is None:
            action = args[0]
        action.setShortcutContext(QtCore.Qt.WidgetShortcut)
        self.tree_view.addAction(action)
        return action

    def init_actions(self):
        # New - submenu
        self.menu_new = self.addMenu("&New")
        self.menu_new.setIcon(
            icons.icon('document-new', None, 'fa.plus'))
        # https://github.com/pyQode/pyqode.core/pull/153
        new_user_actions = self.get_new_user_actions()
        if len(new_user_actions) > 0:
            self.menu_new.addSeparator()
            for user_new_action in new_user_actions:
                self.menu_new.addAction(user_new_action)
        # New file
        self.action_create_file = QtWidgets.QAction(_('&File'), self)
        self.action_create_file.triggered.connect(
            self._on_create_file_triggered)
        icon_provider = self.tree_view._icon_provider
        self.action_create_file.setIcon(icon_provider.icon(
            icon_provider.File))
        self.menu_new.addAction(self.action_create_file)
        # New directory
        self.action_create_directory = QtWidgets.QAction(
            _('&Directory'), self)
        self.action_create_directory.triggered.connect(
            self._on_create_directory_triggered)
        self.action_create_directory.setIcon(icon_provider.icon(
            icon_provider.Folder))
        self.menu_new.addAction(self.action_create_directory)
        self.addSeparator()

        # cut
        self.action_cut = QtWidgets.QAction(_('&Cut'), self)
        self.action_cut.setShortcut(QtGui.QKeySequence.Cut)
        self.action_cut.setIcon(icons.icon(
            'edit-cut', ':/pyqode-icons/rc/edit-cut.png', 'fa.cut'))
        self.addAction(self.action_cut)
        self.action_cut.triggered.connect(self._on_cut_triggered)
        # copy
        self.action_copy = QtWidgets.QAction(_('&Copy'), self)
        self.action_copy.setShortcut(QtGui.QKeySequence.Copy)
        self.action_copy.setIcon(icons.icon(
            'edit-copy', ':/pyqode-icons/rc/edit-copy.png', 'fa.copy'))
        self.addAction(self.action_copy)
        self.action_copy.triggered.connect(self._on_copy_triggered)
        # copy path
        self.action_copy_path = QtWidgets.QAction(_('&Copy path'), self)
        self.action_copy_path.setShortcut('Ctrl+Shift+C')
        self.addAction(self.action_copy_path)
        self.action_copy_path.triggered.connect(self._on_copy_path_triggered)
        # Paste
        self.action_paste = QtWidgets.QAction(_('&Paste'), self)
        self.action_paste.setShortcut(QtGui.QKeySequence.Paste)
        self.action_paste.setIcon(icons.icon(
            'edit-paste', ':/pyqode-icons/rc/edit-paste.png', 'fa.paste'))
        self.action_paste.triggered.connect(self._on_paste_triggered)
        self.addAction(self.action_paste)
        self.addSeparator()
        # Rename
        self.action_rename = QtWidgets.QAction(_('&Rename'), self)
        self.action_rename.setShortcut('F2')
        self.action_rename.triggered.connect(self._on_rename_triggered)
        self.action_rename.setIcon(QtGui.QIcon.fromTheme('edit-rename'))
        self.addAction(self.action_rename)
        # Delete
        self.action_delete = QtWidgets.QAction(_('&Delete'), self)
        self.action_delete.setShortcut(QtGui.QKeySequence.Delete)
        self.action_delete.setIcon(icons.icon(
            'edit-delete', ':/pyqode-icons/rc/edit-delete.png', 'fa.remove'))
        self.action_delete.triggered.connect(self._on_delete_triggered)
        self.addAction(self.action_delete)
        self.addSeparator()

        text = 'Show in %s' % self.get_file_explorer_name()
        action = self.action_show_in_explorer = self.addAction(text)
        action.setIcon(QtGui.QIcon.fromTheme('system-file-manager'))
        action.triggered.connect(self._on_show_in_explorer_triggered)
        self._action_show_in_explorer = action

    def update_show_in_explorer_action(self):
        self.action_show_in_explorer.setText(
            _('Show in %s') % self.get_file_explorer_name())

    def get_new_user_actions(self):
        """
        Returns user actions for "new" sub-menu.

        Example::

            def get_new_user_actions(self):
                actions = []
                action_python_package = QtWidgets.QAction(
                    '&Python package', self)
                action_python_package.triggered.connect(
                    self._on_create_python_package_triggered)
                actions.append(action_python_package)
                return actions

        """
        return []

    def _on_copy_triggered(self):
        self.tree_view.helper.copy_to_clipboard()

    def _on_cut_triggered(self):
        self.tree_view.helper.copy_to_clipboard(copy=False)

    def _on_paste_triggered(self):
        self.tree_view.helper.paste_from_clipboard()

    def _on_delete_triggered(self):
        self.tree_view.helper.delete()

    def _on_copy_path_triggered(self):
        self.tree_view.helper.copy_path_to_clipboard()

    def _on_rename_triggered(self):
        self.tree_view.helper.rename()

    def _on_create_directory_triggered(self):
        self.tree_view.helper.create_directory()

    def _on_create_file_triggered(self):
        self.tree_view.helper.create_file()

    @classmethod
    def get_linux_file_explorer(cls):
        if cls._explorer is None:
            try:
                output = subprocess.check_output(
                    ['xdg-mime', 'query', 'default', 'inode/directory']).decode()
            except subprocess.CalledProcessError:
                output = ''
            if output:
                explorer = output.splitlines()[0].replace(
                    '.desktop', '').replace('-folder-handler', '').split(
                        '.')[-1].lower()
                FileSystemContextMenu._explorer = explorer
                return explorer
            return ''
        else:
            return cls._explorer

    @classmethod
    def get_file_explorer_name(cls):
        system = platform.system()
        if system == 'Darwin':
            pgm = 'finder'
        elif system == 'Windows':
            pgm = 'explorer'
        else:
            pgm = cls.get_file_explorer_command().split(' ')[0]
            if os.path.isabs(pgm):
                pgm = os.path.split(pgm)[1]
        return pgm.capitalize()

    def _on_show_in_explorer_triggered(self):
        path = self.tree_view.helper.get_current_path()
        self.show_in_explorer(path, self.tree_view)

    @classmethod
    def get_file_explorer_command(cls):
        if cls._command is None:
            system = platform.system()
            if system == 'Linux':
                explorer = cls.get_linux_file_explorer()
                if explorer in ['nautilus', 'dolphin']:
                    explorer_cmd = '%s --select %s' % (explorer, '%s')
                else:
                    explorer_cmd = '%s %s' % (explorer, '%s')
            elif system == 'Windows':
                explorer_cmd = 'explorer /select,%s'
            elif system == 'Darwin':
                explorer_cmd = 'open -R %s'
            cls._command = explorer_cmd
            return explorer_cmd
        else:
            return cls._command

    @classmethod
    def set_file_explorer_command(cls, command):
        pgm = command.split(' ')[0]
        if os.path.isabs(pgm):
            pgm = os.path.split(pgm)[1]
        cls._explorer = pgm
        cls._command = command

    @classmethod
    def show_in_explorer(cls, path, parent):
        try:
            cmd = cls.get_file_explorer_command() % os.path.normpath(path)
            _logger().info('show file in explorer: %s' % cmd)
            args = cmd.split(' ')
            subprocess.Popen(args)
        except Exception as e:
            QtWidgets.QMessageBox.warning(
                parent, _('Open in explorer'),
                _('Failed to open file in explorer.\n\n%s') % str(e))
