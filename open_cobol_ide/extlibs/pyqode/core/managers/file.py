"""
This module contains the file helper implementation

"""
try:
    from future.builtins import open
    from future.builtins import str
except:
    pass  # python 3.2 not supported
import locale
import logging
import mimetypes
import os
from pyqode.core.api.manager import Manager
from pyqode.core.api.utils import TextHelper
from pyqode.qt import QtCore, QtWidgets
from pyqode.core.cache import Cache


# needed on windows
mimetypes.add_type('text/x-python', '.py')
mimetypes.add_type('text/xml', '.ui')


def _logger():
    return logging.getLogger(__name__)


class FileManager(Manager):
    """
    Helps manage file operations:
        - opening and saving files
        - providing file icon
        - detecting mimetype

    Example of usage::

        editor = CodeEdit()
        assert editor.file.path == ''
        # open a file with default locale encoding or using the cached one.
        editor.open(__file__)
        assert editor.file.path == __file__
        print(editor.file.encoding)

        # reload with another encoding
        editor.open(__file__, encoding='cp1252', use_cached_encoding=False)
        assert editor.file.path == __file__
        editor.file.encoding == 'cp1252'

    """
    class EOL:
        """
        This class enumerates the possible EOL conventions:
            - System: apply the system EOL
            - Linux: force the use of Linux EOL (\n)
            - Mac: force the use of Macintosh EOL (\r)
            - Windows: force the use of Windows EOL (\r\n)
        """
        #:
        System = 0
        #: Linux EOL: \n
        Linux = 1
        #: Macintosh EOL: \r
        Mac = 2
        #: Windows EOL: \r\n
        Windows = 3

        _map = {
            System: os.linesep,
            Linux: '\n',
            Mac: '\r',
            Windows: '\r\n'
        }

        @classmethod
        def string(cls, value):
            return cls._map[value]

    @property
    def path(self):
        """ Gets the file path """
        if self._path:
            return os.path.normpath(self._path)
        return ''

    @property
    def name(self):
        """ Gets the file base name """
        return os.path.split(self.path)[1]

    @property
    def extension(self):
        """ Gets the file path """
        return os.path.splitext(self.path)[1]

    @property
    def dirname(self):
        """ Gets the file directory name """
        return os.path.dirname(self._path)

    @property
    def encoding(self):
        """ Gets the file encoding """
        return self._encoding

    @property
    def icon(self):
        """ Gets the file icon, provided by _get_icon """
        return self._get_icon()

    @property
    def autodetect_eol(self):
        return self._autodetect_eol

    @autodetect_eol.setter
    def autodetect_eol(self, value):
        self._autodetect_eol = value
        if not self._autodetect_eol:
            self._eol = self.EOL.string(self._preferred_eol)

    @property
    def preferred_eol(self):
        return self._preferred_eol

    @preferred_eol.setter
    def preferred_eol(self, eol):
        self._preferred_eol = eol
        if not self._autodetect_eol:
            self._eol = self.EOL.string(self._preferred_eol)

    @property
    def file_size_limit(self):
        """
        Returns the file size limit. If the size of the file to open
        is superior to the limit, then we disabled syntax highlighting, code
        folding,... to improve the load time and the runtime performances.

        Default is 10MB.
        """
        return self._limit

    @file_size_limit.setter
    def file_size_limit(self, value):
        self._limit = value

    def _get_icon(self):
        return QtWidgets.QFileIconProvider().icon(QtCore.QFileInfo(self.path))

    def __init__(self, editor, replace_tabs_by_spaces=True):
        """
        :param editor: Code edit instance to work on.
        :param replace_tabs_by_spaces: True to replace tabs by spaces on
            load/save.
        """
        super(FileManager, self).__init__(editor)
        self._limit = 10000000
        self._path = ''
        #: File mimetype
        self.mimetype = ''
        #: store the last file encoding used to open or save the file.
        self._encoding = locale.getpreferredencoding()
        #: True to replace tabs by spaces
        self.replace_tabs_by_spaces = replace_tabs_by_spaces
        #: Opening flag. Set to true during the opening of a file.
        self.opening = False
        #: Saving flag. Set to while saving the editor content to a file.
        self.saving = True
        #: If True, the file is saved to a temporary file first. If the save
        #: went fine, the temporary file is renamed to the final filename.
        self.safe_save = True
        #: True to clean trailing whitespaces of changed lines. Default is
        #: True
        self.clean_trailing_whitespaces = True
        #: True to restore cursor position (if the document has already been
        # opened once).
        self.restore_cursor = True
        #: Preferred EOL convention. This setting will be used for saving the
        #: document unles autodetect_eol is True.
        self._preferred_eol = self.EOL.System
        self._eol = self.EOL.string(self._preferred_eol)
        #: If true, automatically detects file EOL and use it instead of the
        #: preferred EOL when saving files.
        self._autodetect_eol = True

    @staticmethod
    def get_mimetype(path):
        """
        Guesses the mime type of a file. If mime type cannot be detected, plain
        text is assumed.

        :param path: path of the file
        :return: the corresponding mime type.
        """
        filename = os.path.split(path)[1]
        mimetype = mimetypes.guess_type(filename)[0]
        if mimetype is None:
            mimetype = 'text/x-plain'
        _logger().debug('mimetype detected: %s', mimetype)
        return mimetype

    def open(self, path, encoding=None, use_cached_encoding=True):
        """
        Open a file and set its content on the editor widget.

        pyqode does not try to guess encoding. It's up to the client code to
        handle encodings. You can either use a charset detector to detect
        encoding or rely on a settings in your application. It is also up to
        you to handle UnicodeDecodeError, unless you've added
        class:`pyqode.core.panels.EncodingPanel` on the editor.

        pyqode automatically caches file encoding that you can later reuse it
        automatically.

        :param path: Path of the file to open.
        :param encoding: Default file encoding. Default is to use the locale
                         encoding.
        :param use_cached_encoding: True to use the cached encoding instead
            of ``encoding``. Set it to True if you want to force reload with a
            new encoding.

        :raises: UnicodeDecodeError in case of error if no EncodingPanel
            were set on the editor.
        """
        ret_val = False
        if encoding is None:
            encoding = locale.getpreferredencoding()
        self.opening = True
        settings = Cache()
        self._path = path
        # get encoding from cache
        if use_cached_encoding:
            try:
                cached_encoding = settings.get_file_encoding(
                    path, preferred_encoding=encoding)
            except KeyError:
                pass
            else:
                encoding = cached_encoding
        enable_modes = os.path.getsize(path) < self._limit
        for m in self.editor.modes:
            if m.enabled:
                m.enabled = enable_modes
        # open file and get its content
        try:
            with open(path, 'Ur', encoding=encoding) as file:
                content = file.read()
                if self.autodetect_eol:
                    self._eol = file.newlines
                    if isinstance(self._eol, tuple):
                        self._eol = self._eol[0]
                    if self._eol is None:
                        # empty file has no newlines
                        self._eol = self.EOL.string(self.preferred_eol)
                else:
                    self._eol = self.EOL.string(self.preferred_eol)
        except (UnicodeDecodeError, UnicodeError) as e:
            try:
                from pyqode.core.panels import EncodingPanel
                panel = self.editor.panels.get(EncodingPanel)
            except KeyError:
                raise e  # panel not found, not automatic error management
            else:
                panel.on_open_failed(path, encoding)
        else:
            # success! Cache the encoding
            settings.set_file_encoding(path, encoding)
            self._encoding = encoding
            # replace tabs by spaces
            if self.replace_tabs_by_spaces:
                content = content.replace("\t", " " * self.editor.tab_length)
            # set plain text
            self.editor.setPlainText(
                content, self.get_mimetype(path), self.encoding)
            self.editor.setDocumentTitle(self.editor.file.name)
            ret_val = True
            _logger().debug('file open: %s', path)
        self.opening = False
        if self.restore_cursor:
            self._restore_cached_pos()
        self._check_for_readonly()
        return ret_val

    def _check_for_readonly(self):
        self.read_only = not os.access(self.path, os.W_OK)
        self.editor.setReadOnly(self.read_only)

    def _restore_cached_pos(self):
        pos = Cache().get_cursor_position(self.path)
        max_pos = self.editor.document().characterCount()
        if pos > max_pos:
            pos = max_pos - 1
        tc = self.editor.textCursor()
        tc.setPosition(pos)
        self.editor.setTextCursor(tc)
        QtCore.QTimer.singleShot(1, self.editor.centerCursor)

    def reload(self, encoding):
        """
        Reload the file with another encoding.

        :param encoding: the new encoding to use to reload the file.
        """
        assert os.path.exists(self.path)
        self.open(self.path, encoding=encoding,
                  use_cached_encoding=False)

    @staticmethod
    def _rm(tmp_path):
        if os.path.exists(tmp_path):
            os.remove(tmp_path)

    def _reset_selection(self, sel_end, sel_start):
        text_cursor = self.editor.textCursor()
        text_cursor.setPosition(sel_start)
        text_cursor.setPosition(sel_end, text_cursor.KeepAnchor)
        self.editor.setTextCursor(text_cursor)

    def _get_selection(self):
        sel_start = self.editor.textCursor().selectionStart()
        sel_end = self.editor.textCursor().selectionEnd()
        return sel_end, sel_start

    def _get_text(self, encoding):
        lines = self.editor.toPlainText().splitlines()
        if self.clean_trailing_whitespaces:
            lines = [l.rstrip() for l in lines]
        # remove emtpy ending lines
        try:
            last_line = lines[-1]
        except IndexError:
            pass  # empty file
        else:
            while last_line == '':
                try:
                    lines.pop()
                    last_line = lines[-1]
                except IndexError:
                    last_line = None
        text = self._eol.join(lines) + self._eol
        return text.encode(encoding)

    def save(self, path=None, encoding=None, fallback_encoding=None):
        """
        Save the editor content to a file.

        :param path: optional file path. Set it to None to save using the
                     current path (save), set a new path to save as.
        :param encoding: optional encoding, will use the current
                         file encoding if None.
        :param fallback_encoding: Fallback encoding to use in case of encoding
            error. None to use the locale preferred encoding

        """
        if not self.editor.dirty and \
                (encoding is None and encoding == self.encoding) and \
                (path is None and path == self.path):
            # avoid saving if editor not dirty or if encoding or path did not
            # change
            return
        if fallback_encoding is None:
            fallback_encoding = locale.getpreferredencoding()
        _logger().log(
            5, "saving %r with %r encoding", path, encoding)
        if path is None:
            if self.path:
                path = self.path
            else:
                _logger().debug(
                    'failed to save file, path argument cannot be None if '
                    'FileManager.path is also None')
                return False
        # use cached encoding if None were specified
        if encoding is None:
            encoding = self._encoding
        self.saving = True
        self.editor.text_saving.emit(str(path))

        # get file persmission on linux
        try:
            st_mode = os.stat(path).st_mode
        except (ImportError, TypeError, AttributeError, OSError):
            st_mode = None

        # perform a safe save: we first save to a temporary file, if the save
        # succeeded we just rename the temporary file to the final file name
        # and remove it.
        if self.safe_save:
            tmp_path = path + '~'
        else:
            tmp_path = path
        try:
            with open(tmp_path, 'wb') as file:
                file.write(self._get_text(encoding))
        except UnicodeEncodeError:
            # fallback to utf-8 in case of error.
            with open(tmp_path, 'wb') as file:
                file.write(self._get_text(fallback_encoding))
        except (IOError, OSError) as e:
            self._rm(tmp_path)
            self.saving = False
            self.editor.text_saved.emit(str(path))
            raise e
        # cache update encoding
        Cache().set_file_encoding(path, encoding)
        self._encoding = encoding
        # remove path and rename temp file, if safe save is on
        if self.safe_save:
            self._rm(path)
            os.rename(tmp_path, path)
            self._rm(tmp_path)
        # reset dirty flags
        self.editor.document().setModified(False)
        # remember path for next save
        self._path = os.path.normpath(path)
        self.editor.text_saved.emit(str(path))
        self.saving = False
        _logger().debug('file saved: %s', path)
        self._check_for_readonly()

        # restore file permission
        if st_mode:
            try:
                os.chmod(path, st_mode)
            except (ImportError, TypeError, AttributeError):
                pass

    def close(self, clear=True):
        """
        Close the file open in the editor:
            - clear editor content
            - reset file attributes to their default values

        :param clear: True to clear the editor content. Default is True.
        """
        Cache().set_cursor_position(
            self.path, self.editor.textCursor().position())
        self.editor._original_text = ''
        if clear:
            self.editor.clear()
        self._path = ''
        self.mimetype = ''
        self._encoding = locale.getpreferredencoding()

    def clone_settings(self, original):
        self.replace_tabs_by_spaces = original.replace_tabs_by_spaces
        self.safe_save = original.replace_tabs_by_spaces
        self.clean_trailing_whitespaces = original.clean_trailing_whitespaces
        self.restore_cursor = original.restore_cursor
