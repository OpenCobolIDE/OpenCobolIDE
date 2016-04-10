"""
This module contains a class to access the pyQode settings (QSettings).

QSettings are used to cache some specific settings.

At the moment, we use it to store the lists of encoding that appears in
the encoding menu (to not have a too big encoding menu, user can choose which
encoding should be display, offering a small compact menu with all
its favorite encodings). We also cache encoding used to save or load a
file so that we can reuse it automatically next time the user want to
open the same file.

We also use this to cache some editor states (such as the last cursor position
for a specific file path)

We do not store editor styles and settings here. Those kind of settings are
better handled at the application level.

"""
import json
import locale
import logging
from pyqode.qt import QtCore

try:
    from future.builtins import open
    from future.builtins import str
except:
    pass  # python 3.2 not supported


class Cache(object):
    """
    Provides an easy acces to the cache by exposing some wrapper properties
    over QSettings.

    """
    def __init__(self, suffix='', qsettings=None):
        if qsettings is None:
            self._settings = QtCore.QSettings('pyQode', 'pyqode.core%s' % suffix)
        else:
            self._settings = qsettings

    def clear(self):
        """
        Clears the cache.
        """
        self._settings.clear()

    @property
    def preferred_encodings(self):
        """
        The list of user defined encodings, for display in the encodings
        menu/combobox.

        """
        default_encodings = [
            locale.getpreferredencoding().lower().replace('-', '_')]
        if 'utf_8' not in default_encodings:
            default_encodings.append('utf_8')
        default_encodings = list(set(default_encodings))
        return json.loads(self._settings.value(
            'userDefinedEncodings', json.dumps(default_encodings)))

    @preferred_encodings.setter
    def preferred_encodings(self, value):
        from pyqode.core.api import encodings
        lst = [encodings.convert_to_codec_key(v) for v in value]
        self._settings.setValue('userDefinedEncodings',
                                json.dumps(list(set(lst))))

    def get_file_encoding(self, file_path, preferred_encoding=None):
        """
        Gets an eventual cached encoding for file_path.

        Raises a KeyError if no encoding were cached for the specified file
        path.

        :param file_path: path of the file to look up
        :returns: The cached encoding.
        """
        _logger().debug('getting encoding for %s', file_path)
        try:
            map = json.loads(self._settings.value('cachedFileEncodings'))
        except TypeError:
            map = {}
        try:
            return map[file_path]
        except KeyError:
            encodings = self.preferred_encodings
            if preferred_encoding:
                encodings.insert(0, preferred_encoding)
            for encoding in encodings:
                _logger().debug('trying encoding: %s', encoding)
                try:
                    with open(file_path, encoding=encoding) as f:
                        f.read()
                except (UnicodeDecodeError, IOError, OSError):
                    pass
                else:
                    return encoding
            raise KeyError(file_path)

    def set_file_encoding(self, path, encoding):
        """
        Cache encoding for the specified file path.

        :param path: path of the file to cache
        :param encoding: encoding to cache
        """
        try:
            map = json.loads(self._settings.value('cachedFileEncodings'))
        except TypeError:
            map = {}
        map[path] = encoding
        self._settings.setValue('cachedFileEncodings', json.dumps(map))

    def get_cursor_position(self, file_path):
        """
        Gets the cached cursor position for file_path

        :param file_path: path of the file in the cache
        :return: Cached cursor position or (0, 0)
        """
        try:
            map = json.loads(self._settings.value('cachedCursorPosition'))
        except TypeError:
            map = {}
        try:
            pos = map[file_path]
        except KeyError:
            pos = 0
        if isinstance(pos, list):
            # changed in pyqode 2.6.3, now we store the cursor position
            # instead of the line and column  (faster)
            pos = 0
        return pos

    def set_cursor_position(self, path, position):
        """
        Cache encoding for the specified file path.

        :param path: path of the file to cache
        :param position: cursor position to cache
        """
        try:
            map = json.loads(self._settings.value('cachedCursorPosition'))
        except TypeError:
            map = {}
        map[path] = position
        self._settings.setValue('cachedCursorPosition', json.dumps(map))


def _logger():
    return logging.getLogger(__name__)
