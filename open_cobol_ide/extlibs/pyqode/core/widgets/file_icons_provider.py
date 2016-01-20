import mimetypes
import sys
from pyqode.qt import QtCore, QtGui, QtWidgets


class FileIconProvider(QtWidgets.QFileIconProvider):
    """
    Provides file/folder icons based on their mimetype.
    """
    plugins = []

    @staticmethod
    def mimetype_icon(path, fallback=None):
        """
        Tries to create an icon from theme using the file mimetype.

        E.g.::

            return self.mimetype_icon(
                path, fallback=':/icons/text-x-python.png')

        :param path: file path for which the icon must be created
        :param fallback: fallback icon path (qrc or file system)
        :returns: QIcon or None if the file mimetype icon could not be found.
        """
        mime = mimetypes.guess_type(path)[0]
        if mime:
            icon = mime.replace('/', '-')
            # if system.WINDOWS:
            #     return icons.file()
            if QtGui.QIcon.hasThemeIcon(icon):
                icon = QtGui.QIcon.fromTheme(icon)
                if not icon.isNull():
                    return icon
        if fallback:
            return QtGui.QIcon(fallback)
        return QtGui.QIcon.fromTheme('text-x-generic')

    def icon(self, type_or_info):
        if 'linux' in sys.platform:
            if isinstance(type_or_info, QtCore.QFileInfo):
                if type_or_info.isDir():
                    return QtGui.QIcon.fromTheme('folder')
                else:
                    ret_val = self.mimetype_icon(
                        type_or_info.absoluteFilePath())
                    return ret_val
            else:
                map = {
                    FileIconProvider.File: QtGui.QIcon.fromTheme('text-x-generic'),
                    FileIconProvider.Folder: QtGui.QIcon.fromTheme('folder'),
                }
                try:
                    return map[type_or_info]
                except KeyError:
                    return super().icon(type_or_info)
        else:
            return QtWidgets.QFileIconProvider().icon(type_or_info)
