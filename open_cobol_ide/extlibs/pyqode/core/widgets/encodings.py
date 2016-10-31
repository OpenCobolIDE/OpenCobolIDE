"""
This module contains the encodings related widgets (combox, menus,...)
"""
import locale
import logging
from pyqode.core.api import ENCODINGS_MAP, convert_to_codec_key
from pyqode.qt import QtCore, QtWidgets
from pyqode.core.cache import Cache


def _logger():
    return logging.getLogger(__name__)


class EncodingsComboBox(QtWidgets.QComboBox):
    """
    This combo box display the list of user preferred encoding.
    The last item let you choose an additional encoding from the list
    of encodings define in :mod:`pyqode.core.api.encodings` using
    the EncodingsEditorDialog.

    You can also set the current encoding, it will be automatically appended
    if not in the user list or set as the current index.
    """
    @property
    def current_encoding(self):
        """
        Gets/Sets the current encoding
        """
        return self._current_encoding

    @current_encoding.setter
    def current_encoding(self, value):
        self._current_encoding = convert_to_codec_key(value)
        self._refresh_items()

    def __init__(self, parent, default_encoding=locale.getpreferredencoding()):
        super(EncodingsComboBox, self).__init__(parent)
        self._current_encoding = default_encoding
        self._lock = False
        self._refresh_items()
        self.currentIndexChanged.connect(self._on_current_changed)

    def _on_current_changed(self, index):
        if self._lock:
            return
        if index == self.count() - 1:
            from pyqode.core.dialogs import DlgPreferredEncodingsEditor
            if DlgPreferredEncodingsEditor.edit_encoding(self):
                self._refresh_items()
        else:
            self._current_encoding = self.itemData(index, QtCore.Qt.UserRole)

    def _refresh_items(self):
        self._lock = True
        self.clear()
        for i, encoding in enumerate(sorted(Cache().preferred_encodings)):
            encoding = convert_to_codec_key(encoding)
            try:
                alias, lang = ENCODINGS_MAP[encoding]
            except KeyError:
                _logger().warning('KeyError with encoding: %s', encoding)
            else:
                self.addItem('%s (%s)' % (alias, lang))
                self.setItemData(i, encoding, QtCore.Qt.UserRole)
                if encoding == self._current_encoding or \
                        convert_to_codec_key(alias) == self._current_encoding:
                    self.setCurrentIndex(i)
        self.insertSeparator(self.count())
        self.addItem('Add or remove')
        self._lock = False


class EncodingsMenu(QtWidgets.QMenu):
    """
    Implements a menu that show the user preferred encoding and emit
    reload_requested when the user changed the selected encoding.

    This menu is a general purpose menu that you can put anywhere in your
    application but you will have to manage the reload operation yourself.
    For an integrated context menu, prefer using
    :class:`pyqode.core.widgets.EncodingsContextMenu`.

    """
    #: Signal emitted when the user triggered an action.
    reload_requested = QtCore.Signal(str)

    @property
    def current_encoding(self):
        """
        Gets/Sets the current encoding
        """
        return self._current_encoding

    @current_encoding.setter
    def current_encoding(self, value):
        self._current_encoding = convert_to_codec_key(value)
        self._refresh()

    def __init__(self, title=_('Encodings'), parent=None,
                 selected_encoding=locale.getpreferredencoding()):
        super(EncodingsMenu, self).__init__(parent)
        self.setTitle(title)
        self._group = QtWidgets.QActionGroup(self)
        self._edit_action = None
        self._current_encoding = ''
        self.current_encoding = selected_encoding

    def _clear_actions(self):
        for action in self._group.actions():
            self._group.removeAction(action)
            self.removeAction(action)
        self.removeAction(self._edit_action)

    def _refresh(self):
        self._clear_actions()
        for i, encoding in enumerate(sorted(Cache().preferred_encodings)):
            encoding = convert_to_codec_key(encoding)
            try:
                alias, lang = ENCODINGS_MAP[encoding]
            except KeyError:
                _logger().warn('KeyError with encoding:', encoding)
            else:
                action = QtWidgets.QAction('%s (%s)' % (alias, lang), self)
                action.setData(encoding)
                action.setCheckable(True)
                if encoding == self._current_encoding or \
                        convert_to_codec_key(alias) == self._current_encoding:
                    action.setChecked(True)
                self.addAction(action)
                self._group.addAction(action)
        self._group.triggered.connect(self._on_encoding_triggered)
        self.addSeparator()
        self._edit_action = QtWidgets.QAction(_('Add or remove'), self)
        self._edit_action.triggered.connect(self._on_edit_requested)
        self.addAction(self._edit_action)

    def _on_edit_requested(self):
        from pyqode.core.dialogs import DlgPreferredEncodingsEditor
        if DlgPreferredEncodingsEditor.edit_encoding(self):
            self._refresh()

    def _on_encoding_triggered(self, action):
        self.reload_requested.emit(action.data())


class EncodingsContextMenu(EncodingsMenu):
    """
    Extends the encoding menu to be tightly coupled with a CodeEdit instance
    for an easier integration (automatically reload the editor file content
    when the encoding changed).

    The parent widget of the menu must be set to a CodeEdit instance.
    """
    def __init__(self, title='Encodings', parent=None,
                 selected_encoding=locale.getpreferredencoding()):
        from pyqode.core.api import CodeEdit
        assert isinstance(parent, CodeEdit)
        super(EncodingsContextMenu, self).__init__(
            title, parent, selected_encoding)
        self.reload_requested.connect(self._on_reload_requested)
        parent.new_text_set.connect(self._refresh)
        sep = QtWidgets.QAction(self.parent())
        sep.setSeparator(True)
        self.parent().add_menu(self)
        self._timer = QtCore.QTimer()
        self._timer.setInterval(1)
        self._timer.timeout.connect(self._reload)
        self._refresh()

    def _refresh(self):
        self._current_encoding = convert_to_codec_key(
            self.parent().file.encoding)
        super(EncodingsContextMenu, self)._refresh()

    def _on_reload_requested(self, encoding):
        self._encoding = encoding
        self._timer.start()

    def _reload(self):
        self._timer.stop()
        try:
            self.parent().file.reload(self._encoding)
        except UnicodeDecodeError:
            QtWidgets.QMessageBox.warning(
                self.parent(), _('Decoding error'),
                _('Failed to reload file with encoding %s') % self._encoding)
        else:
            try:
                from pyqode.core.panels import EncodingPanel
                panel = self.parent().panels.get(EncodingPanel)
            except KeyError:
                pass
            else:
                panel.close_panel()
