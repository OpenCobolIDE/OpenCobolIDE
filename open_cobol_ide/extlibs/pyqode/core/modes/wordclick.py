# -*- coding: utf-8 -*-
"""
This module contains the WordClickMode
"""
from pyqode.core.api.decoration import TextDecoration
from pyqode.core.api.mode import Mode
from pyqode.qt import QtCore, QtGui
from pyqode.core.api import TextHelper, DelayJobRunner


class WordClickMode(Mode, QtCore.QObject):
    """ Adds support for word click events.

    It will highlight the click-able word when the user press control and move
    the mouse over a word.

    Detecting whether a word is click-able is the responsability of the
    subclasses. You must override ``_check_word_cursor`` and call
    ``_select_word_cursor`` if this is a click-able word (this
    process might be asynchrone) otherwise _clear_selection.

    :attr:`pyqode.core.modes.WordClickMode.word_clicked` is emitted
    when the word is clicked by the user (while keeping control pressed).
    """
    #: Signal emitted when a word is clicked. The parameter is a
    #: QTextCursor with the clicked word set as the selected text.
    word_clicked = QtCore.Signal(QtGui.QTextCursor)

    def __init__(self):
        QtCore.QObject.__init__(self)
        Mode.__init__(self)
        self._previous_cursor_start = -1
        self._previous_cursor_end = -1
        self._deco = None
        self._cursor = None
        self._timer = DelayJobRunner(delay=200)

    def on_state_changed(self, state):
        if state:
            self.editor.mouse_moved.connect(self._on_mouse_moved)
            self.editor.mouse_released.connect(self._on_mouse_released)
            self.editor.key_released.connect(self._on_key_released)
            self.editor.mouse_double_clicked.connect(
                self._on_mouse_double_clicked)
        else:
            self.editor.mouse_moved.disconnect(self._on_mouse_moved)
            self.editor.mouse_released.disconnect(self._on_mouse_released)
            self.editor.key_released.disconnect(self._on_key_released)
            self.editor.mouse_double_clicked.disconnect(
                self._on_mouse_double_clicked)

    def _on_mouse_double_clicked(self):
        self._timer.cancel_requests()

    def _on_key_released(self, event):
        if event.key() == QtCore.Qt.Key_Control:
            self._clear_selection()
            self._cursor = None

    def _select_word_cursor(self):
        """ Selects the word under the mouse cursor. """
        cursor = TextHelper(self.editor).word_under_mouse_cursor()
        if (self._previous_cursor_start != cursor.selectionStart() and
                self._previous_cursor_end != cursor.selectionEnd()):
            self._remove_decoration()
            self._add_decoration(cursor)
        self._previous_cursor_start = cursor.selectionStart()
        self._previous_cursor_end = cursor.selectionEnd()

    def _clear_selection(self):
        try:
            self._remove_decoration()
        except ValueError:
            pass
        self.editor.set_mouse_cursor(QtCore.Qt.IBeamCursor)
        self._previous_cursor_start = -1
        self._previous_cursor_end = -1

    def _on_mouse_moved(self, event):
        """ mouse moved callback """
        if event.modifiers() & QtCore.Qt.ControlModifier:
            cursor = TextHelper(self.editor).word_under_mouse_cursor()
            if (not self._cursor or
                    cursor.position() != self._cursor.position()):
                self._check_word_cursor(cursor)
            self._cursor = cursor
        else:
            self._cursor = None
            self._clear_selection()

    def _check_word_cursor(self, cursor):
        pass

    def _on_mouse_released(self, event):
        """ mouse pressed callback """
        if event.button() == 1 and self._deco:
            cursor = TextHelper(self.editor).word_under_mouse_cursor()
            if cursor and cursor.selectedText():
                self._timer.request_job(
                    self.word_clicked.emit, cursor)

    def _add_decoration(self, cursor):
        """
        Adds a decoration for the word under ``cursor``.
        """
        if self._deco is None:
            if cursor.selectedText():
                self._deco = TextDecoration(cursor)
                if self.editor.background.lightness() < 128:
                    self._deco.set_foreground(QtGui.QColor('#0681e0'))
                else:
                    self._deco.set_foreground(QtCore.Qt.blue)
                self._deco.set_as_underlined()
                self.editor.decorations.append(self._deco)
                self.editor.set_mouse_cursor(QtCore.Qt.PointingHandCursor)
            else:
                self.editor.set_mouse_cursor(QtCore.Qt.IBeamCursor)

    def _remove_decoration(self):
        """
        Removes the word under cursor's decoration
        """
        if self._deco is not None:
            self.editor.decorations.remove(self._deco)
            self._deco = None
