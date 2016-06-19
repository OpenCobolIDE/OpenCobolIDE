# -*- coding: utf-8 -*-
""" Contains the AutoCompleteMode """
import logging
from pyqode.qt import QtCore, QtGui
from pyqode.core.api import TextHelper
from pyqode.core.api.mode import Mode


class AutoCompleteMode(Mode):
    """ Automatically complete quotes and parentheses

    Generic auto complete mode that automatically completes the following
    symbols:

        - " -> "
        - ' -> '
        - ( -> )
        - [ -> ]
        - { -> }
    """
    def __init__(self):
        super(AutoCompleteMode, self).__init__()
        #: Auto complete mapping, maps input key with completion text.
        self.MAPPING = {'"': '"', "'": "'", "(": ")", "{": "}", "[": "]"}
        #: The format to use for each symbol in mapping when there is a selection
        self.SELECTED_QUOTES_FORMATS = {key: '%s%s%s' for key in self.MAPPING.keys()}
        #: The format to use for each symbol in mapping when there is no selection
        self.QUOTES_FORMATS = {key: '%s' for key in self.MAPPING.keys()}
        self.logger = logging.getLogger(__name__)
        self._ignore_post = False

    def on_state_changed(self, state):
        if state:
            self.editor.post_key_pressed.connect(self._on_post_key_pressed)
            self.editor.key_pressed.connect(self._on_key_pressed)
        else:
            self.editor.post_key_pressed.disconnect(self._on_post_key_pressed)
            self.editor.key_pressed.disconnect(self._on_key_pressed)

    def _on_post_key_pressed(self, event):
        if not event.isAccepted() and not self._ignore_post:
            txt = event.text()
            trav = self.editor.textCursor()
            assert isinstance(trav, QtGui.QTextCursor)
            trav.movePosition(trav.Left, trav.MoveAnchor, 2)
            literal = TextHelper(self.editor).is_comment_or_string(trav)
            if not literal:
                next_char = TextHelper(self.editor).get_right_character()
                if txt in self.MAPPING:
                    to_insert = self.MAPPING[txt]
                    if (not next_char or next_char in self.MAPPING.keys() or
                            next_char in self.MAPPING.values() or
                            next_char.isspace()):
                        TextHelper(self.editor).insert_text(
                            self.QUOTES_FORMATS[txt] % to_insert)
        self._ignore_post = False

    def _on_key_pressed(self, event):
        txt = event.text()
        cursor = self.editor.textCursor()
        from pyqode.qt import QtGui
        assert isinstance(cursor, QtGui.QTextCursor)
        if cursor.hasSelection():
            # quoting of selected text
            if event.text() in self.MAPPING.keys():
                first = event.text()
                last = self.MAPPING[event.text()]
                cursor.insertText(
                    self.SELECTED_QUOTES_FORMATS[event.text()] % (
                        first, cursor.selectedText(), last))
                self.editor.setTextCursor(cursor)
                event.accept()
            else:
                self._ignore_post = True
            return
        next_char = TextHelper(self.editor).get_right_character()
        self.logger.debug('next char: %s', next_char)
        ignore = False
        if event.key() == QtCore.Qt.Key_Backspace:
            # get the character that will get deleted
            tc = self.editor.textCursor()
            pos = tc.position()
            tc.movePosition(tc.Left)
            tc.movePosition(tc.Right, tc.KeepAnchor)
            del_char = tc.selectedText()
            if del_char in self.MAPPING and \
                    self.MAPPING[del_char] == next_char:
                tc.beginEditBlock()
                tc.movePosition(tc.Right, tc.KeepAnchor)
                tc.insertText('')
                tc.setPosition(pos - 2)
                tc.endEditBlock()
                self.editor.setTextCursor(tc)
                ignore = True
        elif txt and next_char == txt and next_char in self.MAPPING:
            ignore = True
        elif event.text() == ')' or event.text() == ']' or event.text() == '}':
            # if typing the same symbol twice, the symbol should not be written
            # and the cursor moved just after the char
            # e.g. if you type ) just before ), the cursor will just move after
            # the existing )
            if next_char == event.text():
                ignore = True
        if ignore:
            event.accept()
            TextHelper(self.editor).clear_selection()
            TextHelper(self.editor).move_right()
