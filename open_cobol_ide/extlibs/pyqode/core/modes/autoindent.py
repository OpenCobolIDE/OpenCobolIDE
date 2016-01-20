# -*- coding: utf-8 -*-
""" Contains the automatic generic indenter """
from pyqode.core.api import TextHelper
from pyqode.core.api.mode import Mode
from pyqode.qt.QtCore import Qt


class AutoIndentMode(Mode):
    """ Indents text automatically.
    Generic indenter mode that indents the text when the user press RETURN.

    You can customize this mode by overriding
    :meth:`pyqode.core.modes.AutoIndentMode._get_indent`
    """
    def __init__(self):
        super(AutoIndentMode, self).__init__()

    def _get_indent(self, cursor):
        """
        Return the indentation text (a series of spaces or tabs)

        :param cursor: QTextCursor

        :returns: Tuple (text before new line, text after new line)
        """
        indent = TextHelper(self.editor).line_indent() * ' '
        return "", indent

    def on_state_changed(self, state):
        if state is True:
            self.editor.key_pressed.connect(self._on_key_pressed)
        else:
            self.editor.key_pressed.disconnect(self._on_key_pressed)

    def _on_key_pressed(self, event):
        """
        Auto indent if the released key is the return key.
        :param event: the key event
        """
        if not event.isAccepted():
            if event.key() == Qt.Key_Return or event.key() == Qt.Key_Enter:
                cursor = self.editor.textCursor()
                pre, post = self._get_indent(cursor)
                cursor.beginEditBlock()
                cursor.insertText("%s\n%s" % (pre, post))

                # eats possible whitespaces
                cursor.movePosition(cursor.WordRight, cursor.KeepAnchor)
                txt = cursor.selectedText()
                if txt.startswith(' '):
                    new_txt = txt.replace(" ", '')
                    if len(txt) > len(new_txt):
                        cursor.insertText(new_txt)
                cursor.endEditBlock()
                event.accept()
