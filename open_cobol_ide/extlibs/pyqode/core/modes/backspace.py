"""
This module contains the smart backspace mode
"""
from pyqode.qt import QtCore, QtGui
from pyqode.core.api import Mode


class SmartBackSpaceMode(Mode):
    """ Improves backspace behaviour.

    When you press backspace and there are spaces on the left of the cursor,
    those spaces will be deleted (at most tab_len spaces).

    Basically this turns backspace into Shitf+Tab
    """
    def on_state_changed(self, state):
        if state:
            self.editor.key_pressed.connect(self._on_key_pressed)
        else:
            self.editor.key_pressed.disconnect(self._on_key_pressed)

    def _on_key_pressed(self, event):
        no_modifiers = int(event.modifiers()) == QtCore.Qt.NoModifier
        if event.key() == QtCore.Qt.Key_Backspace and no_modifiers:
            if self.editor.textCursor().atBlockStart():
                return
            tab_len = self.editor.tab_length
            tab_len = self.editor.textCursor().positionInBlock() % tab_len
            if tab_len == 0:
                tab_len = self.editor.tab_length
            # count the number of spaces deletable, stop at tab len
            spaces = 0
            cursor = QtGui.QTextCursor(self.editor.textCursor())
            while spaces < tab_len or cursor.atBlockStart():
                pos = cursor.position()
                cursor.movePosition(cursor.Left, cursor.KeepAnchor)
                char = cursor.selectedText()
                if char == " ":
                    spaces += 1
                else:
                    break
                cursor.setPosition(pos - 1)
            cursor = self.editor.textCursor()
            if spaces == 0:
                return
            cursor.beginEditBlock()
            for _ in range(spaces):
                cursor.deletePreviousChar()
            cursor.endEditBlock()
            self.editor.setTextCursor(cursor)
            event.accept()
