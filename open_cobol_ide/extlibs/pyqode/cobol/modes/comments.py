"""
This module the commenter mode
"""
import os
from pyqode.core.api import Mode
from pyqode.qt import QtCore, QtGui, QtWidgets


class CommentsMode(Mode):
    """
    Comments/uncomments a set of lines (Ctrl+/)

    This mode adds a contextual action to the code editor to let the user
    comment/uncomment the selected lines.

    If no lines were selected, the current line is commented/uncommented and
    the cursor move to the next line.
    """

    def on_state_changed(self, state):
        """
        Called when the mode is activated/deactivated
        """
        if state:
            self.action = QtWidgets.QAction(_("Comment/Uncomment"),
                                            self.editor)
            self.action.setShortcut("Ctrl+/")
            self.action.triggered.connect(self.comment)
            self.editor.add_action(self.action, sub_menu='COBOL')
            # workaround numpad shortcuts not received with Qt5 (they have an
            # accepted bug report for that)
            # TODO: check if we can disable this workaround in a later version
            # of pyqt5 (wrote this in 2014: pyqt 5.3.1).
            if 'pyqt5' in os.environ['QT_API'].lower():
                self.editor.key_pressed.connect(self.on_key_pressed)
        else:
            self.editor.remove_action(self.action, sub_menu='Python')
            if 'pyqt5' in os.environ['QT_API'].lower():
                self.editor.key_pressed.disconnect(self.on_key_pressed)

    def on_key_pressed(self, key_event):
        ctrl = (key_event.modifiers() & QtCore.Qt.ControlModifier ==
                QtCore.Qt.ControlModifier)
        if key_event.key() == QtCore.Qt.Key_Slash and ctrl:
            self.comment()
            key_event.accept()

    def _detect_operation(self, comment_symbol, cursor, nb_lines):
        comment = False
        for i in range(nb_lines):
            cursor.movePosition(QtGui.QTextCursor.StartOfLine)
            cursor.movePosition(QtGui.QTextCursor.EndOfLine, cursor.KeepAnchor)
            line = cursor.selectedText()
            if not self.editor.free_format:
                line = line[6:]
            line = line.strip()
            if not line.startswith(comment_symbol):
                comment = True
                break
            # next line
            cursor.movePosition(QtGui.QTextCursor.EndOfLine)
            cursor.setPosition(cursor.position() + 1)
        return comment

    def comment(self):
        cursor = self.editor.textCursor()
        cursor.beginEditBlock()
        sel_start = cursor.selectionStart()
        sel_end = cursor.selectionEnd()
        has_selection = True
        if not cursor.hasSelection():
            cursor.select(QtGui.QTextCursor.LineUnderCursor)
            has_selection = False
        lines = cursor.selection().toPlainText().splitlines()
        nb_lines = len(lines)
        cursor.setPosition(sel_start)
        comment_symbol = self.editor.comment_indicator
        # check if we must comment or uncomment
        comment = self._detect_operation(comment_symbol, cursor, nb_lines)
        cursor.setPosition(sel_start)
        l = len(comment_symbol)
        for i in range(nb_lines):
            cursor.movePosition(QtGui.QTextCursor.StartOfLine)
            cursor.movePosition(QtGui.QTextCursor.EndOfLine, cursor.KeepAnchor)
            full_line = cursor.selectedText()
            if not self.editor.free_format:
                full_line = 6 * ' ' + full_line[6:]
            line = full_line.lstrip()
            if comment_symbol.strip() == '*':
                indent = 6
            else:
                indent = len(full_line) - len(line)
            if line != "":
                cursor.movePosition(QtGui.QTextCursor.StartOfLine)
                # Uncomment
                if not comment:
                    cursor.setPosition(cursor.position() + indent)
                    cursor.movePosition(cursor.Right, cursor.KeepAnchor, l)
                    cursor.insertText("")
                    if i == 0:
                        sel_start -= l
                        sel_end -= l
                    else:
                        sel_end -= l
                # comment
                else:
                    cursor.movePosition(QtGui.QTextCursor.StartOfLine)
                    cursor.setPosition(cursor.position() + indent)
                    cursor.insertText(comment_symbol)
                    if i == 0:
                        sel_start += l
                        sel_end += l
                    else:
                        sel_end += l
            # next line
            cursor.movePosition(QtGui.QTextCursor.EndOfLine)
            if not cursor.atEnd():
                cursor.setPosition(cursor.position() + 1)
        cursor.endEditBlock()
        if has_selection:
            if comment:
                cursor.setPosition(sel_start - len(comment_symbol))
            else:
                cursor.setPosition(sel_start + len(comment_symbol))
            cursor.setPosition(sel_end, QtGui.QTextCursor.KeepAnchor)
        else:
            if not cursor.atEnd():
                cursor.setPosition(sel_start + (l if not comment else -l))
                cursor.movePosition(cursor.Down, cursor.MoveAnchor, 1)
        self.editor.setTextCursor(cursor)
