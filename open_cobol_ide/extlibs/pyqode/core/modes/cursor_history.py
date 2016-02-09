import weakref
from pyqode.qt import QtCore, QtWidgets
from pyqode.core import api


class MoveCursorCommand(QtWidgets.QUndoCommand):
    def __init__(self, new_pos, prev_pos, editor):
        super(MoveCursorCommand, self).__init__(
            '(Goto line %d)' % (new_pos[0] + 1))
        self._new_pos = new_pos
        self._prev_pos = prev_pos
        self._editor = weakref.ref(editor)

    def _move(self, line, column):
        self._editor().blockSignals(True)
        api.TextHelper(self._editor()).goto_line(line, column)
        self._editor().blockSignals(False)
        try:
            caret_mode = self._editor().modes.get('CaretLineHighlighterMode')
        except KeyError:
            pass
        else:
            caret_mode.refresh()

    def redo(self):
        self._move(*self._new_pos)

    def undo(self):
        self._move(*self._prev_pos)


class CursorHistoryMode(api.Mode):
    def __init__(self):
        super(CursorHistoryMode, self).__init__()
        self._prev_pos = 0, 0
        self.undo_stack = QtWidgets.QUndoStack()
        self.undo_stack.setUndoLimit(10)

    def on_state_changed(self, state):
        if state:
            menu = QtWidgets.QMenu(self.editor)
            menu.setTitle(_('Cursor history'))
            self.action_undo = self.undo_stack.createUndoAction(self.editor)
            self.action_undo.setShortcut('Ctrl+Alt+Z')
            self.action_undo.setEnabled(True)
            menu.addAction(self.action_undo)
            self.action_redo = self.undo_stack.createRedoAction(self.editor)
            self.action_redo.setShortcut('Ctrl+Alt+Y')
            menu.addAction(self.action_redo)
            self.editor.add_action(menu.menuAction())
            self.editor.cursorPositionChanged.connect(
                self._on_cursor_position_changed)
            self.editor.key_pressed.connect(self._on_key_pressed)
        else:
            self.editor.cursorPositionChanged.disconnect(
                self._on_cursor_position_changed)
            self.editor.remove_action(self.action_undo)
            self.editor.remove_action(self.action_redo)

    def _on_cursor_position_changed(self):
        if self.editor.textCursor().hasSelection():
            return
        new_pos = api.TextHelper(self.editor).cursor_position()
        if abs(new_pos[0] - self._prev_pos[0]) > 1:
            # only record when line changed and don't record change if the user
            # just wen to the previous/next line
            cmd = MoveCursorCommand(new_pos, self._prev_pos, self.editor)
            self.undo_stack.push(cmd)
        self._prev_pos = new_pos

    def _on_key_pressed(self, event):
        control = event.modifiers() & QtCore.Qt.ControlModifier
        alt = event.modifiers() & QtCore.Qt.AltModifier
        if event.text() in ['Z', 'Y'] and control and alt:
            event.accept()
