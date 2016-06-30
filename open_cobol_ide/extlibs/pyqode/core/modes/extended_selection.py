"""
This module contains the extended selection mode.
"""
from pyqode.qt import QtCore, QtWidgets, QtGui
from pyqode.core.api import Mode, TextHelper


class ExtendedSelectionMode(Mode):
    """ Adds extended selection capabilities (Ctrl/Alt + Double click).

    This mode adds extended selections capabilities to CodeEdit.

    Extended selection is a feature that can be found in the Ulipad editor:
    https://code.google.com/p/ulipad

    It consists in adding a few shortcuts and contextual action to do some
    smarter selections. This mode adds the following new kind of selections:

        - word selection: select word under cursor
        - extended word selection: select word under cursor including
          continuation characters such as ".".
        - matched selection: select text inside quotes or parenthesis
        - line selection: select the whole line
        - select all: select entire document

    Extended selection and matched selection can be performed by combining
    ctrl or alt with a double click (modifiers are configurable through
    ``extended_sel_modifier`` or ``matched_sel_modifier``).

    """

    def __init__(self):
        super(ExtendedSelectionMode, self).__init__()
        self.extended_sel_modifier = QtCore.Qt.ControlModifier
        self.matched_sel_modifier = QtCore.Qt.AltModifier
        self.continuation_characters = ('.',)
        self.word_sel_shortcut = QtGui.QKeySequence('Ctrl+Alt+M')
        self.action_select_word = QtWidgets.QAction(self.editor)
        self.action_select_word.setText(_('Select word'))
        self.action_select_word.setShortcut(self.word_sel_shortcut)
        self.action_select_word.triggered.connect(self.perform_word_selection)
        self.action_select_word.setShortcutContext(
            QtCore.Qt.WidgetShortcut)

        self.extended_sel_shortcut = QtGui.QKeySequence('Ctrl+Shift+M')
        self.action_select_extended_word = QtWidgets.QAction(self.editor)
        self.action_select_extended_word.setText(_('Select extended word'))
        self.action_select_extended_word.setShortcut(
            self.extended_sel_shortcut)
        self.action_select_extended_word.triggered.connect(
            self.perform_extended_selection)
        self.action_select_extended_word.setShortcutContext(
            QtCore.Qt.WidgetShortcut)

        self.matched_sel_shortcut = QtGui.QKeySequence('Ctrl+E')
        self.action_select_matched = QtWidgets.QAction(self.editor)
        self.action_select_matched.setText(_('Matched select'))
        self.action_select_matched.setShortcut(self.matched_sel_shortcut)
        self.action_select_matched.triggered.connect(
            self.perform_matched_selection)
        self.action_select_matched.setShortcutContext(
            QtCore.Qt.WidgetShortcut)

        self.line_sel_shortcut = QtGui.QKeySequence('Ctrl+Shift+L')
        self.action_select_line = QtWidgets.QAction(self.editor)
        self.action_select_line.setText(_('Select line'))
        self.action_select_line.setShortcut(self.line_sel_shortcut)
        self.action_select_line.triggered.connect(self.perform_line_selection)
        self.action_select_line.setShortcutContext(
            QtCore.Qt.WidgetShortcut)

    def create_menu(self):
        """
        Creates the extended selection menu.
        """
        # setup menu
        menu = QtWidgets.QMenu(self.editor)
        menu.setTitle(_('Select'))
        menu.menuAction().setIcon(QtGui.QIcon.fromTheme('edit-select'))
        # setup actions
        menu.addAction(self.action_select_word)
        menu.addAction(self.action_select_extended_word)
        menu.addAction(self.action_select_matched)
        menu.addAction(self.action_select_line)
        menu.addSeparator()
        menu.addAction(self.editor.action_select_all)
        icon = QtGui.QIcon.fromTheme(
            'edit-select-all', QtGui.QIcon(
                ':/pyqode-icons/rc/edit-select-all.png'))
        self.editor.action_select_all.setIcon(icon)
        return menu

    def on_install(self, editor):
        super(ExtendedSelectionMode, self).on_install(editor)
        try:
            self.editor.remove_action(self.editor.action_select_all)
        except (ValueError, AttributeError):
            pass
        self.editor.add_action(self.create_menu().menuAction())
        self.editor.addActions([
            self.action_select_extended_word, self.action_select_line,
            self.action_select_matched, self.action_select_word])

    def on_state_changed(self, state):
        if state:
            self.editor.mouse_double_clicked.connect(self._on_double_click)
        else:
            self.editor.mouse_double_clicked.disconnect(self._on_double_click)

    def _on_double_click(self, event):
        modifiers = event.modifiers()
        if modifiers & self.extended_sel_modifier:
            self.editor.textCursor().clearSelection()
            self.perform_extended_selection(event=event)
        elif modifiers & self.matched_sel_modifier:
            # self.editor.textCursor().clearSelection()
            self.perform_matched_selection(event=event)
        elif int(modifiers) == QtCore.Qt.NoModifier:
            self.perform_word_selection(event=event)

    def perform_word_selection(self, event=None):
        """
        Performs word selection
        :param event: QMouseEvent
        """
        self.editor.setTextCursor(
            TextHelper(self.editor).word_under_cursor(True))
        if event:
            event.accept()

    def perform_extended_selection(self, event=None):
        """
        Performs extended word selection.
        :param event: QMouseEvent
        """
        TextHelper(self.editor).select_extended_word(
            continuation_chars=self.continuation_characters)
        if event:
            event.accept()

    def perform_matched_selection(self, event):
        """
        Performs matched selection.
        :param event: QMouseEvent
        """
        selected = TextHelper(self.editor).match_select()
        if selected and event:
            event.accept()

    def perform_line_selection(self):
        """
        Performs line selection (select the entire line).
        """
        TextHelper(self.editor).select_whole_line()
