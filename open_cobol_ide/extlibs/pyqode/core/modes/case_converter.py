# -*- coding: utf-8 -*-
"""
Contains a case converter mode.
"""
from pyqode.core.api import TextHelper
from pyqode.core.api.mode import Mode
from pyqode.qt import QtCore, QtWidgets


class CaseConverterMode(Mode):
    """ Provides context actions for converting case of the selected text.

    Converts selected text to lower case or UPPER case.

    It does so by adding two new menu entries to the editor's context menu:
      - *Convert to lower case*: ctrl-u
      - *Convert to UPPER CASE*: ctrl+shift+u
    """
    def __init__(self):
        Mode.__init__(self)
        self._actions_created = False
        self.action_to_lower = None
        self.action_to_upper = None

    def to_upper(self):
        """
        Converts selected text to upper
        """
        TextHelper(self.editor).selected_text_to_upper()

    def to_lower(self):
        """
        Converts selected text to lower
        """
        TextHelper(self.editor).selected_text_to_lower()

    def _create_actions(self):
        """ Create associated actions """
        self.action_to_lower = QtWidgets.QAction(self.editor)
        self.action_to_lower.triggered.connect(self.to_lower)
        self.action_to_upper = QtWidgets.QAction(self.editor)
        self.action_to_upper.triggered.connect(self.to_upper)
        self.action_to_lower.setText(_('Convert to lower case'))
        self.action_to_lower.setShortcut('Ctrl+U')
        self.action_to_upper.setText(_('Convert to UPPER CASE'))
        self.action_to_upper.setShortcut('Ctrl+Shift+U')
        self.menu = QtWidgets.QMenu(_('Case'), self.editor)
        self.menu.addAction(self.action_to_lower)
        self.menu.addAction(self.action_to_upper)
        self._actions_created = True

    def on_state_changed(self, state):
        if state:
            if not self._actions_created:
                self._create_actions()
            self.editor.add_action(self.menu.menuAction())
        else:
            self.editor.remove_action(self.menu.menuAction())
