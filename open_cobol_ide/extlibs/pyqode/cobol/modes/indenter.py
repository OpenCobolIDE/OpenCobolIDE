# -*- coding: utf-8 -*-
"""
Contains the default indenter.
"""
import logging
from pyqode.core.api import TextHelper
from pyqode.core.api.mode import Mode
from pyqode.qt import QtGui


def _logger():
    return logging.getLogger(__name__)


class IndenterMode(Mode):
    """ Implements classic indentation/tabulation (Tab/Shift+Tab)

    It inserts/removes tabulations (a series of spaces defined by the
    tabLength settings) at the cursor position if there is no selection,
    otherwise it fully indents/un-indents selected lines.

    To trigger an indentation/un-indentation programatically, you must emit
    :attr:`pyqode.core.api.CodeEdit.indent_requested` or
    :attr:`pyqode.core.api.CodeEdit.unindent_requested`.
    """
    @property
    def min_column(self):
        return 0 if self.editor.free_format else 7

    def __init__(self):
        super(IndenterMode, self).__init__()

    def on_state_changed(self, state):
        if state:
            self.editor.indent_requested.connect(self.indent)
            self.editor.unindent_requested.connect(self.unindent)
        else:
            self.editor.indent_requested.disconnect(self.indent)
            self.editor.unindent_requested.disconnect(self.unindent)

    def indent_selection(self, cursor):
        """
        Indent selected text

        :param cursor: QTextCursor
        """
        doc = self.editor.document()
        tab_len = self.editor.tab_length
        cursor.beginEditBlock()
        nb_lines = len(cursor.selection().toPlainText().splitlines())
        if (cursor.atBlockStart() and cursor.position() == cursor.selectionEnd()):
            nb_lines += 1
        block = doc.findBlock(cursor.selectionStart())
        i = 0
        # indent every lines
        while i < nb_lines:
            nb_space_to_add = tab_len
            cursor = QtGui.QTextCursor(block)
            cursor.movePosition(cursor.StartOfLine, cursor.MoveAnchor)
            cursor.movePosition(cursor.Right, cursor.MoveAnchor, self.min_column)
            if self.editor.use_spaces_instead_of_tabs:
                for _ in range(nb_space_to_add):
                    cursor.insertText(" ")
            else:
                cursor.insertText('\t')
            block = block.next()
            i += 1
        cursor.endEditBlock()

    def unindent_selection(self, cursor):
        """
        Un-indents selected text

        :param cursor: QTextCursor
        """
        doc = self.editor.document()
        tab_len = self.editor.tab_length
        nb_lines = len(cursor.selection().toPlainText().splitlines())
        if nb_lines == 0:
            nb_lines = 1
        block = doc.findBlock(cursor.selectionStart())
        assert isinstance(block, QtGui.QTextBlock)
        i = 0
        _logger().debug('unindent selection: %d lines', nb_lines)
        while i < nb_lines:
            txt = block.text()[self.min_column:]
            _logger().debug('line to unindent: %s', txt)
            _logger().debug('self.editor.use_spaces_instead_of_tabs: %r',
                            self.editor.use_spaces_instead_of_tabs)
            if self.editor.use_spaces_instead_of_tabs:
                indentation = len(txt) - len(txt.lstrip())
            else:
                indentation = len(txt) - len(txt.replace('\t', ''))
            _logger().debug('unindent line %d: %d spaces (min indent=%d)', i, indentation, self.min_column)
            if indentation > 0:
                c = QtGui.QTextCursor(block)
                c.movePosition(c.StartOfLine, cursor.MoveAnchor)
                c.movePosition(c.Right, cursor.MoveAnchor, indentation + self.min_column)
                max_spaces = indentation % tab_len
                if max_spaces == 0:
                    max_spaces = tab_len
                spaces = self.count_deletable_spaces(c, max_spaces)
                for _ in range(spaces):
                    c.deletePreviousChar()
            block = block.next()
            i += 1
        return cursor

    def indent(self):
        """
        Indents text at cursor position.
        """
        cursor = self.editor.textCursor()
        assert isinstance(cursor, QtGui.QTextCursor)
        if cursor.hasSelection():
            self.indent_selection(cursor)
        else:
            # simply insert indentation at the cursor position
            tab_len = self.editor.tab_length
            if cursor.positionInBlock() < self.min_column and not cursor.atBlockEnd():
                cursor.movePosition(cursor.Right, cursor.MoveAnchor, self.min_column)
            cursor.beginEditBlock()
            if self.editor.use_spaces_instead_of_tabs:
                nb_space_to_add = tab_len - (cursor.positionInBlock() - self.min_column) % tab_len
                cursor.insertText(nb_space_to_add * " ")
            else:
                cursor.insertText('\t')
            cursor.endEditBlock()
            self.editor.setTextCursor(cursor)

    def count_deletable_spaces(self, cursor, max_spaces):
        # count the number of spaces deletable, stop at tab len
        max_spaces = abs(max_spaces)
        if max_spaces > self.editor.tab_length:
            max_spaces = self.editor.tab_length
        spaces = 0
        trav_cursor = QtGui.QTextCursor(cursor)
        while spaces < max_spaces or trav_cursor.atBlockStart():
            pos = trav_cursor.position()
            trav_cursor.movePosition(cursor.Left, cursor.KeepAnchor)
            char = trav_cursor.selectedText()
            if char == " ":
                spaces += 1
            else:
                break
            trav_cursor.setPosition(pos - 1)
        return spaces

    def unindent(self):
        """
        Un-indents text at cursor position.
        """

        _logger().debug('unindent')
        cursor = self.editor.textCursor()
        _logger().debug('cursor has selection %r', cursor.hasSelection())
        if cursor.hasSelection():
            cursor.beginEditBlock()
            self.unindent_selection(cursor)
            cursor.endEditBlock()
            self.editor.setTextCursor(cursor)
        else:
            tab_len = self.editor.tab_length
            indentation = cursor.positionInBlock()
            indentation -= self.min_column
            if indentation == 0:
                return
            max_spaces = indentation % tab_len
            if max_spaces == 0:
                max_spaces = tab_len
            spaces = self.count_deletable_spaces(cursor, max_spaces)
            _logger().info('deleting %d space before cursor' % spaces)
            cursor.beginEditBlock()
            for _ in range(spaces):
                cursor.deletePreviousChar()
            cursor.endEditBlock()
            self.editor.setTextCursor(cursor)
            _logger().debug(cursor.block().text())
