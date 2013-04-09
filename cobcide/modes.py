#!/usr/bin/env python
# This file is part of cobcide.
#
# cobcide is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# cobcide is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with cobcide.  If not, see <http://www.gnu.org/licenses/>.
"""
Contains cobol specific modes:
  - ToUpperMode
  - DocumentAnalyserMode
"""
from PySide.QtCore import QObject, Qt
from PySide.QtCore import Signal
from PySide.QtGui import QKeyEvent
from PySide.QtGui import QTextCursor

from pcef.core import Mode
from pcef.panels.folding import FoldPanel

from cobcide import cobol
from cobcide.cobol import DocumentNode


class ToUpperMode(Mode):
    """
    Your mode documentation goes here
    """
    NAME = "ToUpper"
    DESCRIPTION = "Automatically transform alpha char to upper case"

    def __init__(self):
        Mode.__init__(self, self.NAME, self.DESCRIPTION)

    def _onStateChanged(self, state):
        """
        Called when the mode is activated/deactivated
        """
        if state:
            self.editor.codeEdit.keyPressed.connect(self.__onKeyPressed)
        else:
            self.editor.codeEdit.keyPressed.disconnect(self.__onKeyPressed)

    def __onKeyPressed(self, ev):
        """
        :type ev: QKeyEvent
        """
        if ev.text().isalpha() and ev.text().islower():
            tc = self.editor.codeEdit.textCursor()
            assert isinstance(tc, QTextCursor)
            pos = tc.position()
            anchor = tc.anchor()
            tc.movePosition(QTextCursor.StartOfLine, QTextCursor.KeepAnchor)
            line_before_cursor = str(tc.selectedText())
            tc.setPosition(pos)
            tc.setPosition(anchor, QTextCursor.KeepAnchor)
            # pas en comment (no start with *)
            if not(line_before_cursor.count("*") or
                   line_before_cursor.count("'") % 2 != 0 or
                   line_before_cursor.count('"') % 2 != 0):
                ev.stop = True
                tc.insertText(ev.text().upper())


class DocumentAnalyserMode(Mode, QObject):
    """
    Your mode documentation goes here
    """
    NAME = "DocAnalyser"
    DESCRIPTION = "Analyse document on new text or when the text is saved"

    #: Signal emitted when the document layout changed
    documentLayoutChanged = Signal()

    @property
    def root_node(self):
        """
        Returns the document root node.
        """
        return self.__root_node

    @property
    def variables(self):
        """
        Returns the list of variable document nodes
        """
        return self.__vars

    @property
    def paragraphs(self):
        """
        Returns the list of paragraphs document nodes
        """
        return self.__paragraphs

    def __init__(self):
        QObject.__init__(self)
        Mode.__init__(self, self.NAME, self.DESCRIPTION)
        self.__root_node = None
        self.__vars = []
        self.__paragraphs = []

    def _onStateChanged(self, state):
        """
        Called when the mode is activated/deactivated
        """
        if state:
            self.editor.codeEdit.newTextSet.connect(self.parse)
            self.editor.codeEdit.textSaved.connect(self.parse)
        else:
            self.editor.codeEdit.newTextSet.disconnect(self.parse)
            self.editor.codeEdit.textSaved.disconnect(self.parse)

    def parse(self):
        """ Parse the document layout.

        To get the results, use the following properties:
            - root_node
            - variables
            - paragraphs
        """
        try:
            root_node, variables, paragraphs = cobol.parse_document_layout(
                self.editor.codeEdit.tagFilename)
            changed = False
            if(self.__root_node is None or
               cobol.cmp_doc_node(root_node, self.__root_node)):
                changed = True
            self.__root_node = root_node
            self.__vars = variables
            self.__paragraphs = paragraphs
            if changed:
                self.documentLayoutChanged.emit()
        except TypeError or IOError:
            pass


class FolderMode(Mode):
    """
    Mode that manage the fold panel using the DocumentAnalyser
    """
    NAME = "FolderMode"
    DESCRIPTION = "Manage the fold panel"

    def __init__(self):
        Mode.__init__(self, self.NAME, self.DESCRIPTION)

    def _onStateChanged(self, state):
        """
        Called when the mode is activated/deactivated
        """
        if state:
            self.editor.documentAnalyserMode.documentLayoutChanged.connect(
                self.__on_documentLayoutChanged)
        else:
            self.editor.codeEdit.keyPressed.documentLayoutChanged.disconnect(
                self.__on_documentLayoutChanged)

    def __on_documentLayoutChanged(self):
        root_node = self.editor.documentAnalyserMode.root_node
        paragraphes = self.editor.documentAnalyserMode.paragraphs
        foldPanel = self.editor.foldPanel
        assert isinstance(foldPanel, FoldPanel)
        foldPanel.clearIndicators()
        for div_node in root_node.children:
            if div_node.end_line - div_node.line > 1:
                foldPanel.addIndicator(div_node.line, div_node.end_line)
            for section_node in div_node.children:
                if section_node.node_type == DocumentNode.Type.Section:
                    if section_node.end_line - section_node.line > 1:
                        foldPanel.addIndicator(
                            section_node.line, section_node.end_line)
        for p in paragraphes:
            if p.end_line - p.line > 1:
                foldPanel.addIndicator(p.line, p.end_line)


class CommentsMode(Mode):
    """
    Mode that allow to comment/uncomment a set of lines.
    """
    NAME = "CommentsMode"
    DESCRIPTION = "Comment/uncomment a set of lines"

    def __init__(self):
        Mode.__init__(self, self.NAME, self.DESCRIPTION)

    def _onStateChanged(self, state):
        """
        Called when the mode is activated/deactivated
        """
        if state:
            self.editor.codeEdit.keyPressed.connect(
                self.__on_keyPressed)
        else:
            self.editor.codeEdit.keyPressed.disconnect(
                self.__on_keyPressed)

    def comment(self):
        cursor = self.editor.codeEdit.textCursor()
        cursor.beginEditBlock()
        sel_start = cursor.selectionStart()
        sel_end = cursor.selectionEnd()
        has_selection = True
        if not cursor.hasSelection():
            cursor.select(QTextCursor.LineUnderCursor)
            has_selection = False
        lines = cursor.selection().toPlainText().splitlines()
        nb_lines = len(lines)
        cursor.setPosition(cursor.selectionStart())
        for i in range(nb_lines):
            cursor.movePosition(QTextCursor.StartOfLine)
            cursor.movePosition(QTextCursor.EndOfLine, cursor.KeepAnchor)
            line = cursor.selectedText().lstrip()
            if line != "":
                cursor.movePosition(QTextCursor.StartOfLine)
                # Uncomment
                if line.startswith("*"):
                    cursor.setPosition(cursor.position() + 6)
                    cursor.movePosition(cursor.Right, cursor.KeepAnchor, 1)
                    cursor.insertText("")
                    if i == 0:
                        sel_start -= 1
                        sel_end -= 1
                    else:
                        sel_end -= 1
                # comment
                else:
                    cursor.movePosition(QTextCursor.StartOfLine)
                    cursor.setPosition(cursor.position() + 6)
                    cursor.insertText("*")
                    if i == 0:
                        sel_start += 1
                        sel_end += 1
                    else:
                        sel_end += 1
                # next line
            cursor.movePosition(QTextCursor.EndOfLine)
            cursor.setPosition(cursor.position() + 1)
        cursor.setPosition(sel_start)
        if has_selection:
            cursor.setPosition(sel_end,
                               QTextCursor.KeepAnchor)
        cursor.endEditBlock()
        self.editor.codeEdit.setTextCursor(cursor)

    def __on_keyPressed(self, event):
        if(event.modifiers() & Qt.ControlModifier and
           event.key() == Qt.Key_Slash):
            event.stop = True
            self.comment()
