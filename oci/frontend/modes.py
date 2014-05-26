# Copyright (c) <2013-2014> Colin Duquesnoy
#
# This file is part of OpenCobolIDE.
#
# OpenCobolIDE is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# OpenCobolIDE is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# OpenCobolIDE. If not, see http://www.gnu.org/licenses/.
"""
Contains cobol specific modes
"""
import logging
import os

from pyqode.core import frontend
from pyqode.qt.QtCore import Qt, QObject, Signal, QTimer, Slot
from pyqode.qt.QtWidgets import QAction
from pyqode.qt.QtGui import QTextCursor, QIcon
from pyqode.core.frontend import Mode
from pyqode.core.frontend.modes import CheckerMode, RightMarginMode

from oci.backend import workers
from oci.backend.parser import cmp_doc_node, parse_ast
from oci.backend.pic_parser import get_field_infos


class ToUpperMode(Mode):
    """
    Your mode documentation goes here
    """
    IDENTIFIER = "toUpperMode"
    DESCRIPTION = "Automatically transform alpha char to upper case"

    def _on_state_changed(self, state):
        """
        Called when the mode is activated/deactivated
        """
        if state:
            self.editor.key_pressed.connect(self.__onKeyPressed)
        else:
            self.editor.key_pressed.disconnect(self.__onKeyPressed)

    def __onKeyPressed(self, ev):
        """
        :type ev: QKeyEvent
        """
        if ev.text().isalpha() and ev.text().islower():
            tc = self.editor.textCursor()
            assert isinstance(tc, QTextCursor)
            pos = tc.position()
            anchor = tc.anchor()
            tc.movePosition(QTextCursor.StartOfLine, QTextCursor.KeepAnchor)
            line_before_cursor = tc.selectedText()
            tc.setPosition(pos)
            tc.setPosition(anchor, QTextCursor.KeepAnchor)
            # pas en comment (no start with *)
            if not(line_before_cursor.count("*") or
                   line_before_cursor.count("'") % 2 != 0 or
                   line_before_cursor.count('"') % 2 != 0):
                ev.accept()
                tc.insertText(ev.text().upper())


class CommentsMode(Mode):
    """
    Mode that allow to comment/uncomment a set of lines.
    """
    IDENTIFIER = "commentsMode"
    DESCRIPTION = "Comments/uncomments a set of lines (Ctrl+/)"

    def _on_state_changed(self, state):
        """
        Called when the mode is activated/deactivated
        """
        if state:
            self.action = QAction("Comment/Uncomment", self.editor)
            self.action.setShortcut("Ctrl+/")
            self.action.triggered.connect(self.comment)
            self.separator = self.editor.add_separator()
            self.editor.add_action(self.action)
        else:
            self.editor.remove_action(self.action)
            self.editor.remove_action(self.separator)

    def comment(self):
        cursor = self.editor.textCursor()
        cursor.beginEditBlock()
        sel_start = cursor.selectionStart()
        sel_end = cursor.selectionEnd()
        reversed_selection = cursor.position() == sel_start
        has_selection = True
        if not cursor.hasSelection():
            cursor.select(QTextCursor.LineUnderCursor)
            has_selection = False
        lines = cursor.selection().toPlainText().splitlines()
        nb_lines = len(lines)
        cursor.setPosition(sel_start)
        comment = False
        for i in range(nb_lines):
            cursor.movePosition(QTextCursor.StartOfLine)
            cursor.movePosition(QTextCursor.EndOfLine, cursor.KeepAnchor)
            line = cursor.selectedText().lstrip()
            if not line.startswith("*"):
                comment = True
                break
            # next line
            cursor.movePosition(QTextCursor.EndOfLine)
            cursor.setPosition(cursor.position() + 1)
        cursor.setPosition(sel_start)
        for i in range(nb_lines):
            cursor.movePosition(QTextCursor.StartOfLine)
            cursor.movePosition(QTextCursor.EndOfLine, cursor.KeepAnchor)
            line = cursor.selectedText().lstrip()
            if line != "":
                cursor.movePosition(QTextCursor.StartOfLine)
                # Uncomment
                if not comment:
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
        cursor.setPosition(sel_start + (1 if not comment else -1))
        cursor.endEditBlock()
        if has_selection:
            pos = sel_end if not reversed_selection else sel_start
            cursor.setPosition(pos, QTextCursor.MoveAnchor)
        else:
            cursor.movePosition(cursor.Down, cursor.MoveAnchor, 1)
        self.editor.setTextCursor(cursor)

    def __on_keyPressed(self, event):
        if(event.modifiers() & Qt.ControlModifier and
           event.key() == Qt.Key_Slash):
            event.accept()
            self.comment()


class LeftMarginMode(RightMarginMode):
    """
    Show a left margin at column 7
    """
    IDENTIFIER = "leftMarginMode"

    def __init__(self):
        super().__init__()

    def _on_install(self, editor):
        super()._on_install(editor)
        self.position = 7


class LinterMode(CheckerMode):
    IDENTIFIER = "cobolCheckerMode"
    DESCRIPTION = "Checks your cobol code on the fly (by compiling it to a " \
                  "temp file"

    def __init__(self):
        CheckerMode.__init__(self, workers.checkFile)


class DocumentAnalyserMode(QObject, Mode):
    """
    Your mode documentation goes here
    """
    IDENTIFIER = "analyserMode"
    DESCRIPTION = "Analyse document when file content is saved/open"

    #: Signal emitted when the document layout changed
    documentLayoutChanged = Signal(object)

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
        Mode.__init__(self)
        self.__root_node = None
        self.__vars = []
        self.__paragraphs = []

    def _on_state_changed(self, state):
        """
        Called when the mode is activated/deactivated
        """
        if state:
            self.editor.new_text_set.connect(self.parse)
            self.editor.text_saved.connect(self.parse)
        else:
            self.editor.new_text_set.disconnect(self.parse)
            self.editor.text_saved.disconnect(self.parse)

    def parse(self):
        """ Parse the document layout.

        To get the results, use the following properties:
            - root_node
            - variables
            - paragraphs
        """
        # preview in preferences dialog have no file path
        if not self.editor.file_path:
            return
        root_node = None
        variables = []
        paragraphs = []
        try:
            root_node, variables, paragraphs = parse_ast(
                self.editor.file_path, encoding=self.editor.file_encoding)
        except (TypeError, IOError):
            # file does not exists
            pass
        except AttributeError:
            # this should never happen but we must exit gracefully
            logging.exception("Failed to parse document, probably due to "
                              "a malformed syntax.")
        changed = False
        if(self.__root_node is None or
           cmp_doc_node(root_node, self.__root_node)):
            changed = True
        self.__root_node = root_node
        self.__vars = variables
        self.__paragraphs = paragraphs
        if changed:
            self.documentLayoutChanged.emit(self.root_node)



class Definition(object):
    """
    Symbol definition: name, line and column
    """
    def __init__(self, line, column, name):
        #: Line number
        self.line = line
        #: Column number
        self.column = column
        self.name = name

    def __str__(self):
        if self.line and self.column:
            return "%s (%s, %s)" % (self.name, self.line, self.column)
        return self.name

    def __repr__(self):
        return "Definition(%r, %r, %r)" % (self.line, self.column, self.name)


class GoToDefinitionMode(Mode, QObject):
    """
    Goes to the assignments (using jedi.Script.goto_assignments). If there are
    more than one assignments, an input dialog is used to ask the user to
    choose the desired assignment.

    This mode will emit :attr:`pyqode.python.GoToAssignmentsMode.outOfDocument`
    if the definition can not be reached in the current document. IDEs will
    typically open a new editor tab and go to the definition.
    """
    IDENTIFIER = "gotoAssignmentsMode"
    DESCRIPTION = "Move the text cursor to the symbol assignments/definitions"

    def __init__(self):
        Mode.__init__(self)
        QObject.__init__(self)
        self._pending = False
        self.aGotToDef = QAction("Go to assignments", self)
        self.aGotToDef.setShortcut("F2")
        self.aGotToDef.triggered.connect(self.requestGoTo)

    def _onInstall(self, editor):
        Mode._onInstall(self, editor)

    def _on_state_changed(self, state):
        if state:
            assert hasattr(self.editor, "wordClickMode")
            self.editor.wordClickMode.word_clicked.connect(self.requestGoTo)
            self.sep = self.editor.add_separator()
            self.editor.add_action(self.aGotToDef)
        else:
            self.editor.wordClickMode.word_clicked.disconnect(self.requestGoTo)
            self.editor.remove_action(self.aGotToDef)
            self.editor.remove_action(self.sep)

    def requestGoTo(self, tc=None):
        """
        Request a go to assignment.

        :param tc: Text cursor which contains the text that we must look for
                   its assignment. Can be None to go to the text that is under
                   the text cursor.
        :type tc: QtGui.QTextCursor
        """
        if not tc:
            tc = frontend.word_under_cursor(self.editor)
        symbol = tc.selectedText()
        analyser = getattr(self.editor, "analyserMode")
        if analyser:
            node = analyser.root_node.find(symbol)
            if node:
                self._definition = node
                QTimer.singleShot(100, self._goToDefinition)

    def _goToDefinition(self):
        line = self._definition.line
        col = self._definition.column
        frontend.goto_line(self.editor,
                           line, move=True, column=col)

    def _makeUnique(self, seq):
        """
        Not performant but works.
        """
        # order preserving
        checked = []
        for e in seq:
            present = False
            for c in checked:
                if str(c) == str(e):
                    present = True
                    break
            if not present:
                checked.append(e)
        return checked


# class CobolFolder(pyqode.core.IndentBasedFoldDetector):
#     def getFoldIndent(self, highlighter, block, text):
#         text = text.upper()
#         indent = int((len(text) - len(text.lstrip())))
#         prev = block.previous()
#         while prev.isValid() and not len(prev.text().strip()):
#             prev = prev.previous()
#         pusd = block.previous().userData()
#         pb = prev
#         if len(text.strip()) == 0:
#             while not len(pb.text().strip()) and pb.isValid():
#                 pb = pb.previous()
#             pbIndent = (len(pb.text()) - len(pb.text().lstrip()))
#             # check next blocks to see if their indent is >= then the last block
#             nb = block.next()
#             while not len(nb.text().strip()) and nb.isValid():
#                 nb = nb.next()
#             nbIndent = (len(nb.text()) - len(nb.text().lstrip()))
#             # print(pb.userState())
#             if nbIndent >= pbIndent or pb.userState() & 0x7F:
#                 if pb.userData():
#                     return pb.userData().foldIndent
#             return -1
#         if indent == 6:
#             if pusd:
#                 return pusd.foldIndent
#             return 0
#         if indent == 7:
#             if "DIVISION" in text:
#                 return 0
#             if "SECTION" in text:
#                 return 2
#             if "END" in text or  "STOP" in text:
#                 return pusd.foldIndent
#             return 3
#         return indent


class OffsetCalculatorMode(QObject, Mode):
    """
    This modes computes the selected PIC fields offsets.

    It adds a "Calculate PIC offsets" action to the editor context menu and
    emits the signal |picInfosAvailable| when the the user triggered the action
    and the pic infos have been computed.
    """
    picInfosAvailable = Signal(list)

    def __init__(self):
        if '4' in os.environ['QT_API']:
            Mode.__init__(self)
        super().__init__()

    def _on_install(self, editor):
        super()._on_install(editor)
        self.action = QAction(editor)
        self.action.setText("Calculate PIC offsets")
        self.action.setIcon(QIcon.fromTheme(
            "accessories-calculator",
            QIcon(":/ide-icons/rc/accessories-calculator.png")))
        editor.add_separator()
        editor.add_action(self.action)
        self.action.triggered.connect(self._computeOffsets)

    @Slot()
    def _computeOffsets(self):
        original_tc = self.editor.textCursor()
        tc = self.editor.textCursor()
        assert isinstance(tc, QTextCursor)
        start = tc.selectionStart()
        end = tc.selectionEnd()
        tc.setPosition(start)
        start_line = tc.blockNumber() + 1
        tc.setPosition(end)
        end_line = tc.blockNumber() + 1
        frontend.select_lines(self.editor, start=start_line, end=end_line,
                              apply_selection=True)
        source = frontend.selected_text(self.editor)
        self.picInfosAvailable.emit(get_field_infos(source))
        self.editor.setTextCursor(original_tc)
