"""
Contains cobol specific modes
"""
import os
from PyQt4.QtCore import Qt, QFileInfo
from PyQt4.QtGui import QTextCursor, QAction
from pyqode.core import Mode, RightMarginMode
from pyqode.core import CheckerMode, CHECK_TRIGGER_TXT_SAVED
from oci import cobol, constants


class ToUpperMode(Mode):
    """
    Your mode documentation goes here
    """
    IDENTIFIER = "toUpperMode"
    DESCRIPTION = "Automatically transform alpha char to upper case"

    def _onStateChanged(self, state):
        """
        Called when the mode is activated/deactivated
        """
        if state:
            self.editor.keyPressed.connect(self.__onKeyPressed)
        else:
            self.editor.keyPressed.disconnect(self.__onKeyPressed)

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
            line_before_cursor = unicode(tc.selectedText())
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

    def _onStateChanged(self, state):
        """
        Called when the mode is activated/deactivated
        """
        if state:
            self.action = QAction("Comment/Uncomment", self.editor)
            self.action.setShortcut("Ctrl+/")
            self.action.triggered.connect(self.comment)
            self.separator = self.editor.addSeparator()
            self.editor.addAction(self.action)
        else:
            self.editor.removeAction(self.action)
            self.editor.removeAction(self.separator)

    def comment(self):
        cursor = self.editor.textCursor()
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

    def _onInstall(self, editor):
        RightMarginMode._onInstall(self, editor)
        # adapt the right margin automatically
        self.editor.settings.setValue("rightMarginPos", 72)
        self.marginPos = int(self.editor.settings.addProperty(
            "leftMarginPos", "7"))

    def _onSettingsChanged(self, section, key):
        #RightMarginMode._onSettingsChanged(self, section, key)
        if key == "leftMarginPos" or not key:
            self.marginPos = self.editor.settings.value("leftMarginPos")


def checkFile(queue, code, filePath, fileEncoding):
    tmp = os.path.join(constants.getAppTempDirectory(),
                       QFileInfo(filePath).fileName())
    with open(tmp, 'wb') as f:
        f.write(code.encode(fileEncoding))
    fileType = cobol.detectFileType(tmp)
    output = os.path.join(constants.getAppTempDirectory(),
                          QFileInfo(tmp).baseName() + fileType[2])
    status, messages = cobol.compile(tmp, fileType, outputFilename=output)
    queue.put(messages)


class CobolCheckerMode(CheckerMode):
    IDENTIFIER = "cobolCheckerMode"
    DESCRIPTION = "Checks your cobol code on the fly (by compiling it to a " \
                  "temp file"

    def __init__(self):
        CheckerMode.__init__(self, checkFile)

