# This file is part of open-cobol-ide.
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
Contains the different tabs types (one for cobol files, another for basic text
files)
"""
from pcef import styles
from pygments.token import Comment
from PySide.QtCore import Slot, QRegExp
from pcef.core import CodeEditorWidget
from pcef.modes.indent import AutoIndentMode
from pcef.modes.clh import HighlightLineMode
from pcef.modes.margin import RightMarginMode
from pcef.modes.sh import SyntaxHighlighterMode
from pcef.modes.zoom import EditorZoomMode
from pcef.modes.cc import CodeCompletionMode
from pcef.panels.lines import LineNumberPanel
from pcef.panels.search import SearchPanel
from pcef.panels.misc import CheckersMarkerPanel
from cobcide import FileType
from cobcide.cc import CobolCompletionModel, COBOL_KEYWORDS
from cobcide.errors_manager import ErrorsManager
from cobcide.toupper_mode import ToUpperMode


class CobolEditor(CodeEditorWidget):
    """
    A cobol specific code editor widget.

    **Installed modes**:

        * AutoIndentMode
        * HighlightLineMode
        * RightMarginMode
        * EditorZoomMode
        * CodeCompletionMode

    **Installed panels**:

        * LineNumberPanel
        * SearchPanel
    """

    #---------------------------------------------------------------------------
    # Properties
    #---------------------------------------------------------------------------
    @property
    def rightMarginMode(self):
        """
        :returns: the right margin mode instance.
        :rtype: pcef.modes.margin.RightMarginMode
        """
        return self.mode(RightMarginMode.IDENTIFIER)

    @property
    def syntaxHighlightingMode(self):
        """
        :returns: the syntax highlighting mode instance.
        :rtype: pcef.modes.sh.SyntaxHighlighterMode
        """
        return self.mode(SyntaxHighlighterMode.IDENTIFIER)

    @property
    def highlightLineMode(self):
        """
        :returns: the highlight active line mode instance.
        :rtype: pcef.modes.clh.HighlightLineMode
        """
        m = self.mode(HighlightLineMode.IDENTIFIER)
        assert isinstance(m, HighlightLineMode)
        return m

    @property
    def zoomMode(self):
        """
        :returns: the editor zoom mode.
        :rtype: pcef.modes.zoom.EditorZoomMode
        """
        return self.mode(EditorZoomMode.IDENTIFIER)

    @property
    def autoIndentMode(self):
        """
        :returns: the editor auto indent mode
        :rtype: pcef.modes.indent.AutoIndentMode
        """
        return self.mode(AutoIndentMode.IDENTIFIER)

    @property
    def codeCompletionMode(self):
        """
        :returns: the code completion mode.
        :rtype: pcef.modes.cc.CodeCompletionMode
        """
        return self.mode(CodeCompletionMode.IDENTIFIER)

    @property
    def lineNumberPanel(self):
        """
        :returns: the line number Panel instance.
        :return: pcef.panels.line_numbers.LineNumberPanel
        """
        return self.panel(LineNumberPanel.IDENTIFIER)

    @property
    def searchPanel(self):
        """
        :returns: the search and replace panel instance

        :return: pcef.panels.search_and_replace.SearchPanel
        """
        return self.panel(SearchPanel.IDENTIFIER)

    @property
    def fileType(self):
        return self.__file_type

    @fileType.setter
    def fileType(self, type):
        self.__file_type = type

    @property
    def checkerPanel(self):
        return self.panel(CheckersMarkerPanel.IDENTIFIER)

    #---------------------------------------------------------------------------
    # Methods
    #---------------------------------------------------------------------------
    def __init__(self, parent=None):
        CodeEditorWidget.__init__(self, parent)
        self.__file_type = FileType.Program
        self.errors_manager = None
        # customise editor style (the default pygments style looks awesome for
        # cobol)
        self.currentStyle.pygmentsStyle = "default"
        self.currentStyle.showWhitespaces = False
        self.currentStyle = self.currentStyle
        # Install actions
        self._installActions()
        # Install extensions
        self._install_panels()
        self._install_modes()

    def _install_panels(self):
        self.installPanel(LineNumberPanel(), self.PANEL_ZONE_LEFT)
        self.installPanel(SearchPanel(), self.PANEL_ZONE_BOTTOM)
        self.installPanel(CheckersMarkerPanel(), self.PANEL_ZONE_LEFT)

    def _install_modes(self):
        # convert char to upper
        self.installMode(ToUpperMode())
        # code completion model, uses document words and the cobol keywords
        self.installMode(CodeCompletionMode())
        self.codeCompletionMode.addModel(CobolCompletionModel())
        self.codeCompletionMode.periodIsTrigger = False
        self.codeCompletionMode.minSuggestions = len(COBOL_KEYWORDS) + 20
        # left margin at col = 7
        left_margin = RightMarginMode()
        left_margin.name = "Left Margin"
        left_margin.marginPos = 7
        self.installMode(left_margin)
        # right margin at col = 72
        self.installMode(RightMarginMode())
        self.rightMarginMode.marginPos = 72
        # highlight current line
        self.installMode(HighlightLineMode())
        # zoom in/out
        self.installMode(EditorZoomMode())
        # auto indent to same indent level than the previous line
        self.installMode(AutoIndentMode())
        # use pygment syntax highlighter + custom highlight for comments
        self.installMode(SyntaxHighlighterMode())
        self.syntaxHighlightingMode.highlighter.hilighlightingBlock.connect(
            self._highlighComments)

    def _installActions(self):
        """
        Installs the standard pcef code edit actions
        """
        self.codeEdit.addAction(self.ui.actionUndo)
        self.codeEdit.addAction(self.ui.actionRedo)
        self.codeEdit.addSeparator()
        self.codeEdit.addAction(self.ui.actionCopy)
        self.codeEdit.addAction(self.ui.actionCut)
        self.codeEdit.addAction(self.ui.actionPaste)
        self.codeEdit.addSeparator()
        self.codeEdit.addAction(self.ui.actionSelectAll)
        self.codeEdit.addSeparator()
        self.codeEdit.addAction(self.ui.actionIndent)
        self.ui.actionUnindent.setShortcut("Shift+Tab")  # unable to set it in
        # the designer
        self.codeEdit.addAction(self.ui.actionUnindent)
        self.on_codeEdit_redoAvailable(False)
        self.on_codeEdit_undoAvailable(False)
        self.ui.codeEdit.setUndoRedoEnabled(True)
        self.on_codeEdit_copyAvailable(False)

    #---------------------------------------------------------------------------
    # Slots
    #---------------------------------------------------------------------------
    @Slot(bool)
    def on_codeEdit_undoAvailable(self, available):
        self.ui.actionUndo.setEnabled(available)

    @Slot(bool)
    def on_codeEdit_redoAvailable(self, available):
        self.ui.actionRedo.setEnabled(available)

    @Slot(bool)
    def on_codeEdit_copyAvailable(self, available):
        self.ui.actionCopy.setEnabled(available)
        self.ui.actionCut.setEnabled(available)

    @Slot()
    def on_actionUndo_triggered(self):
        self.codeEdit.undo()

    @Slot()
    def on_actionRedo_triggered(self):
        self.codeEdit.redo()

    @Slot()
    def on_actionPaste_triggered(self):
        self.codeEdit.paste()

    @Slot()
    def on_actionCopy_triggered(self):
        self.codeEdit.copy()

    @Slot()
    def on_actionCut_triggered(self):
        self.codeEdit.cut()

    @Slot()
    def on_actionIndent_triggered(self):
        self.codeEdit.indent(self.TAB_SIZE)

    @Slot()
    def on_actionUnindent_triggered(self):
        self.codeEdit.unIndent(self.TAB_SIZE)

    @Slot()
    def on_actionSelectAll_triggered(self):
        self.codeEdit.selectAll()

    def _highlighComments(self, original_text, highlighter):
        expression = QRegExp('\*.*')
        index = expression.indexIn(original_text, 0)
        while index >= 0:
            index = expression.pos(0)
            length = len(expression.cap(0))
            highlighter.setFormat(index, length,
                                  highlighter._get_format(Comment))
            index = expression.indexIn(original_text, index + length)