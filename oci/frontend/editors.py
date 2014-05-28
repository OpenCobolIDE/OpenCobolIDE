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
Contains an editor specialised for cobol source code editing.
"""
import logging
import mimetypes
import os
import sys
from pygments.lexers.compiled import CobolFreeformatLexer

from pygments.token import Comment
from pyqode.qt import QtCore
from pyqode.core.frontend import panels
from pyqode.core.frontend import modes
from pyqode.core import frontend

from oci import constants
from oci.backend import server
from oci.backend.parser import detect_file_type
from oci.frontend import modes as cob_modes
from oci.settings import Settings


for ext in constants.COBOL_EXTENSIONS:
    mimetypes.add_type('text/x-cobol', ext)
    mimetypes.add_type('text/x-cobol', ext.lower())


def _start_server(editor):
    if hasattr(sys, "frozen"):
        cwd = os.path.dirname(sys.executable)
        srv = 'server.exe' if sys.platform == 'win32' else 'ociserver'
        srv = os.path.join(cwd, srv)
        with open('/Users/renega_666/Documents/info.txt', 'w') as f:
            f.write(srv)
        frontend.start_server(editor, srv)
    else:
        frontend.start_server(editor, server.__file__)


class CobolCodeEdit(frontend.CodeEdit):
    """
    Extends QCodeEdit with a hardcoded set of modes and panels specifics to
    a cobol code editor widget
    """

    picInfosAvailable = QtCore.Signal(list)

    @property
    def programType(self):
        if self.__fileType == -1 and self.file_path:
            self.detect_file_type()
        return self.__fileType

    @programType.setter
    def programType(self, value):
        self.__fileType = value

    @property
    def icon(self):
        return ":/ide-icons/rc/silex-32x32.png"

    def __init__(self, parent=None):
        super().__init__(parent)
        _start_server(self)
        self.__fileType = -1
        self.setLineWrapMode(self.NoWrap)
        self.setupPanels()
        self.setupModes()
        self.updateSettings()
        self.word_separators.remove('-')

    def __del__(self):
        frontend.stop_server(self)

    def setupPanels(self):
        """
        Setup the editor's panels
        """
        frontend.install_panel(self, panels.LineNumberPanel(),
                               frontend.Panel.Position.LEFT)
        frontend.install_panel(self, panels.MarkerPanel())
        frontend.install_panel(self, panels.SearchAndReplacePanel(),
                               frontend.Panel.Position.BOTTOM)

    def setupModes(self):
        """
        Setup the editor's modes
        """
        # generic modes
        # -------------
        # current line highlighter
        self.caretLineHighlighter = modes.CaretLineHighlighterMode()
        frontend.install_mode(self, self.caretLineHighlighter)
        # zoom
        self.zoom = modes.ZoomMode()
        frontend.install_mode(self, self.zoom)
        # matching braces
        self.symbolMatcher = modes.SymbolMatcherMode()
        frontend.install_mode(self, self.symbolMatcher)
        # indenter
        self.indenter = modes.IndenterMode()
        self.indenter.min_indent = 7
        frontend.install_mode(self, self.indenter)
        # Case converter
        frontend.install_mode(self, modes.CaseConverterMode())
        # File watcher
        frontend.install_mode(self, modes.FileWatcherMode())
        # Auto complete (", ', (, ), {, }, [, ])
        frontend.install_mode(self, modes.AutoCompleteMode())
        # Code completion
        self.codeCompletionMode = modes.CodeCompletionMode()
        frontend.install_mode(self, self.codeCompletionMode)
        self.codeCompletionMode.trigger_symbols[:] = []
        # Auto indent
        self.autoIndentMode = modes.AutoIndentMode()
        frontend.install_mode(self, self.autoIndentMode)
        self.autoIndentMode.min_indent = 7 * " "
        # Syntax highlighter
        self.syntaxHighlighterMode = modes.PygmentsSyntaxHighlighter(
            self.document())
        frontend.install_mode(self, self.syntaxHighlighterMode)
        self.syntaxHighlighterMode.block_highlight_finished.connect(
            self._highlighComments)
        # word click and go to definition
        self.wordClickMode = modes.WordClickMode()
        frontend.install_mode(self, self.wordClickMode)
        # Goto definition
        frontend.install_mode(self, cob_modes.GoToDefinitionMode())

        # cobol specific modes
        # --------------------
        # Right margin
        self.rightMargin = modes.RightMarginMode()
        self.rightMargin.position = 72
        frontend.install_mode(self, self.rightMargin)
        # Left margin
        self.leftMargin = cob_modes.LeftMarginMode()
        frontend.install_mode(self, self.leftMargin)
        # Comment/Uncomment
        frontend.install_mode(self, cob_modes.CommentsMode())
        # Linter
        frontend.install_mode(self, cob_modes.LinterMode())
        # Document analyser (defined names)
        self.analyserMode = cob_modes.DocumentAnalyserMode()
        frontend.install_mode(self, self.analyserMode)
        # Offset calculator
        o = cob_modes.OffsetCalculatorMode()
        o.picInfosAvailable.connect(self.picInfosAvailable.emit)
        frontend.install_mode(self, o)

    def updateSettings(self):
        settings = Settings()
        self.caretLineHighlighter.enabled = settings.highlightCurrentLine
        self.symbolMatcher.enabled = settings.highlightMatchingBraces
        self.syntaxHighlighterMode.pygments_style = settings.colorScheme
        self.rightMargin.enabled = settings.displayMargins
        self.leftMargin.enabled = settings.displayMargins
        self.autoIndentMode.enabled = settings.enableAutoIndent
        self.codeCompletionMode.trigger_length = settings.ccTriggerLen
        self.font_name = settings.fontName
        self.font_size = settings.fontSize
        self.show_whitespaces = settings.highlightWhitespaces
        # todo: tab len

    def detect_file_type(self):
        self.__fileType = detect_file_type(self.file_path, self.file_encoding)

    def openFile(self, file_path, replaceTabsBySpaces=True, encoding=None):
        frontend.open_file(self, file_path,
                           replace_tabs_by_spaces=replaceTabsBySpaces,
                           default_encoding=encoding)
        self.syntaxHighlighterMode._lexer = CobolFreeformatLexer()

    def _highlighComments(self, highlighter, text):
        """
        Custom highlighter to fix comment highlighting

        :param original_text: Original text block

        :param highlighter: QSyntaxHighlighter instance
        """
        expression = QtCore.QRegExp('\*.*')
        index = expression.indexIn(text, 0)
        usd = highlighter.currentBlock().userData()
        while index >= 0:
            index = expression.pos(0)
            length = len(expression.cap(0))
            highlighter.setFormat(index, length,
                                  highlighter._get_format(Comment))
            usd.cc_disabled_zones.append((index, index + length))
            index = expression.indexIn(text, index + length)


class GenericCodeEdit(frontend.CodeEdit):
    """
    A generic code editor widget. This is just a CodeEdit with a preconfigured
    set of modes and panels.

    It does not have any language specific feature.
    """

    def __init__(self, parent):
        super().__init__(parent)
        _start_server(self)
        # add panels
        frontend.install_panel(self, panels.LineNumberPanel())
        frontend.install_panel(self, panels.SearchAndReplacePanel(),
                               panels.SearchAndReplacePanel.Position.BOTTOM)

        # add modes
        frontend.install_mode(self, modes.AutoCompleteMode())
        frontend.install_mode(self, modes.CaseConverterMode())
        frontend.install_mode(self, modes.FileWatcherMode())
        self.caretLineHighlighter = frontend.install_mode(
            self, modes.CaretLineHighlighterMode())
        self.rightMargin = frontend.install_mode(self, modes.RightMarginMode())
        self.highlither = frontend.install_mode(
            self, modes.PygmentsSyntaxHighlighter(self.document()))
        frontend.install_mode(self, modes.ZoomMode())
        self.codeCompletionMode = frontend.install_mode(
            self, modes.CodeCompletionMode())
        self.autoIndenter = frontend.install_mode(self, modes.AutoIndentMode())
        frontend.install_mode(self, modes.IndenterMode())
        self.symbolMatcher = frontend.install_mode(
            self, modes.SymbolMatcherMode())
        self.updateSettings()

    def openFile(self, file_path, replaceTabsBySpaces=True, encoding=None):
        frontend.open_file(self, file_path,
                           replace_tabs_by_spaces=replaceTabsBySpaces,
                           default_encoding=encoding)

    def updateSettings(self):
        settings = Settings()
        self.caretLineHighlighter.enabled = settings.highlightCurrentLine
        self.symbolMatcher.enabled = settings.highlightMatchingBraces
        self.syntaxHighlighterMode.pygments_style = settings.colorScheme
        self.rightMargin.enabled = settings.displayMargins
        self.leftMargin.enabled = settings.displayMargins
        self.autoIndentMode.enabled = settings.enableAutoIndent
        self.codeCompletionMode.trigger_length = settings.ccTriggerLen
        self.font_name = settings.fontName
        self.font_size = settings.fontSize
        self.show_whitespaces = settings.highlightWhitespaces
        # todo: tab len

