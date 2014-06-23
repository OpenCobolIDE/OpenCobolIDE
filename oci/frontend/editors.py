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
import mimetypes
import os
import sys

from pygments.lexers.compiled import CobolFreeformatLexer, CobolLexer
from pygments.token import Comment
from pyqode.core.qt import QtCore
from pyqode.core.qt import QtGui
from pyqode.core import panels
from pyqode.core import modes
from pyqode.core import api
from pyqode.core import managers

from oci import constants
from oci.backend import server
from oci.backend.parser import detect_file_type
from oci.frontend import modes as cob_modes, services
from oci.frontend import panels as cob_panels
from oci.settings import Settings


for ext in constants.COBOL_EXTENSIONS:
    mimetypes.add_type('text/x-cobol', ext)
    mimetypes.add_type('text/x-cobol', ext.lower())


def _start_server(editor):
    if hasattr(sys, "frozen"):
        cwd = os.path.dirname(sys.executable)
        srv = 'ociserver.exe' if sys.platform == 'win32' else 'ociserver'
        srv = os.path.join(cwd, srv)
        editor.backend.start(srv)
    else:
        editor.backend.start(server.__file__)


class CobolCodeEdit(api.CodeEdit):
    """
    Extends QCodeEdit with a hardcoded set of modes and panels specifics to
    a cobol code editor widget
    """
    class FileManager(managers.FileManager):
        def _get_icon(self):
            return QtGui.QIcon(":/ide-icons/rc/silex-32x32.png")

    picInfosAvailable = QtCore.Signal(list)
    programTypeChanged = QtCore.Signal()
    compilationRequested = QtCore.Signal()
    runRequested = QtCore.Signal()
    pgmTypeChangeRequested = QtCore.Signal(object)

    @property
    def programType(self):
        if self.__fileType == -1 and self.file.path:
            self.detect_file_type()
        return self.__fileType

    @programType.setter
    def programType(self, value):
        if value != self.__fileType:
            self.__fileType = value
            self.programTypeChanged.emit()

    def __init__(self, parent=None):
        super().__init__(parent)
        _start_server(self)
        self.file = self.FileManager(self)
        self.__fileType = -1
        self.setLineWrapMode(self.NoWrap)
        self.setupPanels()
        self.setupModes()
        self.updateSettings()
        self.word_separators.remove('-')

    def setupPanels(self):
        """
        Setup the editor's panels
        """
        self.controlPanel = cob_panels.ControlPanel()
        self.controlPanel.compilationRequested.connect(
            self.compilationRequested.emit)
        self.controlPanel.runRequested.connect(
            self.runRequested.emit)
        self.controlPanel.pgmTypeChangeRequested.connect(
            self.pgmTypeChangeRequested.emit)
        self.panels.append(self.controlPanel, api.Panel.Position.RIGHT)
        self.lineNumberPanel = panels.LineNumberPanel()
        self.panels.append(self.lineNumberPanel, api.Panel.Position.LEFT)
        self.panels.append(panels.MarkerPanel())
        self.panels.append(panels.SearchAndReplacePanel(),
                           api.Panel.Position.BOTTOM)

    def setupModes(self):
        """
        Setup the editor's modes
        """
        # generic modes
        # -------------
        # current line highlighter
        self.caretLineHighlighter = self.modes.append(
            modes.CaretLineHighlighterMode())
        # zoom
        self.zoom = self.modes.append(modes.ZoomMode())
        # matching braces
        self.symbolMatcher = self.modes.append(modes.SymbolMatcherMode())
        # indenter
        self.indenter = self.modes.append(modes.IndenterMode())
        self.indenter.min_indent = Settings().left_margin
        # Case converter
        self.modes.append(modes.CaseConverterMode())
        # File watcher
        self.modes.append(modes.FileWatcherMode())
        # Auto complete (", ', (, ), {, }, [, ])
        self.modes.append(modes.AutoCompleteMode())
        # Code completion
        self.codeCompletionMode = self.modes.append(modes.CodeCompletionMode())
        self.codeCompletionMode.trigger_symbols[:] = []
        # Auto indent
        self.autoIndentMode = self.modes.append(modes.AutoIndentMode())
        self.autoIndentMode.min_indent = 7 * " "
        # Syntax highlighter
        self.syntaxHighlighterMode = self.modes.append(
            modes.PygmentsSyntaxHighlighter(self.document()))
        self.syntaxHighlighterMode.block_highlight_finished.connect(
            self._highlighComments)
        # word click and go to definition
        self.wordClickMode = self.modes.append(modes.WordClickMode())
        # Goto definition
        self.modes.append(cob_modes.GoToDefinitionMode())

        # cobol specific modes
        # --------------------
        # Right margin
        self.rightMargin = self.modes.append(modes.RightMarginMode())
        self.rightMargin.position = Settings().right_margin
        # Left margin
        self.leftMargin = self.modes.append(cob_modes.LeftMarginMode())
        self.leftMargin.position = Settings().left_margin
        # Comment/Uncomment
        self.modes.append(cob_modes.CommentsMode())
        # Linter
        self.modes.append(cob_modes.LinterMode())
        # Document analyser (defined names)
        self.analyserMode = self.modes.append(cob_modes.DocumentAnalyserMode())
        # Offset calculator
        self.modes.append(
            cob_modes.OffsetCalculatorMode()).picInfosAvailable.connect(
                self.picInfosAvailable.emit)

    def updateSettings(self):
        settings = Settings()
        self.tab_length = settings.tabWidth
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
        self.controlPanel.enabled = settings.displayControlPanel
        self.controlPanel.setVisible(self.controlPanel.enabled)
        self.lineNumberPanel.enabled = settings.displayLineNumbers
        self.lineNumberPanel.setVisible(settings.displayLineNumbers)
        self.leftMargin.position = settings.left_margin
        self.leftMargin.enabled = settings.left_margin != 0
        self.rightMargin.position = settings.right_margin
        if settings.free_format:
            self.syntaxHighlighterMode._lexer = CobolFreeformatLexer()
        else:
            self.syntaxHighlighterMode._lexer = CobolLexer()
        self.syntaxHighlighterMode.rehighlight()

    def detect_file_type(self):
        self.__fileType = detect_file_type(self.file.path, self.file.encoding)

    def openFile(self, file_path):
        self.file.open(file_path)
        if Settings().free_format:
            # rehighlight with free support.
            self.syntaxHighlighterMode._lexer = CobolFreeformatLexer()
            self.syntaxHighlighterMode.rehighlight()

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


class GenericCodeEdit(api.CodeEdit):
    """
    A generic code editor widget. This is just a CodeEdit with a preconfigured
    set of modes and panels.

    It does not have any language specific feature.
    """

    def __init__(self, parent):
        super().__init__(parent)
        _start_server(self)
        # add panels
        self.panels.append(panels.LineNumberPanel())
        self.panels.append(panels.SearchAndReplacePanel(),
                           panels.SearchAndReplacePanel.Position.BOTTOM)

        # add modes
        self.modes.append(modes.AutoCompleteMode())
        self.modes.append(modes.CaseConverterMode())
        self.modes.append(modes.FileWatcherMode())
        self.caretLineHighlighter = self.modes.append(
            modes.CaretLineHighlighterMode())
        self.rightMargin = self.modes.append(modes.RightMarginMode())
        self.highlither = self.modes.append(modes.PygmentsSyntaxHighlighter(
            self.document()))
        self.modes.append(modes.ZoomMode())
        self.codeCompletionMode = self.modes.append(modes.CodeCompletionMode())
        self.autoIndenter = self.modes.append(modes.AutoIndentMode())
        self.modes.append(modes.IndenterMode())
        self.symbolMatcher = self.modes.append(modes.SymbolMatcherMode())
        self.updateSettings()

    def openFile(self, file_path, replaceTabsBySpaces=True, encoding=None):
        self.file.open(file_path, replace_tabs_by_spaces=replaceTabsBySpaces,
                       default_encoding=encoding)

    def updateSettings(self):
        settings = Settings()
        self.caretLineHighlighter.enabled = settings.highlightCurrentLine
        self.symbolMatcher.enabled = settings.highlightMatchingBraces
        self.highlither.pygments_style = settings.colorScheme
        self.rightMargin.enabled = settings.displayMargins
        self.autoIndenter.enabled = settings.enableAutoIndent
        self.codeCompletionMode.trigger_length = settings.ccTriggerLen
        self.font_name = settings.fontName
        self.font_size = settings.fontSize
        self.show_whitespaces = settings.highlightWhitespaces
        self.tab_length = settings.tabWidth


def make_cobol_editor():
    main_window = services.main_window()
    tab = CobolCodeEdit(main_window.tabWidgetEditors)
    tab.analyserMode.documentLayoutChanged.connect(
        main_window.updateNavigationPanel)
    tab.picInfosAvailable.connect(main_window.displayPICInfos)
    tab.compilationRequested.connect(
        main_window.on_actionCompile_triggered)
    tab.runRequested.connect(
        main_window.on_actionRun_triggered)
    tab.pgmTypeChangeRequested.connect(
        main_window.on_programType_triggered)
    return tab
