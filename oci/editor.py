"""
Contains an editor specialised for cobol source code editing.
"""
from pygments.lexers.compiled import CobolFreeformatLexer
import pyqode.core
from PyQt4 import QtCore
from pygments.token import Comment
from oci import cobol
from oci.code_completion import CobolDocumentWordsProvider, CobolAnalyserProvider
from oci.modes import ToUpperMode, CommentsMode, LeftMarginMode, CobolCheckerMode
from oci.cobol import CobolFolder


# make pygments hihlighter uses our custom cobol fold detector
pyqode.core.PygmentsSyntaxHighlighter.LEXERS_FOLD_DETECTORS[
            CobolFreeformatLexer] = CobolFolder()


class QCobolCodeEdit(pyqode.core.QCodeEdit):
    """
    Extends QCodeEdit with a hardcoded set of modes and panels specifics to
    a cobol code editor widget
    """

    @property
    def programType(self):
        return self.__fileType

    @programType.setter
    def programType(self, value):
        self.__fileType = value

    @property
    def icon(self):
        return ":/ide-icons/rc/silex-32x32.png"

    def __init__(self, parent=None):
        pyqode.core.QCodeEdit.__init__(self, parent)
        self.__fileType = -1
        self.setLineWrapMode(self.NoWrap)
        self.setupPanels()
        self.setupModes()

    def setupPanels(self):
        """
        Setup the editor's panels
        """
        self.installPanel(pyqode.core.FoldingPanel())
        self.installPanel(pyqode.core.LineNumberPanel(),
                          pyqode.core.PanelPosition.LEFT)
        self.installPanel(pyqode.core.MarkerPanel())
        self.installPanel(pyqode.core.SearchAndReplacePanel(),
                          pyqode.core.PanelPosition.BOTTOM)

    def setupModes(self):
        """
        Setup the editor's modes
        """
        self.installMode(pyqode.core.CaretLineHighlighterMode())

        # generic modes
        self.installMode(pyqode.core.ZoomMode())
        self.installMode(pyqode.core.SymbolMatcherMode())

        # code completion
        self.installMode(pyqode.core.CodeCompletionMode())
        self.codeCompletionMode.addCompletionProvider(
            CobolDocumentWordsProvider())
        self.textSaved.connect(self.codeCompletionMode.requestPreload)
        self.codeCompletionMode.addCompletionProvider(CobolAnalyserProvider())
        self.settings.setValue("triggerSymbols", [], section="codeCompletion")

        # auto indent
        self.installMode(pyqode.core.AutoIndentMode())
        self.autoIndentMode.minIndent = 7 * " "

        # syntax highlighter
        self.installMode(pyqode.core.PygmentsSyntaxHighlighter(self.document()))
        self.syntaxHighlighterMode.blockHighlightFinished.connect(
            self._highlighComments)

        # cobol specific modes
        self.installMode(pyqode.core.RightMarginMode())
        self.installMode(LeftMarginMode())
        self.installMode(ToUpperMode())
        self.installMode(CommentsMode())
        self.installMode(CobolCheckerMode())

    def openFile(self, filePath, replaceTabsBySpaces=True, encoding=None,
                 detectEncoding=False):
        pyqode.core.QCodeEdit.openFile(self, filePath, replaceTabsBySpaces,
                                       encoding, detectEncoding)
        self.__fileType = cobol.detectFileType(filePath)

    def _highlighComments(self, highlighter, text):
        """
        Custom highlighter to fix comment highlighting

        :param original_text: Original text block

        :param highlighter: QSyntaxHighlighter instance
        """
        expression = QtCore.QRegExp('\*.*')
        index = expression.indexIn(text, 0)
        while index >= 0:
            index = expression.pos(0)
            length = len(expression.cap(0))
            highlighter.setFormat(index, length,
                                  highlighter._get_format(Comment))
            index = expression.indexIn(text, index + length)
