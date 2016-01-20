"""
This module contains core code edits:

    - TextEdit: code edit specialised for plain text
    - GenericCodeEdit: generic code editor, not that smart and slow.
      Use it as a fallback and look other pyqode packages for language
      specific cod edits.

"""
import sys
from pyqode.core.backend import server
from pyqode.core.api import CodeEdit, Panel, SyntaxHighlighter, \
    CharBasedFoldDetector, IndentFoldDetector, ColorScheme


class TextCodeEdit(CodeEdit):
    """
    CodeEdit specialised for plain text.

    Especially useful for long text file such as log files because it's syntax
    highlighter does not do anything.
    """
    class TextSH(SyntaxHighlighter):
        """
        Empty highlighter, does not do anything (very fast at loading very
        big files).
        """
        def highlight_block(self, text, user_data):
            """
            Does nothing
            """
            pass

    mimetypes = ['text/x-plain', 'text/x-log', 'text/plain']

    DEFAULT_SERVER = server.__file__

    def __init__(self, parent=None, server_script=None,
                 interpreter=sys.executable, args=None,
                 create_default_actions=True, color_scheme='qt',
                 reuse_backend=False):
        from pyqode.core import panels
        from pyqode.core import modes
        if server_script is None:
            server_script = TextCodeEdit.DEFAULT_SERVER
        super(TextCodeEdit, self).__init__(parent, create_default_actions)
        self.backend.start(server_script, interpreter, args,
                           reuse=reuse_backend)

        # append panels
        self.panels.append(panels.SearchAndReplacePanel(),
                           Panel.Position.BOTTOM)
        self.panels.append(panels.FoldingPanel())
        self.panels.append(panels.LineNumberPanel())

        # append modes
        self.modes.append(modes.AutoCompleteMode())
        self.modes.append(modes.ExtendedSelectionMode())
        self.modes.append(modes.CaseConverterMode())
        self.modes.append(modes.FileWatcherMode())
        self.modes.append(modes.CaretLineHighlighterMode())
        self.modes.append(modes.RightMarginMode())
        self.modes.append(TextCodeEdit.TextSH(
            self.document(), ColorScheme(color_scheme)))
        self.modes.append(modes.ZoomMode())
        self.modes.append(modes.OccurrencesHighlighterMode())
        self.modes.append(modes.CodeCompletionMode())
        self.modes.append(modes.AutoIndentMode())
        self.modes.append(modes.IndenterMode())
        self.modes.append(modes.SymbolMatcherMode())

        self.panels.append(panels.EncodingPanel(), Panel.Position.TOP)
        self.panels.append(panels.ReadOnlyPanel(), Panel.Position.TOP)

    def clone(self):
        clone = self.__class__(
            parent=self.parent(), server_script=self.backend.server_script,
            interpreter=self.backend.interpreter, args=self.backend.args,
            color_scheme=self.syntax_highlighter.color_scheme.name)
        return clone


class GenericCodeEdit(CodeEdit):
    """
    This generic code edit uses the PygmentSH for syntax highlighting and
    commpletion engine based on the document words. It is not very smart and
    is probably 2 times slower than a native specialised code edit.
    It is meant to be used as a fallback editor in case you're missing a
    specialised editor.
    """
    # generic
    mimetypes = []

    #: the list of mimetypes that use char based fold detector
    _char_based_mimetypes = [
        'text/x-php',
        'text/x-c++hdr',
        'text/x-c++src',
        'text/x-chdr',
        'text/x-csrc',
        'text/x-csharp',
        'application/javascript'
    ]

    DEFAULT_SERVER = server.__file__

    def __init__(self, parent=None, server_script=None,
                 interpreter=sys.executable, args=None,
                 create_default_actions=True, color_scheme='qt',
                 reuse_backend=False):
        super(GenericCodeEdit, self).__init__(parent, create_default_actions)
        from pyqode.core import panels
        from pyqode.core import modes
        if server_script is None:
            server_script = GenericCodeEdit.DEFAULT_SERVER

        self.backend.start(server_script, interpreter, args,
                           reuse=reuse_backend)
        # append panels
        self.panels.append(panels.LineNumberPanel())
        self.panels.append(panels.SearchAndReplacePanel(),
                           Panel.Position.BOTTOM)
        self.panels.append(panels.FoldingPanel())

        # append modes
        self.modes.append(modes.CursorHistoryMode())
        self.modes.append(modes.AutoCompleteMode())
        self.modes.append(modes.ExtendedSelectionMode())
        self.modes.append(modes.CaseConverterMode())
        self.modes.append(modes.FileWatcherMode())
        self.modes.append(modes.CaretLineHighlighterMode())
        self.modes.append(modes.RightMarginMode())
        self.modes.append(modes.PygmentsSyntaxHighlighter(
            self.document(), color_scheme=ColorScheme(color_scheme)))
        self.modes.append(modes.ZoomMode())
        self.modes.append(modes.CodeCompletionMode())
        self.modes.append(modes.AutoIndentMode())
        self.modes.append(modes.IndenterMode())
        self.modes.append(modes.SymbolMatcherMode())
        self.modes.append(modes.OccurrencesHighlighterMode())
        self.modes.append(modes.SmartBackSpaceMode())

        self.panels.append(panels.EncodingPanel(), Panel.Position.TOP)
        self.panels.append(panels.ReadOnlyPanel(), Panel.Position.TOP)

    def setPlainText(self, txt, mime_type='', encoding=''):
        if mime_type is None:
            mime_type = self.file.mimetype
        if encoding is None:
            encoding = self.file.encoding
        try:
            self.syntax_highlighter.set_lexer_from_filename(self.file.path)
            try:
                mimetype = self.syntax_highlighter._lexer.mimetypes[0]
            except (AttributeError, IndexError):
                mimetype = ''

            if mimetype in self._char_based_mimetypes:
                self.syntax_highlighter.fold_detector = CharBasedFoldDetector()
            else:
                self.syntax_highlighter.fold_detector = IndentFoldDetector()
        except AttributeError:
            # syntax highlighter removed, e.g. file size > FileManager.limit
            pass

        super(GenericCodeEdit, self).setPlainText(txt, mime_type, encoding)

    def clone(self):
        clone = self.__class__(
            parent=self.parent(), server_script=self.backend.server_script,
            interpreter=self.backend.interpreter, args=self.backend.args,
            color_scheme=self.syntax_highlighter.color_scheme.name)
        return clone
