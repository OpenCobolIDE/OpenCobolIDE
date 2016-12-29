"""
This module contains the cobol code edit widget.
"""
import mimetypes
import os
import sys
from pyqode.cobol.backend.workers import get_outline
from pyqode.core import api, panels, modes
from pyqode.core.backend import NotRunning
from pyqode.core.managers import FileManager
from pyqode.qt import QtCore, QtGui
from pyqode.cobol import modes as cobmodes
from pyqode.cobol.api import CobolFoldDetector, icons
from pyqode.cobol.backend import server
from pyqode.cobol._forms import resources_rc


class CobolCodeEdit(api.CodeEdit):
    """
    CodeEdit specialized for cobol source code editing.
    """
    class CobolFileManager(FileManager):
        def _get_icon(self):
            return QtGui.QIcon(icons.ICON_MIMETYPE)

    mimetypes = ['text/x-cobol']
    extensions = [".COB", ".CBL", ".PCO", ".CPY", ".SCB", ".SQB"]

    @classmethod
    def all_extensions(cls):
        return cls.extensions + [ext.lower() for ext in cls.extensions]

    @property
    def free_format(self):
        return self._free_format

    @free_format.setter
    def free_format(self, free_fmt):
        if free_fmt != self._free_format:
            self._free_format = free_fmt
            self.margins.enabled = not free_fmt
            self.syntax_highlighter.rehighlight()
            self._update_backend_format()

    @property
    def lower_case_keywords(self):
        return self._lower_case_keywords

    @lower_case_keywords.setter
    def lower_case_keywords(self, value):
        self._lower_case_keywords = value
        self._update_backend_proposed_kw_case()

    @property
    def comment_indicator(self):
        return self._comment_indicator

    @comment_indicator.setter
    def comment_indicator(self, value):
        self._comment_indicator = value

    def __init__(self, parent=None, color_scheme='qt', free_format=False):
        super().__init__(parent)
        self.file = self.CobolFileManager(self)
        self._lower_case_keywords = False
        self._free_format = None
        self._comment_indicator = '*> '
        self.word_separators.remove('-')
        self._start_server()
        self._setup_panels()
        self._setup_modes(color_scheme)
        self.encoding_panel = self.panels.append(
            panels.EncodingPanel(), api.Panel.Position.TOP
        )
        self.read_only_panel = self.panels.append(
            panels.ReadOnlyPanel(), api.Panel.Position.TOP
        )
        self.free_format = free_format

    def _start_server(self):
        if hasattr(sys, "frozen"):
            cwd = os.path.dirname(sys.executable)
            base = 'cobol-backend'
            srv = base + '.exe' if sys.platform == 'win32' else base
            srv = os.path.join(cwd, srv)
            self.backend.start(srv)
        else:
            self.backend.start(server.__file__)

    def close(self, *args, **kwargs):
        self.cursor_history = None
        self.extended_selection_mode = None
        self.case_converter = None
        self.auto_complete = None
        self.outline_mode = None
        self.add_separator()
        self.goto_def_mode = None
        self.code_completion_mode = None
        self.file_watcher = None
        self.auto_indent_mode = None
        self.caret_line_mode = None
        self.zoom_mode = None
        self.indenter_mode = None
        self.auto_indent_mode = None
        self.left_margin = None
        self.right_margin = None
        self.comments_mode = None
        self.offset_calculator = None
        self.occurences_highlighter_mode = None
        self.backspace_mode = None
        self.search_panel = None
        self.folding_panel = None
        self.line_nbr_panel = None
        self.checker_panel = None
        self.global_checker_panel = None
        self.encoding_panel = None
        self.read_only_panel = None
        self.margins = None
        super().close(*args, **kwargs)

    def _setup_modes(self, color_scheme):
        self.cursor_history = self.modes.append(modes.CursorHistoryMode())
        self.extended_selection_mode = self.modes.append(
            modes.ExtendedSelectionMode()
        )
        self.case_converter = self.modes.append(
            modes.CaseConverterMode()
        )
        self.auto_complete = self.modes.append(
            modes.AutoCompleteMode())
        self.outline_mode = self.modes.append(
            modes.OutlineMode(get_outline))
        self.add_separator()
        self.goto_def_mode = self.modes.append(
            cobmodes.GoToDefinitionMode()
        )
        self.code_completion_mode = self.modes.append(
            modes.CodeCompletionMode()
        )
        self.code_completion_mode.trigger_symbols[:] = []
        self.file_watcher = self.modes.append(
            modes.FileWatcherMode()
        )
        self.auto_indent_mode = self.modes.append(
            cobmodes.CobolAutoIndentMode()
        )
        self.caret_line_mode = self.modes.append(
            modes.CaretLineHighlighterMode()
        )
        self.zoom_mode = self.modes.append(
            modes.ZoomMode()
        )
        self.indenter_mode = self.modes.append(
            cobmodes.IndenterMode()
        )
        self.auto_indent_mode = self.modes.append(
            modes.AutoIndentMode()
        )
        self.modes.append(cobmodes.CobolSyntaxHighlighter(
            self.document(), color_scheme=api.ColorScheme(color_scheme)))
        self.syntax_highlighter.fold_detector = CobolFoldDetector()
        self.margins = self.modes.append(cobmodes.MarginsMode())
        self.comments_mode = self.modes.append(cobmodes.CommentsMode())
        self.offset_calculator = self.modes.append(
            cobmodes.OffsetCalculatorMode())
        self.occurences_highlighter_mode = self.modes.append(
            modes.OccurrencesHighlighterMode()
        )
        self.occurences_highlighter_mode.case_sensitive = False
        self.backspace_mode = self.modes.append(
            cobmodes.SmartBackSpaceMode()
        )

    def _setup_panels(self):
        self.search_panel = self.panels.append(
            panels.SearchAndReplacePanel(), api.Panel.Position.BOTTOM
        )
        self.folding_panel = self.panels.append(
            panels.FoldingPanel(), api.Panel.Position.LEFT
        )
        self.line_nbr_panel = self.panels.append(
            panels.LineNumberPanel(), api.Panel.Position.LEFT
        )
        self.checker_panel = self.panels.append(
            panels.CheckerPanel(), api.Panel.Position.LEFT
        )
        self.global_checker_panel = self.panels.append(
            panels.GlobalCheckerPanel(), api.Panel.Position.RIGHT)

    def _update_backend_format(self):
        from pyqode.cobol.backend.workers import set_free_format
        try:
            self.backend.send_request(set_free_format, self.free_format)
        except NotRunning:
            QtCore.QTimer.singleShot(100, self._update_backend_format)

    def _update_backend_proposed_kw_case(self):
        from pyqode.cobol.backend.workers import set_lower_case_keywords
        try:
            self.backend.send_request(set_lower_case_keywords,
                                      self.lower_case_keywords)
        except NotRunning:
            QtCore.QTimer.singleShot(
                100, self._update_backend_proposed_kw_case)

    def _do_home_key(self, event=None, select=False):
        """ Performs home key action """
        # get nb char to first significative char
        min_column = self.indenter_mode.min_column
        text = api.TextHelper(self).current_line_text()[min_column:]
        indent = len(text) - len(text.lstrip())
        delta = (self.textCursor().positionInBlock() - indent - min_column)
        cursor = self.textCursor()
        move = QtGui.QTextCursor.MoveAnchor
        if select:
            move = QtGui.QTextCursor.KeepAnchor
        if delta > 0:
            cursor.movePosition(QtGui.QTextCursor.Left, move, delta)
        else:
            cursor.movePosition(QtGui.QTextCursor.StartOfBlock, move)
            cursor.movePosition(QtGui.QTextCursor.Right, cursor.MoveAnchor, min_column)
        self.setTextCursor(cursor)
        if event:
            event.accept()

for ext in CobolCodeEdit.extensions:
    mimetypes.add_type(CobolCodeEdit.mimetypes[0], ext)
    mimetypes.add_type(CobolCodeEdit.mimetypes[0], ext.lower())
