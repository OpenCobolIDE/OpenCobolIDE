# -*- coding: utf-8 -*-
"""
This package contains the core modes.

"""
from .autocomplete import AutoCompleteMode
from .autoindent import AutoIndentMode
from .backspace import SmartBackSpaceMode
from .caret_line_highlight import CaretLineHighlighterMode
from .case_converter import CaseConverterMode
from .checker import CheckerMode
from .checker import CheckerMessage
from .checker import CheckerMessages
from .cursor_history import CursorHistoryMode
from .code_completion import CodeCompletionMode
from .extended_selection import ExtendedSelectionMode
from .filewatcher import FileWatcherMode
from .indenter import IndenterMode
from .line_highlighter import LineHighlighterMode
from .matcher import SymbolMatcherMode
from .occurences import OccurrencesHighlighterMode
from .outline import OutlineMode
from .right_margin import RightMarginMode
from .pygments_sh import PygmentsSH
from .wordclick import WordClickMode
from .zoom import ZoomMode
# for backward compatibility
from ..api.syntax_highlighter import PYGMENTS_STYLES
from .pygments_sh import PygmentsSH as PygmentsSyntaxHighlighter


__all__ = [
    'AutoCompleteMode',
    'AutoIndentMode',
    'CaretLineHighlighterMode',
    'CaseConverterMode',
    'CheckerMode',
    'CheckerMessage',
    'CheckerMessages',
    'CodeCompletionMode',
    'CursorHistoryMode',
    'ExtendedSelectionMode',
    'FileWatcherMode',
    'IndenterMode',
    'LineHighlighterMode',
    'OccurrencesHighlighterMode',
    'OutlineMode',
    'PygmentsSH',
    'PygmentsSyntaxHighlighter',
    'PYGMENTS_STYLES',
    'RightMarginMode',
    'SmartBackSpaceMode',
    'SymbolMatcherMode',
    'WordClickMode',
    'ZoomMode',
]
