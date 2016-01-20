"""
This package contains the bases classes of pyqode and some utility
functions.

"""
from .code_edit import CodeEdit
from .decoration import TextDecoration
from .encodings import ENCODINGS_MAP, convert_to_codec_key
from .manager import Manager
from .mode import Mode
from .panel import Panel
from .syntax_highlighter import ColorScheme
from .syntax_highlighter import PYGMENTS_STYLES
from .syntax_highlighter import SyntaxHighlighter
from .syntax_highlighter import TextBlockUserData
from .utils import TextHelper, TextBlockHelper
from .utils import get_block_symbol_data
from .utils import DelayJobRunner
from .folding import FoldDetector
from .folding import IndentFoldDetector
from .folding import CharBasedFoldDetector
from .folding import FoldScope


__all__ = [
    'convert_to_codec_key',
    'get_block_symbol_data',
    'CharBasedFoldDetector',
    'CodeEdit',
    'ColorScheme',
    'DelayJobRunner',
    'ENCODINGS_MAP',
    'FoldDetector',
    'IndentFoldDetector',
    'FoldScope',
    'Manager',
    'Mode',
    'Panel',
    'PYGMENTS_STYLES',
    'SyntaxHighlighter',
    'TextBlockUserData',
    'TextDecoration',
    'TextHelper',
    'TextBlockHelper'
]
