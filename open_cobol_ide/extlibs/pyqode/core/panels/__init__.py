# -*- coding: utf-8 -*-
"""
This package contains the core panels
"""
from .encodings import EncodingPanel
from .line_number import LineNumberPanel
from .marker import Marker
from .marker import MarkerPanel
from .checker import CheckerPanel
from .folding import FoldingPanel
from .search_and_replace import SearchAndReplacePanel
from .global_checker import GlobalCheckerPanel
from .read_only import ReadOnlyPanel


__all__ = [
    'CheckerPanel',
    'EncodingPanel',
    'FoldingPanel',
    'LineNumberPanel',
    'Marker',
    'MarkerPanel',
    'SearchAndReplacePanel',
    'GlobalCheckerPanel',
    'ReadOnlyPanel'
]
