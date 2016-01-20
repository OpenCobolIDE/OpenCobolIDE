"""
The api package contains API classes and functions that are used by the
widgets/panels/modes or even the backend.
"""
from .folding import CobolFoldDetector
from .parsers.names import Name, defined_names, cmp_name
from .pic import PicFieldInfo, get_field_infos


__all__ = [
    # Code folding API
    'CobolFoldDetector',

    # defined names API
    'Name',
    'defined_names',
    'cmp_name',

    # Pic API
    'PicFieldInfo',
    'get_field_infos'
]
