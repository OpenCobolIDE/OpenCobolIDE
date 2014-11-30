"""
Cobol linter; use open cobol to check your your syntax on the fly.
"""
import os
from pyqode.core.modes import CheckerMode
from .compiler import GnuCobolCompiler, get_file_type
from .system import get_cache_directory


def lint(request_data):
    """
    Performs linting of a cobol document.

    This method will perform on the pyqode backend.

    :param request_data: work request data (dict)
    :return: status, messages
    """
    code = request_data['code']
    path = request_data['path']
    tmp_pth = os.path.join(get_cache_directory(), 'temp.cbl')
    with open(tmp_pth, 'w') as f:
        f.write(code)
    compiler = GnuCobolCompiler()
    _, messages = compiler.compile(tmp_pth, get_file_type(path))
    return messages


class CobolLinterMode(CheckerMode):
    def __init__(self):
        super().__init__(lint)
