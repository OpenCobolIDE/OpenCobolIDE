"""
Cobol linter; use open cobol to check your your syntax on the fly.
"""
import os
import tempfile
from pyqode.core.modes import CheckerMode
import time
from open_cobol_ide.compilers import GnuCobolCompiler, get_file_type
from open_cobol_ide import settings


def lint(request_data):
    """
    Performs linting of a cobol document.

    This method will perform on the pyqode backend.

    :param request_data: work request data (dict)
    :return: status, messages
    """
    code = request_data['code']
    path = request_data['path']
    extension = os.path.splitext(path)[1]
    if extension.lower() in settings.Settings().cobc_extensions:
        # code might not have been saved yet, run cobc on a tmp file in the
        # same dir to get relative libraries dir to work, see #119
        # we use a time stamp to avoid overwriting the file another cobc
        # instance might be compiling.
        tmp_pth = os.path.join(os.path.dirname(path),
                               '.oci%s.cbl' % str(int(time.time())))
        print("temp path = %s" % tmp_pth)
        with open(tmp_pth, 'w') as f:
            f.write(code)
        compiler = GnuCobolCompiler()
        tmp = os.path.join(tempfile.gettempdir(), 'OpenCobolIDE')
        _, messages = compiler.compile(tmp_pth, get_file_type(path),
                                       output_dir=tmp)
        os.remove(tmp_pth)
        return messages
    return []


class CobolLinterMode(CheckerMode):
    def __init__(self):
        super().__init__(lint)
