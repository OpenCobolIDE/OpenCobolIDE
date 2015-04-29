"""
Cobol linter; use open cobol to check your your syntax on the fly.
"""
import os
import tempfile
from pyqode.core.modes import CheckerMode
import time
from open_cobol_ide.compilers import GnuCobolCompiler, get_file_type


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
    if extension.upper() in GnuCobolCompiler.EXTENSIONS:
        # time stamped file path
        tmp_pth = os.path.join(tempfile.gettempdir(),
                               'oci%s.cbl' % str(int(time.time())))
        print("temp path = %s" % tmp_pth)
        with open(tmp_pth, 'w') as f:
            f.write(code)
        compiler = GnuCobolCompiler()
        _, messages = compiler.compile(tmp_pth, get_file_type(path))
        # do not leave tmp files
        try:
            os.remove(tmp_pth)
        except OSError:
            pass
        return messages
    return []


class CobolLinterMode(CheckerMode):
    def __init__(self):
        super().__init__(lint)
