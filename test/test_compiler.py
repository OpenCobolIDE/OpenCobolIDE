"""
Tests the compiler module
"""
import re
import pytest
from open_cobol_ide import system
from open_cobol_ide.compilers import GnuCobolCompiler, FileType


def test_extensions():
    exts = GnuCobolCompiler().extensions
    if system.windows:
        assert exts[0] == '.exe'
        assert exts[1] == '.dll'
    else:
        assert exts[0] == ''
        assert exts[1] == '.so'


def test_is_working():
    assert GnuCobolCompiler().is_working()


def test_get_version():
    prog = re.compile(r'^\d.\d.\d$')
    assert prog.match(GnuCobolCompiler().get_version()) is not None


@pytest.mark.parametrize('file_type, expected', [
    (FileType.EXECUTABLE, '.exe' if system.windows else ''),
    (FileType.MODULE, '.dll' if system.windows else '.so'),
])
def test_type_extension(file_type, expected):
    assert GnuCobolCompiler().extension_for_type(file_type) == expected