"""
This module tests the parser module
"""
from oci import parser


def test_parse_dependencies():
    """
    Test to parse the dependencies of a cobol exe
    that use one submodule. Such a program exisits in the
    testfiles directory (TEST-PRINTER.cbl)
    """
    deps = parser.parse_dependencies("test/testfiles/TEST-PRINTER.cbl")
    assert len(deps)
