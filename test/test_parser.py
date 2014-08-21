"""
This module tests the parser module
"""
from oci.backend import parser


def test_parse_dependencies():
    """
    Test to parse the dependencies of a cobol exe
    that use one submodule. Such a program exisits in the
    testfiles directory (TEST-PRINTER.cbl)
    """
    deps = parser.parse_dependencies('test/testfiles/TEST-PRINTER.cbl',
                                     'utf-8')
    assert len(deps)


def test_parse_dependencies_single_quotes():
    """
    See github #29
    """
    deps = parser.parse_dependencies('test/testfiles/TEST-SINGLE-QUOTES.cbl',
                                     'utf-8')
    assert len(deps)


def test_parse_dependencies_encoding():
    """
    See github #31
    """
    deps = parser.parse_dependencies('test/testfiles/HelloWorldLatin1.cbl',
                                     'latin-1')
    assert len(deps) == 0


def test_parse_ast():
    """
    Parses the hello world example
    """
    ast, vars, procs = parser.parse_ast('test/testfiles/HelloWorld.cbl')
    # 4 divs
    assert len(ast.children) == 4
    # 2 sections in env div
    assert len(ast.children[1].children) == 2
    # 2 sections in data div
    assert len(ast.children[2].children) == 2
    assert len(vars) == 0
    assert len(procs) == 1


def test_free_parser():
    """
    HelloWorld.cbl and HelloWorldFree.cbl must have the same ast.
    """
    non_free_ast, non_free_vars, non_free_procs = parser.parse_ast(
        'test/testfiles/HelloWorld.cbl')
    free_ast, free_vars, free_procs = parser.parse_ast(
        'test/testfiles/HelloWorldFree.cbl', free=True)
    result = parser.cmp_doc_node(non_free_ast, free_ast)
    assert result


def test_variables():
    """
    Virtual printer must have 8 vars
    """
    ast, vars, procs = parser.parse_ast('test/testfiles/VIRTUAL-PRINTER.cbl')
    # 8 variables
    assert len(vars) == 8


def test_paragraphes():
    """
    Test printer must have 2 procedures
    """
    ast, vars, procs = parser.parse_ast('test/testfiles/TEST-PRINTER.cbl')
    # 1 procedure
    assert len(procs) == 2


def test_malformed():
    """
    Parses the hello world example
    """
    ast, vars, procs = parser.parse_ast('test/testfiles/MALFORMED.cbl')
    # 4 divs
    assert len(ast.children) == 4
    # 2 sections in env div
    assert len(ast.children[0].children) == 2
    # 2 sections in data div
    assert len(ast.children[1].children) == 2
    assert len(vars) == 0
    assert len(procs) == 1


def test_parse_pco():
    """
    Parses a pco file, which contains characters in column 1-6 (see bug #23)
    """
    ast, vars, procs = parser.parse_ast('test/testfiles/HelloWorld.pco')
    # 4 divs
    assert len(ast.children) == 4
    # 2 sections in env div
    assert len(ast.children[1].children) == 2
    # 2 sections in data div
    assert len(ast.children[2].children) == 2
    assert len(vars) == 0
    assert len(procs) == 1


def test_parse_encoding():
    """
    See bug #31 on github
    """
    ast, vars, procs = parser.parse_ast(
        'test/testfiles/HelloWorldLatin1.cbl', encoding='latin-1')
