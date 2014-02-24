"""
Tests the compiler module
"""
import os
import shutil
from oci import compiler, constants


def teardown_module():
    shutil.rmtree("test/testfiles/bin")
    shutil.rmtree("test/testfiles/path with spaces/bin")


def test_compile_exe():
    """
    Compiles an executable (.exe)
    """
    status, messages = compiler.compile("test/testfiles/HelloWorld.cbl",
                                        constants.ProgramType.Executable)
    assert status == 0
    assert len(messages) == 0


def test_compile_so():
    """
    Compiles a module (.so)
    """
    status, messages = compiler.compile(
        "test/testfiles/VIRTUAL-PRINTER.cbl",
        constants.ProgramType.Module)
    assert status == 0
    assert len(messages) == 0


def test_compile_path_with_spaces():
    """
    Compiles a file located in path that contains spaces (see bug #5)
    """
    status, messages = compiler.compile(
        "test/testfiles/path with spaces/HelloWorld.cbl",
        constants.ProgramType.Executable)
    assert status == 0
    assert len(messages) == 0


def test_compile_path_with_errors():
    """
    Compiles a file that contains errors
    """
    status, messages = compiler.compile("test/testfiles/MALFORMED.cbl",
                                        constants.ProgramType.Executable)
    assert status != 0
    assert len(messages)
