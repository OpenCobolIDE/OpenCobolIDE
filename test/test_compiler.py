"""
Tests the compiler module
"""
import os
import shutil
from oci import compiler, constants

DIR = os.path.join(os.getcwd(), "compile_test_temp")
DIR_WITH_SPACES = os.path.join(DIR, "with spaces")

FN_SRC_EXE = os.path.join(DIR, "hello_exe.cbl")
FN_SRC_EXE_WITH_ERRORS = os.path.join(DIR, "hello_exe_errors.cbl")
FN_SRC_MODULE = os.path.join(DIR, "hello_mod.cbl")
FN_SRC_WITH_SPACES = os.path.join(DIR_WITH_SPACES, "hello.cbl")


def setup_module(module):
    """
    Creates files to compile
    """
    os.mkdir(DIR)
    os.mkdir(DIR_WITH_SPACES)

    filenames = [FN_SRC_EXE, FN_SRC_MODULE, FN_SRC_WITH_SPACES,
                 FN_SRC_EXE_WITH_ERRORS]
    sources = [constants.EXE_TEMPLATE, constants.MODULE_TEMPLATE,
               constants.EXE_TEMPLATE,
               constants.EXECUTABLE_EXTENSION.replace(
                   "DIVISION.", "DIVISION")]

    for fn, src in zip(filenames, sources):
        with open(fn, "w") as f:
            f.write(src)


def teardown_module(module):
    """
    Removes the temp dir
    """
    shutil.rmtree(DIR)


def test_compile_exe():
    """
    Compiles an executable (.exe)
    """
    status, messages = compiler.compile(FN_SRC_EXE, constants.ProgramType.Executable)
    assert status == 0
    assert len(messages) == 0


def test_compile_so():
    """
    Compiles a module (.so)
    """
    status, messages = compiler.compile(FN_SRC_MODULE, constants.ProgramType.Module)
    assert status == 0
    assert len(messages) == 0


def test_compile_path_with_spaces():
    """
    Compiles a file located in path that contains spaces (see bug #5)
    """
    status, messages = compiler.compile(FN_SRC_WITH_SPACES, constants.ProgramType.Executable)
    assert status == 0
    assert len(messages) == 0


def test_compile_path_with_errors():
    """
    Compiles a file located in path that contains spaces (see bug #5)
    """
    status, messages = compiler.compile(FN_SRC_EXE_WITH_ERRORS, constants.ProgramType.Executable)
    assert status == 1
    assert len(messages) == 1
