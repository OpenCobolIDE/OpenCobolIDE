"""
Integration test:
  - start the IDE
  - open a cobol file
  - compile it
  - run the generated executable
"""
import os
from pyqode.core.api import TextHelper
from pyqode.qt import QtWidgets
from pyqode.qt import QtCore
from pyqode.qt.QtTest import QTest
import pytest
from open_cobol_ide import system
from open_cobol_ide.controllers.view import Page
from open_cobol_ide.settings import Settings

path_ok = os.path.join(os.getcwd(), 'test', 'testfiles', 'HelloWorld.cbl')
path_ko = os.path.join(os.getcwd(), 'test', 'testfiles', 'MALFORMED.cbl')
path_txt = os.path.join(os.getcwd(), 'README.rst')
path_interactive = os.path.join(os.getcwd(), 'test', 'testfiles',
                                'TEST-PRINTER.cbl')

compiled_path = os.path.join(
    os.getcwd(), 'test', 'testfiles', 'bin',
    'HelloWorld' + ('.exe' if system.windows else ''))

expected_output = """%s
Hello world

Process finished with exit code 0""" % (compiled_path + ' ')
print(expected_output)


def test_functional(app):
    """
    Does what a typical user expect from an IDE: open a file, compile it
    and finally run the program.

    :type app: open_cobol_ide.app.Application
    """
    app.win.show()
    # Open a cobol file
    app.file.open_file(path_ok)
    assert app.win.ui.actionCompile.isEnabled()
    assert app.edit.current_editor.file.path == path_ok
    assert app.view.ui.stackedWidget.currentIndex() == int(Page.EDIT)
    QTest.qWait(500)
    # compile it
    app.cobol.compile()
    QTest.qWait(1000)
    assert app.cobol.ui.errorsTable.rowCount() == 1
    assert app.win.ui.errorsTable.item(0, 0).text() == 'Info'
    # run it
    assert app.win.ui.actionRun.isEnabled()
    app.cobol.run()
    QTest.qWait(1000)
    assert app.win.ui.consoleOutput.toPlainText()


def test_functional_external_terminal(app):
    """
    Does what a typical user expect from an IDE: open a file, compile it
    and finally run the program.

    :type app: open_cobol_ide.app.Application
    """
    app.win.show()
    # Open a cobol file
    Settings().external_terminal = True
    app.file.open_file(path_ok)
    assert app.win.ui.actionCompile.isEnabled()
    assert app.edit.current_editor.file.path == path_ok
    assert app.view.ui.stackedWidget.currentIndex() == int(Page.EDIT)
    QTest.qWait(500)
    # compile it
    app.cobol.compile()
    QTest.qWait(1000)
    assert app.cobol.ui.errorsTable.rowCount() == 1
    assert app.win.ui.errorsTable.item(0, 0).text() == 'Info'
    # run it
    assert app.win.ui.actionRun.isEnabled()
    app.cobol.run()
    QTest.qWait(1000)
    assert 'Launched in external terminal' in app.win.ui.consoleOutput.toPlainText()

    Settings().external_terminal = False


def test_toggle_perspective(app):
    app.win.show()
    assert Settings().perspective == 'default'
    assert app.win.ui.toolBarCOBOL.isVisible()
    assert app.win.ui.toolBarFile.isVisible()
    app.view.toggle_perspective()
    QTest.qWait(1000)
    assert Settings().perspective == 'minimal'
    assert not app.win.ui.toolBarCOBOL.isVisible()
    assert not app.win.ui.toolBarCOBOL.isVisible()
    app.view.toggle_perspective()
    QTest.qWait(1000)
    assert Settings().perspective == 'default'
    assert app.win.ui.toolBarCOBOL.isVisible()
    assert app.win.ui.toolBarFile.isVisible()


def test_functional_syntax_errors(app):
    """
    Open a cobol file that contains errors and ensures there is an error
    message in the error table when compilation finished.
    """
    app.win.show()
    app.file.open_file(path_ko)
    assert app.edit.current_editor.file.path == path_ko
    QTest.qWait(500)
    app.cobol.compile()
    QTest.qWait(1000)
    item = app.win.ui.errorsTable.item(0, 0)
    assert item.text() == 'Error'
    msg = item.data(QtCore.Qt.UserRole)
    assert msg is not None
    app.cobol. _goto_error_msg(msg)
    QTest.qWait(500)
    assert TextHelper(app.edit.current_editor).cursor_position() == (10, 0)

    # this will never happen, this is just for making coveralls happy.
    app.cobol._run()


def test_open_text_file(app):
    """
    :type app: open_cobol_ide.app.Application
    """
    app.win.show()
    app.file.open_file(path_txt)
    assert not app.win.ui.actionCompile.isEnabled()


def test_cancel_operations(app):
    app.win.show()
    app.file.open_file(path_interactive)
    QTest.qWait(500)
    app.cobol.run()
    QTest.qWait(500)
    assert not app.win.ui.actionCompile.isEnabled()
    app.cobol.cancel()
    assert app.win.ui.actionCompile.isEnabled()


def test_change_pgm_type(app):
    """
    :type app: open_cobol_ide.app.Application
    """
    app.win.show()
    # Open a cobol file
    app.file.open_file(path_ok)
    QTest.qWait(500)
    assert app.win.ui.actionProgram.isChecked()
    assert not app.win.ui.actionSubprogram.isChecked()
    app.win.ui.actionSubprogram.trigger()
    QTest.qWait(500)
    assert not app.win.ui.actionProgram.isChecked()
    assert app.win.ui.actionSubprogram.isChecked()
    app.win.ui.actionProgram.trigger()
    QTest.qWait(500)
    assert app.win.ui.actionProgram.isChecked()
    assert not app.win.ui.actionSubprogram.isChecked()
