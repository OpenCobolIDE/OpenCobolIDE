"""
Integration test:
  - start the IDE
  - open a cobol file
  - compile it
  - run the generated executable
"""
import os
from pyqode.qt.QtTest import QTest
from open_cobol_ide import system
from open_cobol_ide.controllers.view import Page

path = os.path.join(os.getcwd(), 'test', 'testfiles', 'HelloWorld.cbl')
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
    app.file.open_file(path)
    assert app.edit.current_editor.file.path == path
    assert app.view.ui.stackedWidget.currentIndex() == int(Page.EDIT)
    QTest.qWait(500)
    app.cobol.compile()
    QTest.qWait(1000)
    app.cobol.ui.errorsTable.rowCount() == 1
    app.cobol.run()
    QTest.qWait(1000)
    assert app.win.ui.consoleOutput.toPlainText() == expected_output

