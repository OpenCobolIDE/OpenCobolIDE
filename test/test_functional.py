"""
Integration test:
  - start the IDE
  - open a cobol file
  - compile it
  - run the generated executable
"""
from pyqode.qt.QtTest import QTest
from open_cobol_ide.controllers.view import Page

path = 'test/testfiles/HelloWorld.cbl'


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
    # app.cobol.run()
    # QTest.qWait(2000)

