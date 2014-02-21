"""
Integration test:
  - start the IDE
  - open a cobol file
  - compile it
  - run the generated executable
"""
import os
import sys
from pyqode.qt import QtGui, QtCore
from oci import main_window, constants


def setup_module(module):
    """
    Creates hello.cbl based on the hello world template code.
    """
    with open("hello.cbl", "w") as f:
        f.write(constants.EXE_TEMPLATE)


def teardown_module(module):
    """
    Deletes hello.cbl
    """
    os.remove("hello.cbl")
    os.remove("bin/hello.exe")
    os.rmdir("bin")


def test_integration():
    """
    Does what a typical user expect from an IDE: open a file, compile it
    and finally run the program.
    """
    app = QtGui.QApplication(sys.argv)
    window = main_window.MainWindow()
    window.show()
    window.openFile(os.path.abspath("hello.cbl"))
    QtCore.QTimer.singleShot(1000, window.on_actionCompile_triggered)
    QtCore.QTimer.singleShot(3000, window.on_actionRun_triggered)
    QtCore.QTimer.singleShot(5000, app.quit)
    app.exec_()
