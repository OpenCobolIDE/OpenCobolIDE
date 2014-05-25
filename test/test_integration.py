"""
Integration test:
  - start the IDE
  - open a cobol file
  - compile it
  - run the generated executable
"""
import os
import shutil
import sys

from pyqode.qt import QtWidgets, QtCore

from oci.frontend import main_window


def teardown_module():
    shutil.rmtree("test/testfiles/bin")


def test_integration():
    """
    Does what a typical user expect from an IDE: open a file, compile it
    and finally run the program.
    """
    app = QtWidgets.QApplication(sys.argv)
    window = main_window.MainWindow()
    window.show()
    window.openFile(os.path.abspath("test/testfiles/HelloWorld.cbl"))
    QtCore.QTimer.singleShot(1000, window.on_actionCompile_triggered)
    QtCore.QTimer.singleShot(3000, window.on_actionRun_triggered)
    QtCore.QTimer.singleShot(5000, app.quit)
    app.exec_()
    window.close()
