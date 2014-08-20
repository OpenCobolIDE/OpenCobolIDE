"""
This module contains the ide application. This is where we glue the
various managers and gui parts together.

"""
import logging
import os
import sys

from pyqode.qt import QtWidgets

from . import __version__
from .controllers import CobolController
from .controllers import EditController
from .controllers import FileController
from .controllers import HelpController
from .controllers import HomeController
from .controllers import ViewController
from .settings import Settings
from .view.main_window import MainWindow
from .compilers import check_compiler, CompilerNotFound


def _logger():
    return logging.getLogger(__name__)


class Application:
    def __init__(self):
        self._init_env()
        self.name = 'OpenCobolIDE'
        self.version = __version__
        self.title = '%s %s' % (self.name, self.version)
        self.app = QtWidgets.QApplication(sys.argv)
        self.win = MainWindow()
        self.win.setWindowTitle(self.title)
        self.view = ViewController(self)
        self.view.show_home()
        self.file = FileController(self)
        self.home = HomeController(self)
        self.edit = EditController(self)
        self.cobol = CobolController(self)
        self.help = HelpController(self)
        self.win.show()
        try:
            check_compiler()
        except CompilerNotFound as e:
            QtWidgets.QMessageBox.warning(
                self.win, 'Cobol compiler not found',
                "Failed to find GnuCobol compiler!\n\n%s.\n\n"
                "The IDE will continue to work but you won't be able to "
                "compile any file" % e)

    def __del__(self):
        _logger().debug('del app')

    def run(self):
        return self.app.exec_()

    @classmethod
    def _init_env(cls):
        if sys.platform == 'win32':
            cls._windows_init()
        elif sys.platform == 'darwin':
            cls._osx_init()
        path = Settings().custom_compiler_path
        if path:
            sep = ';' if sys.platform == 'win32' else ':'
            os.environ['PATH'] += sep + path

    @staticmethod
    def _windows_init():
        """
        Windows specific initialisation:

        - set env var to embedded OpenCobol variable
        - set PATH to cobol library path only (erase previous values)
        """
        cwd = os.getcwd()
        oc_root_pth = os.path.join(cwd, "OpenCobol")
        os.environ["COB_CONFIG_DIR"] = os.path.join(oc_root_pth, "config")
        os.environ["COB_COPY_DIR"] = os.path.join(oc_root_pth, "copy")
        os.environ["COB_LIBRARY_PATH"] = os.path.join(oc_root_pth, "bin")
        os.environ["COB_INCLUDE_PATH"] = os.path.join(oc_root_pth, "include")
        os.environ["COB_LIB_PATH"] = os.path.join(oc_root_pth, "lib")
        os.environ["PATH"] = os.environ["COB_LIBRARY_PATH"]

    @staticmethod
    def _osx_init():
        # there are some missing paths, see github issue #40
        paths = ['/bin', '/sbin', '/usr/bin', '/usr/sbin', '/usr/local/bin',
                 '/usr/local/sbin', '/opt/bin', '/opt/sbin', '/opt/local/bin',
                 '/opt/local/sbin']
        os.environ["PATH"] = ':'.join(paths)
