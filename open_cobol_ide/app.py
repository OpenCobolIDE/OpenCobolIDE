"""
This module contains the ide application. This is where we glue the
various managers and gui parts together.

"""
import argparse
import logging
import mimetypes
import os
import sys
from pyqode.qt import QtWidgets, QT_API, PYQT5_API, PYSIDE_API
from open_cobol_ide import __version__, logger, system
from open_cobol_ide.controllers import (
    CobolController, EditController, FileController, HelpController,
    HomeController, ViewController)
from open_cobol_ide.compilers import check_compiler, CompilerNotFound
from open_cobol_ide.settings import Settings
from open_cobol_ide.view.main_window import MainWindow


def _logger():
    return logging.getLogger(__name__)


_original_env = os.environ.copy()


class Application:
    """
    Sets up the Qt Application, the main window and the various controllers.

    The application class contains references to the main window user interface
    and to the various controllers so that they can collaborate each others.
    """

    def apply_mimetypes_preferences(self):
        for ext in Settings().all_extensions:
            mimetypes.add_type('text/x-cobol', ext)
            mimetypes.add_type('text/x-cobol', ext.upper())

    def __init__(self, parse_args=True):
        self.init_env()
        self.app = QtWidgets.QApplication(sys.argv)
        if parse_args and not system.darwin:
            args = self.parse_args()
            verbose = args.verbose
            files = args.files
        else:
            verbose = False
            files = []
        logger.setup_logging(__version__, debug=verbose or Settings().verbose)
        self.name = 'OpenCobolIDE'
        self.version = __version__
        self.title = '%s %s' % (self.name, self.version)

        self.apply_mimetypes_preferences()

        self.win = MainWindow()
        self.win.setWindowTitle(self.title)
        self.file = FileController(self)
        self.view = ViewController(self)
        self.home = HomeController(self)
        self.edit = EditController(self)
        self.cobol = CobolController(self)
        self.help = HelpController(self)

        self.view.show_perspective(Settings().perspective)
        self.view.show_home_page()

        self.update_app_style()

        try:
            check_compiler()
        except CompilerNotFound as e:
            QtWidgets.QMessageBox.warning(
                self.win, 'COBOL compiler not found',
                'Failed to find GnuCOBOL compiler!\n\n%s.\n\n'
                "The IDE will continue to work but you won't be able to "
                'compile any file' % e)

        # open specified files
        for f in files:
            self.file.open_file(f)

    def close(self):
        self.view = None
        self.cobol = None
        self.edit = None
        self.file = None
        self.win = None
        self.home = None
        self.help = None

    def update_app_style(self):
        if Settings().dark_style:
            try:
                import qdarkstyle
            except ImportError:
                Settings().dark_style = False
            else:
                qt_api = os.environ[QT_API]
                if qt_api in PYQT5_API:
                    qss = qdarkstyle.load_stylesheet_pyqt5()
                else:
                    qss = qdarkstyle.load_stylesheet(qt_api in PYSIDE_API)
                self.app.setStyleSheet(qss)
                return
        self.app.setStyleSheet('')

    def run(self):
        """
        Run the Qt main loop.
        """
        if Settings().fullscreen:
            self.win.showFullScreen()
        else:
            self.win.showMaximized()
        return self.app.exec_()

    @classmethod
    def init_env(cls):
        """
        Inits the environment
        :return:
        """
        if sys.platform == 'win32':
            cls._windows_init()
        elif sys.platform == 'darwin':
            cls._osx_init()

    @staticmethod
    def _windows_init():
        """
        Windows specific initialisation:

        - set env var to embedded GnuCOBOL variable
        - set PATH to cobol library path only (erase previous values)
        """
        s = Settings()
        if s.path_enabled:
            os.environ['PATH'] = s.path
        else:
            try:
                os.environ['PATH'] = _original_env['PATH']
            except KeyError:
                pass
        if s.cob_config_dir_enabled:
            os.environ['COB_CONFIG_DIR'] = s.cob_config_dir
        else:
            try:
                os.environ['COB_CONFIG_DIR'] = _original_env['COB_CONFIG_DIR']
            except KeyError:
                pass
        if s.cob_copy_dir_enabled:
            os.environ['COB_COPY_DIR'] = s.cob_copy_dir
        else:
            try:
                os.environ['COB_COPY_DIR'] = _original_env['COB_COPY_DIR']
            except KeyError:
                pass
        if s.cob_include_path_enabled:
            os.environ['COB_INCLUDE_PATH'] = s.cob_include_path
        else:
            try:
                os.environ['COB_INCLUDE_PATH'] = _original_env['COB_INCLUDE_PATH']
            except KeyError:
                pass
        if s.cob_lib_path_enabled:
            os.environ['COB_LIB_PATH'] = s.cob_lib_path
        else:
            try:
                os.environ['COB_LIB_PATH'] = _original_env['COB_LIB_PATH']
            except KeyError:
                pass

    @staticmethod
    def _osx_init():
        """
        Mac OSX specific initialisation, adds missing path to the PATH
        environment variable (see github issue #40).

        """
        #
        paths = ['/bin', '/sbin', '/usr/bin', '/usr/sbin', '/usr/local/bin',
                 '/usr/local/sbin', '/opt/bin', '/opt/sbin', '/opt/local/bin',
                 '/opt/local/sbin']
        os.environ['PATH'] = ':'.join(paths)

    def exit(self):
        """
        Closes all top level windows and quits the application (without
        prompting the user, if you need to prompt the user, use
        Application.file.quit())
        """
        self.app.closeAllWindows()
        self.close()

    def parse_args(self):
        parser = argparse.ArgumentParser(
            description='Simple and lightweight COBOL IDE.')
        parser.add_argument('files', type=str, nargs='*',
                            help='List of files to open, if any')
        parser.add_argument('--verbose', dest='verbose', action='store_true',
                            help='Verbose mode will enable debug and info '
                                 'messages to be shown in the application log')
        return parser.parse_args()
