import logging
import os

import pygments
import pyqode.cobol
import pyqode.core
from pyqode.qt import QtGui, QtCore, QtWidgets

from open_cobol_ide import __version__, logger, system
from open_cobol_ide.compilers import GnuCobolCompiler
from open_cobol_ide.settings import Settings
from open_cobol_ide.view.forms import dlg_about_ui


class DlgAbout(QtWidgets.QDialog, dlg_about_ui.Ui_Dialog):
    """
    Shows the about text, the license, the authors list and the 3rd party
    libraries versions.
    """
    _flg_verbose = False

    HEADERS = [
        'GnuCOBOL',
        'Qt',
        'PyQt',
        'pyqode.core',
        'pyqode.cobol',
        'pygments',
        'QDarkStyle'
    ]

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)
        self.checkBoxVerbose.setChecked(Settings().verbose)
        self.tabWidget.setCurrentIndex(0)
        self.tbwVersions.setColumnCount(1)
        self.tbwVersions.setRowCount(len(self.HEADERS))
        self.tbwVersions.setVerticalHeaderLabels(self.HEADERS)
        self.tbwVersions.setHorizontalHeaderLabels(['Version'])
        self.tbwVersions.verticalHeader().setStretchLastSection(False)
        self.labelMain.setText(self.labelMain.text() % __version__)
        self.setMinimumWidth(640)
        self.setMinimumHeight(480)
        self.setWindowIcon(QtGui.QIcon.fromTheme(
            'help-about', QtGui.QIcon(
                ':/ide-icons/rc/dialog-information.png')))
        try:
            import qdarkstyle
        except ImportError:
            qdarkstyle_version = 'Not installed'
        else:
            qdarkstyle_version = qdarkstyle.__version__
        versions = [GnuCobolCompiler().get_version(include_all=False),
                    QtCore.QT_VERSION_STR,
                    QtCore.PYQT_VERSION_STR,
                    pyqode.core.__version__,
                    pyqode.cobol.__version__,
                    pygments.__version__,
                    qdarkstyle_version]
        for i, version in enumerate(versions):
            item = QtWidgets.QTableWidgetItem(version)
            self.tbwVersions.setItem(i, 0, item)
        with open(logger.get_path(), 'r') as f:
            self.textEditLog.setText(f.read())
        self.checkBoxVerbose.toggled.connect(self._on_verbose_toggled)

        self.edit_compiler_infos.setFont(
            QtGui.QFont(Settings().font, 9))

        # from pyqode.core._forms.pyqode_core_rc
        QtGui.QFontDatabase.addApplicationFont(
            ':/fonts/rc/SourceCodePro-Regular.ttf')
        QtGui.QFontDatabase.addApplicationFont(
            ':/fonts/rc/SourceCodePro-Bold.ttf')

        template = '''cobc --info
============

%(cobc_infos)s

cobcrun --runtime-env
=====================

%(cobcrun_infos)s
'''

        gnucobol_infos = template % {
            'cobc_infos': GnuCobolCompiler.get_cobc_infos(),
            'cobcrun_infos': GnuCobolCompiler.get_cobcrun_infos()
        }
        self.edit_compiler_infos.setPlainText(gnucobol_infos)

        self.bt_clear_logs.clicked.connect(self._clear_logs)

    def _on_verbose_toggled(self, state):
        Settings().verbose = state
        if not DlgAbout._flg_verbose:
            QtWidgets.QMessageBox.information(
                self, 'Restart required',
                'You need to restart the IDE for the change to be applied.')
            DlgAbout._flg_verbose = True

    def _clear_logs(self):
        for i in range(6):
            filename = 'OpenCobolIDE.log%s' % ('' if not i else '.%d' % i)
            pth = os.path.join(system.get_cache_directory(), filename)
            try:
                os.remove(pth)
            except OSError:
                if os.path.exists(pth):
                    _logger().exception('failed to remove log file %r', pth)
        QtWidgets.QMessageBox.information(self, 'Logs cleared',
                                          'Log files have been cleared.')
        self.textEditLog.clear()


def _logger():
    return logging.getLogger(__name__)
