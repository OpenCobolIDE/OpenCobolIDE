import pyqode.core
import pyqode.cobol
import pygments
from pyqode.qt import QtGui, QtCore, QtWidgets
from open_cobol_ide import __version__, logger
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
        'GnuCobol',
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
        versions = [GnuCobolCompiler().get_version(),
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

    def keyPressEvent(self, e):
        self.close()

    def _on_verbose_toggled(self, state):
        Settings().verbose = state
        if not DlgAbout._flg_verbose:
            QtWidgets.QMessageBox.information(
                self, 'Restart required',
                'You need to restart the IDE for the change to be applied.')
            DlgAbout._flg_verbose = True
