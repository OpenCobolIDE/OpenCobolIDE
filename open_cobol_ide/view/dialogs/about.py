import pyqode.core
import pyqode.cobol
import pygments
import qdarkstyle

from pyqode.qt import QtCore, QtWidgets
from ..forms import dlg_about_ui
from ... import __version__
from ...compiler import GnuCobolCompiler


class DlgAbout(QtWidgets.QDialog, dlg_about_ui.Ui_Dialog):
    """
    Shows the about text, the license, the authors list and the 3rd party
    libraries versions.
    """
    HEADERS = [
        'OpenCobol',
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
        self.tabWidget.setCurrentIndex(0)
        self.tbwVersions.setColumnCount(1)
        self.tbwVersions.setRowCount(len(self.HEADERS))
        self.tbwVersions.setVerticalHeaderLabels(self.HEADERS)
        self.tbwVersions.setHorizontalHeaderLabels(['Version'])
        self.labelMain.setText(self.labelMain.text() % __version__)
        self.adjustSize()
        versions = [GnuCobolCompiler().get_version(),
                    QtCore.QT_VERSION_STR,
                    QtCore.PYQT_VERSION_STR,
                    pyqode.core.__version__,
                    pyqode.cobol.__version__,
                    pygments.__version__,
                    qdarkstyle.__version__]
        for i, version in enumerate(versions):
            item = QtWidgets.QTableWidgetItem(version)
            self.tbwVersions.setItem(i, 0, item)
