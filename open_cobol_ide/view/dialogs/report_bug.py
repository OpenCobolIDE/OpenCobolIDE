import logging
import pygments
import pyqode.core
import pyqode.cobol
import pyqode.qt
import platform
import urllib.parse
import sys
from pyqode.qt import QtWidgets, QtCore, QtGui
from open_cobol_ide import __version__, logger
from open_cobol_ide.view.forms.dlg_report_bug_ui import Ui_Dialog
from open_cobol_ide.compilers import GnuCobolCompiler


BUG_DESCRIPTION = '''%s

## System information

%s

'''


def _logger():
    return logging.getLogger(__name__)


class DlgReportBug(QtWidgets.QDialog):
    def __init__(self, parent):
        super().__init__(parent)
        self.ui = Ui_Dialog()
        self.ui.setupUi(self)
        self.ui.lineEditTitle.textChanged.connect(self.enable_submit)
        self.ui.plainTextEditDesc.textChanged.connect(self.enable_submit)
        self.ui.pushButtonSubmit.clicked.connect(self.submit)
        self.enable_submit()

    def enable_submit(self, *_):
        self.ui.pushButtonSubmit.setEnabled(
            self.ui.lineEditTitle.text().strip() != '' and
            self.ui.plainTextEditDesc.toPlainText().strip() != '')

    def submit(self):
        title = self.ui.lineEditTitle.text().strip()
        description = self.ui.plainTextEditDesc.toPlainText().strip()
        bug = self.ui.radioButtonBug.isChecked()
        if bug:
            title = '[Bug] %s' % title
            description = BUG_DESCRIPTION % (
                description, self.get_system_infos())
        else:
            title = '[Enhancement] %s' % title
        url_data = urllib.parse.urlencode(
            {'title': title, 'body': description})
        url = 'https://github.com/OpenCobolIDE/OpenCobolIDE/issues/new?' + \
            url_data
        QtWidgets.QMessageBox.information(
            self, 'Complete bug report on www.github.com',
            'To complete the report process, we need you to submit the '
            'generated ticket on our issue tracker. We will open a browser to '
            'our tracker, you just need to login with your github account and '
            'press the submit button at the end of the page.')
        QtGui.QDesktopServices.openUrl(QtCore.QUrl.fromEncoded(url))
        self.accept()

    @classmethod
    def report_bug(cls, parent):
        dlg = cls(parent)
        dlg.exec_()

    def get_system_infos(self):
        try:
            import qdarkstyle
        except ImportError:
            qdarkstyle_version = 'Not installed'
        else:
            qdarkstyle_version = qdarkstyle.__version__

        system = platform.system()
        is_linux = system.lower() == 'linux'
        return '\n'.join([
            '- Operating System: %s' % system +
            ' (' + ' '.join(platform.linux_distribution()) + ')' if is_linux
            else '',
            '- OpenCobolIDE: %s' % __version__,
            '- GnuCobol: %s' % GnuCobolCompiler().get_version(),
            '- Python: %s (%dbits)' % (platform.python_version(), 64
                                       if sys.maxsize > 2**32 else 32),
            '- Qt: %s' % QtCore.QT_VERSION_STR,
            '- PyQt: %s' % QtCore.PYQT_VERSION_STR,
            '- pyqode.core: %s' % pyqode.core.__version__,
            '- pyqode.cobol: %s' % pyqode.cobol.__version__,
            '- pyqode.qt: %s' % pyqode.qt.__version__,
            '- pygments: %s' % pygments.__version__,
            '- QDarkStyle: %s' % qdarkstyle_version
        ])

    def get_application_log(self):
        with open(logger.get_path(), 'r') as f:
            return f.read()
