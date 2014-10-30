from pyqode.qt import QtCore, QtGui
from .base import Controller
from ..view.dialogs.about import DlgAbout


class HelpController(Controller):
    """
    Controls the ? menu: show help contents and about dialog.
    """
    #: url of the documentation on readthedocs
    help_url = 'http://opencobolide.readthedocs.org/en/latest/'

    def __init__(self, app):
        super().__init__(app)
        self.ui.actionHelp.triggered.connect(self.show_help_contents)
        self.ui.actionAbout.triggered.connect(self.show_about_dlg)
        self.ui.actionReport_a_bug.triggered.connect(self.report_bug)

    def show_help_contents(self):
        """
        Opens help_url in the default browser
        """
        QtGui.QDesktopServices.openUrl(QtCore.QUrl(self.help_url))

    def show_about_dlg(self):
        """
        Shows the about dialog.
        """
        dlg = DlgAbout(self.main_window)
        dlg.exec_()

    def report_bug(self):
        QtGui.QDesktopServices.openUrl(QtCore.QUrl.fromEncoded(
            'https://github.com/OpenCobolIDE/OpenCobolIDE/issues/new?tit'
            'le=Issue%3A &body=%23%23%23%20Description%20of%20the%20issue%0A%'
            '0A%0A%23%23%23%20System%20information%0A*%20Operating%20System%3A'
            '%20%0A*%20OpenCobolIDE%20Version%3A'))