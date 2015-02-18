from pyqode.qt import QtCore, QtGui
from open_cobol_ide.view.dialogs.about import DlgAbout
from open_cobol_ide.view.dialogs.report_bug import DlgReportBug
from .base import Controller


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
        DlgReportBug.report_bug(self.main_window)
