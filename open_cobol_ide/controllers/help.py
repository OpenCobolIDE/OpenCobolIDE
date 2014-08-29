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
