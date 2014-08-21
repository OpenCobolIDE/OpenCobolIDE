from pyqode.qt import QtCore, QtGui
from .base import Controller
from ..view.dialogs.about import DlgAbout


class HelpController(Controller):
    """
    Controls the ? menu: show help contents and about dialog.
    """
    def __init__(self, app):
        super().__init__(app)
        self.ui.actionHelp.triggered.connect(self._show_help_contents)
        self.ui.actionAbout.triggered.connect(self._show_about_dlg)

    def _show_help_contents(self):
        QtGui.QDesktopServices.openUrl(
            QtCore.QUrl('http://opencobolide.readthedocs.org/en/latest/'))

    def _show_about_dlg(self):
        dlg = DlgAbout(self.main_window)
        dlg.exec_()
