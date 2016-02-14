from pyqode.qt import QtCore, QtGui, QtWidgets
from open_cobol_ide.settings import Settings
from open_cobol_ide.view.dialogs.about import DlgAbout
from .base import Controller

import qcrash.api as qcrash


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
        self.ui.btAbout.clicked.connect(self.show_about_dlg)
        self.ui.actionReport_a_bug.triggered.connect(self.report_bug)
        self.ui.actionRestore_factory_defaults.triggered.connect(
            self.restore_factory_defaults)

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
        qcrash.show_report_dialog(
            window_icon=self.main_window.windowIcon(),
            parent=self.main_window, include_log=False, include_sys_info=False)

    def restore_factory_defaults(self):
        answer = QtWidgets.QMessageBox.question(
            self.main_window, 'Restory factory defaults?',
            'Are you sure you want to restore factory defaults?\n\n'
            'Clicking yes will remove all your preferences and restart the '
            "IDE with clean settings. Use this only if you're experiencing a "
            "problem!")
        if answer == QtWidgets.QMessageBox.Yes:
            Settings().clear()
            self.app.restart()
