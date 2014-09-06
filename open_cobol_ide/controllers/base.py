"""
Defines the base class for all managers.

"""
import weakref


class Controller:
    """
    Base class for all managers.

    This class simply store a weak reference to the application and a set of
    helpful properties (that are shortcuts to retrieve various parts of the
    app such as the main window user interface.

    """

    @property
    def main_window(self):
        """
        Returns a reference to the main window.
        :rtype: pyqode.qt.QtWidgets.QMainWindow
        """
        return self.app.win

    @property
    def ui(self):
        """
        Returns a reference to the main window user interface.
        :rtype: open_cobol_ide.view.forms.ide_ui.Ui_MainWindow
        """
        return self.main_window.ui

    @property
    def app(self):
        """
        Gets a reference to the application

        :rtype: open_cobol_ide.app.Application
        """
        return self._app()

    def __init__(self, app):
        """
        :param app: Reference to the application instance
        """
        self._app = weakref.ref(app)
