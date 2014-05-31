"""
This module provides application services such as an easy acces to the
app main window and other commonly used parts of the application such as the
errors window, the navigation panel,... Those are essentially globals
internally setup during the construction of the main window. This is done to
make decoupling functionality easier without the need to have a lots of
parameters in function calls and have a more modular approach.

"""

_main_window = None


def _set_main_window(win):
    global _main_window
    _main_window = win


def main_window():
    """
    Returns a reference to the application main window.
    :rtype: oci.frontend.main_window.MainWindow
    """
    global _main_window
    return _main_window
