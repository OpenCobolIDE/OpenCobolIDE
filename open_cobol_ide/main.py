"""
This is the application entry. This is where we create and run the
Application.
"""
from open_cobol_ide.app import Application


def main():
    """
    Application entry point.
    """
    app = Application()
    ret_code = app.run()
    app.close()
    return ret_code
