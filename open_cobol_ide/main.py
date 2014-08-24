"""
This is the application entry. This is where we create and run the
Application.
"""
from . import logger, __version__
from .app import Application


def main():
    """
    Application entry point.
    """
    logger.setup_logging(__version__, debug=True)
    app = Application()
    ret_code = app.run()
    del app
    return ret_code
