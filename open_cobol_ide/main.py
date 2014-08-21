"""
This is the application entry. This is where we create and run the
Application.

"""
from . import logger
from . import __version__
from .app import Application


def main():
    """
    Application entry point.

    """
    logger.setup(__version__, debug=True)
    app = Application()
    ret_code = app.run()
    del app
    return ret_code
