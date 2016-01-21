"""
This module contains the functions needed to setup the logging module for the
application.
"""
import logging
import logging.handlers
import os
from open_cobol_ide.system import get_cache_directory


def get_path():
    """
    Gets the log file path
    """
    pth = os.path.join(get_cache_directory(), "OpenCobolIDE.log")
    return pth


def setup_logging(version, debug):
    """
    Configures the logger, adds a stream handler and a file handler.

    :param version: version of the application
    :param debug: True to enable debug log level, otherwise the info log
        level is used.
    """
    handler = logging.handlers.RotatingFileHandler(
            get_path(), maxBytes=2*1024*1024, backupCount=5)
    handlers = [handler, logging.StreamHandler()]
    logging.basicConfig(
        level=logging.WARNING, handlers=handlers,
        format='%(asctime)s:%(msecs)03d::%(levelname)s::%(process)d::%(name)s'
        '::%(message)s', datefmt='%H:%M:%S')
    logging.getLogger().setLevel(logging.INFO if not debug else logging.DEBUG)
    ocide_logger = logging.getLogger('open_cobol_ide')
    ocide_logger.info('version: %s', version)
