"""
This module contains the functions needed to setup the logging module for the
application.
"""
import logging
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
    level = logging.DEBUG if debug else logging.INFO
    logger = logging.getLogger()
    formatter = logging.Formatter(
        '%(levelname)s::%(name)s::%(message)s',
        '%Y-%m-%d %H:%M:%S')
    handlers = [
        logging.StreamHandler(),
        logging.FileHandler(get_path(), mode='w')
    ]
    logger.setLevel(level)
    for handler in handlers:
        handler.setFormatter(formatter)
    for handler in handlers:
        logger.addHandler(handler)
    logging.getLogger('open_cobol_ide').info('version: %s' % version)
