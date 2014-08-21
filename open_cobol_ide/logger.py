"""
This module contains all the classes related to logging the
application messages.
"""
import logging
import logging.handlers
import os


def get_path():
    return os.path.join(os.path.expanduser("~"), ".OpenCobolIDE", "log")


def setup_logging(version, debug):
    """
    Configures the logger
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
