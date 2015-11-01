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
    for handler in handlers:
        handler.setFormatter(formatter)
    for handler in handlers:
        logger.addHandler(handler)
    logger.setLevel(level)
    ocide_logger = logging.getLogger('open_cobol_ide')
    ocide_logger.info('version: %s' % version)

    # todo: remove this code once we know what encoding to use to decode
    # compiler process output on windows
    import sys
    if sys.platform == 'win32':
        import locale
        from ctypes import cdll

        ocide_logger.info('sys.stdout.encoding: %s',
                          sys.stdout.encoding)
        ocide_logger.info('sys.getfilesystemencoding: %s',
                          sys.getfilesystemencoding())
        ocide_logger.info('locale.getpreferredencoding: %s',
                          locale.getpreferredencoding())
        os_encoding = 'cp' + str(cdll.kernel32.GetACP())
        ocide_logger.info('kernel32.GetACP: %s', os_encoding)
