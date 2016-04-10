"""
This module contains the functions needed to setup the logging module for the
application.
"""
import logging
import logging.handlers
import os
from open_cobol_ide.system import get_cache_directory


rotating_file_handler = None


def get_path():
    """
    Gets the log file path
    """
    pth = os.path.join(get_cache_directory(), "OpenCobolIDE.log")
    print(pth)
    return pth


def setup_logging(version, level=logging.INFO):
    """
    Configures the logger, adds a stream handler and a file handler.

    :param version: version of the application
    :param debug: True to enable debug log level, otherwise the info log
        level is used.
    """
    global rotating_file_handler
    rotating_file_handler = logging.handlers.RotatingFileHandler(
            get_path(), maxBytes=2*1024*1024, backupCount=5)
    handlers = [rotating_file_handler, logging.StreamHandler()]
    logging.basicConfig(
        level=logging.WARNING, handlers=handlers,
        format='%(asctime)s:%(msecs)03d::%(levelname)s::%(process)d::%(name)s'
        '::%(message)s', datefmt='%H:%M:%S')
    logging.getLogger().setLevel(level)
    ocide_logger = logging.getLogger('open_cobol_ide')
    ocide_logger.info('version: %s', version)


def clear_logs():
    rotating_file_handler.doRollover()
    failures = []
    for i in range(6):
        filename = 'OpenCobolIDE.log%s' % ('' if not i else '.%d' % i)
        pth = os.path.join(get_cache_directory(), filename)
        try:
            os.remove(pth)
        except OSError:
            if os.path.exists(pth):
                logging.getLogger('open_cobol_ide').exception(
                    'failed to remove log file %r', pth)
                failures.append(pth)
    return failures


def get_application_log():
    try:
        with open(get_path(), 'r') as f:
            content = f.read()
        return content
    except FileNotFoundError:
        return ''
