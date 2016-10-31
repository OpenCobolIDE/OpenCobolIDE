"""
This module contains the functions needed to setup the logging module for the
application.
"""
import logging
import logging.handlers
import glob
import os
from open_cobol_ide.system import get_cache_directory


file_handler = None


def get_path():
    """
    Gets the log file path
    """
    pth = os.path.join(get_cache_directory(), "OpenCobolIDE-%d.log" % os.getpid())
    return pth


def setup_logging(version, level=logging.INFO):
    """
    Configures the logger, adds a stream handler and a file handler.

    :param version: version of the application
    :param debug: True to enable debug log level, otherwise the info log
        level is used.
    """
    global file_handler
    if len(get_log_files()) > 5:
        clear_logs()
    file_handler = logging.FileHandler(get_path())
    handlers = [file_handler, logging.StreamHandler()]
    logging.basicConfig(
        level=logging.WARNING, handlers=handlers,
        format='%(asctime)s:%(msecs)03d::%(levelname)s::%(process)d::%(name)s'
        '::%(message)s', datefmt='%H:%M:%S')
    logging.getLogger().setLevel(level)
    ocide_logger = logging.getLogger('open_cobol_ide')
    ocide_logger.info('version: %s', version)


def clear_logs():
    if file_handler:
        file_handler.close()
    failures = []
    for filename in get_log_files():
        pth = os.path.join(get_cache_directory(), filename)
        try:
            os.remove(pth)
        except OSError:
            if os.path.exists(pth):
                logging.getLogger('open_cobol_ide').exception(
                    'failed to remove log file %r', pth)
                failures.append(pth)
    return failures


def get_log_files():
    return glob.glob(os.path.join(get_cache_directory(), '*.log'))


def get_application_log():
    try:
        with open(get_path(), 'r') as f:
            content = f.read()
        return content
    except FileNotFoundError:
        return ''
    except MemoryError:
        return 'Failed to open application log...'
