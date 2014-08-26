"""
System utility module (get system info, platform specific path,...).
"""
import functools
import os
import sys

windows = sys.platform == 'win32'
darwin = sys.platform == 'darwin'
linux = sys.platform == 'linux'

def _mkdir(func):
    @functools.wraps(func)
    def wrapper(*args, **kwds):
        ret = func(*args, **kwds)
        try:
            os.makedirs(ret)
        except OSError:
            pass
        return ret
    return wrapper


@_mkdir
def get_cache_directory():
    """
    Gets the platform specific cache directory (where we store the log file and
    the temporary files create by the linter).
    :return: platform specific cache directory.
    """
    if windows:
        return os.path.join(os.path.expanduser("~"), 'OpenCobolIDE', 'cache')
    elif darwin:
        # todo: check if there is a better place for caching file on mac
        return os.path.join(os.path.expanduser("~"), 'OpenCobolIDE', 'cache')
    else:
        return os.path.join(os.path.expanduser("~"), '.cache', '.OpenCobolIDE')
