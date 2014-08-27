"""
System utility module (get system info, platform specific path,...).
"""
import functools
import os
import sys
import platform

windows = platform.system() == 'Windows'
darwin = platform.system() == 'Darwin'
linux = platform.system() == 'Linux'


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
        return os.path.join(os.path.expanduser("~"), 'Library',
                            'Application Support', 'OpenCobolIDE', 'cache')
    else:
        return os.path.join(os.path.expanduser("~"), '.cache', '.OpenCobolIDE')
