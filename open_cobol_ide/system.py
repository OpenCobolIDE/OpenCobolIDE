"""
System utility module (get system info, platform specific path,...).
"""
import functools
import os
import sys

windows = sys.platform == 'win32'
darwin = sys.platform == 'darwin'


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
    if windows:
        return os.path.join(os.path.expanduser("~"), 'OpenCobolIDE', '.cache')
    elif darwin:
        # todo: check if there is a better place for caching file on mac
        return os.path.join(os.path.expanduser("~"), '.OpenCobolIDE', '.cache')
    else:
        return os.path.join(os.path.expanduser("~"), '.cache', '.OpenCobolIDE')
