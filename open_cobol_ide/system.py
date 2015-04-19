"""
System utility module (get system info, platform specific path,...).
"""
import functools
import os
import platform
import sys
from pyqode.qt import QtGui


windows = platform.system() == 'Windows'
darwin = platform.system() == 'Darwin'
linux = platform.system() == 'Linux'
if linux:
    ubuntu = platform.linux_distribution()[0].lower() == 'ubuntu'
else:
    ubuntu = False


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


def which(program):
    """
    Gets the full path of a command, searching in 'PATH'.

    :param program: program  command
    :return: Path or None if the program could not be found
    """
    import os

    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        dirs = os.environ["PATH"].split(os.pathsep)
        dirs.append(os.path.join(os.path.dirname(sys.executable), 'Scripts'))
        dirs.append(os.getcwd())
        for path in dirs:
            if windows:
                pgm = '%s.exe' % program
            else:
                pgm = program
            path = path.strip('"')
            exe_file = os.path.join(path, pgm)
            if is_exe(exe_file):
                return exe_file

    return None


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
                            'Caches', 'OpenCobolIDE')
    else:
        return os.path.join(os.path.expanduser("~"), '.cache', '.OpenCobolIDE')


def icon_themes():
    themes = []
    for path in QtGui.QIcon.themeSearchPaths():
        try:
            dirs = os.listdir(path)
        except OSError:
            pass
        else:
            for d in dirs:
                pth = os.path.join(path, d)
                if os.path.isdir(pth) and 'cursors' not in os.listdir(pth):
                    themes.append(d)
    return themes
