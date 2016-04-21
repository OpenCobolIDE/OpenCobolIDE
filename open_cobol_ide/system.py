"""
System utility module (get system info, platform specific path,...).
"""
import locale
import subprocess
import functools
import os
import platform
import shlex
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


def which(cmd, mode=os.F_OK | os.X_OK, path=None,
          include_settings_path=True):
    """Given a command, mode, and a PATH string, return the path which
    conforms to the given mode on the PATH, or None if there is no such
    file.

    `mode` defaults to os.F_OK | os.X_OK. `path` defaults to the result
    of os.environ.get("PATH"), or can be overridden with a custom search
    path.

    TAKEN from the shutil module (cause it is not available on python <= 3.3)
    """
    from open_cobol_ide.settings import Settings

    # Check that a given file can be accessed with the correct mode.
    # Additionally check that `file` is not a directory, as on Windows
    # directories pass the os.access check.
    def _access_check(fn, mode):
        return (os.path.exists(fn) and os.access(fn, mode)
                and not os.path.isdir(fn))

    # If we're given a path with a directory part, look it up directly rather
    # than referring to PATH directories. This includes checking relative to the
    # current directory, e.g. ./script
    if os.path.dirname(cmd):
        if _access_check(cmd, mode):
            return cmd
        return None

    if path is None:
        path = os.environ.get("PATH", os.defpath)
    if not path:
        return None

    path = path.split(os.pathsep)
    if include_settings_path and Settings().path:
        path = Settings().path.split(os.pathsep) + path

    if sys.platform == "win32":
        # The current directory takes precedence on Windows.
        if not os.curdir in path:
            path.insert(0, os.curdir)

        # PATHEXT is necessary to check on Windows.
        pathext = os.environ.get("PATHEXT", "").split(os.pathsep)
        # See if the given file matches any of the expected path extensions.
        # This will allow us to short circuit when given "python.exe".
        # If it does match, only test that one, otherwise we have to try
        # others.
        if any(cmd.lower().endswith(ext.lower()) for ext in pathext):
            files = [cmd]
        else:
            files = [cmd + ext for ext in pathext]
    else:
        # On other platforms you don't have things like PATHEXT to tell you
        # what file suffixes are executable, so just pass on cmd as-is.
        files = [cmd]

    seen = set()

    for dir in path:
        normdir = os.path.normcase(dir)
        if not normdir in seen:
            seen.add(normdir)
            for thefile in files:
                name = os.path.join(dir, thefile)
                if _access_check(name, mode):
                    return name
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
                if os.path.isfile(pth):
                    continue
                try:
                    files = os.listdir(pth)
                except OSError:
                    files = []
                if 'cursors' not in files and 'index.theme' in files:
                    themes.append(d)
    return sorted(list(set(themes)))


def normpath(path):
    if path:
        return os.path.normpath(path)
    return path


def shell_split(string):
    return shlex.split(string, posix=False)


def get_system_infos():
    from open_cobol_ide import __version__
    from open_cobol_ide.compilers import GnuCobolCompiler
    from pyqode.qt import QtCore
    import pyqode.core
    import pyqode.cobol
    import pygments

    try:
        import qdarkstyle
    except ImportError:
        qdarkstyle_version = 'Not installed'
    else:
        qdarkstyle_version = qdarkstyle.__version__

    def get_linux_distro():
        try:
            out = str(subprocess.check_output(['lsb_release', '-i']),
                      locale.getpreferredencoding())
        except OSError:
            distro = platform.linux_distribution()[0]
            if not distro:
                distro = 'linux distribution not found'
        else:
            distro = out.split(':')[1].strip()
        return distro

    system_info = platform.system()
    if 'linux' in sys.platform.lower():
        system_info = get_linux_distro()
    elif 'darwin' in sys.platform.lower():
        system_info = 'Mac OS X %s' % platform.mac_ver()[0]
    return '\n'.join([
        'Operating System: %s' % system_info,
        'OpenCobolIDE: %s' % __version__,
        'GnuCOBOL: %s' % GnuCobolCompiler().get_version(
            include_all=False),
        'Python: %s (%dbits)' % (platform.python_version(), 64
                                 if sys.maxsize > 2**32 else 32),
        'Qt: %s' % QtCore.QT_VERSION_STR,
        'PyQt: %s' % QtCore.PYQT_VERSION_STR,
        'pyqode.core: %s' % pyqode.core.__version__,
        'pyqode.cobol: %s' % pyqode.cobol.__version__,
        'pyqode.qt: %s' % pyqode.qt.__version__,
        'pygments: %s' % pygments.__version__,
        'QDarkStyle: %s' % qdarkstyle_version
    ])
