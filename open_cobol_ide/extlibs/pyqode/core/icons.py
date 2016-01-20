"""
This module contains a wrapper api over the various icon source pyqode
could use:
    - icon from theme (linux only)
    - icon from qrc
    - icon from qtawesome

qtawesome is an optional dependency, it is disabled by default.
If your application use qtawesome, just set ``USE_QTAWESOME`` to True.
"""
from pyqode.qt import QtGui
try:
    import qtawesome as qta
except ImportError:
    qta = None

#: This flag controls qtawesome icons should be preferred to theme/qrc icons.
USE_QTAWESOME = False

#: Default options used for rendering an icon from qtawesome.
#: Options cannot be changed after the icon has been rendered so make sure
#: to setup those options at startup (i.e. before you create any icon).
QTA_OPTIONS = {
    'color': '',
    'color_disabled': ''
}


def icon(theme_name='', path='', qta_name='', qta_options=None, use_qta=None):
    """
    Creates an icon from qtawesome, from theme or from path.

    :param theme_name: icon name in the current theme (GNU/Linux only)
    :param path: path of the icon (from file system or qrc)
    :param qta_name: icon name in qtawesome
    :param qta_options: the qtawesome options to use for controlling icon
                        rendering. If None, QTA_OPTIONS are used.
    :param use_qta: True to use qtawesome, False to use icon from theme/path.
                    None to use the global setting: USE_QTAWESOME.

    :returns: QtGui.QIcon
    """
    ret_val = None
    if use_qta is None:
        use_qta = USE_QTAWESOME
    if qta_options is None:
        qta_options = QTA_OPTIONS
    if qta is not None and use_qta is True:
        ret_val = qta.icon(qta_name, **qta_options)
    else:
        if theme_name and path:
            ret_val = QtGui.QIcon.fromTheme(theme_name, QtGui.QIcon(path))
        elif theme_name:
            ret_val = QtGui.QIcon.fromTheme(theme_name)
        elif path:
            ret_val = QtGui.QIcon(path)
    return ret_val
