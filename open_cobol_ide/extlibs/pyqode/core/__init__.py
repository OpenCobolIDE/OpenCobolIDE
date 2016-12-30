# -*- coding: utf-8 -*-
"""
The core package contains the core components needed for writing a pyqode based
application. It is the "de facto" requirement for any pyqode extension.

It contains the base classes for both the backend and the frontend and provides
a series of modes and panels that might be useful for any kind of code editor
widget, i.e. pyqode.core is a generic code editor widget.
"""
import logging


__version__ = '2.11.0'


logging.addLevelName(1, "PYQODEDEBUGCOMM")
logging.addLevelName(5, "PYQODEDEBUG")


try:
    # check if application code is using gettext
    _('')
except NameError:
    # install a null translation
    import gettext
    gettext.NullTranslations().install()
