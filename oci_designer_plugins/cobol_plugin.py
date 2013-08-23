# Copyright 2013 Colin Duquesnoy
#
# This file is part of OpenCobolIDE.
#
# OpenCobolIDE is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# OpenCobolIDE is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# OpenCobolIDE. If not, see http://www.gnu.org/licenses/.
"""
This file contains all the pyqode-widgets QtDesigner oci_designer_plugins.
"""
# This only works with PyQt, PySide does not support the QtDesigner module
import os
if not 'QT_API' in os.environ:
    os.environ.setdefault("QT_API", "PyQt")
from pyqode.qt.QtGui import QIcon
from oci.editor import QCobolCodeEdit

PLUGINS_TYPES = {'QCobolCodeEdit': QCobolCodeEdit}

try:
    from pyqode.core.plugins.pyqode_core_plugin import QCodeEditPlugin

    class QCobolCodeEditPlugin(QCodeEditPlugin):
        _module = 'oci.editor'        # path to the widget's module
        _class = 'QCobolCodeEdit'    # name of the widget class
        _name = "QCobolCodeEdit"

        def createWidget(self, parent):
            return QCobolCodeEdit(parent=parent)


except ImportError:
    print("Cannot use pyQode oci_designer_plugins without PyQt4")
