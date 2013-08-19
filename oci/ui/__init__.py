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
import os
import pyqode.qt


def loadUi(uiFileName, baseInstance, rcFilename=None):
    """
    Loads an ui file from the ui package and load the specified qrc (must
    already be compiled)

    :param uiFileName: The ui file name (without path)

    :param baseInstance: The baseInstance on which the ui is built

    :param rcFilename: The optional qrc file to load
    """
    uiFile = os.path.join(os.path.abspath(os.path.join(__file__, "..")),
                          uiFileName)
    if rcFilename:
        rcFile = os.path.join(os.path.abspath(os.path.join(__file__, "..")),
                              rcFilename)
        pyqode.qt.importRc(rcFile)
    pyqode.qt.loadUi(uiFile, baseInstance)
