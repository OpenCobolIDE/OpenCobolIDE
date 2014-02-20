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
Contains application constants
"""
import os
import pyqode.core
import pyqode.widgets
# cobol use - extensively for complex identifier, don't break them!
import sys

pyqode.core.constants.WORD_SEPARATORS.remove("-")


COBOL_EXTENSIONS = [".COB", ".CBL", ".PCO", ".CPY"]
ALL_COBOL_EXTENSIONS = COBOL_EXTENSIONS + [ext.lower() for ext in COBOL_EXTENSIONS]
COBOL_FILES_FILTER = "Cobol files (%s)" % " ".join(ALL_COBOL_EXTENSIONS).replace(".", "*.")
OTHER_FILES_FILTER = "Other text files (*)"
FILTER_SEPARATOR = ";;"

ICON_PARAGRAPH = ":/ide-icons/rc/paragraph"
ICON_VAR = ":/ide-icons/rc/var"
ICON_KEYWORD = ":/ide-icons/rc/keyword"

EXECUTABLE_EXTENSION = ".exe"
MODULE_EXTENSION = ".so"
if sys.platform == "win32":
    MODULE_EXTENSION = ".dll"


WHITE_STYLE = 0
DARK_STYLE = 1


class DarkColorScheme(pyqode.widgets.ColorScheme):
    """
    Define a dark color scheme for easy customization of the stylesheet.
    """

    def __init__(self):
        super(DarkColorScheme, self).__init__()
        self.background_color = "#302F2F"
        self.title_background_color = "#4b4b4b"
        self.text_color = "#C5C5C5"
        self.border_color = "#555555"
        self.selection_bck_color = "#343434"
        self.selection_color = "#C5C5C5"
        self.hover_color = "#343434"


class ProgramType:
    """
    Enumerates the supported file types along with their base compile command
    format
    """
    #: Cobol program (executable compiled with -x switch)
    Executable = (0, ['cobc', "-x", "-o %s", "%s"],
                  EXECUTABLE_EXTENSION)
    #: Cobol subprogram (shared object/dll compiled without the -x switch)
    Module = (1, ['cobc', "-o %s", "%s"],
              MODULE_EXTENSION)

    @staticmethod
    def cmd(programType, input, output, customOptions=None):
        baseCmdArgs = programType[1]
        if programType[0] == 0:
            args = [baseCmdArgs[0], baseCmdArgs[1],
                    baseCmdArgs[2] % output, baseCmdArgs[3] % input]
        else:
            args = [baseCmdArgs[0], baseCmdArgs[1] % output,
                    baseCmdArgs[2] % input]
        if customOptions:
            args += customOptions
        return args


def getAppTempDirectory():
    """
    Returns a platform dependant temp fold
    """
    if sys.platform == "win32":
        pth = os.path.join(os.path.expanduser("~"), "OpenCobolIDE")
        if not os.path.exists(pth):
            os.mkdir(pth)
        pth = os.path.join(pth, "temp")
        if not os.path.exists(pth):
            os.mkdir(pth)
        return pth
    else:
        pth = os.path.join(os.path.expanduser("~"), ".OpenCobolIDE")
        if not os.path.exists(pth):
            os.mkdir(pth)
        pth = os.path.join(pth, "temp")
        if not os.path.exists(pth):
            os.mkdir(pth)
        return pth
