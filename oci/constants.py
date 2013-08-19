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
# cobol use - extensively for complex identifier, don't break them!
import sys

pyqode.core.constants.WORD_SEPARATORS.remove("-")


ICON_PARAGRAPH = ":/ide-icons/rc/paragraph"
ICON_VAR = ":/ide-icons/rc/var"
ICON_KEYWORD = ":/ide-icons/rc/keyword"

EXECUTABLE_EXTENSION = ".exe"
MODULE_EXTENSION = ".so"
if sys.platform == "win32":
    MODULE_EXTENSION = ".dll"

class ProgramType:
    """
    Enumerates the supported file types along with their base compile command
    format
    """
    #: Cobol program (executable compiled with -x switch)
    Executable = (0, 'cobc -x {0} -o {1} {2}', EXECUTABLE_EXTENSION)
    #: Cobol subprogram (shared object/dll compiled without the -x switch)
    Module = (1, 'cobc {0} -o {1} {2}', MODULE_EXTENSION)


def getAppTempDirectory():
    """
    Returns a platform dependant temp fold
    """
    if sys.platform == "win32":
        pth = os.path.join(os.getcwd(), "temp")
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
