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
This is a setup script for cx_freeze to freeze the python package for
distribution.

For convenience, we include OpenCobol-MingW with the frozen executable.

This open cobol distribution was downloaded from here:
http://www.opencobol.org/modules/bwiki/index.php?Assorted%20Documents
"""
import sys
from cx_Freeze import setup, Executable

# get version
execfile('cobcide/__init__.py')

# Dependencies are automatically detected, but it might need fine tuning.
build_exe_options = {"packages": ["pcef", 'pygments'],
                     "excludes": ["tkinter"]}

# GUI applications require a different base on Windows (the default is for a
# console application).
base = None
if sys.platform == "win32":
    base = "Win32GUI"

setup(name="OpenCobolIDE",
      version=__version__,
      description="A simple cobol IDE based on OpenCobol",
      options={"build_exe": build_exe_options},
      executables=[Executable("cobcide/main.py", base=base,
                              targetName="OpenCobolIDE.exe",
                              icon="cobcide/ui/rc/silex-icon.ico")])
