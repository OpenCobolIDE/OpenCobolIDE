#!/usr/bin/env python
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
This scripts wraps the cobol program terminal on windows, it run the
cobol program and ask for a user input when the program finished.

This script is only meant to be executed on windows.
"""
import sys
import os
assert sys.platform == "win32"
if len(sys.argv) == 3:
    target, cwd = sys.argv[1], sys.argv[2]
    if os.path.exists(cwd) and os.path.isdir(cwd):
        os.chdir(cwd)
        if os.path.exists(target) and os.path.isfile(target) and \
                target.endswith(".exe"):
            os.system(target)
        else:
            print("Target <%s> is not an executable cobol program" % target ,
                  sys.stderr)
    else:
        print("Cwd <%s> is not a valid directory" % cwd, sys.stderr)
if sys.version_info[0] == 2:
    raw_input("\nPress ENTER to terminate...")
else:
    input("\nPress ENTER to terminate...")
