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
This script calls the PyQt ui compiler on all ui files found in the cwd.
(It also compiles the qrc files to *_rc.py)
"""
import fnmatch
import os


def findQrcFilesRecursively(root=os.getcwd()):
    matches = []
    for root, dirnames, filenames in os.walk(root):
        for filename in fnmatch.filter(filenames, '*.qrc'):
            matches.append(os.path.join(root, filename))
    return matches


def compile_rc(root=os.getcwd()):
    matches = findQrcFilesRecursively(root)
    for name in matches:
        base = name.split(".")[0]
        cmd = "pyrcc4 -py3 {0} > {1}_pyqt_rc.py".format(name, base)
        print(cmd)
        os.system(cmd)


if __name__ == "__main__":
    compile_rc()
