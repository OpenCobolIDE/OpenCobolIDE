#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright (c) <2013-2014> Colin Duquesnoy
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
This scripts compile the ui and qrc files using pyside dev tools then modify
them to use pyqode.qt instead of PySide. It also adapts the rc imports so that
they works with python3
"""
import glob
import os


def fix_script(script):
    with open(script, 'r') as f_script:
        lines = f_script.read().splitlines()
    new_lines = []
    for l in lines:
        if "from PyQt5 import" in l:
            l = l.replace("from PyQt5 import", "from pyqode.qt import")
        new_lines.append(l)
    with open(script, 'w') as f_script:
        f_script.write("\n".join(new_lines))


def main():
    # ui scripts
    for ui_file in glob.glob("*.ui"):
        base_name = os.path.splitext(ui_file)[0]
        dst = "%s_ui.py" % base_name
        if (not os.path.exists(dst) or
                os.path.getmtime(ui_file) > os.path.getmtime(dst)):
            cmd = "pyuic5 --from-import %s -o %s" % (ui_file, dst)
            print(cmd)
            os.system(cmd)
            fix_script(dst)
        else:
            print("%s up to date" % ui_file)

    # rc scripts
    for rc_file in glob.glob("*.qrc"):
        base_name = os.path.splitext(rc_file)[0]
        dst = "%s_rc.py" % base_name
        cmd = "pyrcc5 %s -o %s" % (rc_file, dst)
        if (not os.path.exists(dst) or
                os.path.getmtime(rc_file) > os.path.getmtime(dst)):
            print(cmd)
            os.system(cmd)
            fix_script(dst)
        else:
            print("%s up to date" % rc_file)


if __name__ == "__main__":
    main()
