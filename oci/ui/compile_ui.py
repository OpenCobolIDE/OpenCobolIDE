#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2013 Colin Duquesnoy
#
# This file is part of pyQode.
#
# pyQode is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option) any
# later version.
#
# pyQode is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
# details.
#
# You should have received a copy of the GNU Lesser General Public License along
# with pyQode. If not, see http://www.gnu.org/licenses/.
#
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
        if l.startswith("import "):
            l = "from . " + l
        if "from PySide import" in l:
            l = l.replace("from PySide import", "from pyqode.qt import")
        new_lines.append(l)
    with open(script, 'w') as f_script:
        f_script.write("\n".join(new_lines))


def main():
    # ui scripts
    for ui_file in glob.glob("*.ui"):
        base_name = os.path.splitext(ui_file)[0]
        dst = "%s_ui.py" % base_name
        cmd = "pyside-uic %s -o %s" % (ui_file, dst)
        print(cmd)
        os.system(cmd)
        fix_script(dst)

    # rc scripts
    for rc_file in glob.glob("*.qrc"):
        base_name = os.path.splitext(rc_file)[0]
        dst = "%s_rc.py" % base_name
        cmd = "pyside-rcc -py3 %s -o %s" % (rc_file, dst)
        print(cmd)
        os.system(cmd)
        fix_script(dst)

if __name__ == "__main__":
    main()
