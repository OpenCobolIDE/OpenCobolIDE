#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# PCEF - PySide Code Editing framework
# Copyright 2013, Colin Duquesnoy <colin.duquesnoy@gmail.com>
#
# This software is released under the LGPLv3 license.
# You should have received a copy of the GNU Lesser General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
"""
This script calls the pyside ui compiler on all ui files found in the cwd.
(It also compiles the qrc files to *_rc.py)
"""
import glob
import os

# compile ui files
for name in glob.glob("*.ui"):
    base = name.split(".")[0]
    pyside_cmd = "pyside-uic {0} > {1}_ui.py".format(name, base)
    print pyside_cmd
    os.system(pyside_cmd)

for name in glob.glob("*.qrc"):
    base = name.split(".")[0]
    pyside_cmd = "pyside-rcc {0} > {1}_rc.py".format(name, base)
    print pyside_cmd
    os.system(pyside_cmd)


