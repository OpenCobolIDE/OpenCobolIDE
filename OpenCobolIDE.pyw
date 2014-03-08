#!/usr/bin/env python
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
This script is intendented to launch OpenCobolIDE on windows. It simply call
oci.main:main function and enable freeze support.
"""
import logging
logging.basicConfig()
from oci.main import main
from oci.constants import getAppTempDirectory
from multiprocessing import freeze_support
import sys
import os

if hasattr(sys, "frozen"):
    sys.stdout = open(os.path.join(getAppTempDirectory(),
                                   "OpenCobolIDE-stdout%s.log" % __name__), 'w')
    sys.stderr = open(os.path.join(getAppTempDirectory(),
                                   "OpenCobolIDE-stdout%s.log" % __name__), 'w')


if __name__ == "__main__":
    freeze_support()

    main()
