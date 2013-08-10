#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# OpenCobolIDE
#
# Copyright 2013, Colin Duquesnoy <colin.duquesnoy@gmail.com>
#
# This software is released under the GPLv3 license.
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#
"""
This the application main script. It simply calls oci.main
"""
import os
# setup environement to force pyqode to use PyQt4
os.environ['QT_API'] = 'PyQt'
import pyqode.core
from oci import main
main()
