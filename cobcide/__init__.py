#!/usr/bin/env python
# This file is part of cobcide.
# 
# cobcide is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# cobcide is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with cobcide.  If not, see <http://www.gnu.org/licenses/>.
"""
cobcide (OpenCobol IDE) is a simple cobol ide based on open cobol.

This module defines the app constants such as the version, the supported file
types,...
"""
#: The OpenCobol IDE version
__version__ = "1.3.1-dev"


class FileType:
    """
    Enumerates the supported file types along with their base compile command
    string
    """
    #: Cobol program (executable compiled with -x switch)
    Program = (0, ['cobc', '-x', '-o {0}', '{0}'])
    #: Cobol subprogram (shared object/dll compiled without the -x switch)
    Subprogram = (1, ['cobc', '-o {0}', '{0}'])
    #: A regular text file, no compile command obviously
    Text = (2, None)
