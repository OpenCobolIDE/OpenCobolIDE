# This file is part of cobcide.
# 
# OCIDE is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# OCIDE is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with OCIDE.  If not, see <http://www.gnu.org/licenses/>.
"""
Script run as root to create a desktop entry
"""
import os
import sys

from PySide.QtCore import QFileInfo


#: The desktop entry base text
DESKTOP_ENTRY = """[Desktop Entry]
Type=Application
Version=1.2
Name=OpenCobolIDE
Comment=Flash card based learning tool
Exec={0}
Icon={1}
Terminal=false
Categories=Development;Languages;Cobol;
"""
#: The desktop entry path
DESKTOP_ENTRY_PATH = "/usr/share/applications/open-cobol-ide.desktop"


def check():
    """
    Checks if we need to create a desktop entry for the installed binary script

    :return True if we need to create a desktop entry
    """
    ret_val = False
    if not os.path.exists(DESKTOP_ENTRY_PATH):
        ret_val = True
    return ret_val


def main():
    """
    Creates the desktop entry for the binary script.
    """
    from cobcide import ui
    cobcide_path = QFileInfo(ui.__file__).dir().path()
    icon_path = os.path.join(cobcide_path, "rc", "silex-192x192.png")
    # check executable, can be the pip installed binary script or our main.py
    executable = "OpenCobolIDE"
    desktop_entry = DESKTOP_ENTRY.format(executable, icon_path)
    with open(DESKTOP_ENTRY_PATH, "w") as f:
        f.write(desktop_entry)


if __name__ == "__main__":
    sys.exit(main())
