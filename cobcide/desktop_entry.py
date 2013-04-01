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
from PySide.QtCore import QFileInfo
import sys

DESKTOP_ENTRY = """[Desktop Entry]
Type=Application
Version=1.2
Name=OpenCobolIDE
Comment=Flash card based learning tool
Exec={0}
Icon= {1}
Terminal=false
Categories=Development;Languages;Cobol;
"""
DESKTOP_ENTRY_PATH = "/usr/share/applications/open-cobol-ide.desktop"


def check(main_file):
    """
    Checks if we need to create a desktop entry.
    There are two reasons to create a new desktop entry;
        - no desktop entry exists on the system
        - the executables does not match (allow to update entry when switching
        from the python script to the binary script installed with pip - mainly
        used by the developpers when they update their system).
    :return:
    """
    import cobcide
    ret_val = False
    # get cobcide path, then our icon path
    cobcide_path = QFileInfo(cobcide.__file__).dir().path()
    svg_path = os.path.join(cobcide_path, "ui", "rc", "silex-icon.svg")
    # check executable, can be the pip installed binary script or our main.py
    executable = "OpenCobolIDE"
    if QFileInfo(main_file).suffix() == "py":
        executable = main_file
    desktop_entry = DESKTOP_ENTRY.format(executable, svg_path)
    if not os.path.exists(DESKTOP_ENTRY_PATH):
        ret_val = True
    else:
        # load desktop entry and check executable
        with open(DESKTOP_ENTRY_PATH, 'r') as entry_file:
            loaded_desktop_entry = entry_file.read()
            if loaded_desktop_entry != desktop_entry:
                ret_val = True
    return ret_val


def main():
    """
    Creates the desktop entry, the script must be ran as root!
    """
    cobcide_path = QFileInfo(__file__).dir().path()
    print cobcide_path
    svg_path = os.path.join(cobcide_path, "ui", "rc", "silex-icon.svg")
    # check executable, can be the pip installed binary script or our main.py
    executable = "OpenCobolIDE"
    if QFileInfo(__file__).suffix() == "py":
        executable = os.path.join(cobcide_path, "main.py")
    desktop_entry = DESKTOP_ENTRY.format(executable, svg_path)
    with open(DESKTOP_ENTRY_PATH, "w") as f:
        print "Writing {0}:\n{1}".format(DESKTOP_ENTRY_PATH, desktop_entry)
        f.write(desktop_entry)

if __name__ == "__main__":
    sys.exit(main())