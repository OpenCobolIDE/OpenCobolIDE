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
This scripts freeze the application and try to run the inno setup compiler
to create an setup executable.
"""
import glob
import os
import shutil
import sys
from cx_Freeze import setup, Executable


# Clean up
if os.path.exists("build"):
    shutil.rmtree(os.path.join(os.getcwd(), "build"))
if os.path.exists("dist"):
    shutil.rmtree(os.path.join(os.getcwd(), "dist"))

# Detect system
windows = sys.platform == 'win32'
osx = sys.platform == 'darwin'

app_script = "open-cobol-ide"
app_name = "OpenCobolIDE"
app_exe = "OpenCobolIDE.exe" if windows else "OpenCobolIDE"
srv_script = "oci/backend/server.py"
srv_name = "ociserver"
srv_exe = "ociserver.exe" if windows else "ociserver"

app_icon = "oci/frontend/ui/rc/silex-icon.ico" if windows else "oci/frontend/ui/rc/silex-192x192.icns"


# Get App version
def read_version():
    """
    Reads the version without self importing
    """
    with open("oci/__init__.py") as f:
        lines = f.read().splitlines()
        for l in lines:
            if "__version__" in l:
                return l.split("=")[1].strip().replace('"', "")
__version__ = read_version()


if len(sys.argv) == 1:
    sys.argv.append("build")

options = {# add the namespace packages that you uses
           "namespace_packages": ["pyqode.core"],
           # freeze the pygments default style along with our executable
           "includes": ["pygments.styles.default", "pygments.styles.monokai",
                        "pygments.styles.native",
                        "pkg_resources"]}

print("### Freezing application\n"
      "#####################################################################\n")
setup(name=app_name,
      version=__version__,
      options={"build_exe": options, "bdist_mac": {'iconfile': app_icon}},
      executables=[
          Executable(app_script,
                     targetName=app_exe,
                     icon=app_icon if windows else None,
                     base="Win32GUI" if windows else None),
          Executable(srv_script, targetName=srv_exe)])

if windows:
    print("### Copying OpenCobol distribution\n"
          "#####################################################################\n")
    build_dir = os.path.join(os.getcwd(), glob.glob("build/*")[0])
    cobc_dir = os.path.join(build_dir, "OpenCobol")
    if not os.path.exists(cobc_dir):
        shutil.copytree(os.path.join(os.getcwd(), "OpenCobol"),
                        os.path.join(build_dir, "OpenCobol"))

    print("\n### Creating windows installer using Inno Setup\n"
          "#####################################################################\n")
    try:
        build_dir = glob.glob("build/*")[0]
        with open("setup.iss.in", "r") as src, open("setup.iss", "w") as dst:
            lines = src.readlines()
            data = []
            for l in lines:
                l = l.replace("@VERSION@", __version__)
                l = l.replace("@BUILD_DIR@", build_dir)
                data.append(l)
            dst.writelines(data)
        # cx_freeze is missing libEGL.dll, copy it ourself
        import PyQt5
        shutil.copy(os.path.join(os.path.dirname(PyQt5.__file__), "libEGL.dll"), build_dir)
        # if not in PATH, inno setup is usually located in C:\Program Files (x86)\Inno Setup 5
        # on Windows
        os.environ["PATH"] += ';C:\Program Files (x86)\Inno Setup 5'
        os.system("iscc %s" % os.path.join(os.getcwd(), "setup.iss"))
    except Exception as e:
        print(e)
