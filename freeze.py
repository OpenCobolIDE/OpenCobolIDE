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

__version__ = "2.0"


def get_build_dir():
    build_dir = glob.glob("build/*")[0]
    return build_dir


def configure_iss_script():
    """
    Configures installer version (setup.iss.in > setup.iss)
    """
    build_dir = get_build_dir()
    with open("setup.iss.in", "r") as src, open("setup.iss", "w") as dst:
        lines = src.readlines()
        data = []
        for l in lines:
            l = l.replace("@VERSION@", __version__)
            l = l.replace("@BUILD_DIR@", build_dir)
            data.append(l)
        dst.writelines(data)

if len(sys.argv) == 1:
    sys.argv.append("build")

options = {"excludes": ["PyQt4.uic.port_v3"],
           # add the namespace packages that you uses
           "namespace_packages": ["pyqode.core", "pyqode.widgets"],
           # freeze the pygments default style along with our executable
           "includes": ["pygments.styles.default", "PyQt4"]}

print("### Freezing application\n"
      "#####################################################################\n")
if os.path.exists("build"):
    shutil.rmtree(os.path.join(os.getcwd(), "build"))
if os.path.exists("dist"):
    shutil.rmtree(os.path.join(os.getcwd(), "dist"))
setup(name="OpenCobolIDE",
      version=__version__,
      options={"build_exe": options},
      executables=[
          Executable("OpenCobolIDE.pyw", targetName="OpenCobolIDE.exe",
                     icon="oci/ui/rc/silex-icon.ico", base="Win32GUI")])

print("### Copying OpenCobol distribution\n"
      "#####################################################################\n")
build_dir = os.path.join(os.getcwd(), get_build_dir())
cobc_dir = os.path.join(build_dir, "OpenCobol")
if not os.path.exists(cobc_dir):
    shutil.copytree(os.path.join(os.getcwd(), "OpenCobol"),
                    os.path.join(build_dir, "OpenCobol"))

print("\n### Creating windows installer using Inno Setup\n"
      "#####################################################################\n")
try:
    configure_iss_script()
    os.system("iscc %s" % os.path.join(os.getcwd(), "setup.iss"))
except Exception as e:
    print(e)
