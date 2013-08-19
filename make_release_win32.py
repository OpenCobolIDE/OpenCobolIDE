# Copyright 2013 Colin Duquesnoy
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
This script create a release for windows:
  - freeze the main script with cx_freeze
  - download OpenCobol from
    https://www.dropbox.com/s/4ssbbonn5pxp31a/opencobol.zip (this is the
    official OpenCobol mingw distribution reduced to a minimum, hosted on my
    drop box account)
  - unzip it to the build dir
  - run inno setup to make the windows installer
"""
import os
import shutil
import subprocess
import urllib
import zipfile
from cobcide import __version__


def unzip(zipFilePath, destDir):
    """
    Unzips a file
    """
    try:
        zfile = zipfile.ZipFile(zipFilePath)
        for name in zfile.namelist():
            (dirName, fileName) = os.path.split(name)
            if fileName == '':
                # directory
                newDir = destDir + '/' + dirName
                if not os.path.exists(newDir):
                    os.mkdir(newDir)
            else:
                # file
                fd = open(destDir + '/' + name, 'wb')
                fd.write(zfile.read(name))
                fd.close()
        zfile.close()
    except zipfile.BadZipfile:
        print "Bad zip file"


def get_unzipped(theurl, thedir):
    """
    Downloads and unzips a file from an url to a directory
    """
    try:
        urllib.urlretrieve(theurl, 'temp.zip')
    except IOError, e:
        print "Can't retrieve %r to %r: %s" % (theurl, thedir, e)
        return
    try:
        unzip("temp.zip", thedir)
    except IOError, e:
        print e
    os.remove("temp.zip")


def configure_version():
    """
    Configures installer version (setup.iss.in > setup.iss)
    """
    with open("setup.iss.in", "r") as src, open("setup.iss", "w") as dst:
        lines = src.readlines()
        data = []
        for l in lines:
            l = l.replace("@VERSION@", __version__)
            data.append(l)
        dst.writelines(data)


def main():
    """
    Creates the windows release:
        - cleans directories
        - freezes the application
        - downloads open cobol from my dropbox account
        - configures installer version
        - compiles the installer script
        - opens the dist folder (where the installer was created)
    """
    print "--- Creating OpenCobol v.%s release" % __version__
    os.environ["PATH"] += ";C:\\Program Files\\Inno Setup 5"

    print '### Clean build directories'
    try:
        shutil.rmtree("build")
        shutil.rmtree("dist")
    except WindowsError:
        pass

    # freeze the main script
    print "### Freezing script"
    os.system("python freeze_setup.py build")

    print "### Clean up frozen script"
    shutil.rmtree("build/exe.win32-2.7/tcl")
    shutil.rmtree("build/exe.win32-2.7/tk")
    os.remove("build/exe.win32-2.7/tcl85.dll")
    os.remove("build/exe.win32-2.7/tk85.dll")

    # download open cobol mingw distribution
    print "### Downloading OpenCobol"
    get_unzipped("https://dl.dropboxusercontent.com/s/4ssbbonn5pxp31a/opencobol.zip?token_hash=AAENJLRjwm6HLQR8Ho26Bk1mI8ASW4reAVXPfSmA17rpPg&dl=1",
                 "build/exe.win32-2.7/")

    # run inno setup
    print "### Configure inno setup installer version"
    configure_version()
    print "### Creating windows installer using Inno Setup"
    os.system("iscc %s" % os.path.join(os.getcwd(), "setup.iss"))

    # show the files
    subprocess.Popen(['start', "dist"], shell=True)


if __name__ == "__main__":
    main()
