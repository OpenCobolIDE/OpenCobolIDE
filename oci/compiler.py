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
Contains and functions to cobol source code analysis
"""
import glob
import shutil
import os
import subprocess
import sys
import pyqode.core

from oci import constants


def makeOutputFilePath(filename, fileType):
    return os.path.normpath(os.path.splitext(filename)[0] + fileType[2])


def make_bin_dir(filename):
    dirname = os.path.join(os.path.dirname(filename), "bin")
    if not os.path.exists(dirname):
        os.mkdir(dirname)
        if sys.platform == "win32":
            # copy the dll
            files = glob.glob(
                os.path.join(os.environ["COB_LIBRARY_PATH"], "*.dll"))
            for f in files:
                shutil.copy(f, dirname)


def compile(filename, fileType, customOptions=None, outputFilename=None):
    """
    Compile a single cobol file, return the compiler exit status and output.
    The output is a list of checker messages (those can be used to implements
    a cobol live checker mode)
    """
    if customOptions is None:
        customOptions = []

    # prepare command
    dirname = os.path.dirname(filename)
    if outputFilename is None:  # user request
        # create a binary dir next to the source
        make_bin_dir(filename)
        output = os.path.join("bin", os.path.splitext(
                              os.path.basename(filename))[0] + fileType[2])
        input = os.path.split(filename)[1]
        cmd = constants.ProgramType.cmd(fileType, input, output, customOptions)
    else:  # from check mode
        input = os.path.split(filename)[1]
        output = os.path.split(filename)[1]
        cmd = constants.ProgramType.cmd(fileType, input, output,
                                        customOptions)

    # run the compiler process
    messages = []
    try:
        if sys.platform == "win32":
            startupinfo = subprocess.STARTUPINFO()
            startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
            p = subprocess.Popen(cmd, shell=False, startupinfo=startupinfo,
                                 env=os.environ.copy(),
                                 cwd=dirname,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        else:
            p = subprocess.Popen(cmd, shell=False, cwd=os.path.dirname(filename),
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE)
    except OSError:
        msg = pyqode.core.CheckerMessage("cobc compiler not found",
                                         pyqode.core.MSG_STATUS_ERROR, -1,
                                         filename=filename)
        msg.filename = filename
        return 1, [msg]
    else:
        # get compilation results
        stdout, stderr = p.communicate()
        status = p.returncode
        if sys.version_info[0] == 2:
            lines = stdout.splitlines() + stderr.splitlines()
        else:
            stdout = str(stdout)
            stderr = str(stderr)
            lines = stdout.splitlines() + stderr.splitlines()

        # parse compilation results
        for l in lines:
            tokens = l.split(":")
            try:
                desc = tokens[len(tokens) - 1]
                errType = tokens[len(tokens) - 2]
                lineNbr = int(tokens[len(tokens) - 3])
            except (ValueError, IndexError):
                # not a compilation message, usually this is a file not found error.
                desc = l
                errType = "Error"
                lineNbr = -1
                status = pyqode.core.MSG_STATUS_WARNING
            if errType == "Error":
                status = pyqode.core.MSG_STATUS_ERROR
            msg = pyqode.core.CheckerMessage(desc, status, lineNbr, filename=filename)
            msg.filename = filename
            messages.append(msg)
        return status, messages


def get_cobc_version():
    """ Returns the OpenCobol compiler version as a string """
    cmd = ["cobc", "--version"]
    try:
        if sys.platform == "win32":
            startupinfo = subprocess.STARTUPINFO()
            startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
            p = subprocess.Popen(cmd, shell=False, startupinfo=startupinfo,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        else:
            p = subprocess.Popen(cmd, shell=False, stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE)
    except OSError:
        return "Not installed"
    stdout, stderr = p.communicate()
    if sys.version_info[0] == 2:
        return stdout.splitlines()[0].split(" ")[2]
    else:
        stdout = str(stdout)
        return stdout.splitlines()[0].split(" ")[2].split("\\n")[0].split(
            "\\r")[0]
