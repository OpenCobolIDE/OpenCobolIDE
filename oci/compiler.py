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
import logging
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


def _logger():
    return logging.getLogger(__name__)


def compile(filename, fileType, customOptions=None, outputFilename=None):
    """
    Compile a single cobol file, return the compiler exit status and output.
    The output is a list of checker messages (those can be used to implements
    a cobol live checker mode)
    """
    _logger().debug('compiling %s -> %s' % (filename, outputFilename))
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

    _logger().debug('working directory = %s' % dirname)
    _logger().info('cd %s && %s' % (dirname, ' '.join(cmd)))

    # run the compiler process
    messages = []
    try:
        if sys.platform == "win32":
            _logger().debug('environment = %r' % os.environ.copy())
            startupinfo = subprocess.STARTUPINFO()
            startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
            _logger().debug('running build command in a subprocess on windows' %
                            os.environ.copy())
            p = subprocess.Popen(cmd, shell=False, startupinfo=startupinfo,
                                 env=os.environ.copy(),
                                 cwd=dirname,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        else:
            _logger().debug('running build command in a subprocess' %
                            os.environ.copy())
            p = subprocess.Popen(cmd, shell=False, cwd=dirname,
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE)
    except OSError:
        _logger().exception('cobc compiler not found')
        msg = pyqode.core.CheckerMessage("cobc compiler not found",
                                         pyqode.core.MSG_STATUS_ERROR, -1,
                                         filename=filename)
        msg.filename = filename
        return 1, [msg]
    else:
        # get compilation results
        stdout, stderr = p.communicate()
        status = p.returncode
        _logger().debug('compilation command terminated with status %d' % status)
        if status != 0:
            lines = []
            _logger().debug('parsing cobc output')
            if stdout:
                lines += str(stdout.decode(sys.getfilesystemencoding())).splitlines()
            if stderr:
                lines += str(stderr.decode(sys.getfilesystemencoding())).splitlines()
            _logger().debug('cobc output: %s' % lines)
            print(lines)
            # parse compilation results
            for l in lines:
                if not l or l == "":
                    continue
                tokens = l.split(":")
                _logger().debug('line tokens: %r' % tokens)
                msg_type = pyqode.core.MSG_STATUS_WARNING
                try:
                    desc = tokens[len(tokens) - 1]
                    errType = tokens[len(tokens) - 2]
                    lineNbr = int(tokens[len(tokens) - 3])
                except (ValueError, IndexError):
                    _logger().debug('failed to process line %r, skipping' % l)
                    continue
                if errType.strip() == "Error":
                    msg_type = pyqode.core.MSG_STATUS_ERROR
                print(errType, msg_type, pyqode.core.MSG_STATUS_WARNING, pyqode.core.MSG_STATUS_ERROR)
                msg = pyqode.core.CheckerMessage(desc, msg_type, lineNbr, filename=filename)
                msg.filename = filename
                _logger().debug('adding compiler message: %r' % msg)
                messages.append(msg)
        return status, messages


def get_cobc_version():
    """ Returns the OpenCobol compiler version as a string """
    cmd = ["cobc", "--version"]
    try:
        _logger().debug('getting cobc version: %s' % ' '.join(cmd))
        if sys.platform == "win32":
            startupinfo = subprocess.STARTUPINFO()
            startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
            p = subprocess.Popen(cmd, shell=False, startupinfo=startupinfo,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        else:
            p = subprocess.Popen(cmd, shell=False, stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE)
    except OSError:
        _logger().exception('OpenCobol compiler not found')
        return "Not installed"
    stdout, stderr = p.communicate()
    stdout = str(stdout)
    version = stdout.splitlines()[0].split(" ")[2].split(
        "\\n")[0].split("\\r")[0]
    return version


def init_env():
    """
    Windows specific initialisation:

    - set env var to embedded OpenCobol variable
    - set PATH to cobol library path only (erase previous values)
    """
    cwd = os.getcwd()
    _logger().info('setting up environment')
    _logger().info('current working directory: %s' % cwd)
    if sys.platform == "win32":
        oc_root_pth = os.path.join(cwd, "OpenCobol")
        os.environ["COB_CONFIG_DIR"] = os.path.join(oc_root_pth, "config")
        os.environ["COB_COPY_DIR"] = os.path.join(oc_root_pth, "copy")
        os.environ["COB_LIBRARY_PATH"] = os.path.join(oc_root_pth, "bin")
        os.environ["COB_INCLUDE_PATH"] = os.path.join(oc_root_pth, "include")
        os.environ["COB_LIB_PATH"] = os.path.join(oc_root_pth, "lib")
        os.environ["PATH"] = os.environ["COB_LIBRARY_PATH"]

        env_vars = ['COB_CONFIG_DIR', 'COB_COPY_DIR', 'COB_LIBRARY_PATH',
                    'COB_INCLUDE_PATH', 'COB_LIB_PATH', 'PATH']
        for var in env_vars:
            _logger().info('%s: %s' % (var, os.environ[var]))


def check_env():
    version = get_cobc_version()
    if version == "Not installed":
        if sys.platform == 'win32':
            expected_root_path = os.path.join(os.getcwd(), "OpenCobol")
            expected_cobc_path = os.path.join(expected_root_path,
                                              'bin', 'cobc.exe')
            if not os.path.exists(expected_root_path):
                _logger().warning('%s does not exists' % expected_root_path)
            elif not os.path.exists(expected_cobc_path):
                _logger().warning('%s does not exisits' % expected_cobc_path)
            else:
                _logger().info('cobc.exe found but not usable.')
        return False
    _logger().info('OpenCobol compiler v.%s' % version)
    return True
