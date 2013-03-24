# This file is part of open-cobol-ide.
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
This module contains functions related to cobol
"""
import os
import subprocess
from PySide.QtCore import QFileInfo, Signal, QObject, QRunnable
import sys
from cobcide import FileType


def cmd_from_file_type(filename, fileType):
    finfo = QFileInfo(filename)
    dir_path = finfo.dir().path()
    base_name = QFileInfo(finfo.fileName()).baseName()
    extension = ".exe"
    if fileType == FileType.Subprogram:
        extension = ".so"
        if sys.platform == "win32":
            extension = ".dll"
    output_filename = os.path.join(dir_path, base_name + extension)
    if len(fileType[1]) == 4:
        cmd = [fileType[1][0], fileType[1][1],
               fileType[1][2].format(output_filename),
               fileType[1][3].format(filename)]
    else:
        cmd = [fileType[1][0],
               fileType[1][1].format(output_filename),
               fileType[1][2].format(filename)]
    return cmd, output_filename


def compile(filename, fileType):
    """
    Compile the filename and return a list of errors
    """
    results = []
    cmd, output_filename = cmd_from_file_type(filename, fileType)
    # cmd += ["-I","e:\\OpenCobol\\include", "-L", "e:\\OpenCobol\\lib"]
    startupinfo = subprocess.STARTUPINFO()
    startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
    p = subprocess.Popen(cmd, shell=False, stdout=subprocess.PIPE,
                         startupinfo=startupinfo,
                         stderr=subprocess.PIPE, env=os.environ.copy())
    while p.poll() is None:
        pass
    std_err = p.communicate()[1]
    nb_tokens_expected = 4
    if sys.platform == "win32":
        nb_tokens_expected += 1
    if p.returncode != 0:
        lines = std_err.splitlines()
        print lines
        for line in lines:
            tokens = line.split(':')
            nb_tokens = len(tokens)
            if nb_tokens == nb_tokens_expected:
                try:
                    message = tokens[nb_tokens - 1]
                    type = tokens[nb_tokens - 2].strip(" ")
                    line = int(tokens[nb_tokens - 3])
                    results.append((type, line, message))
                except ValueError:
                    pass
        if not len(results):
            msg = ""
            for l in lines:
                msg += "%s\n" % l
            results.append(("Error", 0, msg))
    return results, output_filename


class RunnerEvents(QObject):
    lineAvailable = Signal(unicode)
    error = Signal(unicode)
    finished = Signal(bool)


class Runner(QRunnable):
    """
    Takes care of running a process in background and log its stdout to a
    qt text edit
    """

    def __init__(self, filename):
        QRunnable.__init__(self)
        self.events = RunnerEvents()
        self.__filename = filename

    def __get_exe_name(self):
        finfo = QFileInfo(self.__filename)
        dir_path = finfo.dir().path()
        cwd = os.getcwd()
        os.chdir(dir_path)
        base_name = QFileInfo(finfo.fileName()).baseName()
        extension = ".exe"
        exe_filename = os.path.join(dir_path, base_name + extension)
        return cwd, exe_filename

    def run(self):
        cwd, exe_filename = self.__get_exe_name()
        if os.path.exists(exe_filename):
            self.events.lineAvailable.emit("> %s" % exe_filename)
            startupinfo = subprocess.STARTUPINFO()
            startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
            p = subprocess.Popen(exe_filename, shell=False,
                                 startupinfo=startupinfo,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            while p.poll() is None:
                stdout, stderr = p.communicate()
                if stdout:
                    for l in stdout.splitlines():
                        self.events.lineAvailable.emit(unicode(l))
                if stderr:
                    os.chdir(cwd)
                    self.events.error.emit(unicode(stderr))
                    self.events.lineAvailable.emit(
                        ">Program exited with return code %d" % p.returncode)
                    self.events.finished.emit(True)
                    return
            self.events.lineAvailable.emit(">Program exited with return code %d"
                                           % p.returncode)
            os.chdir(cwd)
            self.events.finished.emit(True)
        else:
            self.events.lineAvailable.emit("Failed to start %s, file does not "
                                           "exists" % exe_filename)
            self.events.finished.emit(True)
            os.chdir(cwd)
