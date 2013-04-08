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
This module contains functions related to cobol:
    - create compilation command,
    - compile file
    - run cobol program
    - parse cobol document layout
"""
import os
import subprocess
import sys

from PySide.QtCore import QFileInfo, Signal, QObject, QRunnable
from PySide.QtGui import QTreeWidgetItem, QIcon

from cobcide import FileType
from cobcide.settings import Settings


def get_cobc_version():
    """
    Returns the OpenCobol version string

    :return:
    """
    ret_val = "unknown"
    cmd = ["cobc", "--version"]
    if sys.platform == "win32":
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        p = subprocess.Popen(cmd, shell=False, startupinfo=startupinfo,
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    else:
        p = subprocess.Popen(cmd, shell=False, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
    stdout, stderr = p.communicate()
    return stdout.splitlines()[0].split(" ")[2]


def cmd_from_file_type(filename, fileType):
    """
    Creates a compilation command that take cares about the file type.

    We do not compile a program or a subprogram with the same command, to
    compile a program we must use the -X switch and use a *.exe extension on
    windows. To compile a subprogram we must use the *.dll extension on windows
    and *.so extension on GNU/Linux.

    The base compilation command for every filetype is stored into the
    corresponding FileType dictionnary.

    :param filename: The full filename

    :param fileType: The file type

    :return: The compilation command string
    """
    filename = os.path.normpath(filename)
    finfo = QFileInfo(filename)
    dir_path = finfo.dir().path()
    base_name = QFileInfo(finfo.fileName()).baseName()
    extension = ".exe"
    if fileType == FileType.Subprogram:
        extension = ".so"
        if sys.platform == "win32":
            extension = ".dll"
    output_filename = os.path.join(dir_path, base_name + extension)
    output_filename = os.path.normpath(output_filename)
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
    Compiles a file and return a list of errors/messages
    """
    results = []
    cmd, output_filename = cmd_from_file_type(filename, fileType)
    # cmd += ["-I","e:\\OpenCobol\\include", "-L", "e:\\OpenCobol\\lib"]
    if sys.platform == "win32":
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        p = subprocess.Popen(cmd, shell=False, stdout=subprocess.PIPE,
                             startupinfo=startupinfo,
                             stderr=subprocess.PIPE, env=os.environ.copy())
    else:
        p = subprocess.Popen(cmd, shell=False, stdout=subprocess.PIPE,
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
    """
    Groups runner events.
    """
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
        exe_filename = os.path.normpath(exe_filename)
        return cwd, exe_filename

    def run(self):
        cwd, exe_filename = self.__get_exe_name()
        if os.path.exists(exe_filename):
            self.events.lineAvailable.emit("> %s" % exe_filename)
            s = Settings()
            if sys.platform == "win32":
                if not s.use_external_shell:
                    startupinfo = subprocess.STARTUPINFO()
                    startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
                    p = subprocess.Popen(exe_filename, shell=False,
                                         startupinfo=startupinfo,
                                         stdout=subprocess.PIPE,
                                         stderr=subprocess.PIPE)
                else:
                    p = subprocess.Popen(exe_filename, shell=True)
            else:
                if not s.use_external_shell:
                    p = subprocess.Popen(exe_filename, shell=False,
                                         stdout=subprocess.PIPE,
                                         stderr=subprocess.PIPE)
                else:
                    print os.environ
                    wd = QFileInfo(exe_filename).dir().path()
                    os.chdir(wd)
                    os.system("gnome-terminal -e" + " " + exe_filename)
                    return
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


class DocumentNode(QTreeWidgetItem):
    """
    Data structure used to hold a document node data:
        - type (div, section, var, ...)
        - line number
        - name or identifier
        - a list of child nodes
    """

    class Type:
        Root = -1
        Division = 0
        Section = 1
        Variable = 2
        Paragraph = 3

    ICONS = {
        -1: ":/ide-icons/rc/silex-32x32.png",
        0:  ":/ide-icons/rc/Office-book.png",
        1:  ":/ide-icons/rc/text-x-generic.png",
        2:  ":/ide-icons/rc/var.png",
        3:  ":/ide-icons/rc/paragraph.png"
    }

    def __init__(self, node_type, line, name, description=None):
        QTreeWidgetItem.__init__(self)
        self.node_type = node_type
        self.line = line
        self.name = name
        if description is None:
            description = name
        self.description = description.replace(".", "")
        self.children = []
        self.setText(0, name)
        self.setIcon(0, QIcon(self.ICONS[node_type]))
        self.setToolTip(0, self.description)

    def add_child(self, child):
        """
        Add a child to the node (call QTreeWidgetItem::addChilld automatically)

        :param child: The child node to add
        """
        self.children.append(child)
        self.addChild(child)

    def print_tree(self, indent=0):
        """
        Print the node tree in stdout
        """
        print " " * indent, self.name, self.line
        for c in self.children:
            c.print_tree(indent + 4)


def cmp_doc_node(first_node, second_node):
    """
    Compare two nodes recursively.

    :param first_node: First node

    :param second_node:

    :return:
    """
    ret_val = 0
    if len(first_node.children) == len(second_node.children):
        for first_child, second_child in zip(first_node.children,
                                             second_node.children):
            if first_child.name != second_child.name:
                ret_val = 1
            else:
                ret_val = cmp_doc_node(first_child, second_child)
            if ret_val != 0:
                break
    else:
        ret_val = 1
    return ret_val


def _extract_div_node(i, line, root_node):
    """
    Extracts a division node from a line

    :param i: The line number (starting from 0)

    :param line: The line string (without indentation)

    :param root_node: The document root node.

    :return: tuple(last_div_node, last_section_node)
    """
    name = line
    name = name.replace(".", "")
    node = DocumentNode(DocumentNode.Type.Division, i + 1, name)
    root_node.add_child(node)
    last_div_node = node
    # do not take previous sections into account
    last_section_node = None
    return last_div_node, last_section_node


def _extract_section_node(i, last_div_node, last_vars, line):
    """
    Extracts a section node from a line.

    :param i: The line number (starting from 0)

    :param last_div_node: The last div node found

    :param last_vars: The last vars dict

    :param line: The line string (without indentation)

    :return: last_section_node
    """
    name = line
    name = name.replace(".", "")
    description = "{0}: {1}".format(i + 1, line)
    node = DocumentNode(DocumentNode.Type.Section, i + 1, name)
    last_div_node.add_child(node)
    last_section_node = node
    # do not take previous var into account
    last_vars.clear()
    return last_section_node


def _extract_var_node(i, indentation, last_section_node, last_vars, line):
    """
    Extract a variable node.

    :param i: The line number (starting from 0)

    :param indentation: The current indentation (counted in spaces)

    :param last_section_node: The last section node found

    :param last_vars: The last vars dict

    :param line: The line string (without indentation)

    :return: The extracted variable node
    """
    parent_node = None
    try:
        lvl = int(line.split(" ")[0], 16)
        name = line.split(" ")[1]
    except ValueError:
        lvl = 0
        name = line.split(" ")[0]
    name = name.replace(".", "")
    description = "{1}".format(i + 1, line)
    if indentation == 7:
        lvl = 0
    if lvl == 0:
        parent_node = last_section_node
    else:
        # trouver les premier niveau inferieur
        levels = sorted(last_vars.keys(), reverse=True)
        for l in levels:
            if l < lvl:
                parent_node = last_vars[l]
                break
    node = DocumentNode(DocumentNode.Type.Variable, i + 1, name,
                        description)
    parent_node.add_child(node)
    last_vars[lvl] = node
    return node


def _extract_paragraph_node(i, last_div_node, last_section_node, line):
    """
    Extracts a paragraph node

    :param i: The line number (starting from 0)
    :param last_div_node: The last div node found
    :param last_section_node: The last section node found
    :param line: The line string (without indentation)
    :return: The extracted paragraph node
    """
    name = line.replace(".", "")
    parent_node = last_div_node
    if last_section_node is not None:
        parent_node = last_section_node
    node = DocumentNode(DocumentNode.Type.Paragraph, i + 1, name)
    parent_node.add_child(node)
    return node


def parse_document_layout(filename):
    """
    Parse a cobol file and return  a root DocumentNode that describes the
    layout of the file (sections, divisions, vars,...)

    :param filename: The cobol file path

    :return: the root DocumentNode, a list of variables, a list of paragraphs
    """
    root_node = DocumentNode(DocumentNode.Type.Root, 0,
                             QFileInfo(filename).fileName())
    variables = []
    paragraphs = []
    with open(filename, "r") as f:
        last_div_node = None
        last_section_node = None
        lines = f.readlines()
        last_vars = {}
        for i, line in enumerate(lines):
            indentation = len(line) - len(line.lstrip())
            if indentation >= 7 and not line.isspace():
                line = line.strip().upper()
                # DIVISIONS
                if "DIVISION" in line.upper():
                    last_div_node, last_section_node = _extract_div_node(
                        i, line, root_node)
                # SECTIONS
                elif "SECTION" in line:
                    last_section_node = _extract_section_node(
                        i, last_div_node, last_vars, line)
                # VARIABLES
                elif (last_div_node is not None and
                        "DATA DIVISION" in last_div_node.name):
                    v = _extract_var_node(
                        i, indentation, last_section_node, last_vars, line)
                    if v:
                        variables.append(v)
                # PARAGRAPHS
                elif (last_div_node is not None and
                      "PROCEDURE DIVISION" in last_div_node.name and
                      indentation == 7 and
                      not "EXIT" in line and not "END" in line and not "STOP"
                      in line):
                    p = _extract_paragraph_node(
                        i, last_div_node, last_section_node, line)
                    if p:
                        paragraphs.append(p)
    return root_node, variables, paragraphs
