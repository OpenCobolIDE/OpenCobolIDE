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
Contains and functions to cobol source code analysis
"""
import glob
import logging
import shutil
from PyQt4 import QtCore
import os
import subprocess
import sys


import pyqode.core

from PyQt4.QtCore import QFileInfo
from PyQt4.QtGui import QIcon, QTreeWidgetItem


# taken from pygments_ibm_cobol_lexer.__init__.py
from oci import constants

KEYWORDS = [
    "ACCEPT", "ACCESS", "ADD", "ADDRESS", "ADVANCING", "AFTER", "ALL",
    "ALPHABET",
    "ALPHABETIC", "ALPHABETIC-LOWER", "ALPHABETIC-UPPER", "ALPHANUMERIC",
    "ALPHANUMERIC-EDITED", "ALSO", "ALTER", "ALTERNATE", "AND", "ANY", "APPLY",
    "ARE", "AREA", "AREAS", "ASCENDING",
    "ASSIGN", "AT", "AUTHOR", "BACK", "BEFORE", "BEGINNING", "BINARY", "BLANK",
    "BLOCK", "BOTTOM",
    "BY", "CALL", "CANCEL", "CBL", "CHARACTER", "CHARACTERS", "CLASS", "CLOSE",
    "CODE-SET",
    "COLLATING", "COMMA", "COMMON", "COMP", "COMP-1", "COMP-2", "COMP-3",
    "COMP-4", "COMPUTATIONAL",
    "COMPUTATIONAL-1", "COMPUTATIONAL-2", "COMPUTATIONAL-3", "COMPUTATIONAL-4",
    "COMPUTE", "CONFIGURATION", "CONTAINS", "CONTENT", "CONTINUE", "CONVERTING",
    "COPY", "CORR", "CORRESPONDING", "COUNT", "CURRENCY", "DATA", "DATE",
    "DATE-COMPILED",
    "DATE-WRITTEN", "DAY", "DAY-OF-WEEK", "DBCS", "DEBUGGING", "DECIMAL-POINT",
    "DECLARATIVES",
    "DELETE", "DELIMITED", "DELIMITER", "DEPENDING", "DESCENDING", "DISPLAY",
    "DISPLAY-1",
    "DIVIDE", "DIVISION", "DOWN", "DUPLICATES", "DYNAMIC", "EBCDIC", "EGCS",
    "ELSE",
    "END", "END-ADD", "END-CALL", "END-COMPUTE", "END-DELETE", "END-DIVIDE",
    "END-EVALUATE",
    "END-IF", "END-MULTIPLY", "END-OF-PAGE", "END-PERFORM", "END-READ",
    "END-RETURN",
    "END-REWRITE", "END-SEARCH", "END-START", "END-STRING", "END-SUBTRACT",
    "END-UNSTRING",
    "END-WRITE", "ENDING", "ENTRY", "ENVIRONMENT", "EOP", "EQUAL", "ERROR",
    "EVALUATE",
    "EVERY", "EXCEPTION", "EXIT", "EXTEND", "EXTERNAL", "FALSE", "FD", "FILE",
    "FILE-CONTROL",
    "FILLER", "FIRST", "FOOTING", "FOR", "FROM", "FUNCTION", "GENERATE",
    "GIVING", "GLOBAL",
    "GO", "GOBACK", "GREATER", "HIGH-VALUE", "HIGH-VALUES", "I-O",
    "I-O-CONTROL", "ID",
    "IDENTIFICATION", "IF", "IN", "INDEX", "INDEXED", "INITIAL", "INITIALIZE",
    "INPUT",
    "INPUT-OUTPUT", "INSPECT", "INSTALLATION", "INTO", "INVALID", "IS", "JUST",
    "JUSTIFIED",
    "KANJI", "KEY", "LABEL", "LEADING", "LEFT", "LENGTH", "LESS", "LINAGE",
    "LINAGE-COUNTER",
    "LINE", "LINES", "LINKAGE", "LOCK", "LOW-VALUE", "LOW-VALUES", "MEMORY",
    "MERGE", "MODE",
    "MODULES", "MORE-LABELS", "MOVE", "MULTIPLE", "MULTIPLY", "NATIVE",
    "NEGATIVE",
    "NEXT", "NO", "NOT", "NULL", "NULLS", "NUMERIC", "NUMERIC-EDITED",
    "OBJECT-COMPUTER",
    "OCCURS", "OF", "OFF", "OMITTED", "ON", "OPEN", "OPTIONAL", "OR", "ORDER",
    "ORGANIZATION",
    "OTHER", "OUTPUT", "OVERFLOW", "PACKED-DECIMAL", "PADDING", "PAGE", "PARSE",
    "PASSWORD",
    "PERFORM", "PIC", "PICTURE", "POINTER", "POSITION", "POSITIVE", "PROCEDURE",
    "PROCEDURE-POINTER",
    "PROCEDURES", "PROCEED", "PROCESS", "PROGRAM", "PROGRAM-ID", "QUOTE",
    "QUOTES",
    "RANDOM", "READ", "RECORD", "RECORD-KEY", "RECORDING", "RECORDS",
    "RECURSIVE", "REDEFINES",
    "REEL", "REFERENCE", "RELATIVE", "RELEASE", "REMAINDER", "REMARKS",
    "REMOVAL", "RENAMES",
    "REPLACING", "RERUN", "RESERVE", "RETURN", "RETURN-CODE", "RETURNING",
    "REVERSED",
    "REWIND", "REWRITE", "RIGHT", "ROUNDED", "RUN", "SAME", "SD", "SEARCH",
    "SECTION", "SECURITY",
    "SEGMENT-LIMIT", "SELECT", "SENTENCE", "SEPARATE", "SEQUENCE", "SEQUENTIAL",
    "SET", "SIGN", "SIZE", "SORT", "SORT-MERGE", "SOURCE-COMPUTER",
    "SPACE", "SPACES", "SPECIAL-NAMES", "STANDARD", "STANDARD-1", "STANDARD-2",
    "START",
    "STATUS", "STOP", "STRING", "SUBTRACT", "SUPPRESS", "SYMBOLIC", "SYNC",
    "SYNCHRONIZED",
    "TALLYING", "TAPE", "TEST", "THAN", "THEN", "THROUGH", "THRU", "TIME",
    "TIMES", "TO", "TOP",
    "TRAILING", "TRUE", "TRUETEST", "UNIT", "UNSTRING", "UNTIL", "UP", "UPON",
    "USAGE", "USE",
    "USING", "VALUE", "VALUES", "VARYING", "WHEN", "WITH", "WORDS",
    "WORKING-STORAGE",
    "WRITE", "WRITE-ONLY", "XML", "ZERO", "ZEROES", "ZEROS"]


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
        0:  ":/ide-icons/rc/division",
        1:  ":/ide-icons/rc/section",
        2:  ":/ide-icons/rc/var",
        3:  ":/ide-icons/rc/paragraph"}

    def __init__(self, node_type, line, column, name, description=None, createIcon=True):
        QTreeWidgetItem.__init__(self)
        self.node_type = node_type
        self.line = line
        self.column = column
        self.end_line = -1
        self.name = name
        if description is None:
            description = name
        self.description = description.replace(u".", u"")
        self.children = []
        self.setText(0, name)
        if createIcon:
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
        print(u" " * indent, self.name, self.line, u" - ", self.end_line)
        for c in self.children:
            c.print_tree(indent + 4)

    def find(self, name):
        """
        Finds a possible child whose name match the name parameter.

        :param name: name of the child node to look up
        :type name: str

        :return: DocumentNode or None
        """
        for c in self.children:
            if c.name == name:
                return c
            result = c.find(name)
            if result:
                return result


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


def _extract_div_node(l, c, line, root_node, last_section_node, createIcon):
    """
    Extracts a division node from a line

    :param l: The line number (starting from 0)

    :param line: The line string (without indentation)

    :param root_node: The document root node.

    :return: tuple(last_div_node, last_section_node)
    """
    name = line
    name = name.replace(u".", u"")
    node = DocumentNode(DocumentNode.Type.Division, l + 1, c, name,
                        createIcon=createIcon)
    root_node.add_child(node)
    last_div_node = node
    # do not take previous sections into account
    if last_section_node:
        last_section_node.end_line = l
    last_section_node = None
    return last_div_node, last_section_node


def _extract_section_node(l, c, last_div_node, last_vars, line, createIcon):
    """
    Extracts a section node from a line.

    :param l: The line number (starting from 0)

    :param last_div_node: The last div node found

    :param last_vars: The last vars dict

    :param line: The line string (without indentation)

    :return: last_section_node
    """
    name = line
    name = name.replace(u".", u"")
    description = u"{0}: {1}".format(l + 1, line)
    node = DocumentNode(DocumentNode.Type.Section, l + 1, c, name,
                        createIcon=createIcon)
    last_div_node.add_child(node)
    last_section_node = node
    # do not take previous var into account
    last_vars.clear()
    return last_section_node


def _extract_var_node(i, c, indentation, last_section_node, last_vars, line,
                      createIcon):
    """
    Extract a variable node.

    :param l: The line number (starting from 0)

    :param indentation: The current indentation (counted in spaces)

    :param last_section_node: The last section node found

    :param last_vars: The last vars dict

    :param line: The line string (without indentation)

    :return: The extracted variable node
    """
    parent_node = None
    raw_tokens = line.split(u" ")
    tokens = []
    for t in raw_tokens:
        if not t.isspace() and t != u"":
            tokens.append(t)
    try:
        lvl = int(tokens[0], 16)
        name = tokens[1]
    except ValueError:
        lvl = 0
        name = tokens[0]
    name = name.replace(u".", u"")
    description = u"{1}".format(i + 1, line)
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
    node = DocumentNode(DocumentNode.Type.Variable, i + 1, c, name,
                        description, createIcon=createIcon)
    parent_node.add_child(node)
    last_vars[lvl] = node
    return node


def _extract_paragraph_node(l, c, last_div_node, last_section_node, line,
                            createIcon):
    """
    Extracts a paragraph node

    :param l: The line number (starting from 0)
    :param last_div_node: The last div node found
    :param last_section_node: The last section node found
    :param line: The line string (without indentation)
    :return: The extracted paragraph node
    """
    name = line.replace(u".", u"")
    parent_node = last_div_node
    if last_section_node is not None:
        parent_node = last_section_node
    node = DocumentNode(DocumentNode.Type.Paragraph, l + 1, c, name,
                        createIcon=createIcon)
    parent_node.add_child(node)
    return node


def parse_document_layout(filename, code=None, createIcon=True,
                          encoding="utf-8"):
    """
    Parses a cobol file and return  a root DocumentNode that describes the
    layout of the file (sections, divisions, vars,...), a list of paragraphes
    nodes and variable nodes.

    :param filename: The cobol file path

    :return: The root node, the list of variables, the list of paragraphes
    :rtype: DocumentNode, list of DocumentNode, list of DocumentNode
    """
    root_node = DocumentNode(DocumentNode.Type.Root, 0, 0,
                             QFileInfo(filename).fileName(),
                             createIcon=createIcon)
    variables = []
    paragraphs = []
    if code is None:
        with open(filename, "r") as f:
            lines = f.readlines()
    else:
        lines = code.splitlines()

    last_div_node = None
    last_section_node = None

    last_vars = {}
    last_par = None
    for i, line in enumerate(lines):
        if sys.version_info[0] == 2:
            if not isinstance(line, unicode):
                line = line.decode(encoding)
        indentation = len(line) - len(line.lstrip())
        if indentation >= 7 and not line.isspace():
            line = line.strip()
            # DIVISIONS
            if u"DIVISION" in line.upper():
                # remember
                if last_div_node is not None:
                    last_div_node.end_line = i
                last_div_node, last_section_node = _extract_div_node(
                    i, indentation, line, root_node, last_section_node, createIcon)
            # SECTIONS
            elif u"SECTION" in line:
                if last_section_node:
                    last_section_node.end_line = i
                last_section_node = _extract_section_node(
                    i, indentation, last_div_node, last_vars, line, createIcon)
            # VARIABLES
            elif (last_div_node is not None and
                    u"DATA DIVISION" in last_div_node.name):
                v = _extract_var_node(
                    i, indentation, indentation, last_section_node, last_vars, line,
                    createIcon)
                if v:
                    variables.append(v)
            # PARAGRAPHS
            elif (last_div_node is not None and
                  u"PROCEDURE DIVISION" in last_div_node.name and
                  indentation == 7 and not "*" in line and
                  not u"EXIT" in line and not u"END" in line and not u"STOP"
                  in line):
                if last_par:
                    last_par.end_line = i
                p = _extract_paragraph_node(
                    i, indentation, last_div_node, last_section_node, line, createIcon)
                if p:
                    paragraphs.append(p)
                last_par = p
    # close last div
    if last_par:
        last_par.end_line = len(lines) - 1
    if last_div_node:
        last_div_node.end_line = len(lines)
    return root_node, variables, paragraphs


class CobolFolder(pyqode.core.IndentBasedFoldDetector):
    def getFoldIndent(self, highlighter, block, text):
        text = text.upper()
        indent = int((len(text) - len(text.lstrip())))
        prev = block.previous()
        while prev.isValid() and not len(prev.text().strip()):
            prev = prev.previous()
        pusd = block.previous().userData()
        pb = prev
        if len(text.strip()) == 0:
            while not len(pb.text().strip()) and pb.isValid():
                pb = pb.previous()
            pbIndent = (len(pb.text()) - len(pb.text().lstrip()))
            # check next blocks to see if their indent is >= then the last block
            nb = block.next()
            while not len(nb.text().strip()) and nb.isValid():
                nb = nb.next()
            nbIndent = (len(nb.text()) - len(nb.text().lstrip()))
            # print(pb.userState())
            if nbIndent >= pbIndent or pb.userState() & 0x7F:
                if pb.userData():
                    return pb.userData().foldIndent
            return -1
        if indent == 6:
            if pusd:
                return pusd.foldIndent
            return 0
        if indent == 7:
            if "DIVISION" in text:
                return 0
            if "SECTION" in text:
                return 2
            if "END" in text or  "STOP" in text:
                return pusd.foldIndent
            return 3
        return indent


def detectFileType(filename):
    """
    Detect file type:
        - cobol program
        - cobol subprogram
        - text file

    :param filename: The file name to check
    """
    ext = os.path.splitext(filename)[1].lower()
    type = constants.ProgramType.Executable
    if ext == ".cbl" or ext == ".cob":
        try:
            with open(filename, 'r') as f:
                lines = f.readlines()
                for l in lines:
                    # This is a subprogram
                    if "PROCEDURE DIVISION USING" in l.upper():
                        type = constants.ProgramType.Module
                        break
        except IOError or OSError:
            pass
    return type


def parseDependencies(filename):
    directory = QFileInfo(filename).dir().path()
    dependencies = []
    with open(filename, 'r') as f:
        for l in f.readlines():
            if 'CALL' in l:
                raw_tokens = l.split(" ")
                tokens = []
                for t in raw_tokens:
                    if not t.isspace() and t != "":
                        tokens.append(t)
                dependency = os.path.join(directory, tokens[1].replace('"', "") + ".cbl")
                if os.path.exists(dependency):
                    file_type = detectFileType(dependency)
                    dependencies.append((dependency, file_type))
                    dependencies += parseDependencies(dependency)

    def make_unique_sorted(seq):
        # order preserving
        checked = []
        for e in seq:
            if e not in checked:
                checked.append(e)
        return sorted(checked)
    return make_unique_sorted(dependencies)


def makeOutputFilePath(filename, fileType):
    return os.path.normpath(os.path.splitext(filename)[0] + fileType[2])


def compile(filename, fileType, customOptions=None, outputFilename=None):
    """
    Compile a single cobol file, return the compiler exit status and output.
    The output is a list of checker messages (those can be used to implements
    a cobol live checker mode)
    """
    if customOptions is None:
        customOptions = []
    # prepare command
    customOptionsStr = " ".join(customOptions)
    if outputFilename is None:
        # create a binary dir next to the source
        dirname = os.path.join(os.path.dirname(filename), "bin")
        if not os.path.exists(dirname):
            os.mkdir(dirname)
            if sys.platform == "win32":
                # copy the dll
                files = glob.glob(
                    os.path.join(os.environ["COB_LIBRARY_PATH"], "*.dll"))
                for f in files:
                    shutil.copy(f, dirname)
        fn = os.path.join(dirname, os.path.basename(filename))
        outputFilename = makeOutputFilePath(fn, fileType)
        output = os.path.join(
            os.path.dirname(outputFilename),
            os.path.splitext(os.path.basename(filename))[0] + fileType[2])
        input = filename
        cmd = constants.ProgramType.cmd(fileType, input, output,
                                        customOptions)
    else:
        input = filename
        output = outputFilename
        cmd = constants.ProgramType.cmd(fileType, input, output,
                                        customOptions)
    # run it using pexpect
    messages = []
    if sys.platform == "win32":
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        p = subprocess.Popen(cmd, shell=False, startupinfo=startupinfo,
                             env=os.environ.copy(),
                             cwd=os.path.dirname(filename),
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    else:
        p = subprocess.Popen(cmd, shell=False,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
    stdout, stderr = p.communicate()
    status = p.returncode
    if sys.version_info[0] == 2:
        lines = stdout.splitlines() + stderr.splitlines()
    else:
        stdout = str(stdout)
        stderr = str(stderr)
        lines = stdout.splitlines() + stderr.splitlines()
    nbTokensExpected = 4
    if sys.platform == "win32":
        nbTokensExpected += 1
    for l in lines:
        tokens = l.split(":")
        if len(tokens) == nbTokensExpected:
            desc = tokens[len(tokens) - 1]
            errType = tokens[len(tokens) - 2]
            lineNbr = int(tokens[len(tokens) - 3])
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
    if sys.platform == "win32":
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        p = subprocess.Popen(cmd, shell=False, startupinfo=startupinfo,
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    else:
        p = subprocess.Popen(cmd, shell=False, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
    stdout, stderr = p.communicate()
    if sys.version_info[0] == 2:
        return stdout.splitlines()[0].split(" ")[2]
    else:
        stdout = str(stdout)
        return stdout.splitlines()[0].split(" ")[2].split("\\n")[0].split(
            "\\r")[0]
