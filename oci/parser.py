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
import os
import sys
from PyQt4.QtCore import QFileInfo
from PyQt4.QtGui import QTreeWidgetItem, QIcon
from oci import constants


class Statement(object):
    """
    A statement is a node in the simplified abstract syntax tree.
    """

    class Type:
        """
        Enumerates the possible statement types (div, section, paragraph,...)
        """
        Root = -1
        Division = 0
        Section = 1
        Variable = 2
        Paragraph = 3

    def __init__(self, node_type, line, column, name, description=None):
        self.node_type = node_type
        self.line = line
        self.column = column
        self.end_line = -1
        self.name = name
        if description is None:
            description = name
        self.description = description.replace(".", "")
        self.children = []
        #self.setText(0, name)
        #if createIcon:
        #    self.setIcon(0, QIcon(self.ICONS[node_type]))
        #self.setToolTip(0, self.description)

    def add_child(self, child):
        """
        Add a child to the node (call QTreeWidgetItem::addChilld automatically)

        :param child: The child node to add
        """
        self.children.append(child)
        #self.addChild(child)

    def print_tree(self, indent=0):
        """
        Print the node tree in stdout
        """
        print(" " * indent, self.name, self.line, " - ", self.end_line)
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


def parse_division(l, c, line, root_node, last_section_node):
    """
    Extracts a division node from a line

    :param l: The line number (starting from 0)

    :param c: The column number

    :param line: The line string (without indentation)

    :param root_node: The document root node.

    :return: tuple(last_div_node, last_section_node)
    """
    name = line
    name = name.replace(".", "")
    node = Statement(Statement.Type.Division, l + 1, c, name)
    root_node.add_child(node)
    last_div_node = node
    # do not take previous sections into account
    if last_section_node:
        last_section_node.end_line = l
    last_section_node = None
    return last_div_node, last_section_node


def parse_section(l, c, last_div_node, last_vars, line):
    """
    Extracts a section node from a line.

    :param l: The line number (starting from 0)

    :param last_div_node: The last div node found

    :param last_vars: The last vars dict

    :param line: The line string (without indentation)

    :return: last_section_node
    """
    name = line
    name = name.replace(".", "")
    node = Statement(Statement.Type.Section, l + 1, c, name)
    last_div_node.add_child(node)
    last_section_node = node
    # do not take previous var into account
    last_vars.clear()
    return last_section_node


def parse_pic_field(l, c, last_section_node, last_vars, line):
    """
    Parse a pic field line. Return A VariableNode or None in case of malformed code.

    :param l: The line number (starting from 0)
    :param c: The column number (starting from 0)
    :param last_section_node: The last section node found
    :param last_vars: The last vars dict
    :param line: The line string (without indentation)
    :return: The extracted variable node
    """
    if "FD " in line:
        pass
    parent_node = None
    raw_tokens = line.split(" ")
    tokens = []
    for t in raw_tokens:
        if not t.isspace() and t != "":
            tokens.append(t)
    try:
        if tokens[0] == "FD":
            lvl = 1
        else:
            lvl = int(tokens[0], 16)
        name = tokens[1]
    except ValueError:
        lvl = 1
        name = tokens[0]
    except IndexError:
        # line not complete
        return None
    name = name.replace(".", "")
    description = line
    if lvl == 1:
        parent_node = last_section_node
    else:
        # find parent level
        levels = sorted(last_vars.keys(), reverse=True)
        for lv in levels:
            if lv < lvl:
                parent_node = last_vars[lv]
                break
    if not parent_node:
        # malformed code
        return None
    node = Statement(Statement.Type.Variable, l + 1, c, name, description)
    parent_node.add_child(node)
    last_vars[lvl] = node
    return node


def parse_paragraph(l, c, last_div_node, last_section_node, line):
    """
    Extracts a paragraph node

    :param l: The line number (starting from 0)
    :param last_div_node: The last div node found
    :param last_section_node: The last section node found
    :param line: The line string (without indentation)
    :return: The extracted paragraph node
    """
    name = line.replace(".", "")
    parent_node = last_div_node
    if last_section_node is not None:
        parent_node = last_section_node
    node = Statement(Statement.Type.Paragraph, l + 1, c, name)
    parent_node.add_child(node)
    return node


def parse(filename, code=None, encoding="utf-8"):
    """
    Parse a cobol document and build as simple syntax tree. For convenience, it also
    returns the list of variables (PIC) and procedures (paragraphs).

    :param filename: The cobol file path to open in case code is None.
    :param code: cobol code to parse. Default is None.
    :type encoding: file encoding

    :return: A tuple made up of the AST root node, the list of variables, the list of paragraphes.
    :rtype: Statement, list of Statement, list of Statement
    """
    root_node = Statement(Statement.Type.Root, 0, 0,
                             QFileInfo(filename).fileName())
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
        column = len(line) - len(line.lstrip())
        if not line.isspace() and not line.strip().startswith("*"):
            line = line.strip()
            # DIVISIONS
            if "DIVISION" in line.upper():
                # remember
                if last_div_node is not None:
                    last_div_node.end_line = i
                last_div_node, last_section_node = parse_division(
                    i, column, line, root_node, last_section_node)
            # SECTIONS
            elif "SECTION" in line:
                if last_section_node:
                    last_section_node.end_line = i
                last_section_node = parse_section(
                    i, column, last_div_node, last_vars, line)
            # VARIABLES
            elif (last_div_node is not None and
                    "DATA DIVISION" in last_div_node.name):
                v = parse_pic_field(
                    i, column, last_section_node, last_vars, line)
                if v:
                    variables.append(v)
            # PARAGRAPHS
            elif (last_div_node is not None and
                          "PROCEDURE DIVISION" in last_div_node.name):
                tokens = line.split(" ")
                if len(tokens) == 1 and not tokens[0] in constants.COBOL_KEYWORDS:
                    if last_par:
                        last_par.end_line = i
                    p = parse_paragraph(
                        i, column, last_div_node, last_section_node, line)
                    if p:
                        paragraphs.append(p)
                    last_par = p
    # close last div
    if last_par:
        last_par.end_line = len(lines) - 1
    if last_div_node:
        last_div_node.end_line = len(lines)
    return root_node, variables, paragraphs


def detect_file_type(filename):
    """
    Detect file type:
        - cobol program
        - cobol subprogram
        - text file

    :param filename: The file name to check
    """
    ext = os.path.splitext(filename)[1].lower()
    type = constants.ProgramType.Executable
    if ext in constants.ALL_COBOL_EXTENSIONS:
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


def parse_dependencies(filename):
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
                    file_type = detect_file_type(dependency)
                    dependencies.append((dependency, file_type))
                    dependencies += parse_dependencies(dependency)

    def make_unique_sorted(seq):
        # order preserving
        checked = []
        for e in seq:
            if e not in checked:
                checked.append(e)
        return sorted(checked)
    return make_unique_sorted(dependencies)