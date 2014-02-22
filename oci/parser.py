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
        self.description = description.replace(".", "")
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


def _extract_div_node(l, c, line, root_node, last_section_node, createIcon):
    """
    Extracts a division node from a line

    :param l: The line number (starting from 0)

    :param line: The line string (without indentation)

    :param root_node: The document root node.

    :return: tuple(last_div_node, last_section_node)
    """
    name = line
    name = name.replace(".", "")
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
    name = name.replace(".", "")
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
    raw_tokens = line.split(" ")
    tokens = []
    for t in raw_tokens:
        if not t.isspace() and t != "":
            tokens.append(t)
    try:
        lvl = int(tokens[0], 16)
        name = tokens[1]
    except ValueError:
        lvl = 0
        name = tokens[0]
    name = name.replace(".", "")
    description = "{1}".format(i + 1, line)
    # if indentation == 7:
    #     lvl = 0
    if lvl == 1:
        parent_node = last_section_node
    else:
        # trouver les premier niveau inferieur
        levels = sorted(last_vars.keys(), reverse=True)
        for l in levels:
            if l < lvl:
                parent_node = last_vars[l]
                break
    if not parent_node:
        # malformed code
        return None
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
    name = line.replace(".", "")
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
        if not line.isspace():
            line = line.strip()
            # DIVISIONS
            if "DIVISION" in line.upper():
                # remember
                if last_div_node is not None:
                    last_div_node.end_line = i
                last_div_node, last_section_node = _extract_div_node(
                    i, indentation, line, root_node, last_section_node, createIcon)
            # SECTIONS
            elif "SECTION" in line:
                if last_section_node:
                    last_section_node.end_line = i
                last_section_node = _extract_section_node(
                    i, indentation, last_div_node, last_vars, line, createIcon)
            # VARIABLES
            elif (last_div_node is not None and
                    "DATA DIVISION" in last_div_node.name):
                v = _extract_var_node(
                    i, indentation, indentation, last_section_node, last_vars, line,
                    createIcon)
                if v:
                    variables.append(v)
            # PARAGRAPHS
            elif (last_div_node is not None and
                  "PROCEDURE DIVISION" in last_div_node.name and
                      not "*" in line and not "EXIT" in line and
                      not "END" in line and not "STOP" in line):
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
                    file_type = detectFileType(dependency)
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