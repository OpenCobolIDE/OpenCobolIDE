"""
Contains and functions to cobol source code analysis
"""
import pyqode.core
from PyQt4.QtCore import QFileInfo
from PySide.QtGui import QIcon, QTreeWidgetItem


# taken from pygments_ibm_cobol_lexer.__init__.py
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
        0:  ":/ide-icons/rc/Office-book.png",
        1:  ":/ide-icons/rc/text-x-generic.png",
        2:  ":/ide-icons/rc/var.png",
        3:  ":/ide-icons/rc/paragraph.png"}

    def __init__(self, node_type, line, name, description=None):
        QTreeWidgetItem.__init__(self)
        self.node_type = node_type
        self.line = line
        self.end_line = -1
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
        print " " * indent, self.name, self.line, " - ", self.end_line
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


def _extract_div_node(i, line, root_node, last_section_node):
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
    if last_section_node:
        last_section_node.end_line = i
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
    Parses a cobol file and return  a root DocumentNode that describes the
    layout of the file (sections, divisions, vars,...), a list of paragraphes
    nodes and variable nodes.

    :param filename: The cobol file path

    :return: The root node, the list of variables, the list of paragraphes
    :rtype: DocumentNode, list of DocumentNode, list of DocumentNode
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
        last_par = None
        for i, line in enumerate(lines):
            indentation = len(line) - len(line.lstrip())
            if indentation >= 7 and not line.isspace():
                line = line.strip().upper()
                # DIVISIONS
                if "DIVISION" in line.upper():
                    # remember
                    if last_div_node is not None:
                        last_div_node.end_line = i
                    last_div_node, last_section_node = _extract_div_node(
                        i, line, root_node, last_section_node)
                # SECTIONS
                elif "SECTION" in line:
                    if last_section_node:
                        last_section_node.end_line = i
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
                      indentation == 7 and not "*" in line and
                      not "EXIT" in line and not "END" in line and not "STOP"
                      in line):
                    if last_par:
                        last_par.end_line = i
                    p = _extract_paragraph_node(
                        i, last_div_node, last_section_node, line)
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
        if len(text.strip()) == 0:
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
