"""
This module contains the various regular expressions used through the whole
project.

"""
from pyqode.qt import QtCore

#: This pattern identifies a struct definition (01 XXXX.)
STRUCT_PATTERN = QtCore.QRegExp(r'((^|^\s*)\d\d [\w\-]+\.$)')
#: This pattern identifies a paragraph definition
PARAGRAPH_PATTERN = QtCore.QRegExp(r'((^|^\s{7})[\w\-]+\.\s*$)')
#: This pattern identifies a loop pattern
LOOP_PATTERN = QtCore.QRegExp(r'PERFORM.+(UNTIL|TIMES){1}')
#: This pattern identifies the start of a branch
BRANCH_START = QtCore.QRegExp(r'((^|\s)IF\b|ELSE)')
#: This pattern identifies the end of a branch
BRANCH_END = QtCore.QRegExp(r'END-\w*$')
#: This pattern identifiers a DIVISION
DIVISION = QtCore.QRegExp(r'.*DIVISION( USING [a-zA-Z-]*)?.*\.$')
#: This pattern identifiers a SECTION
SECTION = QtCore.QRegExp(r'.*SECTION.*\.$')
#: This pattern identify
VAR_PATTERN = QtCore.QRegExp(r'\s*\d+\s.*.+')
