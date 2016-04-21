"""
This module contains the various regular expressions used through the whole
project.

"""
from pyqode.qt import QtCore

#: This pattern identifies a struct definition (01 XXXX.)
STRUCT_PATTERN = QtCore.QRegExp(r'((^|^\s*)\d\d [\w\-]+\.$)')
#: This pattern identifies a paragraph definition
PARAGRAPH_PATTERN = QtCore.QRegExp(r'((^|^\s*)\.?[\w\-]+\.\s*$)')
#: This pattern identifies a loop pattern
LOOP_PATTERN = QtCore.QRegExp(r'(^|^\s)PERFORM.*(VARYING|UNTIL|TIMES|WITH|TEST|AFTER)*')
#: This pattern identifies the start of a branch
BRANCH_START = QtCore.QRegExp(r'((^|\s)IF\b|ELSE)')
#: This pattern identifies the end of a branch
BRANCH_END = QtCore.QRegExp(r'END-(PERFORM|IF|READ)\.?')
#: This pattern identifiers a DIVISION
DIVISION = QtCore.QRegExp(r'.*\s+DIVISION.*\.*')
#: This pattern identifiers a SECTION
SECTION = QtCore.QRegExp(r'.*\s+SECTION.*\.*')
#: This pattern identify
VAR_PATTERN = QtCore.QRegExp(r'\s*\d+\s.*.+')
