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
Contains application constants
"""
import os
import pyqode.core
import pyqode.widgets
# cobol use - extensively for complex identifier, don't break them!
import sys

pyqode.core.constants.WORD_SEPARATORS.remove("-")


COBOL_EXTENSIONS = [".COB", ".CBL", ".PCO", ".CPY"]
ALL_COBOL_EXTENSIONS = COBOL_EXTENSIONS + [ext.lower() for ext in COBOL_EXTENSIONS]
COBOL_FILES_FILTER = "Cobol files (%s)" % " ".join(ALL_COBOL_EXTENSIONS).replace(".", "*.")
OTHER_FILES_FILTER = "Other text files (*)"
FILTER_SEPARATOR = ";;"

ICON_PARAGRAPH = ":/ide-icons/rc/paragraph"
ICON_VAR = ":/ide-icons/rc/var"
ICON_KEYWORD = ":/ide-icons/rc/keyword"

EXECUTABLE_EXTENSION = ".exe"
MODULE_EXTENSION = ".so"
if sys.platform == "win32":
    MODULE_EXTENSION = ".dll"


WHITE_STYLE = 0
DARK_STYLE = 1


class DarkColorScheme(pyqode.widgets.ColorScheme):
    """
    Define a dark color scheme for easy customization of the stylesheet.
    """

    def __init__(self):
        super(DarkColorScheme, self).__init__()
        self.background_color = "#302F2F"
        self.title_background_color = "#4b4b4b"
        self.text_color = "#C5C5C5"
        self.border_color = "#555555"
        self.selection_bck_color = "#343434"
        self.selection_color = "#C5C5C5"
        self.hover_color = "#343434"


class ProgramType:
    """
    Enumerates the supported file types along with their base compile command
    format
    """
    #: Cobol program (executable compiled with -x switch)
    Executable = (0, ['cobc', "-x", "-o %s", "%s"],
                  EXECUTABLE_EXTENSION)
    #: Cobol subprogram (shared object/dll compiled without the -x switch)
    Module = (1, ['cobc', "-o %s", "%s"],
              MODULE_EXTENSION)

    @staticmethod
    def cmd(programType, input, output, customOptions=None):
        baseCmdArgs = programType[1]
        if programType[0] == 0:
            args = [baseCmdArgs[0], baseCmdArgs[1],
                    baseCmdArgs[2] % output, baseCmdArgs[3] % input]
        else:
            args = [baseCmdArgs[0], baseCmdArgs[1] % output,
                    baseCmdArgs[2] % input]
        if customOptions:
            args += customOptions
        return args


def getAppTempDirectory():
    """
    Returns a platform dependant temp fold
    """
    if sys.platform == "win32":
        pth = os.path.join(os.path.expanduser("~"), "OpenCobolIDE")
        if not os.path.exists(pth):
            os.mkdir(pth)
        pth = os.path.join(pth, "temp")
        if not os.path.exists(pth):
            os.mkdir(pth)
        return pth
    else:
        pth = os.path.join(os.path.expanduser("~"), ".OpenCobolIDE")
        if not os.path.exists(pth):
            os.mkdir(pth)
        pth = os.path.join(pth, "temp")
        if not os.path.exists(pth):
            os.mkdir(pth)
        return pth


EXE_TEMPLATE = """      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
            DISPLAY "Hello world"
            STOP RUN.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.

"""

MODULE_TEMPLATE = """      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       LINKAGE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       01 PARAMETRES.
      **
      * Input/Output parameters from/to the calling PROGRAM
      **
           02 PA-RETURN-CODE PIC 99 VALUE 0.
       PROCEDURE DIVISION USING PARAMETRES.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
        DISPLAY "Hello world"
        MOVE 0 TO PA-RETURN-CODE
        STOP RUN.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM.

"""

TEMPLATES = [EXE_TEMPLATE, MODULE_TEMPLATE, ""]


COBOL_KEYWORDS = [
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