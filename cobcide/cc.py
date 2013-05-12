# This file is part of cobcide.
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
Contains the cobol specific completion model
"""
from pcef.modes.cc import CompletionModel
from pcef.modes.cc import Suggestion

# take from pygments_ibm_cobol_lexer.__init__.py
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


class CobolCompletionModel(CompletionModel):
    """
    A simple static completion model based on a list of Cobol keywords
    """
    def __init__(self, analyserMode):
        super(CobolCompletionModel, self).__init__(priority=1)
        self.analyserMode = analyserMode
        self.analyserMode.documentLayoutChanged.connect(self._updateSuggestions)
        # customize cobol keywords icons
        self.__reserved_suggestions = []
        for keyword in COBOL_KEYWORDS:
            self.__reserved_suggestions.append(
                Suggestion(keyword, ":/ide-icons/rc/keyword.png"))
        self._initialised = False

    def update(self, source_code, line, col, filename, encoding):
        """
        Updates the suggestions list
        """
        pass

    def _updateSuggestions(self):
        self._suggestions[:] = []
        # vars
        variables = self.analyserMode.variables
        for var in variables:
            self._suggestions.append(
                Suggestion(var.name, icon=":/ide-icons/rc/var.png",
                           description=var.description))
        # paragraphs
        paragraphs = self.analyserMode.paragraphs
        for p in paragraphs:
            self._suggestions.append(
                Suggestion(p.name, icon=":/ide-icons/rc/paragraph.png"))

        # reserved keywords suggestions
        self._suggestions += self.__reserved_suggestions
        self._initialised = True

