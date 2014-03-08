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
Contains the code completions providers:
  - one based on the document words
  - one based on the document analyser
"""
import pyqode.core
from oci import constants
from oci.parser import parse_ast


class CobolDocumentWordsProvider(pyqode.core.DocumentWordCompletionProvider):
    pass


class CobolAnalyserProvider(pyqode.core.CompletionProvider):

    def __init__(self):
        pyqode.core.CompletionProvider.__init__(self)
        self.PRIORITY = 2
        self.__keywordsCompletions = []
        for keyword in constants.COBOL_KEYWORDS:
            self.__keywordsCompletions.append(pyqode.core.Completion(
                keyword, icon=constants.ICON_KEYWORD))

    def preload(self, code, filePath, fileEncoding):
        return self.complete(code, 1, 0, "", filePath, fileEncoding)

    def complete(self, code, line, column, completionPrefix,
                 filePath, fileEncoding):
        completions = []
        try:
            root, vars, functions = parse_ast(
                filePath, code=code, encoding=fileEncoding)
        except AttributeError:
            vars = []
            functions = []
        for var in vars:
            completions.append(pyqode.core.Completion(
                var.name, icon=constants.ICON_VAR, tooltip=var.description))
        for func in functions:
            completions.append(pyqode.core.Completion(
                func.name, icon=constants.ICON_PARAGRAPH))
        completions += self.__keywordsCompletions
        return completions
