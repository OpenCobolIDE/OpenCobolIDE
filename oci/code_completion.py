"""
Contains the code completions providers:
  - one based on the document words
  - one based on the document analyser
"""
import pyqode.core
from oci import cobol
from oci import constants


class CobolDocumentWordsProvider(pyqode.core.DocumentWordCompletionProvider):
    def parse(self, code, wordSeparators=pyqode.core.constants.WORD_SEPARATORS):
        return pyqode.core.DocumentWordCompletionProvider.parse(self, code,
                                                                wordSeparators)


class CobolAnalyserProvider(pyqode.core.CompletionProvider):

    def __init__(self):
        pyqode.core.CompletionProvider.__init__(self)
        self.PRIORITY = 2
        self.__keywordsCompletions = []
        for keyword in cobol.KEYWORDS:
            self.__keywordsCompletions.append(pyqode.core.Completion(
                keyword, icon=constants.ICON_KEYWORD))

    def preload(self, code, filePath, fileEncoding):
        return self.complete(code, 1, 0, "", filePath, fileEncoding)

    def complete(self, code, line, column, completionPrefix,
                 filePath, fileEncoding):
        completions = []
        root, vars, functions = cobol.parse_document_layout(filePath)
        for var in vars:
            completions.append(pyqode.core.Completion(
                var.name, icon=constants.ICON_VAR))
        for func in functions:
            completions.append(pyqode.core.Completion(
                func.name, icon=constants.ICON_PARAGRAPH))
        completions += self.__keywordsCompletions
        return completions
