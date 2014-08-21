import os

from oci_old import constants
from oci_old.backend import compiler
from oci_old.backend.parser import detect_file_type, parse_ast
from oci_old.settings import Settings


def checkFile(data):
    filename = os.path.split(data['path'])[1]
    basename = os.path.splitext(filename)[0]
    tmp = os.path.join(constants.getAppTempDirectory(), filename)
    if os.path.isdir(tmp):
        return False, []
    with open(tmp, 'w') as tempfile:
        tempfile.write(data['code'])
    fileType = detect_file_type(tmp, data['encoding'])
    output = os.path.join(constants.getAppTempDirectory(),
                          basename + fileType[2])
    status, messages = compiler.compile(tmp, fileType, outputFilename=output)
    return True, messages


class CobolAnalyserProvider:
    def __init__(self):
        self.__keywordsCompletions = []
        for keyword in constants.COBOL_KEYWORDS:
            self.__keywordsCompletions.append(
                {'name': keyword, 'icon': constants.ICON_KEYWORD})

    def complete(self, code, line, column, completionPrefix,
                 file_path, file_encoding):
        completions = []
        try:
            root, vars, functions = parse_ast(
                file_path, code=code, encoding=file_encoding, free=Settings().free_format)
        except AttributeError:
            vars = []
            functions = []
        for var in vars:
            completions.append(
                {'name': var.name, 'icon': constants.ICON_VAR,
                 'tooltip': var.description})
        for func in functions:
            completions.append(
                {'name': func.name, 'icon': constants.ICON_PARAGRAPH})
        completions += self.__keywordsCompletions
        return completions