#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Cobol backend server which adds a CobolAnalyserProvider and a
DocumentWordsProvider to the CodeCompletion worker.

.. note::
    On Windows and Mac OSX, you should freeze this script as a console
    executable and name it cobol-backend.exe on Windows or cobol-backend on
    OSX.

"""
from pyqode.core import backend

from pyqode.cobol.backend.workers import CobolCodeCompletionProvider


if __name__ == '__main__':
    backend.CodeCompletionWorker.providers.append(
        CobolCodeCompletionProvider())
    backend.DocumentWordsProvider.separators.remove('-')
    backend.CodeCompletionWorker.providers.append(
        backend.DocumentWordsProvider())
    backend.serve_forever()
