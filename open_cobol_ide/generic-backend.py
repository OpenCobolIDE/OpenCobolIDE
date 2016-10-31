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
import os
import sys

sys.path.insert(0, os.environ.get('OCIDE_EXTLIBS_PATH', ''))


if __name__ == '__main__':
    from pyqode.core import backend

    backend.CodeCompletionWorker.providers.append(
        backend.DocumentWordsProvider())
    backend.serve_forever()
