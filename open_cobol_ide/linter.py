"""
Cobol linter; use open COBOL to check your your syntax on the fly.
"""
import locale
import os
import tempfile
import time
from pyqode.qt import QtCore
from pyqode.core.modes import CheckerMode
from open_cobol_ide import settings, msvc
from open_cobol_ide.compilers import GnuCobolCompiler


def make_linter_command(cobol_file_name, original_file_path):
    from .settings import Settings
    settings = Settings()
    args = ['-fsyntax-only', '-I%s' % os.path.dirname(original_file_path)]
    args.append('-std=%s' % str(settings.cobol_standard).replace(
        'GnuCobolStandard.', ''))
    args += settings.compiler_flags
    original_path = os.path.dirname(original_file_path)
    if settings.free_format:
        args.append('-free')
    if settings.copybook_paths:
        for pth in settings.copybook_paths.split(';'):
            if not pth:
                continue
            if not os.path.isabs(pth):
                # expand relative path based on the original source path
                # See github issue #119
                pth = os.path.abspath(os.path.join(original_path, pth))
            args.append('-I%s' % pth)

    if settings.library_search_path:
        for pth in settings.library_search_path.split(';'):
            if pth:
                args.append('-L%s' % pth)
    if settings.libraries:
        for lib in settings.libraries.split(' '):
            if lib:
                args.append('-l%s' % lib)
    args.append(cobol_file_name)
    pgm = Settings().compiler_path
    return pgm, args


def lint(request_data):
    """
    Performs linting of a COBOL document.

    This method will perform on the pyqode backend.

    :param request_data: work request data (dict)
    :return: status, messages
    """
    from open_cobol_ide.app import Application
    Application.update_environment_vars()
    code = request_data['code']
    path = request_data['path']
    extension = os.path.splitext(path)[1]
    if extension.lower() in settings.Settings().cobc_extensions:
        # code might not have been saved yet, run cobc on a tmp file
        # we use a time stamp to avoid overwriting the file another cobc
        # instance might be compiling.
        vcvarsall = settings.Settings().vcvarsall
        if vcvarsall:
            msvc.initialize(vcvarsall, settings.Settings().vcvarsall_arch)
        file_name = os.path.split(path)[1]
        file_name, ext = os.path.splitext(file_name)
        tmp_name = '%s.%s%s' % (file_name, str(int(time.time())), ext)
        tmp_pth = os.path.join(tempfile.gettempdir(), tmp_name)
        with open(tmp_pth, 'w') as f:
            f.write(code)
        compiler = GnuCobolCompiler()
        pgm, args = make_linter_command(tmp_name, path)
        process = QtCore.QProcess()
        process.setWorkingDirectory(os.path.dirname(tmp_pth))
        process.setProcessChannelMode(QtCore.QProcess.MergedChannels)
        print('linter command: %s %s' % (pgm, ' '.join(args)))
        print('working directory: %s' % process.workingDirectory())
        process.start(pgm, args)
        process.waitForFinished()
        output = process.readAllStandardOutput().data().decode(
            locale.getpreferredencoding())
        print('linter raw output: %s' % output)
        messages = compiler.parse_output(output, process.workingDirectory())
        print('linter parsed output: %r' % messages)
        os.remove(tmp_pth)
        return messages
    return []


class CobolLinterMode(CheckerMode):
    def __init__(self):
        super().__init__(lint)
