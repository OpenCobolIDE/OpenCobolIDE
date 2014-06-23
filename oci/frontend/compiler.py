"""
This module contains functions and classes that helps manipulate the cobol
compiler from the gui side; this includes a compilation manager object (
QObject) with signals and slots to run a batch of compilation commands and be
easily notified about errors and success. There is also a compilation output
window which is added to the main window as a promoted widget in Qt designer.

"""
import logging
import os
from pyqode.core.modes import CheckerMessage
from pyqode.core.qt import QtCore, QtWidgets, QtGui
from oci.backend.compiler import parse_output


def _logger():
    return logging.getLogger(__name__)


class CompilationProcess(QtCore.QProcess):
    def __init__(self, cmd, dirname):
        super().__init__()
        self.program = cmd[0]
        self.cmd = cmd[1:]
        self.setWorkingDirectory(dirname)

    def compile(self):
        _logger().info('Compilation process: %s %s in %s' %
                       (self.program, self.cmd, self.workingDirectory()))
        self.start(self.program, self.cmd)

    def __str__(self):
        return '%s %s' % (self.program, ' '.join(self.cmd))


class CompilationManager(QtCore.QObject):
    """
    Manager object that runs a series of compilation commands using QProcess
    """
    #: Signal emitted when some data are available from the compiler stdout
    stdoutAvailable = QtCore.Signal(str)
    #: Signal emitted when some data are available from the compiler stderr
    stderrAvailable = QtCore.Signal(str)
    #: Signal emitted when the manager recognized a valid compilation message
    #: (error or warning). This ususally happens a the end of the compiler
    #: process. If no erros were found, an info message will be emitted to tell
    #: the user everything went fine.
    compilerMessageAvailable = QtCore.Signal(object)
    #: Emitted when the compilation has finished (this includes compilation
    #: of dependencies as well).
    compilationFinished = QtCore.Signal(bool)
    #: Emitted on compiler process error (not a compilation error).
    processError = QtCore.Signal(str)
    #: Emitted when a command has started running
    cmdStarted = QtCore.Signal(str)

    def __init__(self):
        super().__init__()
        self._process_queue = []
        self._current_process = None
        self._process_output = ''
        self._process_error = ''

    def run_compilation_commands(self, commands):
        """
        Run a batch of compilation commands.

        :param commands: List of commands as given by
            :meth=`backend.compiler.get_all_commands`
        """
        for cmd, dirname in commands:
            p = CompilationProcess(cmd, dirname)
            p.readyReadStandardError.connect(self._read_stdout)
            p.readyReadStandardError.connect(self._read_stderr)
            p.finished.connect(self._on_finished)
            p.error.connect(self._on_error)
            self._process_queue.append(p)
        self._error = False
        self.start()

    def start(self):
        if len(self._process_queue):
            self._current_process = self._process_queue.pop(0)
            self._process_output = ''
            self._process_error = ''
            self.cmdStarted.emit((str(self._current_process)))
            self._current_process.compile()
        else:
            self.compilationFinished.emit(self._error)

    def _read_stdout(self):
        stdout = self._current_process.readAllStandardOutput().data().decode(
            'utf-8')
        self._process_output += stdout
        self.stdoutAvailable.emit(stdout)

    def _read_stderr(self):
        stderr = self._current_process.readAllStandardError().data().decode(
            'utf-8')
        self._process_error += stderr
        self.stderrAvailable.emit(stderr)

    def _on_error(self, error):
        self.processError.emit(error)

    def _on_finished(self, exitCode, exitStatus):
        # parse output
        if exitCode:
            self._parse_output()
        else:
            self.compilerMessageAvailable.emit(
                CheckerMessage("Compilation succeeded", 0, -1, 0,
                               ':/ide-icons/rc/accept.png',
                               None, self.get_filename_from_cmd()))
        # start next one
        self.start()

    def get_filename_from_cmd(self):
        return os.path.normpath(os.path.join(
            self._current_process.workingDirectory(),
            self._current_process.cmd[-1]))

    def _parse_output(self):
        for msg in parse_output((
                self._process_output + self._process_error).splitlines(),
                self.get_filename_from_cmd()):
            self.compilerMessageAvailable.emit(CheckerMessage(*msg))
            self._error = True


class CompilerOutputEdit(QtWidgets.QTextEdit):
    @QtCore.Slot(str)
    def writeStdout(self, text):
        for line in text.splitlines():
            if line:
                self.setTextColor(self.palette().text().color())
                self.append(line)

    @QtCore.Slot(str)
    def writeStderr(self, text):
        for line in text.splitlines():
            if line:
                self.setTextColor(QtGui.QColor('gray'))
                self.append(line)

    @QtCore.Slot(str)
    def writeCmd(self, cmd):
        self.setTextColor(QtGui.QColor('green'))
        self.append(cmd)