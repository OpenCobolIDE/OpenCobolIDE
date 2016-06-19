# -*- coding: utf-8 -*-
"""
This module contains interactive widgets:
    - interactive console: a text edit made to run subprocesses interactively
"""
import locale
import logging
import sys

from pyqode.core.api.client import PROCESS_ERROR_STRING
from pyqode.core.managers.decorations import TextDecorationsManager
from pyqode.core.managers.panels import PanelsManager
from pyqode.qt.QtCore import Qt, Signal, QProcess, QProcessEnvironment
from pyqode.qt.QtWidgets import QTextEdit, QAction, QApplication
from pyqode.qt.QtGui import QColor, QTextCursor, QFont, QKeySequence


def _logger():
    return logging.getLogger(__name__)


class InteractiveConsole(QTextEdit):
    """
    An interactive console is a QTextEdit specialised to run a process
    interactively

    The user will see the process outputs and will be able to
    interact with the process by typing some text, this text will be forwarded
    to the process stdin.

    You can customize the colors using the following attributes:

        - stdout_color: color of the process stdout
        - stdin_color: color of the user inputs. Green by default
        - app_msg_color: color for custom application message (
          process started, process finished)
        - stderr_color: color of the process stderr

    .. deprecated: Since v2.10.0, this widget is deprecated, you should use
        :class:`pyqode.core.widgets.OutputWindow` instead.

    """
    #: Signal emitted when the process has finished.
    process_finished = Signal(int)
    process_started = Signal()

    def __init__(self, parent=None):
        super(InteractiveConsole, self).__init__(parent)
        self.panels = PanelsManager(self)
        self.decorations = TextDecorationsManager(self)
        from pyqode.core.panels import SearchAndReplacePanel
        self.panels.append(SearchAndReplacePanel(),
                           SearchAndReplacePanel.Position.TOP)
        self._stdout_col = QColor("#404040")
        self._app_msg_col = QColor("#4040FF")
        self._stdin_col = QColor("#22AA22")
        self._stderr_col = QColor("#FF0000")
        self._write_app_messages = True
        self._process_name = ''
        self.process = None
        self._args = None
        self._usr_buffer = ""
        self._clear_on_start = True
        self._merge_outputs = False
        self._running = False
        self._writer = self.write
        self._user_stop = False
        font = "monospace"
        if sys.platform == "win32":
            font = "Consolas"
        elif sys.platform == "darwin":
            font = 'Monaco'
        self._font_family = font
        self.setFont(QFont(font, 10))
        self.setReadOnly(True)
        self._mask_user_input = False
        action = QAction(_('Copy'), self)
        action.setShortcut(QKeySequence.Copy)
        action.triggered.connect(self.copy)
        self.add_action(action)
        action = QAction(_('Paste'), self)
        action.setShortcut(QKeySequence.Paste)
        action.triggered.connect(self.paste)
        self.add_action(action)

    def showEvent(self, event):
        super(InteractiveConsole, self).showEvent(event)
        self.panels.refresh()

    def resizeEvent(self, e):
        super(InteractiveConsole, self).resizeEvent(e)
        self.panels.resize()

    def add_action(self, action):
        self.addAction(action)
        action.setShortcutContext(Qt.WidgetShortcut)

    def set_writer(self, writer):
        """
        Changes the writer function to handle writing to the text edit.

        A writer function must have the following prototype:

        .. code-block:: python

            def write(text_edit, text, color)

        :param writer: write function as described above.
        """
        if self._writer != writer and self._writer:
            self._writer = None
        if writer:
            self._writer = writer

    def _on_stdout(self):
        raw = bytes(self.process.readAllStandardOutput())
        try:
            txt = raw.decode(sys.getfilesystemencoding())
        except UnicodeDecodeError:
            txt = str(raw).replace("b'", '')[:-1].replace(
                '\\r\\n', '\n').replace('\\\\', '\\')
        _logger().debug('stdout: %s', txt)
        self._writer(self, txt, self.stdout_color)

    def _on_stderr(self):
        raw = bytes(self.process.readAllStandardError())
        try:
            txt = raw.decode(sys.getfilesystemencoding())
        except UnicodeDecodeError:
            txt = str(raw).replace("b'", '')[:-1].replace(
                '\\r\\n', '\n').replace('\\\\', '\\')
        _logger().debug('stderr: %s', txt)
        self._writer(self, txt, self.stderr_color)

    @property
    def exit_code(self):
        if self.is_running:
            return None
        exit_status = self.process.exitStatus()
        if exit_status == self.process.Crashed:
            exit_code = 139
        else:
            exit_code = self.process.exitCode()
        return exit_code

    @property
    def write_app_messages(self):
        return self._write_app_messages

    @write_app_messages.setter
    def write_app_messages(self, value):
        self._write_app_messages = value

    @property
    def background_color(self):
        """ The console background color. Default is white. """
        pal = self.palette()
        return pal.color(pal.Base)

    @background_color.setter
    def background_color(self, color):
        pal = self.palette()
        pal.setColor(pal.Base, color)
        pal.setColor(pal.Text, self.stdout_color)
        self.setPalette(pal)

    @property
    def stdout_color(self):
        """ STDOUT color. Default is black. """
        return self._stdout_col

    @stdout_color.setter
    def stdout_color(self, color):
        self._stdout_col = color
        pal = self.palette()
        pal.setColor(pal.Text, self._stdout_col)
        self.setPalette(pal)

    @property
    def stderr_color(self):
        """
        Color for stderr output if
        :attr:`pyqode.core.widgets.InteractiveConsole.merge_outputs` is False.

        Default is Red.
        """
        return self._stderr_col

    @stderr_color.setter
    def stderr_color(self, color):
        self._stderr_col = color

    @property
    def stdin_color(self):
        """
        STDIN color. Default is green.
        """
        return self._stdin_col

    @stdin_color.setter
    def stdin_color(self, color):
        self._stdin_col = color

    @property
    def app_msg_color(self):
        """
        Color of the application messages (e.g.: 'Process started',
        'Process finished with status %d')
        """
        return self._app_msg_col

    @app_msg_color.setter
    def app_msg_color(self, color):
        self._app_msg_col = color

    @property
    def clear_on_start(self):
        """
        True to clear window when starting a new process. False to accumulate
        outputs.
        """
        return self._clear_on_start

    @clear_on_start.setter
    def clear_on_start(self, value):
        self._clear_on_start = value

    @property
    def merge_outputs(self):
        """
        Merge stderr with stdout. Default is False.

        If set to true, stderr and stdin will use the same color: stdin_color.

        """
        return self._merge_outputs

    @merge_outputs.setter
    def merge_outputs(self, value):
        self._merge_outputs = value
        if value:
            self.process.setProcessChannelMode(QProcess.MergedChannels)
        else:
            self.process.setProcessChannelMode(QProcess.SeparateChannels)

    @property
    def is_running(self):
        """
        Checks if the process is running.
        :return:
        """
        return self._running

    @property
    def mask_user_input(self):
        return self._mask_user_input

    @mask_user_input.setter
    def mask_user_input(self, value):
        """
        If true, user input will be replaced by "*".

        Could be useful to run commands as root.
        """
        self._mask_user_input = value

    def closeEvent(self, *args, **kwargs):
        if self.process and self.process.state() == QProcess.Running:
            self.process.terminate()

    def start_process(self, process, args=None, cwd=None, env=None):
        """
        Starts a process interactively.

        :param process: Process to run
        :type process: str

        :param args: List of arguments (list of str)
        :type args: list

        :param cwd: Working directory
        :type cwd: str

        :param env: environment variables (dict).
        """
        self.setReadOnly(False)
        if env is None:
            env = {}
        if args is None:
            args = []
        if not self._running:
            self.process = QProcess()
            self.process.finished.connect(self._on_process_finished)
            self.process.started.connect(self.process_started.emit)
            self.process.error.connect(self._write_error)
            self.process.readyReadStandardError.connect(self._on_stderr)
            self.process.readyReadStandardOutput.connect(self._on_stdout)
            if cwd:
                self.process.setWorkingDirectory(cwd)
            e = self.process.systemEnvironment()
            ev = QProcessEnvironment()
            for v in e:
                values = v.split('=')
                ev.insert(values[0], '='.join(values[1:]))
            for k, v in env.items():
                ev.insert(k, v)
            self.process.setProcessEnvironment(ev)
            self._running = True
            self._process_name = process
            self._args = args
            if self._clear_on_start:
                self.clear()
            self._user_stop = False
            self._write_started()
            self.process.start(process, args)
            self.process.waitForStarted()
        else:
            _logger().warning('a process is already running')

    def stop_process(self):
        """
        Stop the process (by killing it).
        """
        if self.process is not None:
            self._user_stop = True
            self.process.kill()
            self.setReadOnly(True)
            self._running = False

    def get_user_buffer_as_bytes(self):
        """
        Returns the user buffer as a bytes.
        """
        return bytes(self._usr_buffer, locale.getpreferredencoding())

    def keyPressEvent(self, event):
        ctrl = event.modifiers() & Qt.ControlModifier != 0
        if not self.is_running or self.textCursor().hasSelection():
            if event.key() == Qt.Key_C and ctrl:
                self.copy()
            return
        propagate_to_parent = True
        delete = event.key() in [Qt.Key_Backspace, Qt.Key_Delete]
        if delete and not self._usr_buffer:
            return
        if event.key() == Qt.Key_V and ctrl:
            # Paste to usr buffer
            text = QApplication.clipboard().text()
            self._usr_buffer += text
            self.setTextColor(self._stdin_col)
            if self._mask_user_input:
                text = len(text) * '*'
            self.insertPlainText(text)
            return
        if event.key() in [Qt.Key_Return, Qt.Key_Enter]:
            # send the user input to the child process
            if sys.platform == 'win32':
                self._usr_buffer += "\r"
            self._usr_buffer += "\n"
            self.process.write(self.get_user_buffer_as_bytes())
            self._usr_buffer = ""
        else:
            if not delete and len(event.text()):
                txt = event.text()
                self._usr_buffer += txt
                if self._mask_user_input:
                    txt = '*'
                self.setTextColor(self._stdin_col)
                self.insertPlainText(txt)
                propagate_to_parent = False
            elif delete:
                self._usr_buffer = self._usr_buffer[:len(self._usr_buffer) - 1]
        # text is inserted here, the text color must be defined before this
        # line
        if propagate_to_parent:
            super(InteractiveConsole, self).keyPressEvent(event)
        self.setTextColor(self._stdout_col)

    def _on_process_finished(self, exit_code, exit_status):
        if self is None:
            return
        self._running = False
        if not self._user_stop:
            if self._write_app_messages:
                self._writer(
                    self, "\nProcess finished with exit code %d" %
                    self.exit_code, self._app_msg_col)
        _logger().debug('process finished (exit_code=%r, exit_status=%r)',
                        exit_code, exit_status)
        try:
            self.process_finished.emit(exit_code)
        except TypeError:
            # pyqtSignal must be bound to a QObject, not 'InteractiveConsole'
            pass
        else:
            self.setReadOnly(True)

    def _write_started(self):
        if not self._write_app_messages:
            return
        self._writer(self, "{0} {1}\n".format(
            self._process_name, " ".join(self._args)), self._app_msg_col)
        self._running = True

    def _write_error(self, error):
        if self is None:
            return
        if self._user_stop:
            self._writer(self, '\nProcess stopped by the user',
                         self.app_msg_color)
        else:
            err = PROCESS_ERROR_STRING[error]
            self._writer(self, "Error: %s" % err, self.stderr_color)
            _logger().warn('process error: %s', err)
        self._running = False

    @staticmethod
    def write(text_edit, text, color):
        """
        Default write function. Move the cursor to the end and insert text with
        the specified color.

        :param text_edit: QInteractiveConsole instance
        :type text_edit: pyqode.widgets.QInteractiveConsole

        :param text: Text to write
        :type text: str

        :param color: Desired text color
        :type color: QColor
        """
        try:
            text_edit.moveCursor(QTextCursor.End)
            text_edit.setTextColor(color)
            text_edit.insertPlainText(text)
            text_edit.moveCursor(QTextCursor.End)
        except RuntimeError:
            pass

    def apply_color_scheme(self, color_scheme):
        """
        Apply a pygments color scheme to the console.

        As there is not a 1 to 1 mapping between color scheme formats and
        console formats, we decided to make the following mapping (it usually
        looks good for most of the available pygments styles):

            - stdout_color = normal color
            - stderr_color = red (lighter if background is dark)
            - stdin_color = numbers color
            - app_msg_color = string color
            - bacgorund_color = background


        :param color_scheme: pyqode.core.api.ColorScheme to apply
        """
        self.stdout_color = color_scheme.formats['normal'].foreground().color()
        self.stdin_color = color_scheme.formats['number'].foreground().color()
        self.app_msg_color = color_scheme.formats[
            'string'].foreground().color()
        self.background_color = color_scheme.background
        if self.background_color.lightness() < 128:
            self.stderr_color = QColor('#FF8080')
        else:
            self.stderr_color = QColor('red')


if __name__ == '__main__':
    from pyqode.qt import QtWidgets
    app = QtWidgets.QApplication([])
    console = InteractiveConsole()
    console.start_process('cal')
    console.show()
    app.exec_()
