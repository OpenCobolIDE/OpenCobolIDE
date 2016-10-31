"""
This module contains the output window widget that can be used to visually run a child process in a PyQt application.

The widget supports most ANSI Escape Sequences (colors, cursor positioning,...) and can be used to create a rudimentary
terminal emulator (such a widget is available in the terminal module).
"""
import logging
import os
import re
import sys
import string

from collections import namedtuple

from pyqode.core.api import CodeEdit, SyntaxHighlighter, TextHelper
from pyqode.core.api.client import PROCESS_ERROR_STRING
from pyqode.core.backend import server
from pyqode.qt import QtWidgets, QtGui, QtCore
from pyqode.qt.QtGui import QColor
from pyqode.qt.QtWidgets import qApp

from . import pty_wrapper


# ----------------------------------------------------------------------------------------------------------------------
# Widget
# ----------------------------------------------------------------------------------------------------------------------
class OutputWindow(CodeEdit):
    """
    Widget that runs a child process and print its output in a text area.

    This widget can be used to show the output of a process in a Qt application (i.e. the run output of an IDE) or
    it could be used to implement a terminal emulator. While the parser supports most common Ansi Escape Codes, it
    does not aim to support all VT100 features.

    User inputs are handled by an InputHandler, there are two types of input handlers:
        - ImmediateInputHandler: forward ansi code of each key strokes directly to the child process's stdin. Use this
          for an interactive process such as bash or python.
        - BufferedInputHandler: bufferize user inputs until user pressed enter. Use this for any other kind of process.

    Usage:

        - create an OutputWindow instance (you can specify a color scheme and an input handler).
        - call start_process to actually start your child process.
    """
    #
    # Public API
    #
    #: Define the color_scheme of the output window.
    ColorScheme = namedtuple('ColorScheme', 'background foreground error custom red green yellow blue magenta cyan')

    #: Default theme using QPalette colors, must be initialised by a call to ``_init_default_scheme``
    DefaultColorScheme = None
    #: Linux color scheme (black background with ANSI colors)
    LinuxColorScheme = None
    #: Tango theme (black background with pastel colors).
    TangoColorScheme = None
    #: The famous Solarized dark theme.
    SolarizedColorScheme = None

    #: Signal emitted when the user pressed on a file link.
    #: Client code should open the requested file in the editor.
    #: Parameters:
    #:    - path (str)
    #:    - line number (int, 0 based)
    open_file_requested = QtCore.Signal(str, int)

    process_finished = QtCore.Signal()

    @property
    def process(self):
        """
        Returns a reference to the child process being run (QProcess)
        """
        return self._process

    @property
    def is_running(self):
        """
        Checks if the child process is running (or is starting).
        """
        return self._process.state() in [self._process.Running, self._process.Starting]

    @property
    def color_scheme(self):
        """
        Gets/Sets the color scheme
        """
        return self._formatter.theme

    @color_scheme.setter
    def color_scheme(self, scheme):
        self._formatter.color_scheme = scheme
        self.background = scheme.background
        self.foreground = scheme.foreground
        self._reset_stylesheet()

    @property
    def input_handler(self):
        """
        Gets/Sets an input handler (see :class:`InputHandler).
        """
        return self._input_handler

    @input_handler.setter
    def input_handler(self, handler):
        self._input_handler = handler
        self._input_handler.edit = self
        self._input_handler.process = self._process

    def __init__(self, parent=None, color_scheme=None, formatter=None, input_handler=None, backend=server.__file__,
                 link_regex=re.compile(r'("|\')(?P<url>(/|[a-zA-Z]:\\)[\w/\s\\\.\-]*)("|\')(, line (?P<line>\d*))?')):
        """
        :param parent: parent widget, if any
        :param color_scheme: color scheme to use
        :param formatter: The formatter to use to draw the process output. Use our builtin formatter by default.
        :param input_handler: the user input handler, buffered by default.
        :param backend: backend script used for searching in the process output.
        :param link_regex: Regex used to match file links, default regex was made to match file path of python
            tracebacks. You can specify another regex if needed, the only requirements is that the regex must have
            two named capture groups: 'url' and 'line'
        """
        super(OutputWindow, self).__init__(parent)
        if formatter is None:
            formatter = OutputFormatter(self, color_scheme=color_scheme)
        if input_handler is None:
            input_handler = BufferedInputHandler()
        self.link_regex = link_regex
        self._link_match = None
        self._formatter = formatter
        self._init_code_edit(backend)
        self._process = QtCore.QProcess()
        self._process.readyReadStandardOutput.connect(self._read_stdout)
        self._process.readyReadStandardError.connect(self._read_stderr)
        self._process.error.connect(self._on_process_error)
        self._process.finished.connect(self._on_process_finished)
        self._input_handler = input_handler
        self._input_handler.edit = self
        self._input_handler.process = self._process
        self._last_hovered_block = None
        self.flg_use_pty = False

    def start_process(self, program, arguments=None, working_dir=None, print_command=True,
                      use_pseudo_terminal=True, env=None):
        """
        Starts the child process.

        :param program: program to start
        :param arguments: list of program arguments
        :param working_dir: working directory of the child process
        :param print_command: True to print the full command (pgm + arguments) as the first line
            of the output window
        :param use_pseudo_terminal: True to use a pseudo terminal on Unix (pty), False to avoid
            using a pty wrapper. When using a pty wrapper, both stdout and stderr are merged together.
        :param environ: environment variables to set on the child process. If None, os.environ will be used.
        """
        # clear previous output
        self.clear()
        self.setReadOnly(False)
        if arguments is None:
            arguments = []
        if sys.platform != 'win32' and use_pseudo_terminal:
            pgm = sys.executable
            args = [pty_wrapper.__file__, program] + arguments
            self.flg_use_pty = use_pseudo_terminal
        else:
            pgm = program
            args = arguments
            self.flg_use_pty = False  # pty not available on windows

        self._process.setProcessEnvironment(self._setup_process_environment(env))

        if working_dir:
            self._process.setWorkingDirectory(working_dir)

        if print_command:
            self._formatter.append_message('\x1b[0m%s %s\n' % (program, ' '.join(arguments)),
                                           output_format=OutputFormat.CustomFormat)
        self._process.start(pgm, args)

    def stop_process(self):
        """
        Stops the child process.
        """
        self._process.terminate()
        if not self._process.waitForFinished(100):
            self._process.kill()

    def paste(self):
        """
        Paste the content of the clipboard to the child process'stdtin.
        """
        self.input_handler.paste(QtWidgets.qApp.clipboard().text())

    @staticmethod
    def create_color_scheme(background=None, foreground=None, error=None, custom=None, red=None,
                            green=None, yellow=None, blue=None, magenta=None, cyan=None):
        """
        Utility function that creates a color scheme instance, with default values.

        The default colors are chosen based on the current palette.

        :param background: background color
        :param foreground: foreground color
        :param error: color of error messages (stderr)
        :param custom: color of custom messages (e.g. to print the full command or the process exit code)
        :param red: value of the red ANSI color
        :param green: value of the green ANSI color
        :param yellow: value of the yellow ANSI color
        :param blue: value of the blue ANSI color
        :param magenta: value of the magenta ANSI color
        :param cyan: value of the cyan ANSI color
        :return: A ColorScheme instance.
        """
        if background is None:
            background = qApp.palette().base().color()
        if foreground is None:
            foreground = qApp.palette().text().color()
        is_light = background.lightness() >= 128
        if error is None:
            if is_light:
                error = QColor('dark red')
            else:
                error = QColor('#FF5555')
        if red is None:
            red = QColor(error)
        if green is None:
            if is_light:
                green = QColor('dark green')
            else:
                green = QColor('#55FF55')
        if yellow is None:
            if is_light:
                yellow = QColor('#aaaa00')
            else:
                yellow = QColor('#FFFF55')
        if blue is None:
            if is_light:
                blue = QColor('dark blue')
            else:
                blue = QColor('#5555FF')
        if magenta is None:
            if is_light:
                magenta = QColor('dark magenta')
            else:
                magenta = QColor('#FF55FF')
        if cyan is None:
            if is_light:
                cyan = QColor('dark cyan')
            else:
                cyan = QColor('#55FFFF')
        if custom is None:
            custom = QColor('orange')
        return OutputWindow.ColorScheme(background, foreground, error, custom,
                                        red, green, yellow, blue, magenta, cyan)

    #
    # Overriden Qt Methods
    #
    def setReadOnly(self, value):
        QtWidgets.QPlainTextEdit.setReadOnly(self, value)

    def closeEvent(self, event):
        """
        Terminates the child process on close.
        """
        self.stop_process()
        self.backend.stop()
        try:
            self.modes.remove('_LinkHighlighter')
        except KeyError:
            pass  # already removed
        super(OutputWindow, self).closeEvent(event)

    def keyPressEvent(self, event):
        """
        Handle key press event using the defined input handler.
        """
        if self._process.state() != self._process.Running:
            return
        tc = self.textCursor()
        sel_start = tc.selectionStart()
        sel_end = tc.selectionEnd()
        tc.setPosition(self._formatter._last_cursor_pos)
        self.setTextCursor(tc)
        if self.input_handler.key_press_event(event):
            tc.setPosition(sel_start)
            tc.setPosition(sel_end, tc.KeepAnchor)
            self.setTextCursor(tc)
            super(OutputWindow, self).keyPressEvent(event)
        self._formatter._last_cursor_pos = self.textCursor().position()

    def mouseMoveEvent(self, event):
        """
        Handle mouse over file link.
        """
        c = self.cursorForPosition(event.pos())
        block = c.block()
        self._link_match = None
        self.viewport().setCursor(QtCore.Qt.IBeamCursor)
        for match in self.link_regex.finditer(block.text()):
            if not match:
                continue
            start, end = match.span()
            if start <= c.positionInBlock() <= end:
                self._link_match = match
                self.viewport().setCursor(QtCore.Qt.PointingHandCursor)
                break

        self._last_hovered_block = block
        super(OutputWindow, self).mouseMoveEvent(event)

    def mousePressEvent(self, event):
        """
        Handle file link clicks.
        """
        super(OutputWindow, self).mousePressEvent(event)
        if self._link_match:
            path = self._link_match.group('url')
            line = self._link_match.group('line')
            if line is not None:
                line = int(line) - 1
            else:
                line = 0
            self.open_file_requested.emit(path, line)

    def eventFilter(self, *args):
        return False

    #
    # Utility methods
    #
    def _init_code_edit(self, backend):
        """
        Initializes the code editor (setup modes, panels and colors).
        """
        from pyqode.core import panels, modes
        self.modes.append(_LinkHighlighter(self.document()))
        self.background = self._formatter.color_scheme.background
        self.foreground = self._formatter.color_scheme.foreground
        self._reset_stylesheet()
        self.setCenterOnScroll(False)
        self.setMouseTracking(True)
        self.setUndoRedoEnabled(False)
        search_panel = panels.SearchAndReplacePanel()
        self.panels.append(search_panel, search_panel.Position.TOP)
        self.action_copy.setShortcut('Ctrl+Shift+C')
        self.action_paste.setShortcut('Ctrl+Shift+V')
        self.remove_action(self.action_undo, sub_menu=None)
        self.remove_action(self.action_redo, sub_menu=None)
        self.remove_action(self.action_cut, sub_menu=None)
        self.remove_action(self.action_duplicate_line, sub_menu=None)
        self.remove_action(self.action_indent)
        self.remove_action(self.action_un_indent)
        self.remove_action(self.action_goto_line)
        self.remove_action(search_panel.menu.menuAction())
        self.remove_menu(self._sub_menus['Advanced'])
        self.add_action(search_panel.actionSearch, sub_menu=None)
        self.modes.append(modes.ZoomMode())
        self.backend.start(backend)

    def _setup_process_environment(self, env):
        """
        Sets up the process environment.
        """
        environ = self._process.processEnvironment()
        if env is None:
            env = {}
        for k, v in os.environ.items():
            environ.insert(k, v)
        for k, v in env.items():
            environ.insert(k, v)
        if sys.platform != 'win32':
            environ.insert('TERM', 'xterm')
            environ.insert('LINES', '24')
            environ.insert('COLUMNS', '450')
        environ.insert('PYTHONUNBUFFERED', '1')
        environ.insert('QT_LOGGING_TO_CONSOLE', '1')
        return environ

    def _on_process_error(self, error):
        """
        Display child process error in the text edit.
        """
        if self is None:
            return
        err = PROCESS_ERROR_STRING[error]
        self._formatter.append_message(err + '\r\n', output_format=OutputFormat.ErrorMessageFormat)

    def _on_process_finished(self):
        """
        Write the process finished message and emit the `finished` signal.
        """
        exit_code = self._process.exitCode()
        if self._process.exitStatus() != self._process.NormalExit:
            exit_code = 139
        self._formatter.append_message('\x1b[0m\nProcess finished with exit code %d' % exit_code,
                                       output_format=OutputFormat.CustomFormat)
        self.setReadOnly(True)
        self.process_finished.emit()

    def _decode(self, data):
        for encoding in ['utf-8', 'cp850', 'cp1252', 'ascii']:
            try:
                string = data.decode(encoding)
            except UnicodeDecodeError:
                _logger().debug('failed to decode output with encoding=%r', encoding)
                continue
            else:
                _logger().debug('decoding output with encoding=%r succeeded', encoding)
                return string
        return str(data).replace("b'", '')[:-1].replace('\\r', '\r').replace('\\n', '\n').replace('\\\\', '\\')

    def _read_stdout(self):
        """
        Reads the child process' stdout and process it.
        """
        output = self._decode(self._process.readAllStandardOutput().data())
        if self._formatter:
            self._formatter.append_message(output, output_format=OutputFormat.NormalMessageFormat)
        else:
            self.insertPlainText(output)

    def _read_stderr(self):
        """
        Reads the child process' stderr and process it.
        """
        output = self._decode(self._process.readAllStandardError().data())
        if self._formatter:
            self._formatter.append_message(output, output_format=OutputFormat.ErrorMessageFormat)
        else:
            self.insertPlainText(output)


class _LinkHighlighter(SyntaxHighlighter):
    """
    Highlights links using OutputWindow.link_regex.
    """
    def highlight_block(self, text, block):
        for match in self.editor.link_regex.finditer(text):
            if match:
                start, end = match.span('url')
                fmt = QtGui.QTextCharFormat()
                fmt.setForeground(QtWidgets.qApp.palette().highlight().color())
                fmt.setUnderlineStyle(fmt.SingleUnderline)
                self.setFormat(start, end - start, fmt)


# ----------------------------------------------------------------------------------------------------------------------
# Parser
# ----------------------------------------------------------------------------------------------------------------------
#: Represents a formatted text: a string + a QtGui.QTextCharFormat
FormattedText = namedtuple('FormattedText', 'txt fmt')
#: Generic structure for representing a terminal operation (draw, move cursor,...).
Operation = namedtuple('Operation', 'command data')


class AnsiEscapeCodeParser(object):
    """
    The AnsiEscapeCodeParser class parses text and extracts ANSI escape codes from it.

    In order to preserve color information across text segments, an instance of this class
    must be stored for the lifetime of a stream.
    Also, one instance of this class should not handle multiple streams (at least not
    at the same time).

    Its main function is parse_text(), which accepts text and default QTextCharFormat.

    This function is designed to parse text and split colored text to smaller strings,
    with their appropriate formatting information set inside QTextCharFormat.

    Compared to the QtCreator implementation, we added limited support for terminal
    emulation (changing cursor position, erasing display/line,...).

    Usage:
        - Create new instance of AnsiEscapeCodeParser for a stream.
        - To add new text, call parse_text() with the text and a default QTextCharFormat.
          The result of this function is a list of Operation with their associated data.
    """

    _ResetFormat = 0
    _BoldText = 1
    _ItalicText = 3
    _UnderlinedText = 4
    _NotBold = 21
    _NotItalicNotFraktur = 23
    _NotUnderlined = 24
    _Negative = 7
    _Positive = 27
    _TextColorStart = 30
    _TextColorEnd = 37
    _RgbTextColor = 38
    _DefaultTextColor = 39
    _BackgroundColorStart = 40
    _BackgroundColorEnd = 47
    _RgbBackgroundColor = 48
    _DefaultBackgroundColor = 49
    _Dim = 2

    _escape = "\x1b["
    _escape_alts = ["\x1b(", "\x1b)", '\x1b=', '\x1b]', '\x08', '\x07']
    _escape_len = len(_escape)
    _semicolon = ';'
    _color_terminator = 'm'

    _supported_commands = re.compile(r'^(?P<n>\d*;?\d*)(?P<cmd>[ABCDEFGHJKfP]{1})')
    _unsupported_command = re.compile(r'^(\?\d+h)|^(\??\d+l)|^(\d+d)|^(\d+X)|^(\([AB01])|'
                                      r'^(\)[AB01])|^(\d*;?\d*r)|^(=)|^(>)|^(\d;.*\x07)')

    _commands = {
        'A': 'cursor_up',
        'B': 'cursor_down',
        'C': 'cursor_forward',
        'D': 'cursor_back',
        'E': 'cursor_next_line',
        'F': 'cursor_previous_lined',
        'G': 'cursor_horizontal_absolute',
        'H': 'cursor_position',
        'J': 'erase_display',
        'K': 'erase_in_line',
        'f': 'cursor_position',  # same as H
        'P': 'delete_chars'
    }

    DIM_FACTOR = 120

    def __init__(self):
        fmt = QtGui.QTextCharFormat()
        fmt.setForeground(QtWidgets.qApp.palette().text().color())
        fmt.setBackground(QtWidgets.qApp.palette().base().color())
        FormattedText.__new__.__defaults__ = '', fmt
        self._prev_fmt_closed = True
        self._prev_fmt = fmt
        self._pending_text = ''
        self.color_scheme = None

    def parse_text(self, formatted_text):
        """
        Retursn a list of operations (draw, cup, ed,...).

        Each operation consist of a command and its associated data.

        :param formatted_text: text to parse with the default char format to apply.
        :return: list of Operation
        """
        assert isinstance(formatted_text, FormattedText)
        ret_val = []
        fmt = formatted_text.fmt if self._prev_fmt_closed else self._prev_fmt
        fmt = QtGui.QTextCharFormat(fmt)
        if not self._pending_text:
            stripped_text = formatted_text.txt
        else:
            stripped_text = self._pending_text + formatted_text.txt
            self._pending_text = ''
        while stripped_text:
            try:
                escape_pos = stripped_text.index(self._escape[0])
            except ValueError:
                ret_val.append(Operation('draw', FormattedText(stripped_text, fmt)))
                break
            else:
                if escape_pos != 0:
                    ret_val.append(Operation('draw', FormattedText(stripped_text[:escape_pos], fmt)))
                    stripped_text = stripped_text[escape_pos:]
                    fmt = QtGui.QTextCharFormat(fmt)
            assert stripped_text[0] == self._escape[0]
            while stripped_text and stripped_text[0] == self._escape[0]:
                if self._escape.startswith(stripped_text):
                    # control sequence not complete
                    self._pending_text += stripped_text
                    stripped_text = ''
                    break
                if not stripped_text.startswith(self._escape):
                    # check vt100 escape sequences
                    ctrl_seq = False
                    for alt_seq in self._escape_alts:
                        if stripped_text.startswith(alt_seq):
                            ctrl_seq = True
                            break
                    if not ctrl_seq:
                        # not a control sequence
                        self._pending_text = ''
                        ret_val.append(Operation('draw', FormattedText(stripped_text[:1], fmt)))
                        fmt = QtGui.QTextCharFormat(fmt)
                        stripped_text = stripped_text[1:]
                        continue
                self._pending_text += _mid(stripped_text, 0, self._escape_len)
                stripped_text = stripped_text[self._escape_len:]

                # Non draw related command (cursor/erase)
                if self._pending_text in [self._escape] + self._escape_alts:
                    m = self._supported_commands.match(stripped_text)
                    if m and self._pending_text == self._escape:
                        _, e = m.span()
                        n = m.group('n')
                        cmd = m.group('cmd')
                        if not n:
                            n = 0
                        ret_val.append(Operation(self._commands[cmd], n))
                        self._pending_text = ''
                        stripped_text = stripped_text[e:]
                        continue
                    else:
                        m = self._unsupported_command.match(stripped_text)
                        if m:
                            self._pending_text = ''
                            stripped_text = stripped_text[m.span()[1]:]
                            continue
                        elif self._pending_text in ['\x1b=', '\x1b>']:
                            self._pending_text = ''
                            continue

                # Handle Select Graphic Rendition commands
                # get the number
                str_nbr = ''
                numbers = []
                while stripped_text:
                    if stripped_text[0].isdigit():
                        str_nbr += stripped_text[0]
                    else:
                        if str_nbr:
                            numbers.append(str_nbr)
                        if not str_nbr or stripped_text[0] != self._semicolon:
                            break
                        str_nbr = ''
                    self._pending_text += _mid(stripped_text, 0, 1)
                    stripped_text = stripped_text[1:]

                if not stripped_text:
                    break

                # remove terminating char
                if not stripped_text.startswith(self._color_terminator):
                    # _logger().warn('removing %s', repr(self._pending_text + stripped_text[0]))
                    self._pending_text = ''
                    stripped_text = stripped_text[1:]
                    break

                # got consistent control sequence, ok to clear pending text
                self._pending_text = ''
                stripped_text = stripped_text[1:]

                if not numbers:
                    fmt = QtGui.QTextCharFormat(formatted_text.fmt)
                    self.end_format_scope()

                i_offset = 0
                n = len(numbers)
                for i in range(n):
                    i += i_offset
                    code = int(numbers[i])

                    if self._TextColorStart <= code <= self._TextColorEnd:
                        fmt.setForeground(_ansi_color(code - self._TextColorStart, self.color_scheme))
                        self._set_format_scope(fmt)
                    elif self._BackgroundColorStart <= code <= self._BackgroundColorEnd:
                        fmt.setBackground(_ansi_color(code - self._BackgroundColorStart, self.color_scheme))
                        self._set_format_scope(fmt)
                    else:
                        if code == self._ResetFormat:
                            fmt = QtGui.QTextCharFormat(formatted_text.fmt)
                            self.end_format_scope()
                        elif code == self._BoldText:
                            fmt.setFontWeight(QtGui.QFont.Bold)
                            self._set_format_scope(fmt)
                        elif code == self._NotBold:
                            fmt.setFontWeight(QtGui.QFont.Normal)
                            self._set_format_scope(fmt)
                        elif code == self._ItalicText:
                            fmt.setFontItalic(True)
                            self._set_format_scope(fmt)
                        elif code == self._NotItalicNotFraktur:
                            fmt.setFontItalic(False)
                            self._set_format_scope(fmt)
                        elif code == self._UnderlinedText:
                            fmt.setUnderlineStyle(fmt.SingleUnderline)
                            fmt.setUnderlineColor(fmt.foreground().color())
                            self._set_format_scope(fmt)
                        elif code == self._NotUnderlined:
                            fmt.setUnderlineStyle(fmt.NoUnderline)
                            self._set_format_scope(fmt)
                        elif code == self._DefaultTextColor:
                            fmt.setForeground(formatted_text.fmt.foreground())
                            self._set_format_scope(fmt)
                        elif code == self._DefaultBackgroundColor:
                            fmt.setBackground(formatted_text.fmt.background())
                            self._set_format_scope(fmt)
                        elif code == self._Dim:
                            fmt = QtGui.QTextCharFormat(fmt)
                            fmt.setForeground(fmt.foreground().color().darker(self.DIM_FACTOR))
                        elif code == self._Negative:
                            normal_fmt = fmt
                            fmt = QtGui.QTextCharFormat(fmt)
                            fmt.setForeground(normal_fmt.background())
                            fmt.setBackground(normal_fmt.foreground())
                        elif code == self._Positive:
                            fmt = QtGui.QTextCharFormat(formatted_text.fmt)
                        elif code in [self._RgbBackgroundColor, self._RgbTextColor]:
                            # See http://en.wikipedia.org/wiki/ANSI_escape_code#Colors
                            i += 1
                            if i == n:
                                break
                            next_code = int(numbers[i])
                            if next_code == 2:
                                # RGB set with format: 38;2;<r>;<g>;<b>
                                if i + 3 < n:
                                    method = fmt.setForeground if code == self._RgbTextColor else fmt.setBackground
                                    method(QtGui.QColor(int(numbers[i + 1]), int(numbers[i + 2]), int(numbers[i + 3])))
                                    self._set_format_scope(fmt)
                                i_offset = 3
                            elif next_code == 5:
                                # 256 color mode with format: 38;5;<i>
                                index = int(numbers[i + 1])
                                if index < 8:
                                    # The first 8 colors are standard low-intensity ANSI colors.
                                    color = _ansi_color(index, self.color_scheme)
                                elif index < 16:
                                    # The next 8 colors are standard high-intensity ANSI colors.
                                    color = _ansi_color(index - 8, self.color_scheme).lighter(150)
                                elif index < 232:
                                    # The next 216 colors are a 6x6x6 RGB cube.
                                    o = index - 16
                                    color = QtGui.QColor((o / 36) * 51, ((o / 6) % 6) * 51, (o % 6) * 51)
                                else:
                                    # The last 24 colors are a greyscale gradient.
                                    grey = (index - 232) * 11
                                    color = QtGui.QColor(grey, grey, grey)

                                if code == self._RgbTextColor:
                                    fmt.setForeground(color)
                                else:
                                    fmt.setBackground(color)

                                self._set_format_scope(fmt)
                        else:
                            _logger().warn('unsupported SGR code: %r', code)
        return ret_val

    def end_format_scope(self):
        """
        Close the format scope
        """
        self._prev_fmt_closed = True

    def _set_format_scope(self, fmt):
        """
        Opens the format scope.
        """
        self._prev_fmt = QtGui.QTextCharFormat(fmt)
        self._prev_fmt_closed = False


def _mid(string, start, end=None):
    """
    Returns a substring delimited by start and end position.
    """
    if end is None:
        end = len(string)
    return string[start:start + end]


def _ansi_color(code, theme):
    """
    Converts an ansi code to a QColor, taking the color scheme (theme) into account.
    """
    red = 170 if code & 1 else 0
    green = 170 if code & 2 else 0
    blue = 170 if code & 4 else 0
    color = QtGui.QColor(red, green, blue)
    if theme is not None:
        mappings = {
            '#aa0000': theme.red,
            '#00aa00': theme.green,
            '#aaaa00': theme.yellow,
            '#0000aa': theme.blue,
            '#aa00aa': theme.magenta,
            '#00aaaa': theme.cyan,
            '#000000': theme.background,
            "#ffffff": theme.foreground
        }
        try:
            return mappings[color.name()]
        except KeyError:
            pass
    return color


# ----------------------------------------------------------------------------------------------------------------------
# Input handlers
# ----------------------------------------------------------------------------------------------------------------------
class InputHandler(object):
    """
    Base class for handling user inputs
    """
    def __init__(self):
        # references set by the outout window instance that owns the handler.
        self.edit = None
        self.process = None


class ImmediateInputHandler(InputHandler):
    """
    Write ascii key code immediately to the process' stdin.
    """
    def key_press_event(self, event):
        """
        Directly writes the ascii code of the key to the process' stdin.

        :retuns: False to prevent the event from being propagated to the parent widget.
        """
        if event.key() == QtCore.Qt.Key_Return:
            cursor = self.edit.textCursor()
            cursor.movePosition(cursor.EndOfBlock)
            self.edit.setTextCursor(cursor)
        code = _qkey_to_ascii(event)
        if code:
            self.process.writeData(code)
            return False
        return True

    def paste(self, text):
        self.process.write(text.encode())


def _qkey_to_ascii(event):
    """
    (Try to) convert the Qt key event to the corresponding ASCII sequence for
    the terminal. This works fine for standard alphanumerical characters, but
    most other characters require terminal specific control_modifier sequences.
    The conversion below works for TERM="linux' terminals.
    """
    if sys.platform == 'darwin':
        control_modifier = QtCore.Qt.MetaModifier
    else:
        control_modifier = QtCore.Qt.ControlModifier
    ctrl = int(event.modifiers() & control_modifier) != 0
    if ctrl:
        if event.key() == QtCore.Qt.Key_P:
            return b'\x10'
        elif event.key() == QtCore.Qt.Key_N:
            return b'\x0E'
        elif event.key() == QtCore.Qt.Key_C:
            return b'\x03'
        elif event.key() == QtCore.Qt.Key_L:
            return b'\x0C'
        elif event.key() == QtCore.Qt.Key_B:
            return b'\x02'
        elif event.key() == QtCore.Qt.Key_F:
            return b'\x06'
        elif event.key() == QtCore.Qt.Key_D:
            return b'\x04'
        elif event.key() == QtCore.Qt.Key_O:
            return b'\x0F'
        elif event.key() == QtCore.Qt.Key_V:
            return QtWidgets.qApp.clipboard().text().encode('utf-8')
        else:
            return None
    else:
        if event.key() == QtCore.Qt.Key_Return:
            return '\n'.encode('utf-8')
        elif event.key() == QtCore.Qt.Key_Enter:
            return '\n'.encode('utf-8')
        elif event.key() == QtCore.Qt.Key_Tab:
            return '\t'.encode('utf-8')
        elif event.key() == QtCore.Qt.Key_Backspace:
            return b'\x08'
        elif event.key() == QtCore.Qt.Key_Delete:
            return b'\x06\x08'
        elif event.key() == QtCore.Qt.Key_Enter:
            return '\n'.encode('utf-8')
        elif event.key() == QtCore.Qt.Key_Home:
            return b'\x1b[H'
        elif event.key() == QtCore.Qt.Key_End:
            return b'\x1b[F'
        elif event.key() == QtCore.Qt.Key_Left:
            return b'\x02'
        elif event.key() == QtCore.Qt.Key_Up:
            return b'\x10'
        elif event.key() == QtCore.Qt.Key_Right:
            return b'\x06'
        elif event.key() == QtCore.Qt.Key_Down:
            return b'\x0E'
        elif event.key() == QtCore.Qt.Key_PageUp:
            return b'\x49'
        elif event.key() == QtCore.Qt.Key_PageDown:
            return b'\x51'
        elif event.key() == QtCore.Qt.Key_F1:
            return b'\x1b\x31'
        elif event.key() == QtCore.Qt.Key_F2:
            return b'\x1b\x32'
        elif event.key() == QtCore.Qt.Key_F3:
            return b'\x00\x3b'
        elif event.key() == QtCore.Qt.Key_F4:
            return b'\x1b\x34'
        elif event.key() == QtCore.Qt.Key_F5:
            return b'\x1b\x35'
        elif event.key() == QtCore.Qt.Key_F6:
            return b'\x1b\x36'
        elif event.key() == QtCore.Qt.Key_F7:
            return b'\x1b\x37'
        elif event.key() == QtCore.Qt.Key_F8:
            return b'\x1b\x38'
        elif event.key() == QtCore.Qt.Key_F9:
            return b'\x1b\x39'
        elif event.key() == QtCore.Qt.Key_F10:
            return b'\x1b\x30'
        elif event.key() == QtCore.Qt.Key_F11:
            return b'\x45'
        elif event.key() == QtCore.Qt.Key_F12:
            return b'\x46'
        elif event.text() in ('abcdefghijklmnopqrstuvwxyz'
                              'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
                              '[],=-.;/`&^~*@|#(){}$><%+?"_!'
                              "'\\ :"):
            return event.text().encode('utf8')
        else:
            return None


class CommandHistory(object):
    """
    A very basic history of commands.

    Use add_command when user press RETURN, use scroll_up/scroll_down to scroll the history.
    """
    def __init__(self):
        self._history = []
        self._index = -1

    def add_command(self, command):
        """
        Adds a command to the history and reset history index.
        """
        try:
            self._history.remove(command)
        except ValueError:
            pass
        self._history.insert(0, command)
        self._index = -1

    def scroll_up(self):
        """
        Returns the previous command, if any.
        """
        self._index += 1
        nb_commands = len(self._history)
        if self._index >= nb_commands:
            self._index = nb_commands - 1
        try:
            return self._history[self._index]
        except IndexError:
            return ''

    def scroll_down(self):
        """
        Returns the next command if any.
        """
        self._index -= 1
        if self._index < 0:
            self._index = -1
            return ''
        try:
            return self._history[self._index]
        except IndexError:
            return ''


class BufferedInputHandler(InputHandler):
    """
    Bufferise user inputs until user press RETURN.

    Use :class:`CommandHistory` to manage the history of commands/inputs.
    """
    def __init__(self):
        super(BufferedInputHandler, self).__init__()
        self._history = CommandHistory()

    def _insert_command(self, command):
        """
        Insert command by replacing the current input buffer and display it on the text edit.
        """
        self._clear_user_buffer()
        tc = self.edit.textCursor()
        tc.insertText(command)
        self.edit.setTextCursor(tc)

    def _clear_user_buffer(self):
        tc = self.edit.textCursor()
        for _ in self._get_input_buffer():
            tc.deletePreviousChar()
        self.edit.setTextCursor(tc)

    def is_code_completion_popup_visible(self):
        try:
            mode = self.edit.modes.get('CodeCompletionMode')
        except KeyError:
            pass
        else:
            return mode._completer.popup().isVisible()

    def key_press_event(self, event):
        """
        Manages our own buffer and send it to the subprocess when user pressed RETURN.
        """
        input_buffer = self._get_input_buffer()
        ctrl = int(event.modifiers() & QtCore.Qt.ControlModifier) != 0
        shift = int(event.modifiers() & QtCore.Qt.ShiftModifier) != 0
        delete = event.key() in [QtCore.Qt.Key_Backspace, QtCore.Qt.Key_Delete]
        ignore = False
        if delete and not input_buffer and not shift:
            return False
        if ctrl:
            if shift and event.key() == QtCore.Qt.Key_V:
                self.edit.insertPlainText(QtWidgets.qApp.clipboard().text())
                return False
            elif event.key() == QtCore.Qt.Key_L:
                self.edit.clear()
                if sys.platform == 'win32':
                    self.process.write(b'\r')
                self.process.write(b'\n')
                return False
        if (shift or ctrl) and event.key() == QtCore.Qt.Key_Backspace:
            if input_buffer.strip() != '':
                return True
            self._clear_user_buffer()
            return False
        if event.key() == QtCore.Qt.Key_Up:
            if self.is_code_completion_popup_visible():
                return True
            self._insert_command(self._history.scroll_up())
            return False
        if event.key() == QtCore.Qt.Key_Left:
            return bool(input_buffer)
        if event.key() == QtCore.Qt.Key_Down:
            if self.is_code_completion_popup_visible():
                return True
            self._insert_command(self._history.scroll_down())
            return False
        if event.key() == QtCore.Qt.Key_Home:
            tc = self.edit.textCursor()
            tc.movePosition(tc.StartOfBlock)
            tc.movePosition(tc.Right, tc.MoveAnchor, self.edit._formatter._prefix_len)
            self.edit.setTextCursor(tc)
            return False
        if event.key() == QtCore.Qt.Key_End:
            tc = self.edit.textCursor()
            tc.movePosition(tc.EndOfBlock)
            self.edit.setTextCursor(tc)
            self._cursor_pos = len(self._get_input_buffer())
            return False
        if event.key() in [QtCore.Qt.Key_Return, QtCore.Qt.Key_Enter]:
            if self.is_code_completion_popup_visible():
                return True
            tc = self.edit.textCursor()
            tc.movePosition(tc.EndOfBlock)
            self.edit.setTextCursor(tc)
            # send the user input to the child process
            if self.edit.flg_use_pty or 'cmd.exe' in self.process.program():
                # remove user buffer from text edit, the content of the buffer will be
                # drawn as soon as we write it to the process stdin
                tc = self.edit.textCursor()
                for _ in input_buffer:
                    tc.deletePreviousChar()
                self.edit.setTextCursor(tc)
            self._history.add_command(input_buffer)
            if sys.platform == 'win32':
                input_buffer += "\r"
            input_buffer += "\n"
            self.process.write(input_buffer.encode())
            if self.edit.flg_use_pty or 'cmd.exe' in self.process.program():
                ignore = True
        return not ignore

    def _get_input_buffer(self):
        current_line = TextHelper(self.edit).current_line_text()
        return current_line[self.edit._formatter._prefix_len:]


# ----------------------------------------------------------------------------------------------------------------------
# Formatter
# ----------------------------------------------------------------------------------------------------------------------
class OutputFormat:
    """
    Enumerates the possible output formats.
    """
    #: format used to display normal messages
    NormalMessageFormat = 0
    #: format used to display normal messages
    ErrorMessageFormat = 1
    #: format used to display custom messages
    CustomFormat = 2


class OutputFormatter(object):
    """
    Perform formatting (draw text, move cursor,...).
    """
    @property
    def color_scheme(self):
        """
        Gets/Sets the formatter color scheme
        """
        return self._color_scheme

    @color_scheme.setter
    def color_scheme(self, new_theme):
        self._color_scheme = new_theme
        self._init_formats()
        self._parser.color_scheme = new_theme

    def __init__(self, text_edit, color_scheme=None):
        if text_edit is None:
            raise ValueError('text_edit parameter cannot be None')
        self._last_cursor_pos = 0
        self._text_edit = text_edit
        self._parser = AnsiEscapeCodeParser()
        self._cursor = text_edit.textCursor()
        self._formats = {}
        self._overwrite_output = False
        _init_default_scheme()
        self._color_scheme = color_scheme
        if self._color_scheme is None:
            self._color_scheme = OutputWindow.DefaultColorScheme
        self._init_formats()
        self._parser.color_scheme = self._color_scheme
        self.flg_bash = False

    def append_message(self, text, output_format=OutputFormat.NormalMessageFormat):
        """
        Parses and append message to the text edit.
        """
        self._append_message(text, self._formats[output_format])

    def flush(self):
        """
        Flush intermediary resutls: close the format scope.
        """
        self._parser.end_format_scope()

    # Utility methods
    def _append_message(self, text, char_format):
        """
        Parses text and executes parsed operations.
        """
        self._cursor = self._text_edit.textCursor()
        operations = self._parser.parse_text(FormattedText(text, char_format))
        for i, operation in enumerate(operations):
            try:
                func = getattr(self, '_%s' % operation.command)
            except AttributeError:
                print('command not implemented: %r - %r' % (
                    operation.command, operation.data))
            else:
                try:
                    func(operation.data)
                except Exception:
                    _logger().exception('exception while running %r', operation)
                    # uncomment next line for debugging commands
                    self._text_edit.repaint()

    def _init_formats(self):
        """
        Initialise default formats.
        """
        theme = self._color_scheme
        # normal message format
        fmt = QtGui.QTextCharFormat()
        fmt.setForeground(theme.foreground)
        fmt.setBackground(theme.background)
        self._formats[OutputFormat.NormalMessageFormat] = fmt

        # error message
        fmt = QtGui.QTextCharFormat()
        fmt.setForeground(theme.error)
        fmt.setBackground(theme.background)
        self._formats[OutputFormat.ErrorMessageFormat] = fmt

        # debug message
        fmt = QtGui.QTextCharFormat()
        fmt.setForeground(theme.custom)
        fmt.setBackground(theme.background)
        self._formats[OutputFormat.CustomFormat] = fmt

    # Commands implementation
    def _draw(self, data):
        """
        Draw text
        """
        self._cursor.clearSelection()
        self._cursor.setPosition(self._last_cursor_pos)

        if '\x07' in data.txt:
            print('\a')
        txt = data.txt.replace('\x07', '')

        if '\x08' in txt:
            parts = txt.split('\x08')
        else:
            parts = [txt]

        for i, part in enumerate(parts):
            if part:
                part = part.replace('\r\r', '\r')
                if len(part) >= 80 * 24 * 8:
                    # big output, process it in one step (\r and \n will not be handled)
                    self._draw_chars(data, part)
                    continue
                to_draw = ''
                for n, char in enumerate(part):
                    if char == '\n':
                        self._draw_chars(data, to_draw)
                        to_draw = ''
                        self._linefeed()
                    elif char == '\r':
                        self._draw_chars(data, to_draw)
                        to_draw = ''
                        self._erase_in_line(0)
                        try:
                            nchar = part[n + 1]
                        except IndexError:
                            nchar = None
                        if self._cursor.positionInBlock() > 80 and self.flg_bash and nchar != '\n':
                            self._linefeed()
                        self._cursor.movePosition(self._cursor.StartOfBlock)
                        self._text_edit.setTextCursor(self._cursor)
                    else:
                        to_draw += char
                if to_draw:
                    self._draw_chars(data, to_draw)
            if i != len(parts) - 1:
                self._cursor_back(1)
        self._last_cursor_pos = self._cursor.position()
        self._prefix_len = self._cursor.positionInBlock()
        self._text_edit.setTextCursor(self._cursor)

    def _draw_chars(self, data, to_draw):
        """
        Draw the specified charachters using the specified format.
        """
        i = 0
        while not self._cursor.atBlockEnd() and i < len(to_draw) and len(to_draw) > 1:
            self._cursor.deleteChar()
            i += 1
        self._cursor.insertText(to_draw, data.fmt)

    def _linefeed(self):
        """
        Performs a line feed.
        """
        last_line = self._cursor.blockNumber() == self._text_edit.blockCount() - 1
        if self._cursor.atEnd() or last_line:
            if last_line:
                self._cursor.movePosition(self._cursor.EndOfBlock)
            self._cursor.insertText('\n')
        else:
            self._cursor.movePosition(self._cursor.Down)
            self._cursor.movePosition(self._cursor.StartOfBlock)
        self._text_edit.setTextCursor(self._cursor)

    def _cursor_down(self, value):
        """
        Moves the cursor down by ``value``.
        """
        self._cursor.clearSelection()
        if self._cursor.atEnd():
            self._cursor.insertText('\n')
        else:
            self._cursor.movePosition(self._cursor.Down, self._cursor.MoveAnchor, value)
        self._last_cursor_pos = self._cursor.position()

    def _cursor_up(self, value):
        """
        Moves the cursor up by ``value``.
        """
        value = int(value)
        if value == 0:
            value = 1
        self._cursor.clearSelection()
        self._cursor.movePosition(self._cursor.Up, self._cursor.MoveAnchor, value)
        self._last_cursor_pos = self._cursor.position()

    def _cursor_position(self, data):
        """
        Moves the cursor position.
        """
        column, line = self._get_line_and_col(data)
        self._move_cursor_to_line(line)
        self._move_cursor_to_column(column)
        self._last_cursor_pos = self._cursor.position()

    def _move_cursor_to_column(self, column):
        """
        Moves the cursor to the specified column, if possible.
        """
        last_col = len(self._cursor.block().text())
        self._cursor.movePosition(self._cursor.EndOfBlock)
        to_insert = ''
        for i in range(column - last_col):
            to_insert += ' '
        if to_insert:
            self._cursor.insertText(to_insert)
        self._cursor.movePosition(self._cursor.StartOfBlock)
        self._cursor.movePosition(self._cursor.Right, self._cursor.MoveAnchor, column)
        self._last_cursor_pos = self._cursor.position()

    def _move_cursor_to_line(self, line):
        """
        Moves the cursor to the specified line, if possible.
        """
        last_line = self._text_edit.document().blockCount() - 1
        self._cursor.clearSelection()
        self._cursor.movePosition(self._cursor.End)
        to_insert = ''
        for i in range(line - last_line):
            to_insert += '\n'
        if to_insert:
            self._cursor.insertText(to_insert)
        self._cursor.movePosition(self._cursor.Start)
        self._cursor.movePosition(self._cursor.Down, self._cursor.MoveAnchor, line)
        self._last_cursor_pos = self._cursor.position()

    def _cursor_horizontal_absolute(self, column):
        """
        Moves the cursor to the specified column, if possible.
        """
        self._move_cursor_to_column(int(column) - 1)

    @staticmethod
    def _get_line_and_col(data):
        """
        Gets line and column from a string like the following: "1;5" or "1;" or ";5"

        and convers the column/line numbers to 0 base.
        """
        try:
            line, column = data.split(';')
        except AttributeError:
            line = int(data)
            column = 1
        # handle empty values and convert them to 0 based indices
        if not line:
            line = 0
        else:
            line = int(line) - 1
            if line < 0:
                line = 0
        if not column:
            column = 0
        else:
            column = int(column) - 1
            if column < 0:
                column = 0
        return column, line

    def _erase_in_line(self, value):
        """
        Erases charachters in line.
        """
        initial_pos = self._cursor.position()
        if value == 0:
            # delete end of line
            self._cursor.movePosition(self._cursor.EndOfBlock, self._cursor.KeepAnchor)
        elif value == 1:
            # delete start of line
            self._cursor.movePosition(self._cursor.StartOfBlock, self._cursor.KeepAnchor)
        else:
            # delete whole line
            self._cursor.movePosition(self._cursor.StartOfBlock)
            self._cursor.movePosition(self._cursor.EndOfBlock, self._cursor.KeepAnchor)
        self._cursor.insertText(' ' * len(self._cursor.selectedText()))
        self._cursor.setPosition(initial_pos)
        self._text_edit.setTextCursor(self._cursor)
        self._last_cursor_pos = self._cursor.position()

    def _erase_display(self, value):
        """
        Erases display.
        """
        if value == 0:
            # delete end of line
            self._cursor.movePosition(self._cursor.End, self._cursor.KeepAnchor)
        elif value == 1:
            # delete start of line
            self._cursor.movePosition(self._cursor.Start, self._cursor.KeepAnchor)
        else:
            # delete whole line
            self._cursor.movePosition(self._cursor.Start)
            self._cursor.movePosition(self._cursor.End, self._cursor.KeepAnchor)
        self._cursor.removeSelectedText()
        self._last_cursor_pos = self._cursor.position()

    def _cursor_back(self, value):
        """
        Moves the cursor back.
        """
        if value <= 0:
            value = 1
        self._cursor.movePosition(self._cursor.Left, self._cursor.MoveAnchor, value)
        self._text_edit.setTextCursor(self._cursor)
        self._last_cursor_pos = self._cursor.position()

    def _cursor_forward(self, value):
        """
        Moves the cursor forward.
        """
        if value <= 0:
            value = 1
        self._cursor.movePosition(self._cursor.Right, self._cursor.MoveAnchor, value)
        self._text_edit.setTextCursor(self._cursor)
        self._last_cursor_pos = self._cursor.position()

    def _delete_chars(self, value):
        """
        Deletes the specified number of charachters.
        """
        value = int(value)
        if value <= 0:
            value = 1
        for i in range(value):
            self._cursor.deleteChar()
        self._text_edit.setTextCursor(self._cursor)
        self._last_cursor_pos = self._cursor.position()


# ----------------------------------------------------------------------------------------------------------------------
# Color schemes definition
# ----------------------------------------------------------------------------------------------------------------------
def _init_default_scheme():
    """
    Initialises the default color scheme with colors based on QPalette.

    Call this function once after QApplication has been created (otherwise QPalette cannot be used).
    """
    if OutputWindow.DefaultColorScheme is None:
        OutputWindow.DefaultColorScheme = OutputWindow.create_color_scheme()

# Initialize non-palette dependant themes.
OutputWindow.LinuxColorScheme = OutputWindow.create_color_scheme(
    background=QColor('black'), foreground=QColor('white'), red=QColor('#FF5555'), green=QColor('#55FF55'),
    yellow=QColor('#FFFF55'), blue=QColor('#5555FF'), magenta=QColor('#FF55FF'), cyan=QColor('#55FFFF'))
#: Tango theme (black background with pastel colors).
OutputWindow.TangoColorScheme = OutputWindow.create_color_scheme(
    background=QColor('black'), foreground=QColor('white'), red=QColor('#CC0000'), green=QColor('#4E9A06'),
    yellow=QColor('#C4A000'), blue=QColor('#3465A4'), magenta=QColor('#75507B'), cyan=QColor('#06989A'))
#: The famous Solarized dark theme.
OutputWindow.SolarizedColorScheme = OutputWindow.create_color_scheme(
    background=QColor('#073642'), foreground=QColor('#EEE8D5'), red=QColor('#DC322F'), green=QColor('#859900'),
    yellow=QColor('#B58900'), blue=QColor('#268BD2'), magenta=QColor('#D33682'), cyan=QColor('#2AA198'))


def _logger():
    """
    Returns a logger instance for this module.
    """
    return logging.getLogger(__name__)
