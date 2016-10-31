import os
import sys

from pyqode.core.backend import server
from pyqode.core import modes

from . import output_window


class Terminal(output_window.OutputWindow):
    """
    Simple (rudimentary) terminal widget.

    It will run cmd.exe on Windows and bash on GNU/Linux.

    Please note that this widget does not support all VT100 features, only the most basic one. The goal is to have
    a small widget for running commands in a PyQt application, it does not aim to be a perfect emulator,
    just a quick one.
    """
    def __init__(self, parent=None, color_scheme=None, backend=server.__file__):
        if sys.platform == 'win32':
            input_handler = output_window.BufferedInputHandler()
            program = 'cmd.exe'
            args = []
            use_pty = False
            flg_bash = False
        else:
            program = 'bash'
            args = ['-l']
            input_handler = output_window.ImmediateInputHandler()
            use_pty = True
            flg_bash = True
        working_dir = os.path.expanduser('~')
        super(Terminal, self).__init__(parent=parent, color_scheme=color_scheme, input_handler=input_handler,
                                       backend=backend)
        self._formatter.flg_bash = flg_bash
        self.start_process(program, arguments=args, print_command=False, use_pseudo_terminal=use_pty,
                           working_dir=working_dir)

    def _init_code_edit(self, backend):
        self.modes.append(modes.SymbolMatcherMode())
        self.modes.append(modes.IndenterMode())
        super(Terminal, self)._init_code_edit(backend)
        try:
            self.panels.remove('ReadOnlyPanel')
        except KeyError:
            pass

    def change_directory(self, directory):
        """
        Changes the current directory.

        Change is made by running a "cd" command followed by a "clear" command.
        :param directory:
        :return:
        """
        self._process.write(('cd %s\n' % directory).encode())
        if sys.platform == 'win32':
            self._process.write((os.path.splitdrive(directory)[0] + '\r\n').encode())
            self.clear()
        else:
            self._process.write(b'\x0C')

    def terminate_process(self):
        self._process.write(b'\x04')
        self._process.waitForBytesWritten()
