from enum import IntEnum
import logging
import os
import re
import subprocess
import sys
from .. import system


def _logger():
    return logging.getLogger(__name__)


class GnuCobolStandard(IntEnum):
    default = 0
    cobol2002 = 1
    cobol85 = 2
    ibm = 3
    mvs = 4
    bs2000 = 5
    mf = 6


class GnuCobolCompiler:
    """
    Provides an interface to the GnuCobol compiler (cobc)
    """

    def __init__(self):
        self.extensions = [
            # no extension for exe on linux and mac
            '.exe' if system.windows else '',
            # dll on windows, so everywhere else
            '.dll' if system.windows else '.so'
        ]

    @staticmethod
    def get_version():
        """
        Returns the GnuCobol compiler version as a string
        """
        cmd = ['cobc', '--version']
        try:
            _logger().debug('getting cobc version: %s' % ' '.join(cmd))
            if sys.platform == 'win32':
                startupinfo = subprocess.STARTUPINFO()
                startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
                p = subprocess.Popen(cmd, shell=False, startupinfo=startupinfo,
                                     stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                     stdin=subprocess.PIPE)
            else:
                p = subprocess.Popen(cmd, shell=False, stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE)
        except OSError:
            _logger().exception('OpenCobol compiler not found')
            return 'Not installed'
        else:
            stdout, stderr = p.communicate()
            stdout = str(stdout)
            lversion = stdout.splitlines()[0]
            _logger().debug('parsing version line: %s' % lversion)
            prog = re.compile(r'\d.\d.\d')
            for v in prog.finditer(lversion):
                s, e = v.span()
                return lversion[s: e]

    def is_working(self):
        """
        Checks if the GNU Cobol compiler is working.
        """
        version = self.get_version()
        if version == 'Not installed':
            if sys.platform == 'win32':
                expected_root_path = os.path.join(os.getcwd(), 'OpenCobol')
                expected_cobc_path = os.path.join(
                    expected_root_path, 'bin', 'cobc.exe')
                if not os.path.exists(expected_root_path):
                    _logger().warning('%s does not exists' % expected_root_path)
                elif not os.path.exists(expected_cobc_path):
                    _logger().warning('%s does not exists' % expected_cobc_path)
                else:
                    _logger().info('cobc.exe found but not usable.')
            return False
        _logger().info('OpenCobol compiler v.%s' % version)
        return True

    def extension_for_type(self, file_type):
        return self.extensions[int(file_type)]

    def get_command(self, file_name, file_type):
        pass

    def parse_output(self, compiler_output):
        pass
