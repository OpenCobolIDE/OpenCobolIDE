import logging
import os
import re
import sys
import subprocess


def _logger():
    return logging.getLogger(__name__)


def get_cobc_version():
    """ Returns the OpenCobol compiler version as a string """
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


def check_env():
    """
    Checks environment and sees if the OpenCobol compiler can be found
    :return:
    """
    version = get_cobc_version()
    if version == 'Not installed':
        if sys.platform == 'win32':
            expected_root_path = os.path.join(os.getcwd(), 'OpenCobol')
            expected_cobc_path = os.path.join(
                expected_root_path, 'bin', 'cobc.exe')
            if not os.path.exists(expected_root_path):
                _logger().warning('%s does not exists' % expected_root_path)
            elif not os.path.exists(expected_cobc_path):
                _logger().warning('%s does not exisits' % expected_cobc_path)
            else:
                _logger().info('cobc.exe found but not usable.')
        return False
    _logger().info('OpenCobol compiler v.%s' % version)
    return True


def is_working():
    """
    Checks if the GNU Cobol compiler is working.
    """
    return check_env()