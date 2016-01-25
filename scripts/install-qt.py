"""
Simple script to install PyQt or PySide in CI (Travis and AppVeyor).
"""
from __future__ import print_function
import os
import sys
import subprocess


def apt_get_install(packages):
    print('Installing %s...' % ', '.join(packages))
    subprocess.check_call(['sudo', 'apt-get', 'install', '-y', '-qq'] +
                          packages + ['--fix-missing'])

py3k = sys.version_info[0] == 3
pyqt_version = {'pyqt4': 4, 'pyqt5': 5}
pyqt_ver = pyqt_version[os.environ['QT_API']]
if py3k:
    pkg = 'python3-pyqt%s' % pyqt_ver
else:
    pkg = 'python-qt%s' % pyqt_ver
apt_get_install([pkg])
