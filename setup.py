#!/usr/bin/env python3
"""
Setup script for OpenCobolIDE

You will need to install PyQt4 (or PyQt5) and GnuCOBOL on your own.

"""
import sys
from setuptools import setup, find_packages
from setuptools.command.test import test as TestCommand
from open_cobol_ide import __version__

try:
    from pyqt_distutils.build_ui import build_ui
    cmdclass = {'build_ui': build_ui}
except ImportError:
    build_ui = None
    cmdclass = {}


class PyTest(TestCommand):
    user_options = [('pytest-args=', 'a', "Arguments to pass to py.test")]

    def initialize_options(self):
        TestCommand.initialize_options(self)
        self.pytest_args = []

    def run_tests(self):
        # import here, cause outside the eggs aren't loaded
        import pytest
        if self.pytest_args:
            self.pytest_args = self.pytest_args.replace('"', '').split(' ')
        else:
            self.pytest_args = []
        print('running test command: py.test "%s"' % ' '.join(
            self.pytest_args))
        errno = pytest.main(self.pytest_args)
        sys.exit(errno)

cmdclass['test'] = PyTest


DESCRIPTION = 'A simple COBOL IDE'

# get long description
with open('README.rst', 'r') as readme:
    if 'bdist_deb' in sys.argv or 'sdist_dsc' in sys.argv or 'bdist_rpm' in sys.argv:
        LONG_DESC = DESCRIPTION + ' based on GnuCOBOL and PyQode'
    else:
        LONG_DESC = readme.read()


data_files = []
if sys.platform == 'linux':
    data_files.append(('/usr/share/applications',
                       ['share/OpenCobolIDE.desktop']))
    data_files.append(('/usr/share/pixmaps', ['share/OpenCobolIDE.png']))


if 'bdist_wheel' in sys.argv:
    raise RuntimeError("This setup.py does not support wheels")


setup(
    name='OpenCobolIDE',
    version=__version__,
    keywords=['Cobol; OpenCobol; IDE'],
    url='https://github.com/OpenCobolIDE/OpenCobolIDE',
    license='GPL v3',
    author='Colin Duquesnoy',
    author_email='colin.duquesnoy@gmail.com',
    description=DESCRIPTION,
    long_description=LONG_DESC,
    packages=[p for p in find_packages() if 'test' not in p],
    data_files=data_files,
    include_package_data=True,
    entry_points={'gui_scripts': ['opencobolide = open_cobol_ide.main:main'],
                  'console_scripts':
                  ['opencobolide-console = open_cobol_ide.main:main']
                  if sys.platform == 'win32' else []},
    cmdclass=cmdclass,
    zip_safe=False,
    tests_require=['pytest-cov', 'pytest-pep8', 'pytest'],
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Environment :: X11 Applications :: Qt',
        'Environment :: Win32 (MS Windows)',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GNU General Public License v3 or later '
        '(GPLv3+)',
        'Operating System :: Microsoft :: Windows',
        'Operating System :: POSIX :: Linux',
        'Programming Language :: Python :: 3.2',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Topic :: Text Editors :: Integrated Development Environments (IDE)']
)
