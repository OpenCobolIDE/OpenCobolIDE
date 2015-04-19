#!/usr/bin/env python3
"""
Setup script for OpenCobolIDE

You will need to install PyQt4 (or PyQt5) and OpenCobol on your own.

"""
import sys
from setuptools import setup, find_packages
from open_cobol_ide import __version__

try:
    from pyqt_distutils.build_ui import build_ui
    cmdclass = {'build_ui': build_ui}
except ImportError:
    build_ui = None
    cmdclass = {}


DESCRIPTION = 'A simple COBOL IDE'

# get long description
with open('README.rst', 'r') as readme:
    if 'bdist_deb' in sys.argv or 'sdist_dsc' in sys.argv:
        LONG_DESC = DESCRIPTION + ' based on GnuCobol and PyQode'
    else:
        LONG_DESC = readme.read()


# install requirements
requirements = [
    'pygments>=1.6',
    'pyqode.core>=2.5.0',
    'pyqode.cobol>=2.5.0',
    'pyqode.qt>=2.5.0',
    'qdarkstyle>=1.11'
]


if int('%s%s' % sys.version_info[:2]) < 34:
    # python < 3.4 needs enum backport package
    requirements.append('enum34')


data_files = []
if sys.platform == 'linux':
    data_files.append(('/usr/share/applications',
                       ['share/OpenCobolIDE.desktop']))
    data_files.append(('/usr/share/pixmaps', ['share/OpenCobolIDE.png']))


setup(
    name='OpenCobolIDE',
    version=__version__,
    packages=[p for p in find_packages() if 'test' not in p],
    keywords=['Cobol; OpenCobol; IDE'],
    data_files=data_files,
    url='https://github.com/OpenCobolIDE/OpenCobolIDE',
    license='GPL v3',
    author='Colin Duquesnoy',
    author_email='colin.duquesnoy@gmail.com',
    description=DESCRIPTION,
    long_description=LONG_DESC,
    install_requires=requirements,
    entry_points={'gui_scripts': ['OpenCobolIDE = open_cobol_ide.main:main']},
    cmdclass=cmdclass,
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
