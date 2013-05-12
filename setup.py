#!/usr/bin/env python2
# -*- coding: utf-8 -*-
#
# PCEF - PySide Code Editing framework
# Copyright 2013, Colin Duquesnoy <colin.duquesnoy@gmail.com>
#
# This software is released under the LGPLv3 license.
# You should have received a copy of the GNU Lesser General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#
"""
PCEF is code editor framework for PySide applications

This is the setup script, install it as any python package.

.. note:: You will need to install PySide and OpenCobol on your own
"""
from setuptools import setup, find_packages

# properly get pcef version
execfile('cobcide/__init__.py')

# get long description
with open('README.rst', 'r') as readme:
    long_desc = readme.read()


setup(
    name='OpenCobolIDE',
    version=__version__,
    packages=find_packages(),
    keywords=["Cobol IDE"],
    package_data={'cobcide.ui': ['*.ui', 'rc/*']},
    package_dir={'cobcide': 'cobcide'},
    url='https://launchpad.net/cobcide',
    license='GPL v3',
    author='Colin Duquesnoy',
    author_email='colin.duquesnoy@gmail.com',
    description='A simple cobol IDE',
    long_description=long_desc,
    install_requires=['pygments>=1.6', 'pcef>=0.2.1', 'chardet', 'qwelcomewindow'],
    entry_points={'gui_scripts': ['OpenCobolIDE = cobcide.main:main']}
)
