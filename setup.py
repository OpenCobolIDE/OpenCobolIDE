#!/usr/bin/env python3
"""
Setup script for OpenCobolIDE

You will need to install pyqode.core.qt (or PyQt5) and OpenCobol on your own.

"""
import os
import sys
from setuptools import setup, find_packages
try:
    from pyqt_distutils.build_ui import build_ui
    cmdclass = {'build_ui': build_ui}
except ImportError:
    build_ui = None
    cmdclass = {}


def read_version():
    """
    Reads the version without self importing
    """
    with open("oci/__init__.py") as f:
        lines = f.read().splitlines()
        for l in lines:
            if "__version__" in l:
                return l.split("=")[1].strip().replace('"', "")


def run_as_root():
    return os.getuid() == 0


# get long description
with open('README.rst', 'r') as readme:
    long_desc = readme.read()


# install requirements
requirements = ['pygments>=1.6', 'pyqode.core>=2.0.0-beta1',
                'qdarkstyle>=1.9', 'chardet']
if int('%s%s' % sys.version_info[:2]) < 34:
    # python < 3.4 needs enum backport package
    requirements.append('enum34')


data_files = []
if sys.platform == "linux" and run_as_root():
    data_files.append(('/usr/share/applications',
                       ['share/open-cobol-ide.desktop']))
    data_files.append(('/usr/share/pixmaps', ['share/OpenCobolIDE.png']))


setup(
    name='OpenCobolIDE',
    version=read_version(),
    packages=[p for p in find_packages() if not 'test' in p],
    keywords=["Cobol; OpenCobol; IDE"],
    package_dir={'oci': 'oci', "oci_designer_plugins": "oci_designer_plugins"},
    data_files=data_files,
    url='https://github.com/OpenCobolIDE/OpenCobolIDE',
    license='GPL v3',
    author='Colin Duquesnoy',
    author_email='colin.duquesnoy@gmail.com',
    description='A simple cobol IDE',
    long_description=long_desc,
    zip_safe=False,
    install_requires=requirements,
    entry_points={'gui_scripts': ['open-cobol-ide = oci.main:main'],
                  'pyqode_plugins': [
                      'oci_widgets = oci_designer_plugins.cobol_plugin']},
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
