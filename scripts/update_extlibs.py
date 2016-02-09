"""
This script updates OCIDE's extlibs (the pure python dependencies are now
bundled into the open_cobol_ide package as package data,
../open_cobol_ide/extlibs).

Here is the list of bundled packages:

  - pyqode.qt
  - pyqode.core
  - pyqode.cobol
  - pygments
  - future
  - qdarkstyle
  - githubpy
  - keyring

Those package must be installed in the development environment. You can install
them all by running ``sudo pip3 -r requirements.txt``.

"""
import os
import shutil

# packages to embed
import pyqode.qt
import pyqode.core
import pyqode.cobol
import pygments
import future
import qdarkstyle
import keyring
import enum
import qcrash


BUILD = os.path.abspath('../open_cobol_ide/extlibs')

README = '''This directory contains the pure python dependencies of OCIDE.

You can update the dependencies by running ``update_extlibs.py`` (located in
the ``scripts`` folder)
'''


def copy_tree(src, dest):
    try:
        shutil.copytree(src, dest)
    except OSError as e:
        print(e)


def embed_packages(packages):
    for package in packages:
        if package.__file__.endswith('__init__.py'):
            # package
            src = os.path.dirname(package.__file__)
            _, dirname = os.path.split(os.path.dirname(package.__file__))
            dest = os.path.join(BUILD, dirname)
            if 'pyqode' in package.__file__:
                dest = os.path.join(BUILD, 'pyqode', dirname)
            print('copying %s to %s' % (src, dest))
            copy_tree(src, dest)
            if 'pyqode' in package.__file__:
                with open(os.path.join(
                        BUILD, 'pyqode', '__init__.py'), 'w'):
                    pass
        else:
            # single module package, copy it directly
            src = package.__file__
            dest = BUILD
            print('copying %s to %s' % (src, dest))
            shutil.copy(src, dest)

    with open(os.path.join(BUILD, 'readme.rst'), 'w') as f:
        f.write(README)


try:
    os.mkdir(BUILD)
except FileExistsError:
    shutil.rmtree(BUILD)
    os.mkdir(BUILD)
finally:
    embed_packages([future, pygments, qdarkstyle, keyring, pyqode.qt,
                    pyqode.core, pyqode.cobol, enum, qcrash])
