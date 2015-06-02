OpenCobolIDE
------------

.. image:: https://img.shields.io/pypi/v/OpenCobolIDE.svg
   :target: https://pypi.python.org/pypi/OpenCobolIDE/
   :alt: Latest PyPI version

.. image:: https://img.shields.io/pypi/dm/OpenCobolIDE.svg
   :target: https://pypi.python.org/pypi/OpenCobolIDE/
   :alt: Number of PyPI downloads

.. image:: https://img.shields.io/pypi/l/OpenCobolIDE.svg

.. image:: https://travis-ci.org/OpenCobolIDE/OpenCobolIDE.svg?branch=master
   :target: https://travis-ci.org/OpenCobolIDE/OpenCobolIDE
   :alt: Travis-CI build status

.. image:: https://coveralls.io/repos/OpenCobolIDE/OpenCobolIDE/badge.svg?branch=master
   :target: https://coveralls.io/r/OpenCobolIDE/OpenCobolIDE?branch=master
   :alt: Coverage Status


OpenCobolIDE is a simple and lightweight cobol IDE based on the GnuCobol
compiler.


The software is written in Python using `PyQt5`_ (or `PyQt4`_) and `pyQode`_

Features:
---------

- native (fast) cobol syntax highlighter
- cobol code completion
- cobol code folding
- navigable tree view of division, sections, paragraphs etc (fully synchronised
  with the code folding panel)
- smart auto indentation
- tool for computing PIC fields offsets
- compile as a program (.exe) or as a subprogram (.so/.dll)
- run the program from the editor or from a configurable external terminal (
  necessary if you are using the SCREEN section).
- dark color schemes and theme
- cross platform: works on **GNU/Linux**, **Windows** and **Mac OSX**
- dbpre integration on Linux


License
-------

OpenCobolIDE is released under the **GPL** version 3


Requirements
------------

- `GnuCobol`_
- `Python3`_
- `PyQt4`_ or `PyQt5`_
- `setuptools`_
- `pyqode.qt`_
- `pyqode.core`_
- `pyqode.cobol`_
- `Pygments`_
- `qdarkstyle`_


Installation
------------

GNU/Linux
#########

*Note: starting from v4.6.2, the installed executable name is lowercase: opencobolide*

Ubuntu
++++++

OpenCobolIDE is available from a ppa (for Ubuntu >= 14.04)::

    sudo apt-add-repository ppa:colin-duquesnoy/stable
    sudo apt-get update
    sudo apt-get install python3-opencobolide

ArchLinux
+++++++++

OpenCobolIDE is available from the AUR::

    yaourt -S open-cobol-ide

Other distributions:
++++++++++++++++++++

Install pyqt5, open-cobol and pip3 using your package manager, then run the following commands::

    sudo pip3 install OpenCobolIDE --upgrade


Note that if you have both PyQt5 and PyQt4 on your system, the IDE will use
PyQt5 by default. To force the use of PyQt4, you should set the
``QT_API`` environment variable to ``pyqt4``.


Windows
#######

There is a windows installer available here: https://launchpad.net/cobcide/+download

Mac OSX
#######

There is a dmg image available here: https://launchpad.net/cobcide/+download

Before running the app, you first have to install the GnuCobol compiler, e.g.
using homebrew::

    brew install open-cobol


If you installed the compiler in a non-standard path and it is not recognized
by the IDE, you can specify the path to the compiler in the preferences
dialog under the ``Build & Run`` section (make sure to only specify the
directory where the compiler can be found, not the full path).


Resources
---------

-  `Downloads`_
-  `Source repository`_
-  `Issue tracker`_
-  `Documentation`_


Screenshots
-----------

* Home page:

.. image:: doc/source/_static/Home.png
    :align: center

* Editor:

.. image:: doc/source/_static/MainWindow.png
    :align: center

* Minimal view:

.. image:: doc/source/_static/MinimalView.png
    :align: center

* Code folding:

.. image:: doc/source/_static/Folding.png
    :align: center

* Offset calculator

.. image:: doc/source/_static/PicOffsets.png
    :align: center


* Dark style support

.. image:: doc/source/_static/Dark.png
    :align: center




.. _chardet: https://pypi.python.org/pypi/chardet
.. _PyQt4: http://www.riverbankcomputing.co.uk/software/pyqt/download
.. _Downloads: https://launchpad.net/cobcide/+download
.. _Source repository: https://github.com/OpenCobolIDE/OpenCobolIDE/
.. _Issue tracker: https://github.com/OpenCobolIDE/OpenCobolIDE/issues?state=open
.. _Documentation: http://opencobolide.readthedocs.org/en/latest/
.. _Pygments: http://pygments.org/
.. _pyqode.core: https://github.com/pyQode/pyqode.core/
.. _pyqode.cobol: https://github.com/pyQode/pyqode.cobol/
.. _pyqode.qt: https://github.com/pyQode/pyqode.qt/
.. _GnuCobol: http://sourceforge.net/projects/open-cobol/
.. _setuptools: https://pypi.python.org/pypi/setuptools
.. _Python3: http://python.org/
.. _PyQt5: http://www.riverbankcomputing.co.uk/software/pyqt/download
.. _qdarkstyle: https://github.com/ColinDuquesnoy/QDarkStyleSheet
.. _pyQode: https://github.com/pyQode/
.. _github3.py: https://github.com/sigmavirus24/github3.py
