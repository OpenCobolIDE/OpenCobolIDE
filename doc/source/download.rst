Download & Install
==================

Here you'll find all the necessary explanations for installing OpenCobolIDE.


Requirements:
-------------
OpenCobolIDE depends on the following packages:

    - Python 3
    - PyQt5 or PyQt4
    - OpenCOBOL/GnuCOBOL
    - pyqode.core
    - pyqode.cobol
    - pygments

GNU/Linux
---------

Install pyqt5, open-cobol and pip for python3 using your package manager, then run the
following commands::

    sudo pip3 install OpenCobolIDE --upgrade


If you are on Archlinux, you can install OpenCobolIDE and all its dependencies
from the AUR: https://aur.archlinux.org/packages/open-cobol-ide/

.. note:: if you have both PyQt5 and PyQt4 on your system, the IDE will use
          PyQt5 by default. To force the use of PyQt4, you should set the
          ``QT_API`` environment variable to ``pyqt4``.

.. note:: starting from v4.6.2, the name of the executable is lowercase:
          opencobolide instead of OpenCobolIDE.

Windows
-------

There is a windows installer available here: https://launchpad.net/cobcide/+download

Mac OSX
-------

There is a dmg image available here: https://launchpad.net/cobcide/+download

The only thing you have to do is:

- to install GnuCOBOL compiler, e.g. using homebrew::

    brew install gnu-cobol

- run the OpenCobolIDE app

.. note:: If you installed the compiler in a non-standard path and it is not recognized, you
          can specify and the path to the compiler in the preferences dialog under the ``Build & Run`` section. Just
          make sure to only specify the directory where the compiler can be found, not the full path.


.. _`release section on github`: https://github.com/OpenCobolIDE/OpenCobolIDE/releases

.. _homebrew: http://brew.sh/

.. _PPA: https://launchpad.net/~open-cobol-ide/+archive/stable
