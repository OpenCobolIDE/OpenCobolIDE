Download & Install
==================

Here you'll find all the necessary explanations for installing OpenCobolIDE.


Requirements:
-------------
OpenCobolIDE depends on the following packages:

    - Python 3
    - PyQt5
    - OpenCobol
    - pyqode.core
    - pygments >= 1.6
    - chardet

GNU/Linux
---------

Install pyqt5, open-cobol and pip using your package manager, then run the following commands::

    sudo pip3 install OpenCobolIDE


If you are on Archlinux, you can install OpenCobolIDE and all its dependencies from the AUR: https://aur.archlinux.org/packages/open-cobol-ide/


Windows
-------

There is a windows installer available here: https://launchpad.net/cobcide/+download

Mac OSX
-------

There is a dmg image available here: https://launchpad.net/cobcide/+download

The only thing you have to do is:

- to install OpenCobol compiler, e.g. using homebrew::

    brew install open-cobol

- run the OpenCobolIDE app

.. note:: If you installed the compiler in a non-standard path and it is not recognized, you
          can specify and the path to the compiler in the preferences dialog under the ``Build & Run`` section. Just
          make sure to only specify the directory where the compiler can be found, not the full path.


.. _`release section on github`: https://github.com/OpenCobolIDE/OpenCobolIDE/releases

.. _homebrew: http://brew.sh/

.. _PPA: https://launchpad.net/~open-cobol-ide/+archive/stable
