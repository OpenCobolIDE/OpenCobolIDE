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

If you are on Ubuntu 14.04 or derivatives, you can use the `PPA`_ to easily
install OpenCobolIDE::

    sudo apt-add-repository ppa:open-cobol-ide/stable
    sudo apt-get update
    sudo apt-get install open-cobol-ide

If you are on archlinux, packages are available on the AUR.

Otherwise you will have to install from source:

1) Install OpenCobol and PyQt5

.. code-block:: bash

    sudo apt-get install python3-pyqt5 open-cobol

2) Install OpenCobolIDE

Using pip::

    sudo pip3 install OpenCobolIDE

or from source::

    cd /path/to/source
    sudo python3 setup.py install

Both methods will install a desktop entry on your system.

An **open-cobol-ide** entry should appear in the development category. If the
entry does not appear, you can always open a terminal and type
*open-cobol-ide* to run the IDE.


Windows
-------

Please use the Windows Installer available in the `release section on github`_.


Mac OSX
-------

There is a standalone app available in the `release section on github`_.

The app is standalone, you don't have to install any of its libraries but **you
will need a working open-cobol compiler**.

One can easily install OpenCobol on Mac using `homebrew`_::

    brew install open-cobol


.. _`release section on github`: https://github.com/OpenCobolIDE/OpenCobolIDE/releases

.. _homebrew: http://brew.sh/

.. _PPA: https://launchpad.net/~open-cobol-ide/+archive/stable


.. note:: Installation from source is possible on Mac and Windows but you will
          need to install all requirements yourself and run the open-cobol-ide
          script from the *extracted archive* or *git repository* without
          actually installing OpenCobolIDE.
          **Do not run `setup.py install` on Windows or Mac OSX**.