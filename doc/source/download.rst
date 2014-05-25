Download & Install
==================

Here you'll find all the necessary explanations for installing OpenCobolIDE.


Requirements:
-------------
OpenCobolIDE depends on the following packages

    - Python 3
    - pyqode.qt
    - OpenCobol
    - pyqode.core
    - pyqode.widgets
    - pygments >= 1.6
    - chardet (chardet2 if you are using python3)

Installation
------------

GNU/Linux
+++++++++

If you are on Ubuntu 13.10 or drivatives, you can use the ppa to easily
install OpenCobolIDE::

    sudo apt-add-repository ppa:open-cobol-ide/stable
    sudo apt-get update
    sudo apt-get install open-cobol-ide

Otherwise you will have to install from source:

1) Install OpenCobol and pyqode.qt

.. code-block:: bash

    sudo apt-get install python3-pyqt4 open-cobol

2) Install OpenCobolIDE

Using pip::

    sudo pip3 install OpenCobolIDE

From source::

    cd /path/to/source
    sudo python3 setup.py install

.. note:: You need pip for python3. Depending on your distribution, you might
          replace ``pip`` by ``pip3`` or ``pip-3.2``


Both methods will install a desktop entry on your system.

An **open-cobol-ide** entry should appear in the development category. If the
entry does not appear, you can always open a terminal and type *open-cobol-ide* to run the IDE.


Windows
++++++++++++

Please use the Windows Installer available `here`_.

.. _`here`: https://github.com/OpenCobolIDE/OpenCobolIDE/releases


.. note:: Installation from source is possible but you need to install all
          requirements yourself and run the OpenCobolIDE.pyw from the *extracted
          archive* or *git repository* without actually installing OpenCobolIDE
          (**do not run** *setup.py install* on windows).