Download & Install
=========================

Here you'll find all the necessary explanations for installing pyqode.python.


Requirements:
----------------
OpenCobolIDE depends on the following packages

    - Python 2.7 or Python 3
    - PyQt4
    - OpenCobol
    - pyqode.core
    - pyqode.widgets
    - pygments >= 1.6
    - chardet (chardet2 if you are using python3)

Installation
------------------

GNU/Linux
++++++++++

1) Install OpenCobol and PyQt4

.. code-block:: bash

    sudo apt-get install python-qt open-cobol

.. note:: If you want to use python 3, you need to install python3-pyqt4
          instead of python-qt.

2) Install OpenCobolIDE

Using pip or easy_install::

    sudo pip install OpenCobolIDE

From source::

    cd /path/to/source
    sudo python setup.py install

.. note:: Replace *python* by *python3* to install OpenCobolIDE for Python 3.

Windows
++++++++++++

Please use the Windows Installer available `here`_.

.. _`here`: https://github.com/ColinDuquesnoy/OpenCobolIDE/releases

This will let you install OpenCobolIDE easily and have a shortcut on your
desktop.

.. note:: Installation from source is possible but you need to install all
          requirements yourself and run the OpenCobolIDE.pyw from the *extracted
          archive* or *git repository* without actually installing OpenCobolIDE
          (**do not run** *setup.py install* on windows).