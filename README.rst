OpenCobolIDE
--------------------
.. image:: https://pypip.in/d/OpenCobolIDE/badge.png
    :target: https://crate.io/packages/OpenCobolIDE/
    :alt: Number of PyPI downloads

.. image:: https://pypip.in/v/OpenCobolIDE/badge.png
    :target: https://crate.io/packages/OpenCobolIDE/
    :alt: Latest PyPI version

OpenCobolIDE is a simple and lightweight cobol IDE based on the OpenCobol
compiler.


The software is written in Python using PyQt4 and pyQode

Features:
    - syntax highlighting
    - code completion
    - code folding
    - compile as program (.exe) or as subprogram (.so)
    - run the program from the editor
    - also open text files


License
--------------------

OpenCobolIDE is released under the **GPL** version 3


Installation
--------------------

GNU/Linux
############

The package can be installed from pypi::

    pip install OpenCobolIDE


You will still have to install PyQt4 and OpenCobol on your own::

    sudo apt-get install python-qt open-cobol

Windows
############

There is a windows installer available here: https://launchpad.net/cobcide/+download


Requirements
--------------------

The project depend on the following library:
    - **Python 2.7** or **Python 3.3**
    - **PyQt4**
    - **OpenCobol**
    - **pyqode.core**
    - **pyqode.widgets**
    - **Pygments >= 1.6**
    - **chardet** (chardet2 if you are using python3)

Here are the instructions to install the dependencies manually::

   sudo apt-get install python2.7 python-qt open-cobol python-setuptools
   sudo easy_install pcef


Disclaimer
--------------------

I am by no way a cobol expert, I just had to work on a cobol project at school
with an awful IDE (NetExpress on a Windows Xp virtual machine). As I was writing
pyqode, I thought it would be nice to use it to make a simple cobol editor for
GNU/Linux.

I've only learnt COBOL 74 and the IDE has been thought with this standard in
mind. I have personally no plan on working with cobol at the moment nor to learn
a new standard but if you found a missing feature, feel free to open a feature
request. I'm always looking forward to make OpenCobolIDE better for the experts.

Your advice will be very appreciated!


Screenshots
-------------

* Home page:

.. image:: https://raw.github.com/ColinDuquesnoy/OpenCobolIDE/develop/doc/source/_static/Home.png
    :align: center

* Editor:

.. image:: https://raw.github.com/ColinDuquesnoy/OpenCobolIDE/develop/doc/source/_static/CompilerOutput.png
    :align: center



