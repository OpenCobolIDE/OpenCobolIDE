OpenCobolIDE
--------------------
*version 2.0b1*

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
    - **PySide**
    - **OpenCobol**
    - **pyqode0core**
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
PCEF (a code editor widget for PySide), I thought it would be nice to use it
to make a simple cobol editor for Ubuntu.

I will only cover the needs for my project so it might miss a lots of
things for the expert. Don't hesitate to collaborate on the project, report bugs
and post blueprints on launchpad.