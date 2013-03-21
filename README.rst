OpenCobolIDE
--------------------

OpenCobolIDE is a simple and lightweight cobol IDE for Ubuntu/Debian.


The software is written in Python using the PySide Qt bindings and PCEF for the
code editor widget.

Features:
    - syntax highlighting
    - basic code completion (based on document words)
    - compile as program (.exe) or as subprogram (.so)
    - run the program from the editor
    - also open text files


License
--------------------

OpenCobolIDE is released under the **GPL** version 3


Installation
--------------------

The package can be installed from pypi::

    easy_install OpenCobolIDE

Requirements
--------------------

The project depend on the following library:
    - **Python 2.7**
    - **PySide**
    - **OpenCobol**
    - **PCEF**
    - **Pygments >= 1.6**

Here are the instructions to install the dependencies manually::

   sudo apt-get install python2.7 python-pyside open-cobol python-setuptools
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