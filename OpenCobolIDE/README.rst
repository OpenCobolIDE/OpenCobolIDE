open-cobol-ide
=========

cobcide (OpenCobolIDE) is a simple and lightweight cobol IDE for Ubuntu/Debian.


The software is written in Python using the PySide Qt bindings and PCEF for the
code editor widget.

Features:
    - syntax highlighting
    - basic code completion (based on document words)
    - compile as program (.exe) or as subprogram (.so)
    - run the program from the editor
    - also open text files


License
=========

Ocide is released under the **GPL** version 3


Installation
==============

Currently the ide has not been packaged. Plans are to package it on both
pypi (as a gui script) and on a ppa.


Requirements
================

The project depend on the following library:
    - **Python**
    - **PySide**
    - **OpenCobol**
    - **PCEF**

Here are the instructions to install the dependencies manually:

Install the following package using **apt-get install**:
    - python2.7
    - python-pyside
    - open-cobol

Install the following package using **pip install** or **easy_install**:
    - pcef


Disclaimer
==============

I am by no way a cobol expert, I just had to work on a cobol project at school
with an awful IDE (NetExpress on a Windows Xp virtual machine). As I was writing
PCEF (a code editor widget for PySide), I thought it would be nice to use it
to make a simple cobol editor for Ubuntu.

I will only cover the needs for my project so it might miss a lots of
things for the expert. Don't hesitate to collaborate on the project, report bugs
and post blueprints on launchpad.
