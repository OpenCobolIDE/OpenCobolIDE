FAQ
===

1) Where are the generated binaries?
++++++++++++++++++++++++++++++++++++

The executable program or module can be found in the bin folder next to your
source file

2) I cannot compile on Windows. What can I do?
++++++++++++++++++++++++++++++++++++++++++++++

*(the compiler freeze or I get a execution error)*

First ensure that you do not have a conflicting installation of MinGW in your
PATH. If yes, remove it.

If the problem persists, do not hesitate to open an issue.

OpenCobolIDE is known to work on Windows (from Xp, to 8.1). Chances are that
the issue come from your configuration.

3) Cannot detect OpenCobol compiler on Mac OSX. What can I do?
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

If you installed the open cobol compiler in a non-standard path, you will have
to tell OpenCobolIDE where to look.

You can specify the path to the compiler in the preferences dialog
(Build & Run tab).


4) Is it possible to install from source on Windows or Mac OSX?
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


Installation from source is possible on Mac and Windows:

- install all requirements (Python3, PyQt5, pyqode.core)
- run the ``open-cobol-ide`` script from the *extracted archive* or
  *git repository* without actually installing OpenCobolIDE.
  Do not run `setup.py install` on Windows or Mac OSX, it won't work.