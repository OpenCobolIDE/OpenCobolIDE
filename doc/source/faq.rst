FAQ
===

1) Where are the generated binaries?
------------------------------------

The executable program or module can be found in the bin folder next to your
source file

2) I cannot compile on Windows, the compiler freeze or I get a execution error. What can I do?
----------------------------------------------------------------------------------------------

First ensure that you do not have a conflicting installation of MinGW in your
PATH. If yes, remove it.

If the problem persists, do not hesitate to open an issue but OpenCobolIDE is
known to work on Windows (from Xp, to 8). Chances are that the issue come from
your configuration.

3) Cannot detect OpenCobol compiler on Mac OSX. What can I do?
--------------------------------------------------------------

If you installed the open cobol compiler in a non-standard path, you will have
to tell OpenCobolIDE where to look.

You can specify the path to the compiler in the preferences dialog
(Build & Run tab).
