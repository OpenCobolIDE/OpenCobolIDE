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

OpenCobolIDE is known to work on Windows (from Windows Xp, to Windows 10).

Chances are that the issue come from your configuration.

3) Cannot detect OpenCOBOL compiler on Mac OSX. What can I do?
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

If you installed the GnuCOBOL compiler in a non-standard path, you will have
to tell OpenCobolIDE where to look.

You can specify the path to the compiler in the preferences dialog
(Build & Run tab).

4) Cannot compile source code that contains EXEC SQL statements. What can I do?
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

GnuCOBOL does not support EXEC SQL statements. You need to use a COBOL precompiler that will
convert your sql statements to regular COBOL code that can be compiled with GnuCOBOL.

Read the :ref:`sql-guide`.

5) OpenCobolIDE failed to open a file due to an encoding issue. What can I do?
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

OpenCobolIDE does not detect file encoding automatically. It first tries with your
locale file encoding (e.g. UTF-8 on GNU/Linux and Mac OSX and whatever windows-XXX encoding
on Windows). If that fails, the editor will open with a blue panel at the top
asking you to select another encoding and retry:

.. image:: _static/encoding_issue.png
    :align: center

1) You must choose another encoding from the character encoding combo box. This combo
   box contains your preferred file encodings. Usually there is only one entry in this list
   and you need to add a new file encoding to your preferred encodings. To do that, click
   on ``Add or remove``.

.. image:: _static/encoding_add_to_preferred.png
    :align: center

2) Add one or more file encodings from the ``Available encodings`` to your
   ``Preferred encodings`` and click on ``Ok``.

.. image:: _static/encodings_dialog.png
    :align: center

3) Select the encoding to use in the `Character encoding``combo box and click on ``Retry``

.. image:: _static/new_encoding_selected.png
    :align: center

If you selected the correct encoding, the file should now load properly in the editor. If not, try
another encoding...


6) Where can I find OCIDE's log file?
+++++++++++++++++++++++++++++++++++++

You can find the OCIDE log file here:

- ``%USERPROFILE%/OpenCobolIDE/cache/OpenCobolIDE.log`` on Windows
- ``~/.cache/.OpenCobolIDE/OpenCobolIDE.log`` on GNU/Linux
- ``~/Library/Caches/OpenCobolIDE/OpenCobolIDE.log`` on OSX




7) Using paths with spaces in extra compiler flags
++++++++++++++++++++++++++++++++++++++++++++++++++

Paths with spaces should now be working as long as you wrap the with quotes::

    -I "D:\my test"

Note that if you're using a mingw based compiler, you will need to escape whitespaces::

    -I "/d/A\ path\ with\ some\
