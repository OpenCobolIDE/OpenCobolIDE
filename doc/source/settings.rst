Application preferences
=======================

This page describes the preferences settings that are available for you to
customise your experience with the IDE.

To open the preferences dialog, go to Edit->Preferences (``F2`` on Windows and Linux and ``CMD+,`` on OS X).

Editor settings
---------------

.. image:: _static/editor-settings.png
    :align: center

This tab let you change the cobol source code editor settings.


View options:
+++++++++++++

- **Display line numbers**: show/hide the line numbers panel on the left of the editor
- **Highlight current line**: enable/disable highlighting of the current line
- **Highlight whitespaces**: show/hide visual whitespaces
- **Show errors**: enable/disable cobol linter (errors appears as red lines in the editor and as icons on the left of
                   the editor).

Indentation otions:
+++++++++++++++++++

- **Width**: the number of spaces for a tabulation
- **Automatic indentation**: enable/disable automatic *smart* indentation. The editor will move the cursor to the
  correct indentation level when you press ENTER (e.g. it will indent automatically after an if statement and so on.
- **Intelligent backspace**: If enabled, backspace will de-indent (eat at most ``Width`` spaces).


Code completion options:
++++++++++++++++++++++++

- **Trigger length**: specify the amount of characters needed to automatically trigger a code completion request.
  A value of 1 make the code completion trigger automatically as soon as you type.
- **Prosed keywords**: let you choose a convention for the cobol keywords: lower-case or UPPER-CASE keywords


Style settings
--------------

.. image:: _static/style-settings.png
    :align: center

This tab let you change the appearance of the cobol editor and the interface.

Application style options
+++++++++++++++++++++++++

This group of settings let you change the appearance of the interface. You can either choose to use
the **Native** theme (the app will look similar to the native apps of your system) or use a **Dark** stylesheet.

On GNU/Linux, you can also choose a custom **icon theme**.

Editor font options
+++++++++++++++++++

This group of settings let you change the code editor font settings (font name and default size).

Editor color scheme
+++++++++++++++++++

This let you choose a color scheme for the source code editor and the run output window.

OpenCobolIDE uses the pygments library for its color schemes definition. You can easily add some
custom color schemes by installing python plugin (this won't work with a frozen application on Windows and OS X).


Compiler settings
-----------------

.. image:: _static/compiler-settings.png
    :align: center

This tab let you change the GnuCobol compiler settings.

.. warning:: Those settings are applied globally to every file you compile with the IDE. At the moment there is no way
             to set custom compiler settings per file. Open a issue on the bug tracker if you feel like this feature
             is needed!


Standard
++++++++

This option let you choose the target COBOL standard.

Free format
+++++++++++

Enable/Disable coding in free format.

C compiler flags
++++++++++++++++

This let you change some common C compiler settings. Every checkbox has a tooltip that describes what the setting is
used for.

There is a line edit widget where you can add additional missing compiler flags (separate them with a blank space).

Library paths
+++++++++++++

This option let you add custom library paths, e.g. to locate a c library that you need to link with (such as
mysqlclient)

Libraries
+++++++++

This option let you specify the libraries you want to link with. Separate them with a blank space.

Custom compiler path
++++++++++++++++++++

Let you specify a custom compiler path. This option is not used on Linux. You can use it on Window to point the IDE to
the location of a custom GnuCobol build. On OS X you may want to use this setting to help the IDE find the cobc
executable if you did not installed it in a standard path.

VCVARS32 path:
++++++++++++++

*This option is not visible on the above screenshot because it is available only on windows.*

This option let you specify the path to VCVARS32.bat which is needed if you are using a custom GnuCompiler built with
Visual Studio. VCVARS32.bat can be found in the ``Bin`` folder of your Visual C++ installation. Just make sure to use
the same visual studio version as the one used to build the compiler.


Run settings
------------

.. image:: _static/run-settings.png
    :align: center

This tab let you change the way the IDE run executable programs. By default the program will run inside the IDE, in the
program output window. This work nice for basic program but will fail as soon as you start using the ``SCREEN-SECTION``,
an error message about being unable to redirect output will appear in the program output. To run such a program you need
to run it in an external console window.

To enable running a program in an external terminal:

* check ``Run in external terminal``
* specify the terminal program to use if necessary:

    * **On Windows**, the IDE will automatically use ``cmd.exe``.
    * **On OS X**, the IDE will automatically use ``open``.
    * **On linux**, it depends on the distribution and the desktop environment you are using.
      The IDE will try to pick up one of those if available: ``gnome-terminal``, ``konsole`` and ``xfce-terminal``.
      If you are using another terminal, please indicate the command to use.


SQL Cobol settings
------------------

.. image:: _static/sql-cobol-settings.png
    :align: center

This tab let you configure `dbpre`_ integration to get mysql support with COBOL!

.. warning:: This has been tested and validated on Linux only.


DBPRE Configuration
+++++++++++++++++++

It's up to you to install and setup dbpre. Once done, you can configure integration with the IDE.

This group of settings let you specify where to find the various parts of the dbpre framework:

- **dbpre**: location of the dbpre executable
- **cobmysqlapi**: location of the cobmysqlapi object file.
- **Framework**: path to the directory that contains the cobol copybooks that are needed to compile and run your sql cobol
  program


DB Connection Parameters
++++++++++++++++++++++++

This group of settings let you specify the content of the .param file that will be generated after the
compilation of your program succeeded. This file contains database connection settins. Read the dbpre documentation
to get more info!


.. _dbpre: http://sourceforge.net/projects/dbpre/