Advanced topics
===============


.. _windows-gnu-cobol:

Setup a custom compiler on Windows
----------------------------------

Starting from version 4.6.0, you have all the tools to setup a custom GnuCOBOL compiler on windows.

The first step is to download or build the compiler and make sure it works from the command line.

Then fire up OpenCobolIDE, open the preferences dialog and go to the compiler tab:

    - specify the compiler path in  "Custom compiler path". When you choose a new compiler, you will
      be presented with a dialog that let you check if the compiler works. If the test failed, then this
      might be because you need to adjust some environment variables
    - adjust the environment variables (PATH and COBC_CONFIG_DIR are usually required, you might also need
      to adjust the include and copy path depending on the compiler you chose). You may disable variables
      that you don't need by removing the check mark.

      .. note:: when you've changed an environmnent variable, you can check if the compiler works by clicking on the
                ``check compiler`` button.

    - if the compiler is build with MSVC, you will also need to indicate the path of vcvarsall.bat of the
      compiler used to build GnuCOBOL. If using Visual Studio 2010, you can find this file in
      ``c:\Program Files(x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat``

Here is the setup I use for working with `GnuCOBOL 2.0 MSVC`_:

   .. image:: _static/custom_compiler_windows.png
        :align: center


.. _GnuCOBOL 2.0 MSVC: http://www.kiska.net/opencobol/2.0/index.html


.. _sql-guide:

SQL COBOL Guide
---------------

GnuCOBOL does not support SQL statements (such as EXEC SQL) natively but you may use a pre-compiler that
will convert your sql statements to pure COBOL that can then be compiled with GnuCOBOL.

OpenCobolIDE supports the following pre-compilers:

- `dbpre`_  (**UNIX**)
- `esqlOC`_ (**WINDOWS**)

.. _dbpre: http://sourceforge.net/projects/dbpre/
.. _esqlOC: http://sourceforge.net/p/open-cobol/discussion/contrib/thread/4057115f/

Read the below section to know how to setup OpenCobolIDE to work with one of these pre-compilers.


.. warning:: Pre-compilers are associated with one specific extension (**.scb** for dbpre and **.sqb** for esqlOC).
             Those tools will get used only if the file extension match the associated extension!
             E.g. pre-compilers will never get called on regular COBOL files (.cbl, .cob,...).

esqlOC (on Windows)
+++++++++++++++++++

.. warning:: To work with esqlOC, you need to use GnuCOBOL built with Visual Studio.
             Read :ref:`windows-gnu-cobol` to setup the correct compiler.

1) Setup OpenCobolIDE to work with GnuCOBOL compiled with Visual Studio (make sure you can compile a simple
   HelloWorld)
2) Download esqlOC and install it somewhere on your drive (prefer a path without spaces such as ``c:\esqloc``).
3) Open the OpenCobolIDE preferences and go to the **SQL Cobol** tab. There specify the installation directory
   of esqlOC.
4) Open a **.sqb** file and compile it.


DBPRE (on GNU/Linux)
++++++++++++++++++++

1) Download dbpre and follow the instruction for compiling. You don't need to install it system
   wide, just create a clean dbpre directory in your home folder where you you copy the following files:

      - dbpre executable
      - cobmysqlapi.o
      - the copybooks: PGCTBBAT, PGCTBBATWS and SQLCA

   The dbpre directory should look like that:

   .. image:: _static/dbpre_directory_content.png
        :align: center

2) Run OpenCobolIDE and open the preferences dialog.
3) Go to the SQL Cobol setting tab
4) Specify the path to the dbpre executable, cobmysqlapi.o and the directory that contains the copybooks
5) Setup the DB connection parameters to connect to your test db$
6) Go to the compiler settings tab
7) Add a new library path that points to: `/usr/include/mysql`
8) Add `mysqlclient` to the libraries.
9) Open a **.scb** file, compile and enjoy!

Here are some screen-shots of a working configuration:

**DBPRE configuration**:

   .. image:: _static/configured_dbpre.png
        :align: center

**Compiler configuration**:

   .. image:: _static/configured_compiler_for_dbpre.png
        :align: center

Developer mode
--------------

By default, OpenCobolIDE embeds a series of third party libraries and modify python's sys import path to always load the
bundled libraries first. Those libraries are bundled inside the opencobolide python package (this path is internally
used to set the ``OCIDE_EXTLIBS_PATH`` environment variable).

This behaviour can be changed by setting the ``OCIDE_DEV_MODE`` environment variable to 1. If you do so, you'll need to
install the required third-parties yourself. This can be done easily by running the following command at the root
of the OpenCobolIDE's source directory:

``pip install -r requirements.txt``
