Advanced topics
===============

How to use a custom GnuCobol compiler build on WINDOWS?
-------------------------------------------------------

Starting from version 4.3, it is possible to use another GnuCobol compiler than the one that is bundled
with the IDE.

This section describes how to setup OpenCobolIDE to use kiska's GnuCobol 2.0 compiled with Visual Studio.


1) Download and install Visual Studio C++ 2010 (the express edition is OK) from miscrosoft.
2) Download GnuCobol 2.0 from http://www.kiska.net/opencobol/2.0/index.html (the 32 bit version is preferred)
3) Extract the compiler and install it to ``C:\``, you should have ``c:\OpenCobol``.
4) Run OpenCobolIDE, open the preferences dialog and go to the compiler tab
5) Select ``c:\OpenCobol`` for the **custom compiler path**
6) Select the visual studio **vcvars32.bat** path (
   ``c:\Program Files(x86)\Microsoft Visual Studio 10.0\VC\bin\vcvars32.bat``)
7) Create a simple hello world cobol program, compile and run it to make sure everything works OK.


How to setup dbpre integration with OpenCobolIDE?
-------------------------------------------------

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
9) Open a .scb file, compile and enjoy!

Here are some screen-shots of a working configuration:

**DBPRE configuration**:

   .. image:: _static/configured_dbpre.png
        :align: center

**Compiler configuration**:

   .. image:: _static/configured_compiler_for_dbpre.png
        :align: center
