What's New?
===========
This page lists the most prominent milestones achieved by the OpenCobolIDE
developers. For more specific details about what is planned and what has been 
accomplished, please visit the `issues page on github`_.

Next Version
------------

We don't have any plans for the next release. Please report bugs and share
your thoughts to make OpenCobolIDE better!

Changelog
---------

2.4.0
+++++

New features:
    - add **Mac OSX** support

    - move to **PyQt5** (to support retina screens)

    - add support for **pyQode 2.0**:

      the new api is a lot more stable API and is now fully
      tested. The editor style and performances have been improved but **the
      folding panel has been temporarely removed** *(for performance reason)*

    - **new user interface**:

      the menu and toolbar has gone, instead there is now a compile and run button
      inside the editor and a drop down button in the status bar for the most
      important actions.

      The homepage and the preferences dialog also got redesigned.

    - compiler process management has been improved:

      We are now using QProcess instead of subprocess, this allow some neat
      improvements such as auto compile before run.

    - better log message + log window: include information from the log window
      when you report bugs!

Please, read the :doc:`/getting_started` section of this manual to get started
with the new user interface!

2.3.1
+++++

- drop python 2 support (the main script must now be run by a python3
  interpreter)

- fix bug with encoding error, see bug #31 on github

2.3.0
+++++

New features:
    - add ability to run the compiled programe in an external terminal. This is
      useful if you are using the SCREEN SECTION as the embedded terminal does
      not support redirection.

Fixed bugs:
    - fix bug with detection of submodules call if they are enclosed with single quotes
      instead of double quotes

2.2.0
+++++
New features:
    - pic fields offsets calculator
    - case converter, you can convert selected text to lower or TO UPPER using the
      editor context menu.
    - full dark style using `qdarkstyle`_
    - new test suite for compiler and parser modules
    - improved go to line dialog
    - add support for _*.pco_ and _*.cpy_ files

Fixed bugs:
    - fix compilation for file if path contains spaces (Linux and Windows)
    - fix parser crash that prevents from compiling
    - improve cobol parser to support malformed syntax
    - fix ambiguous shortcut overload: F2
    - fix outline not informative for cobol files that have data in column 1-6

2.1.0
+++++

New features:
    - Go to definition for variables and procedures (ctrl + click on symbol)
    - New debian package on ppa:open-cobol-ide/stable and ppa:open-cobol-ide/unstable

Fixed Bugs:
    - code completion should not occur in comments and strings
    - fix column number for navigation panel


2.0.1
+++++

Fixed Bugs:
    - removed un-needed import of pexpect which caused some issue on clean
      system which does not have pexepect.

2.0.0
+++++

This new release is a major update which makes the transition from PCEF to
pyqode. Most of the application has been rewritten from scratch.

Here are the major changes:

    - port to *pyqode 1.0*
    - new compiler errors panel
    - interactive output console for program output
    - uses *pyqode.qt* in place of *PySide*
    - support for both python 2 and python 3
    - *on the fly* syntax check, OpenCobolIDE compile your code in the
      background to quickly warn you about wrong syntax.
    - better integration with most linux desktop environments (use icons and
      colors from theme, desktop entry). Tested with KDE, Gnome, Unity and
      Cinnamon.
    - allow user to type in lower case (https://github.com/OpenCobolIDE/OpenCobolIDE/issues/1)


1.4.2
+++++

  - Update code so support pcef 0.2.2

1.4.1
+++++
Fixed Bugs:
  - slow in large files: https://bugs.launchpad.net/cobcide/+bug/1179228

1.4.0
+++++

New features:
  - shortcuts for dock windows (F9: log panel, F10: navigation panel)
  - show fullscreen shortcut change from F12 to F11
  - the application will restore its geometry and state (maximised, dock window positions)

Fixed Bugs:
  - dock panel shown when switching tab: now the panel is only show when coming from the homepage or when compiling
  - crash when editing/compiling files who have unicode characters in their path
  - focus lost when opening recent files from the menu or the homepage on ubuntu 12.04 -> 13.04

Enhancements and fixed bugs in PCEF 0.2.0:
  - improve performances in general
  - support for custom word separator, allow OpenCobolIDE to remove the '-' character from word separators which brings a better
    code completion for cobol
  - dirty flag is correctly updated
  - scrollbars are now correctly update when folding/unfolding code blocks



1.3.0
+++++
This release improves usability with a focus on the run and compile actions:
  - detect source dependencies and compile them (if a program P requires a subprogram A who requires a subprogram B than the IDE will compile A, B and P)
  - Automatically compile file when the run action is triggered
  - Run the last program if the current tab is a subprogram
  - Avoid compiling a file that is already compiled and up to date

1.2.1
+++++

  - Fix bug: https://launchpad.net/cobcide/+milestone/1.2.1

1.2.0
+++++

Added:
  - Home page with list of recent files
  - A settings page to change a few options (mainly related to the editor style)
  - A navigation panel to quickly browse large files (tree with div, sections, variables and paragraphs)
  - A shortcut to comment/uncomment selected or active lines (ctrl+/)
  - On GNU/Linux, at first start the program will ask the user if he wants to create a desktop files

Bug fixes:
  - windows path not normalized
  - fix bug with mingw when path contains spaces on windows
  - fix bug where no extension was proposed when creating a new file

1.1.0
+++++
   - better encoding detection using chardet
   - cobol specific code completion model
   - status bar infos (filename, encoding, cursor position)
   - windows port (a windows installer is available in the download section)

1.0.1
+++++

    - fix packaging issues

1.0.0
+++++

    - Initial development

.. _issues page on github: https://github.com/OpenCobolIDE/OpenCobolIDE
.. _qdarkstyle: https://github.com/ColinDuquesnoy/QDarkStyleSheet