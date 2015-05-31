What's New?
===========
This page lists the most prominent milestones achieved by the OpenCobolIDE
developers. For more specific details about what is planned and what has been
accomplished, please visit the `issues page on github`_.

Next Version
------------

We plan to improve the code template mechanism to let you define your own code
template that will be used for new file creation. We might also add a small
macro system to automatically fill up details such as author, date, and so
on...

Changelog
---------
4.6.0
+++++

New features/Improvements:
    - Add ability to control where the binaries will be placed
    - Add environment variable settings for compiler (PATH, COBC_CONFIG_DIR,...)
    - Add dialog and a button to check whether your compiler works or not (
      check compilation of a simple hello world)
    - Add .lst to default list of cobol extensions
    - Add a compiler output log view with the complete output of the cobc
      commands
    - Fix some spelling errors and always use GnuCOBOL instead of OpenCOBOL
    - Show PICTURE in navigation item's tooltip (this works but still requires
      some work on the parser to be fully finished)
    - Add ability to run module with cobcrun


Fixed bugs:
    - Fix a crash with corrupted recent files list
    - Fix fullscreen mode not setup on startup but recognized in the options
    - Fix Ctrl+Home not working as expected (go to first line)
    - Fix bug with linter that does not take compiler options into account
      (file not found error for copybooks)
    - Fix misleading compiler settings label
    - Fix file system view reloaded when parent directory has not changed
    - Remove dock widgets hotkeys not working on windows (remove the "&" from
      the dock widgets' titles)
    - Fix SECTION/DIVISION not correctly recognized in navigation panel and
      fold panel if there are some spaces between the keyword and the period.
    - Fix linter running when compiler is not working
    - Fix detection of file type (EXECUTABLE/MODULE) and dependencies of file
      when there are some newlines between CALL, USING and the PAREMTER of the
      call

4.5.1
++++++

New features:
    - add ability to set custom file extension association with the different
      compiler (cobc, dbpre and esqlOC)

Fixed bugs:
    - Fix issues with non cobol files on frozen builds (Windows and OSX only)
    - All bugs fixed in pyqode.core 2.6.1

4.5.0
+++++

New features:
    - EOL management (see issue #110)
    - New filter mode for code completion: subsequence
    - Add support for stdeb (ppa packages available)

Fixed bugs:
    - fix a bug with comment (see issue #109)

4.4.0
+++++

New features:

  - esqlOC integration (SQL precompiler for windows)

Fixed bugs:
  - fix a bug with the new reporter tool on windows (and get rid of the github3.py dependency)
  - improve comment/uncomment when workin in fixed format and column 1-6 is not empty.

4.3.1
+++++

New features:

- new bug report tool that make use of the Github API to submit a new bug report
  using your account that automatically includes system information and
  the application log.

Fixed bugs:

- fix a couple of small bugs in pyqode which should improve the usability (things like
  restoring cursor position after a reload due to an external change, improved auto-completion
  of quotes and parentheses).

4.3.0
+++++

New features:

- experimental support for dbpre on Linux
- experimental support for using a custom GnuCobol compiler on Windows
- support for custom keywords convention (lower or upper case keyword suggestions)
- support for very small screens (10 inches)
- support for opening more than one file

Fixed bugs:

- improvements to the navigation panel: fix issues where exec statements were shown in the outline.
- fixed a bug where the linter mixed the code of two opened tabs
- fixed indentation bugs when indenting source that have characters before column 7 (non free format)
- fixed duplicate entires in the recent files list
- fixed a bug that prevent the IDE to remember the last open/save path
- fixed a bug where compilation/run actions were wrongly disabled


4.2.0
+++++

New features:

- splittable tab widget: you can now split and editor vertically or
  horizontally infinitely.
- a file system tree view that show the content of the directory of the current
  editor
- navigation panel (and file system tree view panel) can now be closed
- make the control panel (buttons in the editor in minimal view) look better
  on windows
- add file association to the windows installer
- allow to disable intelligent backspace (now disabled by default) (#78)
- add a path label to the status bar
- add support for pygments 2 (new color schemes)
- add "Report bug" menu action (clicking on this will open your browser to the
  github issue tracker with a pre-filled error report)

Fixed bugs:

- fix a line ending issue with the run console on windows (#77)
- fix a bug with navigation panel (#76)
- improve usage of rrt theme (#79)

4.1.0
+++++

New features:

- add support for GnuCobol 2.0 on GNU/Linux
- add a way to specify global compiler switches (-g, -ftrace,...)
- improve detection of external terminal on GNU/Linux
- make use of pyqode-console to prompt for a key press at the end of the
  program when run in an external terminal (Windows + GNU/Linux)
- style improvement: the internal terminal will use the same colors as the
  cobol editor.
- performance improvement: avoid useless re-highlight on open
- update to pyqode 2.3 (add occurrences highlighting, global checker,
  better selections, smart backspace, auto complete of quotes and
  parentheses, ...)

4.0.0
+++++

The entire application has been rewritten.

The cobol code editor widget has been moved to the pyqode.cobol package.

New features:

- code folding
- improved auto indentation (after if/else/perform)
- reworked user interface: the default view (from v2) is back as the
  default view but you can switch to the minimal view (from v3) by double
  clicking an editor tab (see issue #47)
- navigation panel is now fully synced with code folding panel of the
  current editor
- you can now cancel a build/run action
- new syntax highlighter which is about 3 times faster than the previous
  highlighter
- more keywords in code completion
- ability to disable the linter (see issue #46)

3.0.0
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
