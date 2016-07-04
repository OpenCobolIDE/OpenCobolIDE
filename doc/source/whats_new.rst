What's New?
************

This page lists the most prominent milestones achieved by the OpenCobolIDE
developers. For more specific details about what is planned and what has been
accomplished, please visit the `issues page on github`_.

Next Version
=============

OpenCobolIDE is in maintenance mode. No new features planned, only critical bugfixes. Note that it may automatically
receive new features if they are implemented in pyQode.

Changelog
=========

4.7
===

Starting from this version, **OCIDE has entered in maintainance mode**: no new
features will be accepted, only bug fixes.

*We are working a new IDE* `HackEdit`_. *This new IDE will have a lot more features
than OCIDE (project support, search/replace in files, configurable preparsers,
ability to write your own plugins, and so on...).
HackEdit is in alpha stage for the now. We will continue to maintain and fix
bugs in OCIDE until the final 1.0 version of HackEdit is ready.*

.. _HackEdit: https://github.com/HackEdit/hackedit

4.7.4
-----

Improvements:

- a new output window with support for the most common ANSI Escape Codes.
- use a white toolbar on Windows 10
- add support for parsing exceptional messages from GnuCOBOL (e.g. when there is a configuration issue)
- improve run in external terminal on OSX (working dir is now correctly used and running a module with cobcrun
  should now be working)
- add ability to choose a custom working directory in the run preferences
- use -v option when checking for compiler checks

Fixed bugs:

- fix compiler path tooltip (the full path is not required since version 4.7)
- fix typo in unhandled exception dialog
- fix typo in compiler check dialog
- fix check compiler can only be used once

4.7.3
-----

Improvements:

- add a checkbox in the status bar to quickly switch between fixed and free format
- improve saving and restoring window state (hidden dock widgets will stay hidden).
- allow to see pyqode debug log messages
- improve logging system to easily make the distinction between the current log content and content of other instances.
- don't include full compiler path in default configuration (already set in PATH)
- update to latest qcrash:
    - split report into general and application log
    - GitHubBackend: upload log file as a gist
    - add option to save login only.
- update to latest pyqode:
    - many improvements to the cobol code folding
    - name parser works with incomplete sources (copybooks or programs which have DIVISIONS in copybooks)
    - add zoom menu to the editor context menu
    - add ability to close tabs on the left/right of the current tab.
    - fix cursor not visible under margins
    - and many other bug fixes
- include all available pygments lexer in the frozen build on Windows and OSX (you will have basic syntax highlighting
  for file that are not cobol sources, e.g. bash, foxpro, ...).
- simplify new file templates + use free format setting to choose the correct template.

Fixed bugs:

- fix compiler output not parsed correctly if ';' in error message
- prevent overriding  RunEnv['PATH'] by CompilerEnv['PATH']
- fix linter not working if no compiler full path specified (will now use PATH to find the full compiler path).
- fix clear logs not working on Windows
- fix PermissionError when determining the available icon themes
- fix spelling error GnuCobol -> GnuCOBOL in about dialog.
- fix external terminal not working anymore on GNU/Linux.
- fix generic editor's backend not working in frozen mode
- fix default icon theme not correctly detected on Plasma 5.6 (maybe on other linux desktops too).

4.7.2
-----

Improvements:

- move the crash report tool to an external package: [qcrash](https://github.com/ColinDuquesnoy/QCrash)
- improve the crash report tool by introducing a review dialog where you can edit the final bug report and remove any sensitive data from the report

Fixed bugs:

- fix FileNotFoundError if wrong vcvarsall path is specified
- fix issues where bad github crendentials would be stored by the bug report tool with no way to correct them

4.7.1
-----

Fix a few issues with the new report tool:

- github credentials not saved correctly
- disable sign in button before signing in
- improve system information to retrieve os name and version on OSX and GNU/Linux

4.7.0
-----

New features:

- New margins mode: you can now configure up to 4 different margins.
- New way to handle MSVC based compilers, including 64 bit support
- Add more command line options to ocide: --compile, --conf, --runtime-env, --cobc-runtime-env
- Add option to save/load preferences.
- Allow to drag & drop paths to the preferences line edits.
- Allow to drag & drop files in the main window to open them in a new editor.
- Add ability to copy runtime dlls to the output directory [Windows].
- Add ability to run a program that requires to set some environment variables.
- Add option to show compiler and runtime configuration.
- Add buttons preferences and about to the home page.
- Add "clean" and "rebuild" actions to the toolbar/menu.
- Add std=nonen, std=cobol2014 and std=acu
- Add a status bar button to forcibly enable/disable the linter (background
  checks), overriding the Show errors setting from the Editor preferences.
- Add option to synchronise navigation pane with the editor
- Add option to go up in the filesystem treeview
- Add ability jump to previous/next cursor position (Ctrl+Alt+Z and Ctrl+Alt+Y)
- Add option to send bug report via email
- Improve github bug report, login to github is now done from OCIDE itself
- Update dbpre integration to work with dbpre 0.4
- Allow to use Shift+Enter from the search/replace panel to search backwards


Fixed bugs:

- Fix broken icon theme selection on GNU/Linux and use more icons from theme
- Use KDE specific build icons
- Fix "failed to decode compiler output with encoding cp1252 with external compilation"
- Fix "log pane (Issues tab): Line breaks from cobc & gcc are not translated correctly"
- Fix various bugs with environment variables and compiler settings
- Fix a few issues related to save as (title not updated after save as,...)
- Fix using extra quotes in compiler flags not working
- Fix using windows paths style in preferences (backslash instead of slash)
- Fix various unhandled exceptions reported by users
- Fix a few issues with file system view (warn user if using a UNC path,
  fix bugs when opening a file that is located at the root of a drive)
- Fix read only property of some fields in the about dialog or the main window (log, compiler output,...)
- Fix cobc warnings treated as errors
- Fix compiler settings not restore if user press Cancel
- Fix a few issues with the offset calculator and some specific types


4.6
===

4.6.6
-----

Improvements:

    - Cancel all compilations if the build errored
    - Prevent the same exception message to be shown more than once during the
      same session

Fixed bugs:

    - Fix a UnicodeDecodeError with the linter on Windows
    - Fix error messages not appearing when using a MSVC based GnuCOBOL.
    - Fix content menu entries not working at mouse position
    - Fix lost of cursor selection after case conversion
    - Fix offset calculator: it now handles lvl 78/88 and redefines
    - Fix a gui bug with offset calculator, disable sorting of items and allow
      user to resize columns.
    - Fix unhandled exception when closing an unsaved editor

4.6.5
-----

New features:

    - Ability to show cursor position in bytes, taking the file encoding into
      account.
    - Excepthook that automatically triggers the bug report tool in case of
      unhandled exception.
    - Simplification of the visual studio wrapper batch

Fixed bugs:

    - Fixed confusing message about compiler not found. The message now
      indicates that a "working compiler" could not be found.
    - Fixed a few typos in the documentation and the readme.
    - Fixed issues with permission errors on compile/save. Now a message box will
      appear to indicate to you that you don't have the permission to
      save/compiler a particular file.
    - Fixed an issue with the issues pane: opening a file with double click
      does not work for relative paths


4.6.4
-----

Fixed bugs:

- fix freeze when compiling a file where column 1-6 are not empty in non-free mode.
- fix detection of submodule when lowercase keywords are used
- fix a few issues with PyQt 5.5

4.6.3
-----

New features:
    - add an option for specifying copybook paths
    - make auto-detect dependencies an optional feature

Fixed bugs:
    - Fix crash on startup if check_compiler failed or if compiler not installed on linux
    - Use full compiler path when VCVARS32 is set
    - Remove duplicates in custom compiler extensions
    - Fix linter errors with relative coybook paths in compiler options
    - Fix compiler preferences not applied on linux/osx
    - Fix misleading tooltip in compiler path line edit
    - Fix info messages interpreted as error messages
    - Fix file recompiled if source is up to date, now the IDE will compare modification time and will skip compilation
      if the source is older than the binary.
    - Fix get_dependencies results: comment should be ignored

4.6.2
-----

Bug fix release - major improvements to the installer on Windows.

New features:
    - [Windows] Installer - own binaries are now digitally signed
    - [Windows] The bundled compiler has been update from OpenCOBOL 1.1 to GnuCOBOL 1.1,
      see the list of differences here: http://opencobol.add1tocobol.com/gnucobol/#what-are-the-differences-between-opencobol-1-1-and-gnucobol-1-1
    - You can now set the full compiler path instead of just the directory (e.g. /usr/bin/cobc instead of /usr/bin)
    - Improve cobc --version parser to include the project name (GnuCOBOL, GnuCOBOL C--,...)
    - Due to a bug with pip and the new wheel package, the executable name on linux is now lowercase (opencobolide instead
      of OpenCobolIDE).
    - Add missing extensions to save as dialog and fix filters of open file dialog.
    - Show a warning before executing restore to factory defaults.

Fixed bugs:
    - Fix installation of desktop files on linux.
    - File system view was still fully reloaded needlessly
    - Fix consistency in cobc commands (sometimes full path were used sometimes not)
    - Fix about dialog closing on [CTRL]
    - Fix an infinite recursion in get_dependencies if a module call itself
    - Fix case of associated compiler extensions
    - Fix a bug with file watcher if a file has been deleted externally and user choose to keep it in the editor
    - Fix unicode decode error when the compiler is broken on windows
    - Fix a few typos
    - Fix creation of temporary files by linter (now they are back to the system temp folder)
    - Fix shortcut conflict: F3 used for both goto and find next. The goto shortcuts has been reassigned to F7
    - Fix misleading/false positive compilation message in case compiler failed but no output was given. Now the IDE
      will remove files before compiling and will check that the expected file has been created before claiming for
      success.

4.6.1
-----

Fix a potential failure on startup (probably just on Windows)

4.6.0
-----

New features/Improvements:
    - Add ability to control where the binaries will be placed
    - Add environment variable settings for compiler (PATH, COBC_CONFIG_DIR,...)
    - Add dialog and a button to check whether your compiler works or not (
      check compilation of a simple hello world)
    - Add .lst to default list of COBOL extensions
    - Add a compiler output log view with the complete output of the cobc
      commands
    - Fix some spelling errors and always use GnuCOBOL instead of OpenCOBOL
    - Show PICTURE in navigation item's tooltip (this works but still requires
      some work on the parser to be fully finished)
    - Add ability to run module with cobcrun


Fixed bugs:
    - Fix a crash with corrupted recent files list
    - Fix fullscreen mode not setup on startup but recognized in the options
    - Fix Ctrl-Home not working as expected (go to first line)
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

4.5
====

4.5.1
------

New features:
    - add ability to set custom file extension association with the different
      compiler (cobc, dbpre and esqlOC)

Fixed bugs:
    - Fix issues with non COBOL files on frozen builds (Windows and OSX only)
    - All bugs fixed in pyqode.core 2.6.1

4.5.0
-----

New features:
    - EOL management (see issue #110)
    - New filter mode for code completion: subsequence
    - Add support for stdeb (ppa packages available)

Fixed bugs:
    - fix a bug with comment (see issue #109)

4.4
===

4.4.0
-----

New features:

  - esqlOC integration (SQL precompiler for windows)

Fixed bugs:
  - fix a bug with the new reporter tool on windows (and get rid of the github3.py dependency)
  - improve comment/uncomment when workin in fixed format and column 1-6 is not empty.

4.3
===

4.3.1
-----

New features:

- new bug report tool that make use of the Github API to submit a new bug report
  using your account that automatically includes system information and
  the application log.

Fixed bugs:

- fix a couple of small bugs in pyqode which should improve the usability (things like
  restoring cursor position after a reload due to an external change, improved auto-completion
  of quotes and parentheses).

4.3.0
-----

New features:

- experimental support for dbpre on Linux
- experimental support for using a custom GnuCOBOL compiler on Windows
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

4.2
===

4.2.0
-----

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


4.1
===

4.1.0
-----

New features:

- add support for GnuCOBOL 2.0 on GNU/Linux
- add a way to specify global compiler switches (-g, -ftrace,...)
- improve detection of external terminal on GNU/Linux
- make use of pyqode-console to prompt for a key press at the end of the
  program when run in an external terminal (Windows - GNU/Linux)
- style improvement: the internal terminal will use the same colors as the
  COBOL editor.
- performance improvement: avoid useless re-highlight on open
- update to pyqode 2.3 (add occurrences highlighting, global checker,
  better selections, smart backspace, auto complete of quotes and
  parentheses, ...)

4.0
===

4.0.0
-----

The entire application has been rewritten.

The COBOL code editor widget has been moved to the pyqode.cobol package.

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

3.0
===

3.0.0
-----

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
- better log message - log window: include information from the log window
  when you report bugs!

Please, read the :doc:`/getting_started` section of this manual to get started
with the new user interface!

2.3
===

2.3.1
-----

- drop python 2 support (the main script must now be run by a python3
  interpreter)

- fix bug with encoding error, see bug #31 on github

2.3.0
-----

New features:

- add ability to run the compiled programe in an external terminal. This is
  useful if you are using the SCREEN SECTION as the embedded terminal does
  not support redirection.

Fixed bugs:

- fix bug with detection of submodules call if they are enclosed with single quotes
  instead of double quotes

2.2
===

2.2.0
-----
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
- improve COBOL parser to support malformed syntax
- fix ambiguous shortcut overload: F2
- fix outline not informative for COBOL files that have data in column 1-6

2.1
===

2.1.0
-----

New features:

- Go to definition for variables and procedures (ctrl - click on symbol)
- New debian package on ppa:open-cobol-ide/stable and ppa:open-cobol-ide/unstable

Fixed Bugs:

- code completion should not occur in comments and strings
- fix column number for navigation panel

2.0
===

2.0.1
-----

Fixed Bugs:

- removed un-needed import of pexpect which caused some issue on clean
  system which does not have pexepect.

2.0.0
-----

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

1.4
===

1.4.2
-----

- Update code so support pcef 0.2.2

1.4.1
-----

Fixed Bugs:

- slow in large files: https://bugs.launchpad.net/cobcide/-bug/1179228

1.4.0
-----

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
  code completion for COBOL
- dirty flag is correctly updated
- scrollbars are now correctly update when folding/unfolding code blocks

1.3
===

1.3.0
-----
This release improves usability with a focus on the run and compile actions:

- detect source dependencies and compile them (if a program P requires a subprogram A who requires a subprogram B than the IDE will compile A, B and P)
- Automatically compile file when the run action is triggered
- Run the last program if the current tab is a subprogram
- Avoid compiling a file that is already compiled and up to date

1.2
===

1.2.1
-----

- Fix bug: https://launchpad.net/cobcide/-milestone/1.2.1

1.2.0
-----

Added:

- Home page with list of recent files
- A settings page to change a few options (mainly related to the editor style)
- A navigation panel to quickly browse large files (tree with div, sections, variables and paragraphs)
- A shortcut to comment/uncomment selected or active lines (ctrl-/)
- On GNU/Linux, at first start the program will ask the user if he wants to create a desktop files

Bug fixes:

- windows path not normalized
- fix bug with mingw when path contains spaces on windows
- fix bug where no extension was proposed when creating a new file

1.1
===

1.1.0
-----

- better encoding detection using chardet
- COBOL specific code completion model
- status bar infos (filename, encoding, cursor position)
- windows port (a windows installer is available in the download section)

1.0
===

1.0.1
-----

- fix packaging issues

1.0.0
-----

- Initial development

.. _issues page on github: https://github.com/OpenCobolIDE/OpenCobolIDE
.. _qdarkstyle: https://github.com/ColinDuquesnoy/QDarkStyleSheet
