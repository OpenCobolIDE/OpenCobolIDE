What's New?
===========
This page lists the most prominent milestones achieved by the OpenCobolIDE
developers. For more specific details about what is planned and what has been 
accomplished, please visit the `issues page on github`_.

Next Version
------------

We don't have any plans for the next release. Please report bugs and share
your thoughts to make OpenCobolIDE better.

Milestones
-------------

2.0.1
++++++++++

    - bug fixe: removed uneeded import of pexpect which caused some issue on
                clean system which does not have pexepect.

2.0
++++++++++

This new release is a major update which makes the transition from PCEF to
pyqode. Most of the application has been rewritten from scratch.

Here are the major changes:

    - port to *pyqode 1.0*
    - new compiler errors panel
    - interactive output console for program output
    - uses *PyQt4* in place of *PySide*
    - support for both python 2 and python 3
    - *on the fly* syntax check, OpenCobolIDE compile your code in the
      background to quickly warn you about wrong syntax.
    - better integration with most linux desktop environments (use icons and
      colors from theme, desktop entry). Tested with KDE, Gnome, Unity and
      Cinnamon.
    - allow user to type in lower case (https://github.com/OpenCobolIDE/OpenCobolIDE/issues/1)


1.4.2
+++++++++

  - Update code so support pcef 0.2.2

1.4.1
+++++++++++
Bug fixed:
  - slow in large files: https://bugs.launchpad.net/cobcide/+bug/1179228

1.4
+++++

New features:
  - shortcuts for dock windows (F9: log panel, F10: navigation panel)
  - show fullscreen shortcut change from F12 to F11
  - the application will restore its geometry and state (maximised, dock window positions)

Bug fixed in cobcide:
  - dock panel shown when switching tab: now the panel is only show when coming from the homepage or when compiling
  - crash when editing/compiling files who have unicode characters in their path
  - focus lost when opening recent files from the menu or the homepage on ubuntu 12.04 -> 13.04

Enhancements and bugs fixed in PCEF 0.2.0:
  - improve performances in general
  - support for custom word separator, allow OpenCobolIDE to remove the '-' character from word separators which brings a better
    code completion for cobol
  - dirty flag is correctly updated
  - scrollbars are now correctly update when folding/unfolding code blocks



1.3
+++++++
This release improves usability with a focus on the run and compile actions:
  - detect source dependencies and compile them (if a program P requires a subprogram A who requires a subprogram B than the IDE will compile A, B and P)
  - Automatically compile file when the run action is triggered
  - Run the last program if the current tab is a subprogram
  - Avoid compiling a file that is already compiled and up to date

1.2.1
+++++++++++

  - Fix bug: https://launchpad.net/cobcide/+milestone/1.2.1

1.2
+++++++++

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

1.1
+++++++
   - better encoding detection using chardet
   - cobol specific code completion model
   - status bar infos (filename, encoding, cursor position)
   - windows port (a windows installer is available in the download section)

1.0.1
+++++++++

    - fix packaging issues

1.0
++++++++

    - Initial development

.. _`issues page on github`: https://github.com/OpenCobolIDE/OpenCobolIDE