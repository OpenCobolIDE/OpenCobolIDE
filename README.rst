OpenCobolIDE
--------------------
.. image:: https://travis-ci.org/OpenCobolIDE/OpenCobolIDE.png?branch=master
    :target: https://travis-ci.org/ColinDuquesnoy/pyqode.python
    :alt: Travis-CI build status
    
.. image:: https://pypip.in/d/OpenCobolIDE/badge.png
    :target: https://crate.io/packages/OpenCobolIDE/
    :alt: Number of PyPI downloads

.. image:: https://pypip.in/v/OpenCobolIDE/badge.png
    :target: https://crate.io/packages/OpenCobolIDE/
    :alt: Latest PyPI version

OpenCobolIDE is a simple and lightweight cobol IDE based on the OpenCobol
compiler.


The software is written in Python using PyQt4 and pyQode

Features:
    - syntax highlighting
    - code completion
    - code folding
    - compile as program (.exe) or as subprogram (.so)
    - run the program from the editor
    - also open text files


License
--------------------

OpenCobolIDE is released under the **GPL** version 3


Installation
--------------------

GNU/Linux
############

From PPA
++++++++++++++++

There is now a PPA for Ubuntu and derivatives::

    $ sudo apt-add-repositiory ppa:open-cobol-ide/stable
    $ sudo apt-get update
    $ sudo apt-get install open-cobol-ide

From pip
++++++++++++
Run the following commands::

    sudo apt-get install python-qt open-cobol
    sudo easy_install OpenCobolIDE


Windows
############

There is a windows installer available here: https://launchpad.net/cobcide/+download


Requirements
--------------------

The project depends on the following packages:

    - Python 2.7 or Python 3
    - setuptools
    - PyQt4
    - OpenCobol
    - pyqode.core
    - pyqode.widgets
    - Pygments >= 1.6
    - chardet (chardet2 if you are using python3)


Resources
---------

-  `Downloads`_
-  `Source repository`_
-  `Issue tracker`_
-  `Documentation`_

.. _Downloads: https://github.com/ColinDuquesnoy/OpenCobolIDE/releases
.. _Source repository: https://github.com/ColinDuquesnoy/OpenCobolIDE/
.. _Issue tracker: https://github.com/ColinDuquesnoy/OpenCobolIDE/issues?state=open
.. _`Documentation`: http://opencobolide.readthedocs.org/en/latest/


Disclaimer
--------------------

I am by no way a cobol expert, I just had to work on a cobol project at school
with an awful IDE (NetExpress on a Windows Xp virtual machine). As I was writing
pyqode, I thought it would be nice to use it to make a simple cobol editor for
GNU/Linux.

I've only learnt COBOL 74 and the IDE has been thought with this standard in
mind. I have personally no plan on working with cobol at the moment nor to learn
a new standard but if you found a missing feature, feel free to open a feature
request. I'm always looking forward to make OpenCobolIDE better for the experts.

Your advice will be very appreciated!


Screenshots
-------------

* Home page:

.. image:: https://raw.github.com/ColinDuquesnoy/OpenCobolIDE/develop/doc/source/_static/Home.png
    :align: center

* Editor:

.. image:: https://raw.github.com/ColinDuquesnoy/OpenCobolIDE/develop/doc/source/_static/CompilerOutput.png
    :align: center





.. image:: https://d2weczhvl823v0.cloudfront.net/ColinDuquesnoy/opencobolide/trend.png
   :alt: Bitdeli badge
   :target: https://bitdeli.com/free

