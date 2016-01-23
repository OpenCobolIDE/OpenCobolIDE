Contributing
============

OpenCobolIDE is an open-source project, and it needs your help to go on growing
and improving.

You can contribute in many different ways:

    - reports new bugs/features on the `issue tracker`_
    - fork the project and send pull requests to merge your work

If you have any question, just open an issue on the issue tracker.

It might also be worth checking the OpenCobolIDE's `wiki`_ (for developers and contributors).

Since v4.3.1, you can report a bug from within the IDE (? -> Report a bug). The first time
you use it, it will prompt you for your github credentials in order to generate a personal
access token that is needed to submit a bug report using your credentials. This tool
will automatically send the system information and the application log, you just have
to write a short title and description.


.. _`issue tracker`: https://github.com/OpenCobolIDE/OpenCobolIDE/issues
.. _`wiki`: https://github.com/OpenCobolIDE/OpenCobolIDE/wiki

Recommendation for submitting a bug report:
-------------------------------------------

To submit a valuable bug report, the following information is required:

    - Operating System
    - Desktop Environment (for GNU/Linux only)
    - Describe the way you installed the IDE (for GNU/Linux only): pip or apt-get or another method?
    - Include the Application log output content (*?->About OpenCobolIDE->Log*)

Starting from v4.2, there is a report bug menu action (*?->Report a bug*) that will open
your web browser to the github bug tracker with a prefilled report template.

Recommendations for submitting pull requests:
---------------------------------------------

Pull Requests are great!

1. Fork the Repo on github.
2. Create a feature or a bugfix branch before you start coding.
3. If you are adding functionality or fixing a bug, please add a test!
4. Add your name to AUTHORS.rst
5. Push to your fork and submit a pull request to **the master branch**.

Please use **PEP8** to style your code (PEP8 compliance is tested Travis CI).

*You can check pep8 compliance before pushing by running the test suite with
the --pep8 option*::

    ($ pip3 install pytest-pep8)
    $ python3 runtests.py --pep8 -m pep8
