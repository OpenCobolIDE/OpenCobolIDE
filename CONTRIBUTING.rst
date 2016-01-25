Report bugs or ask a question
-----------------------------

You can report bugs or ask question on our `bug tracker`_.

Since v4.3.1, you can report a bug from within the IDE (? -> Report a bug). The first time
you use it, it will prompt you for your github credentials in order to generate a personal
access token that is needed to submit a bug report using your credentials. This tool
will automatically send the system information and the application log, you just have
to write a short title and description.


Submitting pull requests:
-------------------------

Pull Requests are great!

1. Fork the Repo on github.
2. Create a feature or a bugfix branch before you start coding.
3. If you are adding functionality or fixing a bug, please add a test!
4. Add your name to AUTHORS.rst
5. Push to your fork and submit a pull request to **the master branch**.

Please use **PEP8** to style your code (PEP8 compliance is tested Travis CI)::

    python setup.py test -a "--pep8 -m pep8"

.. _bug tracker: https://github.com/OpenCobolIDE/OpenCobolIDE/issues?state=open
