The pre-commit hook automatically bumps the dev/alpha/beta/rc version number on each commit.
The post-commit is used to ammend the changes made to __version__.

To use those hooks, copy them to ``.git/hooks`` (or run ``python3 copy_hooks.py``)
