#!/usr/bin/env python3
import os
import shutil
import stat

destination = os.path.abspath(os.path.join('..', '..', '.git', 'hooks'))
for hook in os.listdir():
    if hook not in __file__:
        # make sure the source is executable
        st = os.stat(hook)
        shutil.copy(hook, destination)
        os.chmod(destination, st.st_mode | stat.S_IEXEC)
