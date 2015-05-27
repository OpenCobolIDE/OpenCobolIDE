#!/usr/bin/env python3
import os
import shutil
import stat

destination = os.path.abspath(os.path.join('..', '..', '.git', 'hooks'))
for hook in os.listdir():
    if hook not in __file__:
        # make sure the source is executable
        st = os.stat(hook)
        os.chmod(hook, st.st_mode | stat.S_IEXEC)
        shutil.copy(hook, destination)
