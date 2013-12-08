#!/usr/bin/env python
"""
Copies necessary files for deb source package building to a
destination directory. The destination directory must not exist
"""
import sys
import shutil

if __name__ == '__main__':
    src_dir = sys.argv[1]
    dest_dir = sys.argv[2]

    def ignorefiles(src, names):
        ret_val = []
        for n in names:
            if ".pyc" in n:
                ret_val.append(n)
        ret_val += ["build", "__pycache__", ".git", "pyqode.core.egg-info",
                    "CMakeLists.txt.user", ".gitignore", ".gitbugtraq",
                    "travis.yml", "test.py" ".idea", "ez_setup.pyc",
                    "examples", "doc", "tools", "screenshots", "rc",
                    "pyqode_core.qrc", "search_panel.ui"]
        return ret_val
    shutil.copytree(src_dir, dest_dir, ignore=ignorefiles)