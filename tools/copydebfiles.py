#!/usr/bin/env python3
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
        return ["build", ".git", ".gitignore", ".gitbugtraq",
                "travis.yml", "test.py" ".idea", "ez_setup.pyc",
                "ez_setup.py", "freez.py", "dlg_about.ui", "dlg_file_type.ui",
                "dlg_preferences.ui", "ide.ui", "ide.qrc", "OpenCobol", "doc",
                "oci_designer_plugins", "__pycache__",
                "examples", "doc", "tools", "rc"]
    shutil.copytree(src_dir, dest_dir, ignore=ignorefiles)
