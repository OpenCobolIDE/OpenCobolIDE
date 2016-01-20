#!/usr/bin/env bash
# Require Python 3.4 and PyQt 5.4.2 (does not work with python3.5 and PyQt 5.5)
rm -rf build
python3.4 freeze.py bdist_mac --qt-menu-nib=/Users/Colin/Qt/5.4/clang_64/plugins/platforms
python3.4 freeze.py bdist_dmg
