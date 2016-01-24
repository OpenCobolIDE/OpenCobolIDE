#! /bin/bash

# install deps: sudo apt-get install python3-stdeb

pushd ..

export DEB_BUILD_OPTIONS=nocheck debuild
python3.4 setup.py --command-packages=stdeb.command bdist_deb
