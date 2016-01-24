#! /bin/bash

# This script is meant to be run on Ubuntu 14.04.
#
# Execute the following commands to setup your environment:
#
# sudo apt-get install python3-pip python3-all debhelper devscripts
# sudo pip3 install stdeb
#
pushd ..

export DEB_BUILD_OPTIONS=nocheck debuild
python3.4 setup.py --command-packages=stdeb.command bdist_deb
