"""

"""
import sys
import os
from PyQt5.QtCore import QLibraryInfo
from setuptools import setup
# OSX setup with py2app
APP = ['OpenCobolIDE']
DATA_FILES = []
OPTIONS = {'argv_emulation': False,
           'iconfile': 'share/silex-icon.icns'}

sys.argv += ['py2app', '-A', '--packages=PyQt5']

setup(
    app=APP,
    data_files=DATA_FILES,
    options={'py2app': OPTIONS},
    setup_requires=['py2app'],
)



plugin_path = QLibraryInfo.location(QLibraryInfo.PluginsPath)
os.system('cp -R %s dist/OpenCobolIDE.app/Contents/PlugIns' % plugin_path)
macdeployqt = plugin_path.replace('plugins', 'bin/macdeployqt')
os.system('%s dist/OpenCobolIDE.app' % macdeployqt)

os.system('hdiutil create dist/OpenCobolIDE.dmg -srcfolder dist/OpenCobolIDE.app')
