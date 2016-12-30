#!/usr/bin/env python3
"""
This scripts freeze the application and try to run the inno setup compiler
to create an setup executable.
"""
import glob
import os
os.chdir('..')
import shutil
import sys
from cx_Freeze import setup, Executable
from pyqode.core.tools import console
from open_cobol_ide import __version__, backend
from pyqode.core.backend import server as core_server
from pyqode.core.api.syntax_highlighter import get_all_styles
from pygments import lexers

version = __version__
if '.dev' in version or '.a' in version or '.b' in version or '.rc' in version:
    version = version[:version.index('.dev')]

# Detect system
windows = sys.platform == 'win32'
osx = sys.platform == 'darwin'

app_script = 'OpenCobolIDE.pyw'
app_name = 'OpenCobolIDE'
app_exe = 'OpenCobolIDE.exe' if windows else 'OpenCobolIDE'

# cobol backend
srv_script = backend.__file__
srv_exe = 'cobol-backend.exe' if windows else 'cobol-backend'

# core backend (for non cobol files completion).
core_srv_script = core_server.__file__
core_srv_exe = 'core-backend.exe' if windows else 'core-backend'

# pyqode console (for run in terminal)
console_script = console.__file__
console_name = 'pyqode-console'
console_exe = 'pyqode-console.exe' if windows else 'pyqode-console'


app_icon = 'forms/rc/silex-icon.ico' if windows else 'share/silex-icon.icns'


if len(sys.argv) == 1:
    sys.argv.append('build')


# collect pygments styles
pygments_styles = []
for s in get_all_styles():
    module = 'pygments.styles.%s' % s.replace('-', '_')
    try:
        __import__(module)
    except ImportError:
        pass
    else:
        pygments_styles.append(module)
print('pygment styles', pygments_styles)

pygments_lexers = []
lexers_dir = os.path.dirname(lexers.__file__)
for file in os.listdir(lexers_dir):
    name = os.path.splitext(file)[0]
    if name not in ['__pycache__']:
        pygments_lexers.append('pygments.lexers.%s' % name)
print('pygment lexers', pygments_lexers)


# build options
options = {
    'namespace_packages': ['pyqode'],
    'include_msvcr': True,
    # freeze the pygments default style along with our executable
    'includes': [
        'pkg_resources',
        'keyring.backends.kwallet',
        'keyring.backends.OS_X',
        'keyring.backends.SecretService',
        'keyring.backends.Windows',
        'six',
        'packaging',
        'packaging.markers',
        'packaging.specifiers',
        'packaging.version',
        'packaging.version',
        'packaging._compat',
        'packaging._structures',
        'packaging.__about__',
    ] + pygments_styles + pygments_lexers
}

if windows:
    options['includes'].append('win32timezone')


print(
    '### Freezing application\n'
    '#####################################################################\n')
setup(name=app_name,
      version=version,
      options={'build_exe': options, 'bdist_mac': {
          'iconfile': app_icon, 'custom_info_plist': 'share/Info.plist'}},
      executables=[
          Executable(app_script,
                     targetName=app_exe,
                     icon=app_icon if windows else None,
                     base='Win32GUI' if windows else None),
          Executable(console_script, targetName=console_exe),
          Executable(srv_script, targetName=srv_exe),
          Executable(core_srv_script, targetName=core_srv_exe)
      ])

if windows:
    build_dir = os.path.join(os.getcwd(), glob.glob('build/*')[0])
    # cx_freeze is missing libEGL.dll, copy it ourself
    try:
        import PyQt5
        shutil.copy(os.path.join(
            os.path.dirname(PyQt5.__file__), 'libEGL.dll'), build_dir)
    except (ImportError, RuntimeError):
        pass  # using pyqt4, there is nothing to do
    print('Signing our binaries\n'
          '#####################################################################')
    for executable in ['OpenCobolIDE.exe', 'cobol-backend.exe', 'core-backend.exe',
                       'pyqode-console.exe']:
        path = os.path.join(build_dir, executable)
        os.system('c:\DigiCertUtil.exe sign /noInput %s' % path)
    print(
        '### Copying OpenCobol distribution\n'
        '#####################################################################'
        '\n')
    cobc_dir = os.path.join(build_dir, 'GnuCOBOL')
    if not os.path.exists(cobc_dir):
        shutil.copytree(os.path.join(os.getcwd(), 'GnuCOBOL-Win32-MinGW'),
                        os.path.join(build_dir, 'GnuCOBOL'))

    print(
        '\n### Creating windows installer using Inno Setup\n'
        '#####################################################################'
        '\n')
    build_dir = glob.glob('build/*')[0]
    with open('scripts/setup.iss.in', 'r') as src, \
            open('setup.iss', 'w') as dst:
        lines = src.readlines()
        data = []
        for l in lines:
            l = l.replace('@VERSION@', version)
            l = l.replace('@BUILD_DIR@', build_dir)
            data.append(l)
        dst.writelines(data)
    # if not in PATH, inno setup is usually located in
    # C:\Program Files (x86)\Inno Setup 5 on Windows
    os.environ['PATH'] += ';C:\Program Files (x86)\Inno Setup 5'
    os.system('iscc %s' % os.path.join(os.getcwd(), 'setup.iss'))
