"""
This module contains utility functions
"""
import os
from pyqode.qt import QtGui, QtCore, QtWidgets


AST_ICONS = {
    -1: ":/ide-icons/rc/silex-32x32.png",
    0:  ":/ide-icons/rc/division",
    1:  ":/ide-icons/rc/section",
    2:  ":/ide-icons/rc/var",
    3:  ":/ide-icons/rc/paragraph"}


def convert_statement(statement):
    ti = QtWidgets.QTreeWidgetItem()
    ti.setText(0, statement.name)
    ti.setIcon(0, QtGui.QIcon(AST_ICONS[statement.node_type]))
    ti.setToolTip(0, statement.description)
    ti.setData(0, QtCore.Qt.UserRole, statement)
    
    for child in statement.children:
        ti_ch = convert_statement(child)
        ti.addChild(ti_ch)

    return ti


def ast_to_qtree(root_node):
    """
    Converts each node in an AST (returned by the parser module) to a QTreeWidgetItem
    for display.
    """
    return convert_statement(root_node)


def windows_init():
    """
    Windows specific initialisation:

    - set env var to embedded OpenCobol variable
    - set PATH to cobol library path only (erase previous values)
    """
    cwd = os.getcwd()
    oc_root_pth = os.path.join(cwd, "OpenCobol")
    os.environ["COB_CONFIG_DIR"] = os.path.join(oc_root_pth, "config")
    os.environ["COB_COPY_DIR"] = os.path.join(oc_root_pth, "copy")
    os.environ["COB_LIBRARY_PATH"] = os.path.join(oc_root_pth, "bin")
    os.environ["COB_INCLUDE_PATH"] = os.path.join(oc_root_pth, "include")
    os.environ["COB_LIB_PATH"] = os.path.join(oc_root_pth, "lib")
    os.environ["PATH"] = os.environ["COB_LIBRARY_PATH"]


def osx_init():
    # there are some missing paths, see github issue #40
    paths = ['/bin', '/sbin', '/usr/bin', '/usr/sbin', '/usr/local/bin',
             '/usr/local/sbin', '/opt/bin', '/opt/sbin', '/opt/local/bin',
             '/opt/local/sbin']
    os.environ["PATH"] = ':'.join(paths)
