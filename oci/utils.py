"""
This module contains utility functions
"""
from PyQt4 import QtGui, QtCore


AST_ICONS = {
    -1: ":/ide-icons/rc/silex-32x32.png",
    0:  ":/ide-icons/rc/division",
    1:  ":/ide-icons/rc/section",
    2:  ":/ide-icons/rc/var",
    3:  ":/ide-icons/rc/paragraph"}


def convert_statement(statement):
    ti = QtGui.QTreeWidgetItem()
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
