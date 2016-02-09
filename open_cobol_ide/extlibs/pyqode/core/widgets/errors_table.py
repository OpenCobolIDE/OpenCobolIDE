# -*- coding: utf-8 -*-
"""
Contains a custom QTableWidget for easier displaying of CheckerMessages
"""
from pyqode.core.api.utils import memoized
from pyqode.core.modes import CheckerMessage, CheckerMessages
from pyqode.qt import QtCore, QtWidgets, QtGui


COL_TYPE = 0
COL_FILE_NAME = 1
COL_LINE_NBR = 2
COL_MSG = 3


class ErrorsTable(QtWidgets.QTableWidget):
    """
    Extends a QtWidgets.QTableWidget to easily show
    :class:`pyqode.core.modes.CheckerMessage`.

    You add messages to the table using
    :meth:`pyqode.core.widgets.ErrorsTable.add_message`.

    You clear the table using :meth:`pyqode.core.widgets.ErrorsTable`.
    """
    #: Signal emitted when a message is activated, the clicked signal is passed
    #: as a parameter
    msg_activated = QtCore.Signal(CheckerMessage)

    ICONS = {
        CheckerMessages.INFO: ':pyqode-icons/rc/dialog-info.png',
        CheckerMessages.WARNING: ':pyqode-icons/rc/dialog-warning.png',
        CheckerMessages.ERROR: ':pyqode-icons/rc/dialog-error.png',
    }

    def __init__(self, parent=None):
        QtWidgets.QTableWidget.__init__(self, parent)
        self.setColumnCount(6)
        self.setHorizontalHeaderLabels(
            ["Type", "File name", "Line", "Description", 'Details'])
        try:
            # pyqt4
            self.horizontalHeader().setResizeMode(
                QtWidgets.QHeaderView.ResizeToContents)
            self.horizontalHeader().setResizeMode(
                COL_MSG, QtWidgets.QHeaderView.Stretch)
        except AttributeError:
            # pyqt5
            self.horizontalHeader().setSectionResizeMode(
                QtWidgets.QHeaderView.ResizeToContents)
            self.horizontalHeader().setSectionResizeMode(
                COL_MSG, QtWidgets.QHeaderView.Stretch)
        self.setMinimumSize(900, 200)
        self.itemActivated.connect(self._on_item_activated)
        self.setSelectionMode(self.SingleSelection)
        self.setSelectionBehavior(self.SelectRows)
        self.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_context_menu)
        self.context_mnu = QtWidgets.QMenu()
        self.action_details = QtWidgets.QAction(_('View details'), self)
        self.action_details.triggered.connect(self.showDetails)
        self.action_copy = QtWidgets.QAction(_('Copy error'), self)
        self.action_copy.triggered.connect(self._copy_cell_text)
        self.context_mnu.addAction(self.action_details)
        self.context_mnu.addAction(self.action_copy)
        self.clear()

    def _copy_cell_text(self):
        """
        Copies the description of the selected message to the clipboard
        """
        txt = self.currentItem().text()
        QtWidgets.QApplication.clipboard().setText(txt)

    def _show_context_menu(self, pos):
        """ Shows the context menu """
        self.context_mnu.exec_(self.mapToGlobal(pos))

    def clear(self):
        """
        Clears the tables and the message list
        """
        QtWidgets.QTableWidget.clear(self)
        self.setRowCount(0)
        self.setColumnCount(4)
        self.setHorizontalHeaderLabels(
            ["Type", "File name", "Line", "Description"])

    @classmethod
    @memoized
    def _make_icon(cls, status):
        """
        Make icon from icon filename/tuple (if you want to use a theme)
        """
        icon = cls.ICONS[status]
        if isinstance(icon, tuple):
            return QtGui.QIcon.fromTheme(
                icon[0], QtGui.QIcon(icon[1]))
        elif isinstance(icon, str):
            return QtGui.QIcon(icon)
        elif isinstance(icon, QtGui.QIcon):
            return icon
        else:
            return None

    def add_message(self, msg):
        """
        Adds a checker message to the table.

        :param msg: The message to append
        :type msg: pyqode.core.modes.CheckerMessage
        """
        row = self.rowCount()
        self.insertRow(row)

        # type
        item = QtWidgets.QTableWidgetItem(
            self._make_icon(msg.status), msg.status_string)
        item.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
        item.setData(QtCore.Qt.UserRole, msg)
        self.setItem(row, COL_TYPE, item)

        # filename
        item = QtWidgets.QTableWidgetItem(
            QtCore.QFileInfo(msg.path).fileName())
        item.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
        item.setData(QtCore.Qt.UserRole, msg)
        self.setItem(row, COL_FILE_NAME, item)

        # line
        if msg.line < 0:
            item = QtWidgets.QTableWidgetItem("-")
        else:
            item = QtWidgets.QTableWidgetItem(str(msg.line + 1))
        item.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
        item.setData(QtCore.Qt.UserRole, msg)
        self.setItem(row, COL_LINE_NBR, item)

        # desc
        item = QtWidgets.QTableWidgetItem(msg.description)
        item.setFlags(QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable)
        item.setData(QtCore.Qt.UserRole, msg)
        self.setItem(row, COL_MSG, item)

    def _on_item_activated(self, item):
        """
        Emits the message activated signal
        """
        msg = item.data(QtCore.Qt.UserRole)
        self.msg_activated.emit(msg)

    def showDetails(self):
        """
        Shows the error details.
        """
        msg = self.currentItem().data(QtCore.Qt.UserRole)
        desc = msg.description
        desc = desc.replace('\r\n', '\n').replace('\r', '\n')
        desc = desc.replace('\n', '<br/>')
        QtWidgets.QMessageBox.information(
            self, _('Message details'),
            _("""<p><b>Description:</b><br/>%s</p>
<p><b>File:</b><br/>%s</p>
<p><b>Line:</b><br/>%d</p>
            """) % (desc, msg.path, msg.line + 1, ))
