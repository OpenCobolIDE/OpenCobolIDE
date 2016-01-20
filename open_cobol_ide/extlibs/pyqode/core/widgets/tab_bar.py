"""
This module contains the tab bar used in the splittable tab widget.
"""
from pyqode.qt import QtWidgets, QtCore
from pyqode.core.api import DelayJobRunner


class TabBar(QtWidgets.QTabBar):
    """
    Tab bar specialized to allow the user to close a tab using mouse middle
    click. Also exposes a double clicked signal.
    """
    double_clicked = QtCore.Signal()

    def __init__(self, parent):
        QtWidgets.QTabBar.__init__(self, parent)
        self.setTabsClosable(True)
        self._timer = DelayJobRunner(delay=1)

    def mousePressEvent(self, event):
        QtWidgets.QTabBar.mousePressEvent(self, event)
        if event.button() == QtCore.Qt.MiddleButton:
            tab = self.tabAt(event.pos())
            self._timer.request_job(
                self.parentWidget().tabCloseRequested.emit, tab)

    def mouseDoubleClickEvent(self, event):
        if event.button() == QtCore.Qt.LeftButton:
            self.double_clicked.emit()
