"""
This module contains the GlobalCheckerPanel.

"""
from pyqode.core import modes
from pyqode.core.api import Panel, TextHelper
from pyqode.qt import QtCore, QtGui


class GlobalCheckerPanel(Panel):
    """ Displays all checker messages found in the document.

    The user can click on a marker to quickly go the the error line.

    """

    def __init__(self):
        super(GlobalCheckerPanel, self).__init__()
        self.scrollable = True

    def _draw_messages(self, painter):
        """
        Draw messages from all subclass of CheckerMode currently
        installed on the editor.

        :type painter: QtGui.QPainter
        """
        checker_modes = []
        for m in self.editor.modes:
            if isinstance(m, modes.CheckerMode):
                checker_modes.append(m)
        for checker_mode in checker_modes:
            for msg in checker_mode.messages:
                block = msg.block
                color = QtGui.QColor(msg.color)
                brush = QtGui.QBrush(color)
                rect = QtCore.QRect()
                rect.setX(self.sizeHint().width() / 4)
                rect.setY(block.blockNumber() * self.get_marker_height())
                rect.setSize(self.get_marker_size())
                painter.fillRect(rect, brush)

    def _draw_visible_area(self, painter):
        """
        Draw the visible area.

        This method does not take folded blocks into account.

        :type painter: QtGui.QPainter
        """
        if self.editor.visible_blocks:
            start = self.editor.visible_blocks[0][-1]
            end = self.editor.visible_blocks[-1][-1]
            rect = QtCore.QRect()
            rect.setX(0)
            rect.setY(start.blockNumber() * self.get_marker_height())
            rect.setWidth(self.sizeHint().width())
            rect.setBottom(end.blockNumber() * self.get_marker_height())
            if self.editor.background.lightness() < 128:
                c = self.editor.background.darker(150)
            else:
                c = self.editor.background.darker(110)
            c.setAlpha(128)
            painter.fillRect(rect, c)

    def paintEvent(self, event):
        """
        Pains the messages and the visible area on the panel.
        :param event: paint event infos
        """
        if self.isVisible():
            # fill background
            self._background_brush = QtGui.QBrush(self.editor.background)
            painter = QtGui.QPainter(self)
            painter.fillRect(event.rect(), self._background_brush)
            self._draw_messages(painter)
            self._draw_visible_area(painter)

    def sizeHint(self):
        """
        The panel has a fixed width of 8 pixels.
        """
        return QtCore.QSize(12, 16)

    def get_marker_height(self):
        """
        Gets the height of message marker.
        """
        return self.editor.viewport().height() / TextHelper(
            self.editor).line_count()

    def get_marker_size(self):
        """
        Gets the size of a message marker.
        :return: QSize
        """
        h = self.get_marker_height()
        if h < 1:
            h = 1
        return QtCore.QSize(self.sizeHint().width() / 2, h)

    def mousePressEvent(self, event):
        # Moves the editor text cursor to the clicked line.
        height = event.pos().y()
        line = height // self.get_marker_height()
        TextHelper(self.editor).goto_line(line)
