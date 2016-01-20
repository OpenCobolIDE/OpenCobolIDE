# -*- coding: utf-8 -*-
"""
This module contains the right margin mode.
"""
from pyqode.core.api import TextHelper
from pyqode.core.api import Mode
from pyqode.qt import QtGui


class RightMarginMode(Mode):
    """ Displays a right margin at column the specified position.

    """
    @property
    def color(self):
        """
        Gets/sets the color of the margin
        """
        return self._color

    @color.setter
    def color(self, value):
        self._color = value
        self._pen = QtGui.QPen(self._color)
        TextHelper(self.editor).mark_whole_doc_dirty()
        self.editor.repaint()
        if self.editor:
            for clone in self.editor.clones:
                try:
                    clone.modes.get(self.__class__).color = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    @property
    def position(self):
        """
        Gets/sets the position of the margin
        """
        return self._margin_pos

    @position.setter
    def position(self, value):
        self._margin_pos = value
        if self.editor:
            for clone in self.editor.clones:
                try:
                    clone.modes.get(self.__class__).position = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    def __init__(self):
        Mode.__init__(self)
        self._margin_pos = 79
        self._color = QtGui.QColor('red')
        self._pen = QtGui.QPen(self._color)

    def on_state_changed(self, state):
        """
        Connects/Disconnects to the painted event of the editor

        :param state: Enable state
        """
        if state:
            self.editor.painted.connect(self._paint_margin)
            self.editor.repaint()
        else:
            self.editor.painted.disconnect(self._paint_margin)
            self.editor.repaint()

    def _paint_margin(self, event):
        """ Paints the right margin after editor paint event. """
        font = QtGui.QFont(self.editor.font_name, self.editor.font_size +
                           self.editor.zoom_level)
        metrics = QtGui.QFontMetricsF(font)
        pos = self._margin_pos
        offset = self.editor.contentOffset().x() + \
            self.editor.document().documentMargin()
        x80 = round(metrics.width(' ') * pos) + offset
        painter = QtGui.QPainter(self.editor.viewport())
        painter.setPen(self._pen)
        painter.drawLine(x80, 0, x80, 2 ** 16)

    def clone_settings(self, original):
        self.color = original.color
        self.position = original.position
