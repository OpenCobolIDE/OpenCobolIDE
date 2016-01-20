# -*- coding: utf-8 -*-
"""
This module contains the right margin mode.
"""
from pyqode.core.api import TextHelper
from pyqode.core.api import Mode
from pyqode.qt import QtCore, QtGui


class MarginsMode(Mode):
    """
    Displays four margins at the specified columns.
    """

    @property
    def colors(self):
        """
        Gets/sets the colors of the 4 margins
        """
        return self._colors

    @colors.setter
    def colors(self, value):
        self._colors = value
        self._pens = [QtGui.QPen(c) for c in value]
        TextHelper(self.editor).mark_whole_doc_dirty()
        self.editor.repaint()
        if self.editor:
            for clone in self.editor.clones:
                try:
                    clone.modes.get(self.__class__).colors = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    @property
    def positions(self):
        """
        Gets/sets the 4 position of the margins (tuple made up from 4
        integers).

        The positions are 0 based (use 0 for column 1,...).
        """
        return self._positions

    @positions.setter
    def positions(self, value):
        self._positions = value
        if self.editor:
            for clone in self.editor.clones:
                try:
                    clone.modes.get(self.__class__).position = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    def __init__(self):
        super(MarginsMode, self).__init__()
        self._positions = [7, 11, 72, 79]
        self._colors = [QtGui.QColor('red') for _ in range(4)]
        self._pens = [QtGui.QPen(c) for c in self._colors]

    def on_state_changed(self, state):
        """
        Connects/Disconnects to the painted event of the editor.

        :param state: Enable state
        """
        if state:
            self.editor.painted.connect(self._paint_margins)
            self.editor.key_pressed.connect(self._on_key_pressed)
            self.editor.repaint()
        else:
            self.editor.painted.disconnect(self._paint_margins)
            self.editor.key_pressed.disconnect(self._on_key_pressed)
            self.editor.repaint()

    def _paint_margins(self, event):
        """ Paints the right margin after editor paint event. """
        font = QtGui.QFont(self.editor.font_name, self.editor.font_size +
                           self.editor.zoom_level)
        metrics = QtGui.QFontMetricsF(font)
        painter = QtGui.QPainter(self.editor.viewport())
        for pos, pen in zip(self._positions, self._pens):
            if pos < 0:
                # margin not visible
                continue
            offset = self.editor.contentOffset().x() + \
                self.editor.document().documentMargin()
            x_pos = round(metrics.width(' ') * pos) + offset
            painter.setPen(pen)
            painter.drawLine(x_pos, 0, x_pos, 2 ** 16)

    def _on_key_pressed(self, event):
        Qt = QtCore.Qt
        modifier_match = int(event.modifiers()) == \
            Qt.ControlModifier + Qt.AltModifier
        if event.key() == Qt.Key_Left and modifier_match:
            self._go_to_previous_margin()
            event.accept()
        elif event.key() == Qt.Key_Right and modifier_match:
            self._go_to_next_margin()

            event.accept()

    def _go_to_next_margin(self):
        tc = self.editor.textCursor()
        max_column = len(tc.block().text())
        current_column = TextHelper(self.editor).current_column_nbr()
        next_column = max_column
        for p in self._positions:
            if p < 0:
                continue
            if p > current_column:
                next_column = p
                if next_column > max_column:
                    next_column = max_column
                break
        tc.movePosition(tc.Right, tc.MoveAnchor, next_column - current_column)
        self.editor.setTextCursor(tc)

    def _go_to_previous_margin(self):
        tc = self.editor.textCursor()
        min_column = 1
        current_column = TextHelper(self.editor).current_column_nbr()
        next_column = min_column
        for p in reversed(self._positions):
            if p < 0:
                continue
            if p < current_column:
                next_column = p
                if next_column < min_column:
                    next_column = min_column
                break
        tc.movePosition(tc.Left, tc.MoveAnchor, current_column - next_column)
        self.editor.setTextCursor(tc)

    def clone_settings(self, original):
        self.colors = original.colors
        self.positions = original.positions
