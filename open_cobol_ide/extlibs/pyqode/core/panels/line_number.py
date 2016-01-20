# -*- coding: utf-8 -*-
"""
This module contains the line number panel
"""
from pyqode.core.api.panel import Panel
from pyqode.core.api.utils import drift_color, TextHelper
from pyqode.qt import QtCore, QtGui


class LineNumberPanel(Panel):
    """ Displays the document line numbers. """
    def __init__(self):
        Panel.__init__(self)
        self.scrollable = True
        self._selecting = False
        self._start_line = -1
        self._sel_start = -1
        self._line_color_u = self.palette().color(
            QtGui.QPalette.Disabled, QtGui.QPalette.WindowText)
        self._line_color_s = self.palette().color(
            QtGui.QPalette.Normal, QtGui.QPalette.WindowText)

    def sizeHint(self):
        """
        Returns the panel size hint (as the panel is on the left, we only need
        to compute the width
        """
        return QtCore.QSize(self.line_number_area_width(), 50)

    def line_number_area_width(self):
        """
        Computes the lineNumber area width depending on the number of lines
        in the document

        :return: Widtg
        """
        digits = 1
        count = max(1, self.editor.blockCount())
        while count >= 10:
            count /= 10
            digits += 1
        space = 5 + self.editor.fontMetrics().width("9") * digits
        return space

    def mousePressEvent(self, e):
        """
        Starts selecting
        """
        self._selecting = True
        self._sel_start = e.pos().y()
        start = end = TextHelper(self.editor).line_nbr_from_position(
            self._sel_start)
        self._start_line = start
        TextHelper(self.editor).select_lines(start, end)

    def cancel_selection(self):
        """
        Cancels line selection.
        """
        self._selecting = False
        self._sel_start = -1

    def mouseReleaseEvent(self, event):
        # Cancels selection
        self.cancel_selection()

    # def leaveEvent(self, event):
    #     # Cancels selection
    #     self.cancel_selection()

    def mouseMoveEvent(self, e):
        # Updates end of selection if we are currently selecting
        if self._selecting:
            end_pos = e.pos().y()
            end_line = TextHelper(self.editor).line_nbr_from_position(
                end_pos)
            if end_line == -1 and self.editor.visible_blocks:
                # take last visible block
                if end_pos < 50:
                    _, end_line, _ = self.editor.visible_blocks[0]
                    end_line -= 1
                else:
                    _, end_line, _ = self.editor.visible_blocks[-1]
                    end_line += 1
            TextHelper(self.editor).select_lines(self._start_line, end_line)

    def paintEvent(self, event):
        # Paints the line numbers
        self._line_color_u = drift_color(self._background_brush.color(), 250)
        self._line_color_s = drift_color(self._background_brush.color(), 280)
        Panel.paintEvent(self, event)
        if self.isVisible():
            painter = QtGui.QPainter(self)
            # get style options (font, size)
            width = self.width()
            height = self.editor.fontMetrics().height()
            font = self.editor.font()
            bold_font = self.editor.font()
            bold_font.setBold(True)
            pen = QtGui.QPen(self._line_color_u)
            pen_selected = QtGui.QPen(self._line_color_s)
            painter.setFont(font)
            # get selection range
            sel_start, sel_end = TextHelper(self.editor).selection_range()
            has_sel = sel_start != sel_end
            cl = TextHelper(self.editor).current_line_nbr()
            # draw every visible blocks
            for top, line, block in self.editor.visible_blocks:
                if ((has_sel and sel_start <= line <= sel_end) or
                        (not has_sel and cl == line)):
                    painter.setPen(pen_selected)
                    painter.setFont(bold_font)
                else:
                    painter.setPen(pen)
                    painter.setFont(font)
                painter.drawText(-3, top, width, height,
                                 QtCore.Qt.AlignRight, str(line + 1))
