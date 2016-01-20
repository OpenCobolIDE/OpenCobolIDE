# -*- coding: utf-8 -*-
"""
This module contains the marker panel
"""
import logging

from pyqode.core.api import TextDecoration
from pyqode.core.api.panel import Panel
from pyqode.core.api.utils import DelayJobRunner, TextHelper
from pyqode.qt import QtCore, QtWidgets, QtGui


def _logger():
    """ Gets module's logger """
    return logging.getLogger(__name__)


class Marker(QtCore.QObject):
    """
    A marker is an icon draw on a marker panel at a specific line position and
    with a possible tooltip.
    """

    @property
    def position(self):
        """
        Gets the marker position (line number)
        :type: int
        """
        try:
            return self.block.blockNumber()
        except AttributeError:
            return self._position  # not added yet

    @property
    def icon(self):
        """
        Gets the icon file name. Read-only.
        """
        if isinstance(self._icon, str):
            if QtGui.QIcon.hasThemeIcon(self._icon):
                return QtGui.QIcon.fromTheme(self._icon)
            else:
                return QtGui.QIcon(self._icon)
        elif isinstance(self._icon, tuple):
            return QtGui.QIcon.fromTheme(self._icon[0],
                                         QtGui.QIcon(self._icon[1]))
        elif isinstance(self._icon, QtGui.QIcon):
            return self._icon
        return QtGui.QIcon()

    @property
    def description(self):
        """ Gets the marker description. """
        return self._description

    def __init__(self, position, icon="", description="", parent=None):
        """
        :param position: The marker position/line number.
        :type position: int

        :param icon: The icon to display
        :type icon: QtGui.QIcon

        :param parent: The optional parent object.
        :type parent: QtCore.QObject or None
        """
        QtCore.QObject.__init__(self, parent)
        #: The position of the marker (line number)
        self._position = position
        self._icon = icon
        self._description = description


class MarkerPanel(Panel):
    """
    General purpose marker panel.
    This panels takes care of drawing icons at a specific line number.

    Use addMarker, removeMarker and clearMarkers to manage the collection of
    displayed makers.

    You can create a user editable panel (e.g. a breakpoints panel) by using
    the following signals:

      - :attr:`pyqode.core.panels.MarkerPanel.add_marker_requested`
      - :attr:`pyqode.core.panels.MarkerPanel.remove_marker_requested`

    """
    #: Signal emitted when the user clicked in a place where there is no
    #: marker.
    add_marker_requested = QtCore.Signal(int)
    #: Signal emitted when the user right clicked on an existing marker.
    edit_marker_requested = QtCore.Signal(int)
    #: Signal emitted when the user left clicked on an existing marker.
    remove_marker_requested = QtCore.Signal(int)

    @property
    def background(self):
        """
        Marker background color in editor. Use None if no text decoration
        should be used.
        """
        return self._background

    @background.setter
    def background(self, value):
        self._background = value

    def __init__(self):
        Panel.__init__(self)
        self._background = QtGui.QColor('#FFC8C8')
        self._markers = []
        self._icons = {}
        self._previous_line = -1
        self.scrollable = True
        self._job_runner = DelayJobRunner(delay=100)
        self.setMouseTracking(True)
        self._to_remove = []

    @property
    def markers(self):
        """
        Gets all markers.
        """
        return self._markers

    def add_marker(self, marker):
        """
        Adds the marker to the panel.

        :param marker: Marker to add
        :type marker: pyqode.core.modes.Marker
        """
        self._markers.append(marker)
        doc = self.editor.document()
        assert isinstance(doc, QtGui.QTextDocument)
        block = doc.findBlockByLineNumber(marker._position)
        marker.block = block
        d = TextDecoration(block)
        d.set_full_width()
        if self._background:
            d.set_background(QtGui.QBrush(self._background))
            marker.decoration = d
        self.editor.decorations.append(d)
        self.repaint()

    def remove_marker(self, marker):
        """
        Removes a marker from the panel

        :param marker: Marker to remove
        :type marker: pyqode.core.Marker
        """
        self._markers.remove(marker)
        self._to_remove.append(marker)
        if hasattr(marker, 'decoration'):
            self.editor.decorations.remove(marker.decoration)
        self.repaint()

    def clear_markers(self):
        """ Clears the markers list """
        while len(self._markers):
            self.remove_marker(self._markers[0])

    def marker_for_line(self, line):
        """
        Returns the marker that is displayed at the specified line number if
        any.

        :param line: The marker line.

        :return: Marker of None
        :rtype: pyqode.core.Marker
        """
        markers = []
        for marker in self._markers:
            if line == marker.position:
                markers.append(marker)
        return markers

    def sizeHint(self):
        """
        Returns the panel size hint. (fixed with of 16px)
        """
        metrics = QtGui.QFontMetricsF(self.editor.font())
        size_hint = QtCore.QSize(metrics.height(), metrics.height())
        if size_hint.width() > 16:
            size_hint.setWidth(16)
        return size_hint

    def paintEvent(self, event):
        Panel.paintEvent(self, event)
        painter = QtGui.QPainter(self)
        for top, block_nbr, block in self.editor.visible_blocks:
            for marker in self._markers:
                if marker.block == block and marker.icon:
                    rect = QtCore.QRect()
                    rect.setX(0)
                    rect.setY(top)
                    rect.setWidth(self.sizeHint().width())
                    rect.setHeight(self.sizeHint().height())
                    marker.icon.paint(painter, rect)

    def mousePressEvent(self, event):
        # Handle mouse press:
        # - emit add marker signal if there were no marker under the mouse
        #   cursor
        # - emit remove marker signal if there were one or more markers under
        #   the mouse cursor.
        line = TextHelper(self.editor).line_nbr_from_position(event.pos().y())
        if self.marker_for_line(line):
            if event.button() == QtCore.Qt.LeftButton:
                self.remove_marker_requested.emit(line)
            else:
                self.edit_marker_requested.emit(line)
        else:
            self.add_marker_requested.emit(line)

    def mouseMoveEvent(self, event):
        # Requests a tooltip if the cursor is currently over a marker.
        line = TextHelper(self.editor).line_nbr_from_position(event.pos().y())
        markers = self.marker_for_line(line)
        text = '\n'.join([marker.description for marker in markers if
                          marker.description])
        if len(markers):
            if self._previous_line != line:
                top = TextHelper(self.editor).line_pos_from_number(
                    markers[0].position)
                if top:
                    self._job_runner.request_job(self._display_tooltip,
                                                 text, top)
        else:
            self._job_runner.cancel_requests()
        self._previous_line = line

    def leaveEvent(self, *args, **kwargs):
        """
        Hide tooltip when leaving the panel region.
        """
        QtWidgets.QToolTip.hideText()
        self._previous_line = -1

    def _display_tooltip(self, tooltip, top):
        """
        Display tooltip at the specified top position.
        """
        QtWidgets.QToolTip.showText(self.mapToGlobal(QtCore.QPoint(
            self.sizeHint().width(), top)), tooltip, self)
