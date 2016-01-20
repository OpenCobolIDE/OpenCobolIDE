"""
Checker panels:

    - CheckerPanel: draw checker messages in front of each line
    - GlobalCheckerPanel: draw all checker markers as colored rectangle to
      offer a global view of all errors
"""
from pyqode.core import icons
from pyqode.core.api import DelayJobRunner, TextHelper, CodeEdit
from pyqode.core.api.panel import Panel, _logger
from pyqode.core.modes.checker import CheckerMessages
from pyqode.qt import QtCore, QtGui, QtWidgets


class CheckerPanel(Panel):
    """ Shows messages collected by one or more checker modes """
    def __init__(self):
        super(CheckerPanel, self).__init__()
        self._previous_line = -1
        self.scrollable = True
        self._job_runner = DelayJobRunner(delay=100)
        self.setMouseTracking(True)
        #: Info icon
        self.info_icon = icons.icon(
            'dialog-info', ':pyqode-icons/rc/dialog-info.png',
            'fa.info-circle', qta_options={'color': '#4040DD'})
        self.warning_icon = icons.icon(
            'dialog-warning', ':pyqode-icons/rc/dialog-warning.png',
            'fa.exclamation-triangle', qta_options={'color': '#DDDD40'})
        self.error_icon = icons.icon(
            'dialog-error', ':pyqode-icons/rc/dialog-error.png',
            'fa.exclamation-circle', qta_options={'color': '#DD4040'})

    def marker_for_line(self, line):
        """
        Returns the marker that is displayed at the specified line number if
        any.

        :param line: The marker line.

        :return: Marker of None
        :rtype: pyqode.core.Marker
        """
        block = self.editor.document().findBlockByNumber(line)
        try:
            return block.userData().messages
        except AttributeError:
            return []

    def sizeHint(self):
        """
        Returns the panel size hint. (fixed with of 16px)
        """
        metrics = QtGui.QFontMetricsF(self.editor.font())
        size_hint = QtCore.QSize(metrics.height(), metrics.height())
        if size_hint.width() > 16:
            size_hint.setWidth(16)
        return size_hint

    def on_uninstall(self):
        self._job_runner.cancel_requests()
        super(CheckerPanel, self).on_uninstall()

    def paintEvent(self, event):
        super(CheckerPanel, self).paintEvent(event)
        painter = QtGui.QPainter(self)
        for top, block_nbr, block in self.editor.visible_blocks:
            user_data = block.userData()
            if user_data and user_data.messages:
                for msg in user_data.messages:
                    icon = self._icon_from_message(msg)
                    if icon:
                        rect = QtCore.QRect()
                        rect.setX(0)
                        rect.setY(top)
                        rect.setWidth(self.sizeHint().width())
                        rect.setHeight(self.sizeHint().height())
                        icon.paint(painter, rect)

    def _icon_from_message(self, message):
        icons = {
            CheckerMessages.INFO: self.info_icon,
            CheckerMessages.WARNING: self.warning_icon,
            CheckerMessages.ERROR: self.error_icon
        }
        return icons[message.status]

    def mouseMoveEvent(self, event):
        # Requests a tooltip if the cursor is currently over a marker.
        line = TextHelper(self.editor).line_nbr_from_position(event.pos().y())
        if line:
            markers = self.marker_for_line(line)
            text = '\n'.join([marker.description for marker in markers if
                              marker.description])
            if len(markers):
                if self._previous_line != line:
                    top = TextHelper(self.editor).line_pos_from_number(
                        markers[0].line)
                    if top:
                        self._job_runner.request_job(self._display_tooltip,
                                                     text, top)
            else:
                self._job_runner.cancel_requests()
            self._previous_line = line

    def leaveEvent(self, *args):
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
