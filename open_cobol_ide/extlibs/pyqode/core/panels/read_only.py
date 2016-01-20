"""
Contains a panel to manage unicode decoding/encoding errors.

"""
from pyqode.core.api.panel import Panel
from pyqode.core.api.decoration import TextDecoration
from pyqode.qt import QtCore, QtGui, QtWidgets


class ReadOnlyPanel(Panel):
    """ Displays a message if the opened file is read-only """

    @property
    def color(self):
        """
        Returns the panel color.
        """
        return self._color

    @color.setter
    def color(self, value):
        self._color = value
        self._refresh_stylesheet()
        if self.editor:
            # propagate changes to every clone
            for clone in self.editor.clones:
                try:
                    clone.modes.get(self.__class__).color = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    @property
    def foreground(self):
        return self._foreground

    @foreground.setter
    def foreground(self, value):
        self._foreground = value
        self._refresh_stylesheet()
        # propagate changes to every clone
        if self.editor:
            for clone in self.editor.clones:
                try:
                    clone.modes.get(self.__class__).foreground = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    def _refresh_stylesheet(self):
        try:
            self._lbl_stylesheet = ('color: %s;background: %s' %
                                    (self._foreground.name(),
                                     self._color.name()))
            self.ui.lblDescription.setStyleSheet(self._lbl_stylesheet)
        except AttributeError:
            pass

    def __init__(self):
        super(ReadOnlyPanel, self).__init__(dynamic=True)
        # leave it here otherwise you will have circular import errors
        from pyqode.core._forms.pnl_read_only_ui import Ui_Form
        self.ui = Ui_Form()
        self.ui.setupUi(self)
        self._color = None
        self.color = QtGui.QColor('#8AADD4')
        self._foreground = None
        self.foreground = QtGui.QColor('#FFFFFF')
        self.hide()

    def paintEvent(self, event):
        """ Fills the panel background. """
        super(ReadOnlyPanel, self).paintEvent(event)
        if self.isVisible():
            # fill background
            painter = QtGui.QPainter(self)
            self._background_brush = QtGui.QBrush(self._color)
            painter.fillRect(event.rect(), self._background_brush)
