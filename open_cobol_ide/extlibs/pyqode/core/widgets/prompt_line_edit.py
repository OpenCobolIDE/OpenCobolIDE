"""
This module contains the PromptLineEdit widget implementation.

"""
import os
from pyqode.core import icons
from pyqode.qt import QtWidgets, QtCore, QtGui
from pyqode.qt import PYQT5_API
from pyqode.qt import PYQT4_API
from pyqode.qt import PYSIDE_API


class PromptLineEdit(QtWidgets.QLineEdit):

    """
    Extends QLineEdit to show a prompt text and a clear icon
    """

    #: Signal emitted when the embedded button is clicked
    clear_clicked = QtCore.Signal()

    def __init__(self, parent=None,
                 prompt_text=_(' Search'), button_icon=None):
        super(PromptLineEdit, self).__init__(parent)
        self._margin = self.sizeHint().height() - 2
        self._spacing = 0
        self._prompt_text = prompt_text

        self.button = QtWidgets.QToolButton(self)
        if button_icon is None:
            button_icon = icons.icon(
                'edit-clear', ':/pyqode-icons/rc/clear-left.png',
                'fa.times-circle')
        self.button.setIcon(button_icon)
        self.button.setToolTip(_("Clear"))
        self.button.setStyleSheet(
            "QToolButton { border: none; padding: 5px; }")
        self.button.setCursor(QtCore.Qt.ArrowCursor)
        self.button.setFocusPolicy(QtCore.Qt.NoFocus)
        self.set_button_visible(False)
        self.textChanged.connect(self._on_text_changed)
        self.button.clicked.connect(self.clear)
        self.button.clicked.connect(self.clear_clicked.emit)

    @property
    def prompt_text(self):
        """
        Gets/Sets the prompt text.
        """
        return self._prompt_text

    @prompt_text.setter
    def prompt_text(self, prompt):
        self._prompt_text = prompt
        self.update()

    def paintEvent(self, event):
        super(PromptLineEdit, self).paintEvent(event)

        qt_api = os.environ['QT_API'].lower()
        if self._prompt_text and not self.text() and self.isEnabled():
            if qt_api in PYQT4_API:
                from PyQt4.QtGui import QStyleOptionFrameV3
                option = QStyleOptionFrameV3()
            elif qt_api in PYSIDE_API:
                from PySide.QtGui import QStyleOptionFrameV3
                option = QStyleOptionFrameV3()
            elif qt_api in PYQT5_API:
                from PyQt5.QtWidgets import QStyleOptionFrame
                option = QStyleOptionFrame()
            else:
                msg = 'Qt bindings "%s" is not supported' % qt_api
                raise PythonQtError(msg)

            self.initStyleOption(option)

            left, top, right, bottom = self.getTextMargins()

            va = self.style().visualAlignment(
                self.layoutDirection(), self.alignment())
            rect = self.style().subElementRect(
                QtWidgets.QStyle.SE_LineEditContents, option, self).adjusted(
                    2, 0, 0, 0).adjusted(left, top, -right, -bottom)
            fm = QtGui.QFontMetrics(self.font())
            text = fm.elidedText(
                self._prompt_text, QtCore.Qt.ElideRight, rect.width())
            painter = QtGui.QPainter(self)
            painter.setPen(self.palette().color(
                QtGui.QPalette.Disabled, QtGui.QPalette.Text))
            painter.drawText(rect, va, text)

    def resizeEvent(self, event):
        # Adjusts Clear button position
        super(PromptLineEdit, self).resizeEvent(event)
        self.button.resize(QtCore.QSize(self._margin, self.height() - 2))
        self.button.move(self.width() - self._margin - 3, 0)

    def set_button_visible(self, visible):
        """
        Sets the clear button as ``visible``

        :param visible: Visible state (True = visible, False = hidden).
        """
        self.button.setVisible(visible)
        left, top, right, bottom = self.getTextMargins()
        if visible:
            right = self._margin + self._spacing
        else:
            right = 0
        self.setTextMargins(left, top, right, bottom)

    def _on_text_changed(self, text):
        """Text changed, update Clear button visibility
        """
        self.set_button_visible(len(text) > 0)
