"""
This module contains a widget that can show the html preview of an
editor.
"""
from weakref import proxy
from pyqode.qt import QtCore, QtWidgets
from pyqode.core.api import DelayJobRunner


class HtmlPreviewWidget(QtWidgets.QTextEdit):
    """
    Display html preview of a document as rich text in a QTextEdit.
    """
    hide_requested = QtCore.Signal()
    show_requested = QtCore.Signal()

    def __init__(self, parent=None):
        super(HtmlPreviewWidget, self).__init__(parent)
        self._editor = None
        self._timer = DelayJobRunner(delay=1000)

    def set_editor(self, editor):
        try:
            self.setHtml(editor.to_html())
        except (TypeError, AttributeError):
            self.setHtml('<center>No preview available...</center>')
            self._editor = None
            self.hide_requested.emit()
        else:
            if self._editor is not None and editor != self._editor:
                try:
                    self._editor.textChanged.disconnect(self._on_text_changed)
                except TypeError:
                    pass
            editor.textChanged.connect(self._on_text_changed)
            self._editor = proxy(editor)
            self.show_requested.emit()

    def _on_text_changed(self, *_):
        self._timer.request_job(self._update_preview)

    def _update_preview(self):
        try:
            # remember cursor/scrollbar position
            p = self.textCursor().position()
            v = self.verticalScrollBar().value()

            # display new html
            self.setHtml(self._editor.to_html())

            # restore cursor/scrollbar position
            c = self.textCursor()
            c.setPosition(p)
            self.setTextCursor(c)
            self.verticalScrollBar().setValue(v)
        except (TypeError, AttributeError):
            self.setHtml('<center>No preview available...</center>')
            self.hide_requested.emit()
