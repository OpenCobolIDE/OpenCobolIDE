"""
This module contains a widget that can show the html preview of an
editor.
"""
from weakref import proxy
from pyqode.qt import QtCore, QtWebWidgets, QtWidgets
from pyqode.core.api import DelayJobRunner

if QtWebWidgets.QWebView is None:
    WebView = QtWidgets.QWidget
else:
    WebView = QtWebWidgets.QWebView


class HtmlPreviewWidget(WebView):
    hide_requested = QtCore.Signal()
    show_requested = QtCore.Signal()

    def __init__(self, parent=None):
        super(HtmlPreviewWidget, self).__init__(parent)
        self._editor = None
        self._timer = DelayJobRunner(delay=1000)
        try:
            # prevent opening internal links when using QtWebKit
            self.page().setLinkDelegationPolicy(
                QtWebWidgets.QWebPage.DelegateAllLinks)
        except (TypeError, AttributeError):
            # no needed with QtWebEngine, internal links are properly handled
            # by the default implementation
            pass

    def set_editor(self, editor):
        url = QtCore.QUrl('')
        if editor is not None:
            url = QtCore.QUrl.fromLocalFile(editor.file.path)
        try:
            self.setHtml(editor.to_html(), url)
        except (TypeError, AttributeError):
            self.setHtml('<center>No preview available...</center>', url)
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
            self.page()
        except RuntimeError:
            # c++ object deleted between last call to _on_text_changed and
            # now
            return
        url = QtCore.QUrl('')
        if self._editor is not None:
            url = QtCore.QUrl.fromLocalFile(self._editor.file.path)
        try:
            try:
                frame = self.page().mainFrame()
                pos = frame.scrollBarValue(QtCore.Qt.Vertical)
                self.setHtml(self._editor.to_html(), url)
                frame.setScrollBarValue(QtCore.Qt.Vertical, pos)
            except AttributeError:
                # Not possible with QtWebEngine???
                # self._scroll_pos = self.page().mainFrame().scrollBarValue(
                    # QtCore.Qt.Vertical)
                self.setHtml(self._editor.to_html(), url)
        except (TypeError, AttributeError):
            self.setHtml('<center>No preview available...</center>', url)
            self.hide_requested.emit()
