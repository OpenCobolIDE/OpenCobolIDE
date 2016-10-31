"""
This module contains the occurrences highlighter mode.
"""
from pyqode.qt import QtGui
from pyqode.core.api import Mode, DelayJobRunner, TextHelper, TextDecoration
from pyqode.core.backend import NotRunning
from pyqode.core.backend.workers import findall


class OccurrencesHighlighterMode(Mode):
    """ Highlights occurrences of the word under the text text cursor.

    The ``delay`` before searching for occurrences is configurable.
    """
    @property
    def delay(self):
        """
        Delay before searching for occurrences. The timer is rearmed as soon
        as the cursor position changed.
        """
        return self.timer.delay

    @delay.setter
    def delay(self, value):
        self.timer.delay = value
        if self.editor:
            for clone in self.editor.clones:
                try:
                    clone.modes.get(self.__class__).delay = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    @property
    def background(self):
        """
        Background or underline color (if underlined is True).
        """
        return self._background

    @background.setter
    def background(self, value):
        self._background = value
        if self.editor:
            for clone in self.editor.clones:
                try:
                    clone.modes.get(self.__class__).background = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    @property
    def foreground(self):
        """
        Foreground color of occurences, not used if underlined is True.
        """
        return self._foreground

    @foreground.setter
    def foreground(self, value):
        self._foreground = value
        if self.editor:
            for clone in self.editor.clones:
                try:
                    clone.modes.get(self.__class__).foreground = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    @property
    def underlined(self):
        """
        True to use to underlined occurrences instead of
        changing the background. Default is True.

        If this mode is ON, the foreground color is ignored, the
        background color is then used as the underline color.

        """
        return self._underlined

    @underlined.setter
    def underlined(self, value):
        self._underlined = value
        if self.editor:
            for clone in self.editor.clones:
                try:
                    clone.modes.get(self.__class__).underlined = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    @property
    def case_sensitive(self):
        return self._case_sensitive

    @case_sensitive.setter
    def case_sensitive(self, value):
        self._case_sensitive = value
        self._request_highlight()

    def __init__(self):
        super(OccurrencesHighlighterMode, self).__init__()
        self._decorations = []
        #: Timer used to run the search request with a specific delay
        self.timer = DelayJobRunner(delay=1000)
        self._sub = None
        self._background = QtGui.QColor('#CCFFCC')
        self._foreground = None
        self._underlined = False
        self._case_sensitive = False

    def on_state_changed(self, state):
        if state:
            self.editor.cursorPositionChanged.connect(self._request_highlight)
        else:
            self.editor.cursorPositionChanged.disconnect(
                self._request_highlight)
            self.timer.cancel_requests()

    def _clear_decos(self):
        for d in self._decorations:
            self.editor.decorations.remove(d)
        self._decorations[:] = []

    def _request_highlight(self):
        if self.editor is not None:
            sub = TextHelper(self.editor).word_under_cursor(
                select_whole_word=True).selectedText()
            if sub != self._sub:
                self._clear_decos()
                if len(sub) > 1:
                    self.timer.request_job(self._send_request)

    def _send_request(self):
        if self.editor is None:
            return
        cursor = self.editor.textCursor()
        self._sub = TextHelper(self.editor).word_under_cursor(
            select_whole_word=True).selectedText()
        if not cursor.hasSelection() or cursor.selectedText() == self._sub:
            request_data = {
                'string': self.editor.toPlainText(),
                'sub': self._sub,
                'regex': False,
                'whole_word': True,
                'case_sensitive': self.case_sensitive
            }
            try:
                self.editor.backend.send_request(findall, request_data,
                                                 self._on_results_available)
            except NotRunning:
                self._request_highlight()

    def _on_results_available(self, results):
        if len(results) > 500:
            # limit number of results (on very big file where a lots of
            # occurrences can be found, this would totally freeze the editor
            # during a few seconds, with a limit of 500 we can make sure
            # the editor will always remain responsive).
            results = results[:500]
        current = self.editor.textCursor().position()
        if len(results) > 1:
            for start, end in results:
                if start <= current <= end:
                    continue
                deco = TextDecoration(self.editor.textCursor(),
                                      start_pos=start, end_pos=end)
                if self.underlined:
                    deco.set_as_underlined(self._background)
                else:
                    deco.set_background(QtGui.QBrush(self._background))
                    if self._foreground is not None:
                        deco.set_foreground(self._foreground)
                deco.draw_order = 3
                self.editor.decorations.append(deco)
                self._decorations.append(deco)

    def clone_settings(self, original):
        self.delay = original.delay
        self.background = original.background
        self.foreground = original.foreground
        self.underlined = original.underlined
