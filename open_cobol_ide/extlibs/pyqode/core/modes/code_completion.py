# -*- coding: utf-8 -*-
"""
This module contains the code completion mode and the related classes.
"""
import logging
import re
import sys
import time
from pyqode.core.api.mode import Mode
from pyqode.core.backend import NotRunning
from pyqode.qt import QtWidgets, QtCore, QtGui
from pyqode.core.api.utils import TextHelper
from pyqode.core import backend


def _logger():
    return logging.getLogger(__name__)


def debug(msg, *args):
    """
    Log internal debugger messages (user should not see them, even in debug
    mode)
    """
    return _logger().log(5, msg, *args)



class SubsequenceSortFilterProxyModel(QtCore.QSortFilterProxyModel):
    """
    Performs subsequence matching/sorting (see pyQode/pyQode#1).
    """
    def __init__(self, case, parent=None):
        QtCore.QSortFilterProxyModel.__init__(self, parent)
        self.case = case

    def set_prefix(self, prefix):
        self.filter_patterns = []
        self.filter_patterns_case_sensitive = []
        self.sort_patterns = []
        if self.case == QtCore.Qt.CaseInsensitive:
            flags = re.IGNORECASE
        else:
            flags = 0
        for i in reversed(range(1, len(prefix) + 1)):
            ptrn = '.*%s.*%s' % (prefix[0:i], prefix[i:])
            try:
                self.filter_patterns.append(re.compile(ptrn, flags))
                self.filter_patterns_case_sensitive.append(re.compile(ptrn, 0))
                ptrn = '%s.*%s' % (prefix[0:i], prefix[i:])
                self.sort_patterns.append(re.compile(ptrn, flags))
            except Exception:
                continue
        self.prefix = prefix

    def filterAcceptsRow(self, row, _):
        completion = self.sourceModel().data(self.sourceModel().index(row, 0))
        if len(completion) < len(self.prefix):
            return False
        if len(self.prefix) == 1:
            try:
                prefix = self.prefix
                if self.case == QtCore.Qt.CaseInsensitive:
                    completion = completion.lower()
                    prefix = self.prefix.lower()
                rank = completion.index(prefix)
                self.sourceModel().setData(
                    self.sourceModel().index(row, 0), rank, QtCore.Qt.UserRole)
                return prefix in completion
            except ValueError:
                return False
        for i, patterns in enumerate(zip(self.filter_patterns,
                                         self.filter_patterns_case_sensitive,
                                         self.sort_patterns)):
            pattern, pattern_case, sort_pattern = patterns
            match = re.match(pattern, completion)
            if match:
                # compute rank, the lowest rank the closer it is from the
                # completion
                start = sys.maxsize
                for m in sort_pattern.finditer(completion):
                    start, end = m.span()
                rank = start + i * 10
                if re.match(pattern_case, completion):
                    # favorise completions where case is matched
                    rank -= 10
                self.sourceModel().setData(
                    self.sourceModel().index(row, 0), rank, QtCore.Qt.UserRole)
                return True
        return len(self.prefix) == 0


class SubsequenceCompleter(QtWidgets.QCompleter):
    """
    QCompleter specialised for subsequence matching
    """
    def __init__(self, *args):
        super(SubsequenceCompleter, self).__init__(*args)
        self.local_completion_prefix = ""
        self.source_model = None
        self.filterProxyModel = SubsequenceSortFilterProxyModel(
            self.caseSensitivity(), parent=self)
        self.filterProxyModel.setSortRole(QtCore.Qt.UserRole)
        self._force_next_update = True

    def setModel(self, model):
        self.source_model = model
        self.filterProxyModel = SubsequenceSortFilterProxyModel(
            self.caseSensitivity(), parent=self)
        self.filterProxyModel.setSortRole(QtCore.Qt.UserRole)
        self.filterProxyModel.set_prefix(self.local_completion_prefix)
        self.filterProxyModel.setSourceModel(self.source_model)
        super(SubsequenceCompleter, self).setModel(self.filterProxyModel)
        self.filterProxyModel.invalidate()
        self.filterProxyModel.sort(0)
        self._force_next_update = True

    def update_model(self):
        if (self.completionCount() or
                len(self.local_completion_prefix) <= 1 or
                self._force_next_update):
            self.filterProxyModel.set_prefix(self.local_completion_prefix)
            self.filterProxyModel.invalidate()  # force sorting/filtering
        if self.completionCount() > 1:
            self.filterProxyModel.sort(0)
        self._force_next_update = False

    def splitPath(self, path):
        self.local_completion_prefix = path
        self.update_model()
        return ['']


class CodeCompletionMode(Mode, QtCore.QObject):
    """ Provides code completions when typing or when pressing Ctrl+Space.

    This mode provides a code completion system which is extensible.
    It takes care of running the completion request in a background process
    using one or more completion provider and display the results in a
    QCompleter.

    To add code completion for a specific language, you only need to
    implement a new
    :class:`pyqode.core.backend.workers.CodeCompletionWorker.Provider`

    The completion popup is shown when the user press **ctrl+space** or
    automatically while the user is typing some code (this can be configured
    using a series of properties).
    """
    #: Filter completions based on the prefix. FAST
    FILTER_PREFIX = 0
    #: Filter completions based on whether the prefix is contained in the
    #: suggestion. Only available with PyQt5, if set with PyQt4, FILTER_PREFIX
    #: will be used instead. FAST
    FILTER_CONTAINS = 1
    #: Fuzzy filtering, using the subsequence matcher. This is the most
    #: powerful filter mode but also the SLOWEST.
    FILTER_FUZZY = 2

    @property
    def filter_mode(self):
        """
        The completion filter mode
        """
        return self._filter_mode

    @filter_mode.setter
    def filter_mode(self, value):
        self._filter_mode = value
        self._create_completer()

    @property
    def trigger_key(self):
        """
        The key that triggers code completion (Default is **Space**:
        Ctrl + Space).
        """
        return self._trigger_key

    @trigger_key.setter
    def trigger_key(self, value):
        self._trigger_key = value
        if self.editor:
            # propagate changes to every clone
            for clone in self.editor.clones:
                try:
                    clone.modes.get(CodeCompletionMode).trigger_key = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    @property
    def trigger_length(self):
        """
        The trigger length defines the word length required to run code
        completion.
        """
        return self._trigger_len

    @trigger_length.setter
    def trigger_length(self, value):
        self._trigger_len = value
        if self.editor:
            # propagate changes to every clone
            for clone in self.editor.clones:
                try:
                    clone.modes.get(CodeCompletionMode).trigger_length = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    @property
    def trigger_symbols(self):
        """
        Defines the list of symbols that immediately trigger a code completion
        requiest. BY default, this list contains the dot character.

        For C++, we would add the '->' operator to that list.
        """
        return self._trigger_symbols

    @trigger_symbols.setter
    def trigger_symbols(self, value):
        self._trigger_symbols = value
        if self.editor:
            # propagate changes to every clone
            for clone in self.editor.clones:
                try:
                    clone.modes.get(CodeCompletionMode).trigger_symbols = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    @property
    def case_sensitive(self):
        """
        True to performs case sensitive completion matching.
        """
        return self._case_sensitive

    @case_sensitive.setter
    def case_sensitive(self, value):
        self._case_sensitive = value
        if self.editor:
            # propagate changes to every clone
            for clone in self.editor.clones:
                try:
                    clone.modes.get(CodeCompletionMode).case_sensitive = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    @property
    def completion_prefix(self):
        """
        Returns the current completion prefix
        """
        return self._helper.word_under_cursor(
            select_whole_word=False).selectedText().strip()

    @property
    def show_tooltips(self):
        """
        True to show tooltips next to the current completion.
        """
        return self._show_tooltips

    @show_tooltips.setter
    def show_tooltips(self, value):
        self._show_tooltips = value
        if self.editor:
            # propagate changes to every clone
            for clone in self.editor.clones:
                try:
                    clone.modes.get(CodeCompletionMode).show_tooltips = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    def __init__(self):
        Mode.__init__(self)
        QtCore.QObject.__init__(self)
        self._current_completion = ""
        self._trigger_key = QtCore.Qt.Key_Space
        self._trigger_len = 1
        self._trigger_symbols = ['.']
        self._case_sensitive = False
        self._completer = None
        self._filter_mode = self.FILTER_FUZZY
        self._last_cursor_line = -1
        self._last_cursor_column = -1
        self._tooltips = {}
        self._show_tooltips = False
        self._request_id = self._last_request_id = 0

    def clone_settings(self, original):
        self.trigger_key = original.trigger_key
        self.trigger_length = original.trigger_length
        self.trigger_symbols = original.trigger_symbols
        self.show_tooltips = original.show_tooltips
        self.case_sensitive = original.case_sensitive

    #
    # Mode interface
    #
    def _create_completer(self):
        if self.filter_mode != self.FILTER_FUZZY:
            self._completer = QtWidgets.QCompleter([''], self.editor)
            if self.filter_mode == self.FILTER_CONTAINS:
                try:
                    self._completer.setFilterMode(QtCore.Qt.MatchContains)
                except AttributeError:
                    # only available with PyQt5
                    pass
        else:
            self._completer = SubsequenceCompleter(self.editor)
        self._completer.setCompletionMode(self._completer.PopupCompletion)
        if self.case_sensitive:
            self._completer.setCaseSensitivity(QtCore.Qt.CaseSensitive)
        else:
            self._completer.setCaseSensitivity(QtCore.Qt.CaseInsensitive)
        self._completer.activated.connect(self._insert_completion)
        self._completer.highlighted.connect(
            self._on_selected_completion_changed)
        self._completer.highlighted.connect(self._display_completion_tooltip)

    def on_install(self, editor):
        self._create_completer()
        self._completer.setModel(QtGui.QStandardItemModel())
        self._helper = TextHelper(editor)
        Mode.on_install(self, editor)

    def on_uninstall(self):
        Mode.on_uninstall(self)
        self._completer.popup().hide()
        self._completer = None

    def on_state_changed(self, state):
        if state:
            self.editor.focused_in.connect(self._on_focus_in)
            self.editor.key_pressed.connect(self._on_key_pressed)
            self.editor.post_key_pressed.connect(self._on_key_released)
        else:
            self.editor.focused_in.disconnect(self._on_focus_in)
            self.editor.key_pressed.disconnect(self._on_key_pressed)
            self.editor.post_key_pressed.disconnect(self._on_key_released)

    #
    # Slots
    #
    def _on_key_pressed(self, event):
        def _handle_completer_events():
            nav_key = self._is_navigation_key(event)
            mod = QtCore.Qt.ControlModifier
            ctrl = int(event.modifiers() & mod) == mod
            # complete
            if event.key() in [
                    QtCore.Qt.Key_Enter, QtCore.Qt.Key_Return,
                    QtCore.Qt.Key_Tab]:
                self._insert_completion(self._current_completion)
                self._hide_popup()
                event.accept()
            # hide
            elif (event.key() in [
                    QtCore.Qt.Key_Escape, QtCore.Qt.Key_Backtab] or
                    nav_key and ctrl):
                self._reset_sync_data()
            # move into list
            elif event.key() == QtCore.Qt.Key_Home:
                self._show_popup(index=0)
                event.accept()
            elif event.key() == QtCore.Qt.Key_End:
                self._show_popup(index=self._completer.completionCount() - 1)
                event.accept()

        debug('key pressed: %s' % event.text())
        is_shortcut = self._is_shortcut(event)
        # handle completer popup events ourselves
        if self._completer.popup().isVisible():
            if is_shortcut:
                event.accept()
            else:
                _handle_completer_events()
        elif is_shortcut:
            self._reset_sync_data()
            self.request_completion()
            event.accept()

    def _on_key_released(self, event):
        if self._is_shortcut(event) or event.isAccepted():
            return
        debug('key released:%s' % event.text())
        word = self._helper.word_under_cursor(
            select_whole_word=True).selectedText()
        debug('word: %s' % word)
        if event.text():
            if event.key() == QtCore.Qt.Key_Escape:
                self._hide_popup()
                return
            if self._is_navigation_key(event) and \
                    (not self._is_popup_visible() or word == ''):
                self._reset_sync_data()
                return
            if event.key() == QtCore.Qt.Key_Return:
                return
            if event.text() in self._trigger_symbols:
                # symbol trigger, force request
                self._reset_sync_data()
                self.request_completion()
            elif len(word) >= self._trigger_len and event.text() not in \
                    self.editor.word_separators:
                # Length trigger
                if int(event.modifiers()) in [
                        QtCore.Qt.NoModifier, QtCore.Qt.ShiftModifier]:
                    self.request_completion()
                else:
                    self._hide_popup()
            else:
                self._reset_sync_data()
        else:
            if self._is_navigation_key(event):
                if self._is_popup_visible() and word:
                    self._show_popup()
                    return
                else:
                    self._reset_sync_data()

    def _on_focus_in(self, event):
        """
        Resets completer's widget

        :param event: QFocusEvents
        """
        self._completer.setWidget(self.editor)

    def _on_selected_completion_changed(self, completion):
        self._current_completion = completion

    def _insert_completion(self, completion):
        cursor = self._helper.word_under_cursor(select_whole_word=False)
        cursor.insertText(completion)
        self.editor.setTextCursor(cursor)

    def _on_results_available(self, results):
        debug("completion results (completions=%r), prefix=%s",
                        results, self.completion_prefix)
        context = results[0]
        results = results[1:]
        line, column, request_id = context
        debug('request context: %r', context)
        debug('latest context: %r', (self._last_cursor_line,
                                               self._last_cursor_column,
                                               self._request_id))
        self._last_request_id = request_id
        if (line == self._last_cursor_line and
                column == self._last_cursor_column):
            if self.editor:
                all_results = []
                for res in results:
                    all_results += res
                self._show_completions(all_results)
        else:
            debug('outdated request, dropping')

    #
    # Helper methods
    #
    def _is_popup_visible(self):
        return self._completer.popup().isVisible()

    def _reset_sync_data(self):
        debug('reset sync data and hide popup')
        self._last_cursor_line = -1
        self._last_cursor_column = -1
        self._hide_popup()

    def request_completion(self):
        line = self._helper.current_line_nbr()
        column = self._helper.current_column_nbr() - \
            len(self.completion_prefix)
        same_context = (line == self._last_cursor_line and
                        column == self._last_cursor_column)
        if same_context:
            if self._request_id - 1 == self._last_request_id:
                # context has not changed and the correct results can be
                # directly shown
                debug('request completion ignored, context has not '
                                'changed')
                self._show_popup()
            else:
                # same context but result not yet available
                pass
            return True
        else:
            debug('requesting completion')
            data = {
                'code': self.editor.toPlainText(),
                'line': line,
                'column': column,
                'path': self.editor.file.path,
                'encoding': self.editor.file.encoding,
                'prefix': self.completion_prefix,
                'request_id': self._request_id
            }
            try:
                self.editor.backend.send_request(
                    backend.CodeCompletionWorker, args=data,
                    on_receive=self._on_results_available)
            except NotRunning:
                _logger().exception('failed to send the completion request')
                return False
            else:
                debug('request sent: %r', data)
                self._last_cursor_column = column
                self._last_cursor_line = line
                self._request_id += 1
                return True

    def _is_shortcut(self, event):
        """
        Checks if the event's key and modifiers make the completion shortcut
        (Ctrl+Space)

        :param event: QKeyEvent

        :return: bool
        """
        modifier = (QtCore.Qt.MetaModifier if sys.platform == 'darwin' else
                    QtCore.Qt.ControlModifier)
        valid_modifier = int(event.modifiers() & modifier) == modifier
        valid_key = event.key() == self._trigger_key
        return valid_key and valid_modifier

    def _hide_popup(self):
        """
        Hides the completer popup
        """
        debug('hide popup')
        if (self._completer.popup() is not None and
                self._completer.popup().isVisible()):
            self._completer.popup().hide()
            self._last_cursor_column = -1
            self._last_cursor_line = -1
            QtWidgets.QToolTip.hideText()

    def _get_popup_rect(self):
        cursor_rec = self.editor.cursorRect()
        char_width = self.editor.fontMetrics().width('A')
        prefix_len = (len(self.completion_prefix) * char_width)
        cursor_rec.translate(
            self.editor.panels.margin_size() - prefix_len,
            self.editor.panels.margin_size(0) + 5)
        popup = self._completer.popup()
        width = popup.verticalScrollBar().sizeHint().width()
        cursor_rec.setWidth(
            self._completer.popup().sizeHintForColumn(0) + width)
        return cursor_rec

    def _show_popup(self, index=0):
        """
        Shows the popup at the specified index.
        :param index: index
        :return:
        """
        full_prefix = self._helper.word_under_cursor(
            select_whole_word=False).selectedText()
        if self._case_sensitive:
            self._completer.setCaseSensitivity(QtCore.Qt.CaseSensitive)
        else:
            self._completer.setCaseSensitivity(QtCore.Qt.CaseInsensitive)
        # set prefix
        self._completer.setCompletionPrefix(self.completion_prefix)
        cnt = self._completer.completionCount()
        selected = self._completer.currentCompletion()
        if (full_prefix == selected) and cnt == 1:
            debug('user already typed the only completion that we '
                            'have')
            self._hide_popup()
        else:
            # show the completion list
            if self.editor.isVisible():
                if self._completer.widget() != self.editor:
                    self._completer.setWidget(self.editor)
                self._completer.complete(self._get_popup_rect())
                self._completer.popup().setCurrentIndex(
                    self._completer.completionModel().index(index, 0))
                debug(
                    "popup shown: %r" % self._completer.popup().isVisible())
            else:
                debug('cannot show popup, editor is not visible')

    def _show_completions(self, completions):
        debug("showing %d completions" % len(completions))
        debug('popup state: %r', self._completer.popup().isVisible())
        t = time.time()
        self._update_model(completions)
        elapsed = time.time() - t
        debug("completion model updated: %d items in %f seconds",
                        self._completer.model().rowCount(), elapsed)
        self._show_popup()

    def _update_model(self, completions):
        """
        Creates a QStandardModel that holds the suggestion from the completion
        models for the QCompleter

        :param completionPrefix:
        """
        # build the completion model
        cc_model = QtGui.QStandardItemModel()
        self._tooltips.clear()
        for completion in completions:
            name = completion['name']
            item = QtGui.QStandardItem()
            item.setData(name, QtCore.Qt.DisplayRole)
            if 'tooltip' in completion and completion['tooltip']:
                self._tooltips[name] = completion['tooltip']
            if 'icon' in completion:
                icon = completion['icon']
                if isinstance(icon, list):
                    icon = QtGui.QIcon.fromTheme(icon[0], QtGui.QIcon(icon[1]))
                else:
                    icon = QtGui.QIcon(icon)
                item.setData(QtGui.QIcon(icon),
                             QtCore.Qt.DecorationRole)
            cc_model.appendRow(item)
        try:
            self._completer.setModel(cc_model)
        except RuntimeError:
            self._create_completer()
            self._completer.setModel(cc_model)
        return cc_model

    def _display_completion_tooltip(self, completion):
        if not self._show_tooltips:
            return
        if completion not in self._tooltips:
            QtWidgets.QToolTip.hideText()
            return
        tooltip = self._tooltips[completion].strip()
        pos = self._completer.popup().pos()
        pos.setX(pos.x() + self._completer.popup().size().width())
        pos.setY(pos.y() - 15)
        QtWidgets.QToolTip.showText(pos, tooltip, self.editor)

    @staticmethod
    def _is_navigation_key(event):
        return (event.key() == QtCore.Qt.Key_Backspace or
                event.key() == QtCore.Qt.Key_Back or
                event.key() == QtCore.Qt.Key_Delete or
                event.key() == QtCore.Qt.Key_End or
                event.key() == QtCore.Qt.Key_Home or
                event.key() == QtCore.Qt.Key_Left or
                event.key() == QtCore.Qt.Key_Right or
                event.key() == QtCore.Qt.Key_Up or
                event.key() == QtCore.Qt.Key_Down or
                event.key() == QtCore.Qt.Key_Space)
