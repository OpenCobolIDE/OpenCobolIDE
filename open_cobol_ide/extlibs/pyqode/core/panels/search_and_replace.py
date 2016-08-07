# -*- coding: utf-8 -*-
"""
This module contains the search and replace panel
"""
import re
import sre_constants

from pyqode.qt import QtCore, QtGui, QtWidgets

from pyqode.core import icons
from pyqode.core._forms.search_panel_ui import Ui_SearchPanel
from pyqode.core.api.decoration import TextDecoration
from pyqode.core.api.panel import Panel
from pyqode.core.api.utils import DelayJobRunner, TextHelper
from pyqode.core.backend import NotRunning
from pyqode.core.backend.workers import findall


class SearchAndReplacePanel(Panel, Ui_SearchPanel):
    """ Lets the user search and replace text in the current document.

    It uses the backend API to search for some text. Search operation is
    performed in a background process (the backend process)..

    The search panel can also be used programatically. To do that, the client
    code must first requests a search using :meth:`requestSearch` and connects
    to :attr:`search_finished`. The results of the search can then be
    retrieved using :attr:`cpt_occurences` and :meth:`get_oOccurrences`. The
    client code may now navigate through occurrences using :meth:`select_next`
    or :meth:`select_previous`, or replace the occurrences with a specific
    text using :meth:`replace` or :meth:`replace_all`.
    """
    STYLESHEET = """SearchAndReplacePanel
    {
        background-color: %(bck)s;
        color: %(color)s;
    }

    QtoolButton
    {
        color: %(color)s;
        background-color: transparent;
        padding: 5px;
        min-height: 24px;
        min-width: 24px;
        border: none;
    }

    QtoolButton:hover
    {
        background-color: %(highlight)s;
        border: none;
        border-radius: 5px;
        color: %(color)s;
    }

    QtoolButton:pressed, QCheckBox:pressed
    {
        border: 1px solid %(bck)s;
    }

    QtoolButton:disabled
    {
        color: %(highlight)s;
    }

    QCheckBox
    {
        padding: 4px;
        color: %(color)s;
    }

    QCheckBox:hover
    {
            background-color: %(highlight)s;
            color: %(color)s;
            border-radius: 5px;
    }
    """
    _KEYS = ["panelBackground", "background", "panelForeground",
             "panelHighlight"]

    #: Signal emitted when a search operation finished
    search_finished = QtCore.Signal()

    #: Define the maximum number of occurences that can be highlighted
    #: in the document.
    #:
    #: .. note:: The search operation itself is fast but the creation of all
    #:    the extra selection used to highlight search result can be slow.
    MAX_HIGHLIGHTED_OCCURENCES = 500

    @property
    def background(self):
        """ Text decoration background """
        return self._bg

    @background.setter
    def background(self, value):
        self._bg = value
        self._refresh_decorations()
        # propagate changes to every clone
        if self.editor:
            for clone in self.editor.clones:
                try:
                    clone.modes.get(self.__class__).background = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    @property
    def foreground(self):
        """ Text decoration foreground """
        return self._fg

    @foreground.setter
    def foreground(self, value):
        self._fg = value
        self._refresh_decorations()
        # propagate changes to every clone
        if self.editor:
            for clone in self.editor.clones:
                try:
                    clone.modes.get(self.__class__).foreground = value
                except KeyError:
                    # this should never happen since we're working with clones
                    pass

    def __init__(self):
        Panel.__init__(self, dynamic=True)
        self.job_runner = DelayJobRunner(delay=500)
        Ui_SearchPanel.__init__(self)
        self.setupUi(self)
        self.toolButtonClose.clicked.connect(self.on_close)
        self.actionSearch.triggered.connect(self.on_search)
        self.actionActionSearchAndReplace.triggered.connect(self.on_search_and_replace)
        self.lineEditReplace.prompt_text = _(' Replace')
        #: Occurrences counter
        self.cpt_occurences = 0
        self._previous_stylesheet = ""
        self._separator = None
        self._decorations = []
        self._occurrences = []
        self._current_occurrence_index = 0
        self._bg = None
        self._fg = None
        self._update_buttons(txt="")
        self.lineEditSearch.installEventFilter(self)
        self.lineEditReplace.installEventFilter(self)
        self._init_actions()
        self._init_style()
        self.checkBoxRegex.stateChanged.connect(
            self.checkBoxWholeWords.setDisabled)

    def _init_actions(self):
        icon_size = QtCore.QSize(16, 16)

        icon = icons.icon('edit-find', ':/pyqode-icons/rc/edit-find.png',
                          'fa.search')
        self.actionSearch.setIcon(icon)
        self.actionSearch.setShortcut('Ctrl+F')
        self.labelSearch.setPixmap(icon.pixmap(icon_size))

        icon = icons.icon(
            'edit-find-replace', ':/pyqode-icons/rc/edit-find-replace.png',
            'fa.search-plus')
        self.actionActionSearchAndReplace.setShortcut(
            'Ctrl+H')
        self.actionActionSearchAndReplace.setIcon(icon)
        self.labelReplace.setPixmap(icon.pixmap(icon_size))

        icon = icons.icon('go-up', ':/pyqode-icons/rc/go-up.png',
                          'fa.arrow-up')
        self.actionFindPrevious.setShortcut('Shift+F3')
        self.actionFindPrevious.setIcon(icon)
        self.toolButtonPrevious.setIcon(icon)
        self.toolButtonPrevious.setIconSize(icon_size)

        icon = icons.icon('go-down', ':/pyqode-icons/rc/go-down.png',
                          'fa.arrow-down')
        self.actionFindNext.setShortcut('F3')
        self.actionFindNext.setIcon(icon)
        self.toolButtonNext.setIcon(icon)
        self.toolButtonNext.setIconSize(icon_size)

        icon = icons.icon('window-close', ':/pyqode-icons/rc/close.png',
                          'fa.close')
        self.toolButtonClose.setIcon(icon)
        self.toolButtonClose.setIconSize(icon_size)

        self.menu = QtWidgets.QMenu(self.editor)
        self.menu.setTitle(_('Search'))
        self.menu.menuAction().setIcon(self.actionSearch.icon())
        self.menu.addAction(self.actionSearch)
        self.actionSearch.setShortcutContext(QtCore.Qt.WidgetShortcut)
        self.menu.addAction(self.actionActionSearchAndReplace)
        self.actionActionSearchAndReplace.setShortcutContext(
            QtCore.Qt.WidgetShortcut)
        self.menu.addAction(self.actionFindNext)
        self.actionFindNext.setShortcutContext(
            QtCore.Qt.WidgetShortcut)
        self.menu.addAction(self.actionFindPrevious)
        self.actionFindPrevious.setShortcutContext(
            QtCore.Qt.WidgetShortcut)

    def _init_style(self):
        self._bg = QtGui.QColor('yellow')
        self._outline = QtGui.QPen(QtGui.QColor('gray'), 1)

    def on_install(self, editor):
        super(SearchAndReplacePanel, self).on_install(editor)
        self.hide()
        self.text_helper = TextHelper(editor)

    def _refresh_decorations(self):
        for deco in self._decorations:
            self.editor.decorations.remove(deco)
            deco.set_background(QtGui.QBrush(self.background))
            deco.set_outline(self._outline)
            self.editor.decorations.append(deco)

    def on_state_changed(self, state):
        super(SearchAndReplacePanel, self).on_state_changed(state)
        if state:
            # menu
            self.editor.add_action(self.menu.menuAction())
            # requestSearch slot
            self.editor.textChanged.connect(self.request_search)
            self.lineEditSearch.textChanged.connect(self.request_search)
            self.checkBoxCase.stateChanged.connect(self.request_search)
            self.checkBoxWholeWords.stateChanged.connect(self.request_search)
            self.checkBoxRegex.stateChanged.connect(self.request_search)
            self.checkBoxInSelection.stateChanged.connect(self.request_search)
            # navigation slots
            self.toolButtonNext.clicked.connect(self.select_next)
            self.actionFindNext.triggered.connect(self.select_next)
            self.toolButtonPrevious.clicked.connect(self.select_previous)
            self.actionFindPrevious.triggered.connect(self.select_previous)
            # replace slots
            self.toolButtonReplace.clicked.connect(self.replace)
            self.toolButtonReplaceAll.clicked.connect(self.replace_all)
            # internal updates slots
            self.lineEditReplace.textChanged.connect(self._update_buttons)
            self.search_finished.connect(self._on_search_finished)
        else:
            self.editor.remove_action(self.menu.menuAction())
            # requestSearch slot
            self.editor.textChanged.disconnect(self.request_search)
            self.lineEditSearch.textChanged.disconnect(self.request_search)
            self.checkBoxCase.stateChanged.disconnect(self.request_search)
            self.checkBoxWholeWords.stateChanged.disconnect(
                self.request_search)
            self.checkBoxRegex.stateChanged.disconnect(self.request_search)
            self.checkBoxInSelection.stateChanged.disconnect(
                self.request_search)
            # navigation slots
            self.toolButtonNext.clicked.disconnect(self.select_next)
            self.actionFindNext.triggered.disconnect(self.select_next)
            self.toolButtonPrevious.clicked.disconnect(self.select_previous)
            # replace slots
            self.toolButtonReplace.clicked.disconnect(self.replace)
            self.toolButtonReplaceAll.clicked.disconnect(self.replace_all)
            # internal updates slots
            self.lineEditReplace.textChanged.disconnect(self._update_buttons)
            self.search_finished.disconnect(self._on_search_finished)

    def close_panel(self):
        """
        Closes the panel
        """
        self.hide()
        self.lineEditReplace.clear()
        self.lineEditSearch.clear()

    def on_close(self):
        self.close_panel()

    def on_search(self):
        self.widgetSearch.show()
        self.widgetReplace.hide()
        self.show()
        new_text = self.text_helper.selected_text()
        old_text = self.lineEditSearch.text()
        text_changed = new_text != old_text
        self.lineEditSearch.setText(new_text)
        self.lineEditSearch.selectAll()
        self.lineEditSearch.setFocus()
        self.setFocusPolicy(QtCore.Qt.ClickFocus)
        if not text_changed:
            self.request_search(new_text)

    def on_search_and_replace(self):
        self.widgetSearch.show()
        self.widgetReplace.show()
        self.show()
        new_txt = self.text_helper.selected_text()
        old_txt = self.lineEditSearch.text()
        txt_changed = new_txt != old_txt
        self.lineEditSearch.setText(new_txt)
        self.lineEditReplace.clear()
        self.lineEditReplace.setFocus()
        if not txt_changed:
            self.request_search(new_txt)

    def focusOutEvent(self, event):
        self.job_runner.cancel_requests()
        Panel.focusOutEvent(self, event)

    def request_search(self, txt=None):
        """
        Requests a search operation.

        :param txt: The text to replace. If None, the content of lineEditSearch
                    is used instead.
        """
        if self.checkBoxRegex.isChecked():
            try:
                re.compile(self.lineEditSearch.text(), re.DOTALL)
            except sre_constants.error as e:
                self._show_error(e)
                return
            else:
                self._show_error(None)

        if txt is None or isinstance(txt, int):
            txt = self.lineEditSearch.text()
        if txt:
            self.job_runner.request_job(
                self._exec_search, txt, self._search_flags())
        else:
            self.job_runner.cancel_requests()
            self._clear_occurrences()
            self._on_search_finished()

    @staticmethod
    def _set_widget_background_color(widget, color):
        """
        Changes the base color of a widget (background).
        :param widget: widget to modify
        :param color: the color to apply
        """
        pal = widget.palette()
        pal.setColor(pal.Base, color)
        widget.setPalette(pal)

    def _show_error(self, error):
        if error:
            self._set_widget_background_color(
                self.lineEditSearch, QtGui.QColor('#FFCCCC'))
            self.lineEditSearch.setToolTip(str(error))
        else:
            self._set_widget_background_color(
                self.lineEditSearch, self.palette().color(
                    self.palette().Base))
            self.lineEditSearch.setToolTip('')

    def get_occurences(self):
        """
        Returns the list of text occurrences.

        An occurrence is a tuple that contains start and end positions.

        :return: List of tuple(int, int)
        """
        return self._occurrences

    def select_next(self):
        """
        Selects the next occurrence.

        :return: True in case of success, false if no occurrence could be
                 selected.
        """
        current_occurence = self._current_occurrence()
        occurrences = self.get_occurences()
        if not occurrences:
            return
        current = self._occurrences[current_occurence]
        cursor_pos = self.editor.textCursor().position()
        if cursor_pos not in range(current[0], current[1] + 1) or \
                current_occurence == -1:
            # search first occurrence that occurs after the cursor position
            current_occurence = 0
            for i, (start, end) in enumerate(self._occurrences):
                if end > cursor_pos:
                    current_occurence = i
                    break
        else:
            if (current_occurence == -1 or
                    current_occurence >= len(occurrences) - 1):
                current_occurence = 0
            else:
                current_occurence += 1
        self._set_current_occurrence(current_occurence)
        try:
            cursor = self.editor.textCursor()
            cursor.setPosition(occurrences[current_occurence][0])
            cursor.setPosition(occurrences[current_occurence][1],
                               cursor.KeepAnchor)
            self.editor.setTextCursor(cursor)
            return True
        except IndexError:
            return False

    def select_previous(self):
        """
        Selects previous occurrence.

        :return: True in case of success, false if no occurrence could be
                 selected.
        """
        current_occurence = self._current_occurrence()
        occurrences = self.get_occurences()
        if not occurrences:
            return
        current = self._occurrences[current_occurence]
        cursor_pos = self.editor.textCursor().position()
        if cursor_pos not in range(current[0], current[1] + 1) or \
                current_occurence == -1:
            # search first occurrence that occurs before the cursor position
            current_occurence = len(self._occurrences) - 1
            for i, (start, end) in enumerate(self._occurrences):
                if end >= cursor_pos:
                    current_occurence = i - 1
                    break
        else:
            if (current_occurence == -1 or
                    current_occurence == 0):
                current_occurence = len(occurrences) - 1
            else:
                current_occurence -= 1
        self._set_current_occurrence(current_occurence)
        try:
            cursor = self.editor.textCursor()
            cursor.setPosition(occurrences[current_occurence][0])
            cursor.setPosition(occurrences[current_occurence][1],
                               cursor.KeepAnchor)
            self.editor.setTextCursor(cursor)
            return True
        except IndexError:
            return False

    def replace(self, text=None):
        """
        Replaces the selected occurrence.

        :param text: The replacement text. If it is None, the lineEditReplace's
                     text is used instead.

        :return True if the text could be replace properly, False if there is
                no more occurrences to replace.
        """
        if text is None or isinstance(text, bool):
            text = self.lineEditReplace.text()
        current_occurences = self._current_occurrence()
        occurrences = self.get_occurences()
        if current_occurences == -1:
            self.select_next()
            current_occurences = self._current_occurrence()
        try:
            # prevent search request due to editor textChanged
            try:
                self.editor.textChanged.disconnect(self.request_search)
            except (RuntimeError, TypeError):
                # already disconnected
                pass
            occ = occurrences[current_occurences]
            cursor = self.editor.textCursor()
            cursor.setPosition(occ[0])
            cursor.setPosition(occ[1], cursor.KeepAnchor)
            len_to_replace = len(cursor.selectedText())
            len_replacement = len(text)
            offset = len_replacement - len_to_replace
            cursor.insertText(text)
            self.editor.setTextCursor(cursor)
            self._remove_occurrence(current_occurences, offset)
            current_occurences -= 1
            self._set_current_occurrence(current_occurences)
            self.select_next()
            self.cpt_occurences = len(self.get_occurences())
            self._update_label_matches()
            self._update_buttons()
            return True
        except IndexError:
            return False
        finally:
            self.editor.textChanged.connect(self.request_search)

    def replace_all(self, text=None):
        """
        Replaces all occurrences in the editor's document.

        :param text: The replacement text. If None, the content of the lineEdit
                     replace will be used instead
        """
        cursor = self.editor.textCursor()
        cursor.beginEditBlock()
        remains = self.replace(text=text)
        while remains:
            remains = self.replace(text=text)
        cursor.endEditBlock()

    def eventFilter(self, obj, event):
        if event.type() == QtCore.QEvent.KeyPress:
            if (event.key() == QtCore.Qt.Key_Tab or
                    event.key() == QtCore.Qt.Key_Backtab):
                return True
            elif (event.key() == QtCore.Qt.Key_Return or
                  event.key() == QtCore.Qt.Key_Enter):
                if obj == self.lineEditReplace:
                    if event.modifiers() & QtCore.Qt.ControlModifier:
                        self.replace_all()
                    else:
                        self.replace()
                elif obj == self.lineEditSearch:
                    if event.modifiers() & QtCore.Qt.ShiftModifier:
                        self.select_previous()
                    else:
                        self.select_next()
                return True
            elif event.key() == QtCore.Qt.Key_Escape:
                self.on_close()
        return Panel.eventFilter(self, obj, event)

    def _search_flags(self):
        """
        Returns the user search flag: (regex, case_sensitive, whole_words).
        """
        return (self.checkBoxRegex.isChecked(),
                self.checkBoxCase.isChecked(),
                self.checkBoxWholeWords.isChecked(),
                self.checkBoxInSelection.isChecked())

    def _exec_search(self, sub, flags):
        if self.editor is None:
            return
        regex, case_sensitive, whole_word, in_selection = flags
        tc = self.editor.textCursor()
        assert isinstance(tc, QtGui.QTextCursor)
        if in_selection and tc.hasSelection():
            text = tc.selectedText()
            self._offset = tc.selectionStart()
        else:
            text = self.editor.toPlainText()
            self._offset = 0
        request_data = {
            'string': text,
            'sub': sub,
            'regex': regex,
            'whole_word': whole_word,
            'case_sensitive': case_sensitive
        }
        try:
            self.editor.backend.send_request(findall, request_data,
                                             self._on_results_available)
        except AttributeError:
            self._on_results_available(findall(request_data))
        except NotRunning:
            QtCore.QTimer.singleShot(100, self.request_search)

    def _on_results_available(self, results):
        self._occurrences = [(start + self._offset, end + self._offset)
                             for start, end in results]
        self._on_search_finished()

    def _update_label_matches(self):
        self.labelMatches.setText(_("{0} matches").format(self.cpt_occurences))
        color = "#DD0000"
        if self.cpt_occurences:
            color = "#00DD00"
        self.labelMatches.setStyleSheet("color: %s" % color)
        if self.lineEditSearch.text() == "":
            self.labelMatches.clear()

    def _on_search_finished(self):
        self._clear_decorations()
        all_occurences = self.get_occurences()
        occurrences = all_occurences[:self.MAX_HIGHLIGHTED_OCCURENCES]
        for i, occurrence in enumerate(occurrences):
            deco = self._create_decoration(occurrence[0],
                                           occurrence[1])
            self._decorations.append(deco)
            self.editor.decorations.append(deco)
        self.cpt_occurences = len(all_occurences)
        if not self.cpt_occurences:
            self._current_occurrence_index = -1
        else:
            self._current_occurrence_index = -1
        self._update_label_matches()
        self._update_buttons(txt=self.lineEditReplace.text())

    def _current_occurrence(self):
        ret_val = self._current_occurrence_index
        return ret_val

    def _clear_occurrences(self):
        self._occurrences[:] = []

    def _create_decoration(self, selection_start, selection_end):
        """ Creates the text occurences decoration """
        deco = TextDecoration(self.editor.document(), selection_start,
                              selection_end)
        deco.set_background(QtGui.QBrush(self.background))
        deco.set_outline(self._outline)
        deco.set_foreground(QtCore.Qt.black)
        deco.draw_order = 1
        return deco

    def _clear_decorations(self):
        """ Remove all decorations """
        for deco in self._decorations:
            self.editor.decorations.remove(deco)
        self._decorations[:] = []

    def _set_current_occurrence(self, current_occurence_index):
        self._current_occurrence_index = current_occurence_index

    def _remove_occurrence(self, i, offset=0):
        self._occurrences.pop(i)
        if offset:
            updated_occurences = []
            for j, occ in enumerate(self._occurrences):
                if j >= i:
                    updated_occurences.append(
                        (occ[0] + offset, occ[1] + offset))
                else:
                    updated_occurences.append((occ[0], occ[1]))
            self._occurrences = updated_occurences

    def _update_buttons(self, txt=""):
        enable = self.cpt_occurences > 1
        self.toolButtonNext.setEnabled(enable)
        self.toolButtonPrevious.setEnabled(enable)
        self.actionFindNext.setEnabled(enable)
        self.actionFindPrevious.setEnabled(enable)
        enable = (txt != self.lineEditSearch.text() and
                  self.cpt_occurences)
        self.toolButtonReplace.setEnabled(enable)
        self.toolButtonReplaceAll.setEnabled(enable)

    def clone_settings(self, original):
        self.background = original.background
        self.foreground = original.foreground
