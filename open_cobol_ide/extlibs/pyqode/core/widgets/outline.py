"""
This document contains the tree widget used to display the editor document
outline.

"""
import weakref
from pyqode.core import icons
from pyqode.core.panels import FoldingPanel
from pyqode.core.modes.outline import OutlineMode
from pyqode.qt import QtCore, QtGui, QtWidgets
from pyqode.core.api import TextBlockHelper, TextBlockUserData, TextHelper


class OutlineTreeWidget(QtWidgets.QTreeWidget):
    """
    Displays the outline of a CodeEdit.

    To use this widget:

    1. add an OutlineMode to CodeEdit
    2. call set_editor with a CodeEdit instance to show it's outline.

    """

    sync_with_editor_changed = QtCore.Signal(bool)

    @property
    def sync_with_editor(self):
        return self._sync_with_editor

    @sync_with_editor.setter
    def sync_with_editor(self, value):
        if value != self.sync_with_editor:
            self._sync_with_editor = value
            self._action_sync.setChecked(value)
            self.sync_with_editor_changed.emit(value)

    def __init__(self, parent=None):
        super(OutlineTreeWidget, self).__init__(parent)
        self._context_actions = []
        self._definitions = None
        self._flattened_defs = None
        self._editor = None
        self._outline_mode = None
        self._folding_panel = None
        self._expanded_items = []
        self.setHeaderHidden(True)
        self.itemClicked.connect(self._on_item_clicked)
        self.itemCollapsed.connect(self._on_item_state_changed)
        self.itemExpanded.connect(self._on_item_state_changed)
        self._updating = True
        self.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(self._show_context_menu)
        self._sync_with_editor = True
        self._action_sync = QtWidgets.QAction(
            QtGui.QIcon.fromTheme('view-refresh'), _('Sync with editor'), None)
        self._action_sync.setCheckable(True)
        self._action_sync.setChecked(self._sync_with_editor)
        self._action_sync.toggled.connect(self._on_action_sync_toggled)
        self.add_context_action(self._action_sync)

    def set_editor(self, editor):
        """
        Sets the current editor. The widget display the structure of that
        editor.

        :param editor: CodeEdit
        """
        try:
            self._editor.cursorPositionChanged.disconnect(self.sync)
        except (AttributeError, TypeError, RuntimeError, ReferenceError):
            pass
        try:
            self._outline_mode.document_changed.disconnect(
                self._on_changed)
        except (AttributeError, TypeError, RuntimeError, ReferenceError):
            pass
        try:
            self._folding_panel.trigger_state_changed.disconnect(
                self._on_block_state_changed)
        except (AttributeError, TypeError, RuntimeError, ReferenceError):
            pass
        if editor:
            self._editor = weakref.proxy(editor)
        else:
            self._editor = None

        if editor is not None:
            editor.cursorPositionChanged.connect(self.sync)
            try:
                self._folding_panel = weakref.proxy(
                    editor.panels.get(FoldingPanel))
            except KeyError:
                pass
            else:
                self._folding_panel.trigger_state_changed.connect(
                    self._on_block_state_changed)
            try:
                analyser = editor.modes.get(OutlineMode)
            except KeyError:
                self._outline_mode = None
            else:
                self._outline_mode = weakref.proxy(analyser)
                analyser.document_changed.connect(self._on_changed)
        self._on_changed()

    def _on_item_state_changed(self, item):
        if self._updating:
            return
        block = item.data(0, QtCore.Qt.UserRole).block
        assert isinstance(item, QtWidgets.QTreeWidgetItem)
        item_state = not item.isExpanded()
        block_state = TextBlockHelper.is_collapsed(block)
        if item_state != block_state:
            self._updating = True
            self._folding_panel.toggle_fold_trigger(block)
            self._updating = False

    def _on_block_state_changed(self, block, state):
        if self._updating:
            return
        data = block.userData()
        if data is not None:
            try:
                item_state = not data.tree_item.isExpanded()
                if item_state != state:
                    if state:
                        self.collapseItem(data.tree_item)
                    else:
                        self.expandItem(data.tree_item)
            except AttributeError:
                # a block that is not represented in the tree view has
                # folded/unfolded, just ignore it
                pass

    def _on_changed(self):
        """
        Update the tree items
        """
        self._updating = True
        to_collapse = []
        self.clear()
        if self._editor and self._outline_mode and self._folding_panel:
            items, to_collapse = self.to_tree_widget_items(
                self._outline_mode.definitions, to_collapse=to_collapse)
            if len(items):
                self.addTopLevelItems(items)
                self.expandAll()
                for item in reversed(to_collapse):
                    self.collapseItem(item)
                self._updating = False
                return

        # no data
        root = QtWidgets.QTreeWidgetItem()
        root.setText(0, _('No data'))
        root.setIcon(0, icons.icon(
            'dialog-information', ':/pyqode-icons/rc/dialog-info.png',
            'fa.info-circle'))
        self.addTopLevelItem(root)
        self._updating = False
        self.sync()

    def _on_item_clicked(self, item):
        """
        Go to the item position in the editor.
        """
        if item:
            name = item.data(0, QtCore.Qt.UserRole)
            if name:
                go = name.block.blockNumber()
                helper = TextHelper(self._editor)
                if helper.current_line_nbr() != go:
                    helper.goto_line(go, column=name.column)
                self._editor.setFocus()

    def to_tree_widget_items(self, definitions, to_collapse=None):
        """
        Converts the list of top level definitions to a list of top level
        tree items.
        """
        def flatten(definitions):
            """
            Flattens the document structure tree as a simple sequential list.
            """
            ret_val = []
            for de in definitions:
                ret_val.append(de)
                for sub_d in de.children:
                    ret_val.append(sub_d)
                    ret_val += flatten(sub_d.children)
            return ret_val

        def convert(name, editor, to_collapse):
            ti = QtWidgets.QTreeWidgetItem()
            ti.setText(0, name.name)
            if isinstance(name.icon, list):
                icon = QtGui.QIcon.fromTheme(
                    name.icon[0], QtGui.QIcon(name.icon[1]))
            else:
                icon = QtGui.QIcon(name.icon)
            ti.setIcon(0, icon)
            name.block = editor.document().findBlockByNumber(name.line)
            ti.setData(0, QtCore.Qt.UserRole, name)
            ti.setToolTip(0, name.description)
            name.tree_item = ti
            block_data = name.block.userData()
            if block_data is None:
                block_data = TextBlockUserData()
                name.block.setUserData(block_data)
            block_data.tree_item = ti

            if to_collapse is not None and \
                    TextBlockHelper.is_collapsed(name.block):
                to_collapse.append(ti)

            for ch in name.children:
                ti_ch, to_collapse = convert(ch, editor, to_collapse)
                if ti_ch:
                    ti.addChild(ti_ch)
            return ti, to_collapse

        self._definitions = definitions
        self._flattened_defs = flatten(self._definitions)

        items = []
        for d in definitions:
            value, to_collapse = convert(d, self._editor, to_collapse)
            items.append(value)
        if to_collapse is not None:
            return items, to_collapse

        return items

    def sync(self):
        if not self.sync_with_editor or self._editor is None or \
                not self._definitions:
            return

        to_select = None
        previous = None
        current_line = TextHelper(self._editor).current_line_nbr()
        for d in self._flattened_defs:
            if d.line == current_line:
                to_select = d.tree_item
            elif d.line > current_line:
                to_select = d.tree_item
                if previous is not None:
                    to_select = previous.tree_item
            previous = d
            if to_select is not None:
                break
        else:
            if previous:
                to_select = previous.tree_item

        if to_select:
            try:
                self.setCurrentItem(to_select)
            except RuntimeError:
                # RuntimeError: wrapped C/C++ object of type QTreeWidgetItem
                # has been deleted
                pass

    def add_context_action(self, action):
        self._context_actions.append(action)

    def _show_context_menu(self, pos):
        mnu = QtWidgets.QMenu(self)
        for action in self._context_actions:
            mnu.addAction(action)
        mnu.exec_(self.mapToGlobal(pos))

    def _on_action_sync_toggled(self, value):
        self.sync_with_editor = value
        if value:
            self.sync()
