import os
from pyqode.qt import PYQT4_API, PYSIDE_API
from pyqode.qt.QtCore import QObject, Signal, Slot
from pyqode.qt.QtGui import QIcon
from pyqode.qt.QtWidgets import QAction
from pyqode.cobol.api import get_field_infos
from pyqode.core.api import Mode, TextHelper


class OffsetCalculatorMode(QObject, Mode):
    """
    This modes computes the selected PIC fields offsets.

    It adds a "Calculate PIC offsets" action to the editor context menu and
    emits the signal |pic_infos_available| when the the user triggered the
    action and the pic infos have been computed.
    """
    pic_infos_available = Signal(list)

    def __init__(self):
        if os.environ['QT_API'] in (PYQT4_API + PYSIDE_API):
            QObject.__init__(self)
            Mode.__init__(self)
        else:
            super().__init__()

    def on_install(self, editor):
        super().on_install(editor)
        self.action = QAction(editor)
        self.action.setText(_("Calculate PIC offsets"))
        self.action.setIcon(QIcon.fromTheme('accessories-calculator'))
        self.action.setShortcut('Ctrl+Shift+O')
        self.action.setToolTip(_('Compute the PIC offset of the fields in the '
                                 'selected text'))
        editor.add_action(self.action, sub_menu='COBOL')
        self.action.triggered.connect(self._compute_offsets)

    def _compute_offsets(self):
        original_tc = self.editor.textCursor()
        tc = self.editor.textCursor()
        start = tc.selectionStart()
        end = tc.selectionEnd()
        tc.setPosition(start)
        start_line = tc.blockNumber()
        tc.setPosition(end)
        end_line = tc.blockNumber()
        th = TextHelper(self.editor)
        th.select_lines(start=start_line, end=end_line, apply_selection=True)
        source = th.selected_text()
        results = get_field_infos(source, self.editor.free_format)
        self.editor.setTextCursor(original_tc)
        self.pic_infos_available.emit(results)
