"""
Contains the cobol code editor widget.

"""
from pyqode.core import modes
from pyqode.qt import QtGui, QtWidgets
from pyqode.core.api import Panel, ColorScheme
from pyqode.cobol.widgets import CobolCodeEdit as CodeEditBase

from ...compiler import get_file_type
from ...linter import CobolLinterMode
from ...settings import Settings


class CobolCodeEdit(CodeEditBase):
    """
    Cobol code editor. We specialise the pyqode.cobol code edit to add support
    for our settings system and for some custom properties (such as the
    file type).

    """

    def __init__(self, bt_compile, bt_run, parent=None):
        self._buttons = (bt_compile, bt_run)
        super().__init__(parent)
        self.syntax_highlighter.color_scheme = ColorScheme(
            Settings().color_scheme)
        self.linter_mode = self.modes.append(CobolLinterMode())
        self.auto_complete = self.modes.append(modes.AutoCompleteMode())

    def _setup_panels(self):
        self.control_panel = ControlPanel(*self._buttons)
        self.control_panel.setVisible(Settings().perspective == 'minimal')
        self.panels.append(self.control_panel, ControlPanel.Position.RIGHT)
        super()._setup_panels()

    @property
    def file_type(self):
        return get_file_type(self.file.path)

    @file_type.setter
    def file_type(self, ftype):
        Settings().set_file_type(self.file.path, ftype)


class ControlPanel(Panel):
    dropbtn_stylesheet = """
        QToolButton { /* all types of tool button */
        background-color: transparent;
        border: 1px solid transparent;
        border-radius: 8px;
        padding: 2px;
        }
        QToolButton[popupMode="1"] { /* only for MenuButtonPopup */
        padding-right: 10px; /* make way for the popup button */
        }
        QToolButton[popupMode="2"] { /* only for MenuButtonPopup */
        padding-right: 10px; /* make way for the popup button */
        }
        QToolButton:hover {
        background-color: rgba(128, 128, 128, 20);
        border: 1px solid rgba(128, 128, 128, 40);
        }
        QToolButton:pressed {
        background-color: rgba(128, 128, 128, 40);
        border: 1px solid rgba(128, 128, 128, 80);
        }
        /* the subcontrols below are used only in the MenuButtonPopup mode */
        QToolButton::menu-button {
        background-color: transparent;
        border: 1px transparent black;
        border-top-right-radius: 6px;
        border-bottom-right-radius: 6px;
        /* 16px width + 4px for border = 20px allocated above */
        width: 16px;
        }
        QToolButton::menu-arrow {
        image: url(:/ide-icons/rc/downarrow.png);
        }
        QToolButton::menu-arrow:open {
        top: 1px; left: 1px; /* shift it a bit */
        }
        """

    def __init__(self, bt_compile, bt_run):
        super().__init__()
        layout = QtWidgets.QVBoxLayout()
        spacer = QtWidgets.QSpacerItem(
            20, 20, QtWidgets.QSizePolicy.Minimum,
            QtWidgets.QSizePolicy.Expanding)
        layout.setContentsMargins(3, 3, 3, 3)
        layout.addWidget(bt_compile)
        layout.addWidget(bt_run)
        layout.addSpacerItem(spacer)
        bt_compile.setPopupMode(bt_compile.MenuButtonPopup)
        bt_compile.setStyleSheet(self.dropbtn_stylesheet)
        bt_run.setStyleSheet(self.dropbtn_stylesheet)
        self.setLayout(layout)

    def paintEvent(self, event):
        """ Fills the panel background. """
        # pylint: disable=invalid-name
        if self.isVisible():
            # fill background
            self._background_brush = QtGui.QBrush(
                self.editor.background)
            painter = QtGui.QPainter(self)
            painter.fillRect(event.rect(), self._background_brush)
