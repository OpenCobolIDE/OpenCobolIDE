"""
Contains cobol specific editor panels:
    - compile and run panel

"""
from pyqode.qt import QtWidgets, QtCore, QtGui
from pyqode.core import frontend

from oci.constants import ProgramType
from oci.frontend import services


dropbtn_stylesheet = """

QToolButton  { /* all types of tool button */
    background-color: transparent;
    border: 1px solid transparent;
    border-radius: 8px;
    padding: 2px;
    padding-right: 10px; /* make way for the popup button */
}

QToolButton:hover {
    background-color: rgba(128, 128, 128, 20);
    border: 1px solid rgba(128, 128, 128, 40);
}

QToolButton:pressed  {
    background-color: rgba(128, 128, 128, 40);
    border: 1px solid rgba(128, 128, 128, 80);
}

/* the subcontrols below are used only in the MenuButtonPopup mode */
QToolButton::menu-button  {
    border: 2px solid gray;
    border-top-right-radius: 6px;
    border-bottom-right-radius: 6px;
    /* 16px width + 4px for border = 20px allocated above */
    width: 16px;
}

/*QToolButton::menu-arrow  {
    image: url(downarrow.png);
}*/

QToolButton::menu-arrow:open  {
    top: 1px; left: 1px; /* shift it a bit */
}

"""


class ControlPanel(frontend.Panel):
    compilationRequested = QtCore.Signal()
    runRequested = QtCore.Signal()
    pgmTypeChangeRequested = QtCore.Signal(object)

    def _on_install(self, editor):
        super()._on_install(editor)
        services.main_window().actionCompile.changed.connect(
            self._on_action_changed)
        services.main_window().actionRun.changed.connect(
            self._on_action_changed)
        compileIcon = QtGui.QIcon.fromTheme(
            "application-x-executable", QtGui.QIcon(
                ":/ide-icons/rc/application-x-executable.png"))
        self.btCompile = QtWidgets.QToolButton()
        self.btCompile.setStyleSheet(dropbtn_stylesheet)
        self.btCompile.setIcon(compileIcon)
        runIcon = QtGui.QIcon.fromTheme(
            "media-playback-start", QtGui.QIcon(
                ":/ide-icons/rc/media-playback-start.png"))
        self.btRun = QtWidgets.QToolButton()
        self.btRun.setStyleSheet(dropbtn_stylesheet)
        self.btRun.setIcon(runIcon)
        if (self.position == self.Position.RIGHT or
                self.position == self.Position.LEFT):
            layout = QtWidgets.QVBoxLayout()
            spacer = QtWidgets.QSpacerItem(20, 20,
                                           QtWidgets.QSizePolicy.Minimum,
                                           QtWidgets.QSizePolicy.Expanding)
        else:
            layout = QtWidgets.QHBoxLayout()
            spacer = QtWidgets.QSpacerItem(20, 20,
                                           QtWidgets.QSizePolicy.Expanding,
                                           QtWidgets.QSizePolicy.Minimum)
        layout.setContentsMargins(0, 3, 0, 0)
        menu = QtWidgets.QMenu()
        self.actionGroup = QtWidgets.QActionGroup(self)
        self.actionExe = QtWidgets.QAction("Executable", self)
        self.actionExe.setCheckable(True)
        self.actionModule = QtWidgets.QAction('Module', self)
        self.actionModule.setCheckable(True)
        self.actionGroup.addAction(self.actionExe)
        self.actionGroup.addAction(self.actionModule)
        self.actionGroup.triggered.connect(self.onProgramTypeChangeRequest)
        menu.addActions([self.actionExe, self.actionModule])
        self.btCompile.setMenu(menu)
        self.btCompile.setPopupMode(self.btCompile.DelayedPopup)
        layout.addWidget(self.btCompile)
        layout.addWidget(self.btRun)
        layout.addSpacerItem(spacer)
        self.setLayout(layout)
        self.btCompile.clicked.connect(self.compilationRequested.emit)
        self.btRun.clicked.connect(self.runRequested.emit)

    def __del__(self):
        services.main_window().actionCompile.changed.disconnect(
            self._on_action_changed)
        services.main_window().actionRun.changed.disconnect(
            self._on_action_changed)

    def _on_state_changed(self, state):
        if state:
            self.editor.programTypeChanged.connect(self.updateButtonsStates)
            self.editor.new_text_set.connect(self.updateButtonsStates)
        else:
            self.editor.programTypeChanged.disconnect(self.updateButtonsStates)
            self.editor.new_text_set.connect(self.updateButtonsStates)

    def updateButtonsStates(self):
        flg = self.editor.programType == ProgramType.Executable
        self.btRun.setVisible(
            self.editor.programType == ProgramType.Executable)
        self.actionExe.setChecked(flg)
        self.actionModule.setChecked(not flg)

    def onProgramTypeChangeRequest(self, action):
        self.pgmTypeChangeRequested.emit(action)

    def paintEvent(self, event):
        """ Fills the panel background. """
        # pylint: disable=invalid-name
        if self.isVisible():
            # fill background
            self._background_brush = QtGui.QBrush(self.editor.background)
            painter = QtGui.QPainter(self)
            painter.fillRect(event.rect(), self._background_brush)

    def _on_action_changed(self):
        self.btCompile.setEnabled(
            services.main_window().actionCompile.isEnabled())
        self.btRun.setEnabled(
            services.main_window().actionRun.isEnabled())