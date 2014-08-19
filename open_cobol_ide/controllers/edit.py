"""
Contains the EditController.

"""
from .base import Controller


class EditController(Controller):
    """
    Controls the edit view (and the edit menu).

    """
    def __init__(self, app):
        super().__init__(app)
        self.ui.tabWidgetEditors.last_tab_closed.connect(
            self.app.view.show_home)
        self.ui.tabWidgetEditors.currentChanged.connect(
            self._current_changed)
        self.ui.tableWidgetOffsets.show_requested.connect(
            self.ui.dockWidgetOffsets.show)

    def _current_changed(self, new_index):
        if new_index == -1:
            self.main_window.setWindowTitle(
                self.app.title)
        else:
            editor = self.ui.tabWidgetEditors.currentWidget()
            self.main_window.setWindowTitle(
                '[%s] - %s' % (editor.file.path, self.app.title))
            self.ui.twNavigation.set_editor(editor)
            self.ui.tableWidgetOffsets.set_editor(editor)
