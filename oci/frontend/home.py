# Homepage stylesheets
from pyqode.core.frontend import widgets
from pyqode.qt import QtWidgets, QtCore, QtGui

from oci import constants
from oci.frontend import services


frame_recent_white = """
QFrame
{
border: 1px solid rgb(150, 150, 150);
border-radius: 5px;
background-color: rgb(255, 255, 255);
}
"""

list_recent_white = """
QListWidget
{
border: none;
background-color: transparent;
}
"""

label_recent_white = """border: none;
border-top-left-radius: 3px;
border-top-right-radius: 3px;
border-bottom-left-radius: 0px;
border-bottom-right-radius: 0px;
background-color: rgb(206, 206, 206);
padding: 5px;
border-bottom: 1px solid rgb(150, 150, 150);
"""

frame_recent_dark = """border: 1px solid rgb(80, 80, 80);
border-radius: 5px;
"""

list_recent_dark = """border: none;
background-color: transparent;
"""

label_recent_dark = """border: none;
border-top-left-radius: 3px;
border-top-right-radius: 3px;
border-bottom-left-radius: 0px;
border-bottom-right-radius: 0px;
background-color: rgb(64, 64, 64);
padding: 5px;
border-bottom: 1px solid rgb(80, 80, 80);
"""


def clear_recents():
    services.main_window().recent_files_manager.clear()
    updateRecents()


def remove_current_recent_file():
    filename = services.main_window().listWidgetRecents.currentItem().data(32)
    files = services.main_window().recent_files_manager.get_recent_files()
    files.remove(filename)
    services.main_window().recent_files_manager.remove(filename)
    services.main_window().updateRecents()


def setup_recent_files():
    """ Setup the recent files menu and manager """
    services.main_window().recent_files_manager = widgets.RecentFilesManager(
        'OpenCobolIDE', 'OpenCobolIDE')
    services.main_window().menu_recents = widgets.MenuRecentFiles(
        services.main_window().menuFile, title='Recents',
        recent_files_manager=services.main_window().recent_files_manager)
    services.main_window().menu_recents.open_requested.connect(services.main_window().openFile)
    services.main_window().menu_recents.clear_requested.connect(services.main_window().listWidgetRecents.clear)
    services.main_window().menu_recents.clear()
    services.main_window().menuFile.insertMenu(services.main_window().actionQuit, services.main_window().menu_recents)
    services.main_window().menuFile.insertSeparator(services.main_window().actionQuit)
    updateRecents()
    # setup style
    services.main_window().listWidgetRecents.clear_requested.connect(clear_recents)
    services.main_window().listWidgetRecents.remove_current_requested.connect(
        remove_current_recent_file)
    services.main_window().frameRecents.setStyleSheet(frame_recent_white)
    services.main_window().labelRecents.setStyleSheet(label_recent_white)
    services.main_window().listWidgetRecents.setStyleSheet(list_recent_white)


def updateRecents():
    services.main_window().menu_recents.update_actions()
    services.main_window().listWidgetRecents.clear()
    for file in services.main_window().recent_files_manager.get_recent_files():
        item = QtWidgets.QListWidgetItem()
        if ('.' + QtCore.QFileInfo(file).suffix().upper() in
            constants.COBOL_EXTENSIONS):
            icon = QtGui.QIcon(":/ide-icons/rc/silex-32x32.png")
        else:
            icon = QtGui.QIcon(":/ide-icons/rc/text-x-generic.png")
        item.setText(QtCore.QFileInfo(file).fileName())
        item.setToolTip(file)
        item.setIcon(icon)
        item.setData(32, file)
        services.main_window().listWidgetRecents.addItem(item)