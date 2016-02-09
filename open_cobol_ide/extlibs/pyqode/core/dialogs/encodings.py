"""
This module contains some dialogs to help you manage encodings in
you application.

"""
import locale
from pyqode.core import icons
from pyqode.core.api import encodings
from pyqode.qt import QtCore, QtWidgets
from pyqode.core.cache import Cache
from pyqode.core._forms import dlg_preferred_encodings_editor_ui


class DlgPreferredEncodingsEditor(QtWidgets.QDialog):
    """
    This dialog is used to edit the preferred encodings that appears in
    the encodings menu/combo box.

    """
    def __init__(self, parent=None):
        super(DlgPreferredEncodingsEditor, self).__init__(parent)
        self.ui = dlg_preferred_encodings_editor_ui.Ui_Dialog()
        self.ui.setupUi(self)
        self._load_preferred()
        self.ui.pushButtonAdd.clicked.connect(self._add)
        self.ui.pushButtonAdd.setIcon(icons.icon(
            'go-next', ':/pyqode-icons/rc/go-next.png', 'fa.arrow-right'))
        self.ui.pushButtonRemove.setIcon(icons.icon(
            'go-previous', ':/pyqode-icons/rc/go-previous.png',
            'fa.arrow-left'))
        self.ui.pushButtonRemove.clicked.connect(self._remove)

    def _load_available(self):
        self.ui.tableWidgetAvailable.setColumnCount(2)
        self.ui.tableWidgetAvailable.setSelectionMode(
            self.ui.tableWidgetAvailable.SingleSelection)
        self.ui.tableWidgetAvailable.setSelectionBehavior(
            self.ui.tableWidgetAvailable.SelectRows)
        self.ui.tableWidgetAvailable.setHorizontalHeaderLabels([
            'Encoding', 'Language'])
        self.ui.tableWidgetAvailable.verticalHeader().hide()
        self.ui.tableWidgetAvailable.setSortingEnabled(True)
        preferred = Cache().preferred_encodings
        for key in sorted(encodings.ENCODINGS_MAP.keys()):
            value = encodings.ENCODINGS_MAP[key]
            if key not in preferred:
                # lang_item.setData(QtCore.Qt.UserRole, key)
                row = self.ui.tableWidgetAvailable.rowCount()
                self.ui.tableWidgetAvailable.insertRow(row)
                for column in range(2):
                    item = QtWidgets.QTableWidgetItem(value[column].strip())
                    item.setData(QtCore.Qt.UserRole, key)
                    # item.setData(QtCore.Qt.UserRole, key)
                    self.ui.tableWidgetAvailable.setItem(row, column, item)
        self.ui.tableWidgetAvailable.sortByColumn(0, QtCore.Qt.AscendingOrder)

    def _load_preferred(self):
        self._load_available()  # setup preferred encodings
        self.ui.tableWidgetPreferred.setColumnCount(2)
        self.ui.tableWidgetPreferred.setSelectionMode(
            self.ui.tableWidgetPreferred.SingleSelection)
        self.ui.tableWidgetPreferred.setSelectionBehavior(
            self.ui.tableWidgetPreferred.SelectRows)
        self.ui.tableWidgetPreferred.setHorizontalHeaderLabels([
            'Encoding', 'Language'])
        self.ui.tableWidgetPreferred.verticalHeader().hide()
        self.ui.tableWidgetPreferred.setSortingEnabled(True)
        for i, encoding in enumerate(Cache().preferred_encodings):
            encoding = encodings.convert_to_codec_key(encoding)
            value = encodings.ENCODINGS_MAP[encoding]
            row = self.ui.tableWidgetPreferred.rowCount()
            self.ui.tableWidgetPreferred.insertRow(row)
            for column in range(2):
                item = QtWidgets.QTableWidgetItem(value[column].strip())
                item.setData(QtCore.Qt.UserRole, encoding)
                self.ui.tableWidgetPreferred.setItem(row, column, item)
        self.ui.tableWidgetPreferred.sortByColumn(0, QtCore.Qt.AscendingOrder)

    def _transfer_selected_items(self, source, destination):
        # keeping sorting enabled cause bug for the second transferred item
        destination.setSortingEnabled(False)
        row = source.currentRow()
        if row != -1:
            # take items from source
            items = []
            encoding = source.item(row, 0).data(QtCore.Qt.UserRole)
            is_locale = encoding == encodings.convert_to_codec_key(
                locale.getpreferredencoding())
            if source == self.ui.tableWidgetPreferred and is_locale:
                destination.setSortingEnabled(True)
                return
            for i in range(2):
                items.append(source.takeItem(row, i))
            source.removeRow(row)
            # insert a new row in the taken items into destination
            row = destination.rowCount()
            destination.insertRow(row)
            for col, item in enumerate(items):
                item = QtWidgets.QTableWidgetItem(item)
                destination.setItem(row, col, item)
        destination.setSortingEnabled(True)

    def _add(self):
        self._transfer_selected_items(self.ui.tableWidgetAvailable,
                                      self.ui.tableWidgetPreferred)

    def _remove(self):
        self._transfer_selected_items(self.ui.tableWidgetPreferred,
                                      self.ui.tableWidgetAvailable)

    def get_preferred_encodings(self):
        """
        Gets the list of preferred encodings.
        :return: list
        """
        encodings = []
        for row in range(self.ui.tableWidgetPreferred.rowCount()):
            item = self.ui.tableWidgetPreferred.item(row, 0)
            encodings.append(item.data(QtCore.Qt.UserRole))
        return encodings

    @classmethod
    def edit_encoding(cls, parent):
        """
        Static helper method that shows the encoding editor dialog
        If the dialog was accepted the new encodings are added to the settings.

        :param parent: parent widget
        :return: True in case of succes, False otherwise

        """
        dlg = cls(parent)
        if dlg.exec_() == dlg.Accepted:
            settings = Cache()
            settings.preferred_encodings = dlg.get_preferred_encodings()
            return True
        return False


class DlgEncodingsChoice(QtWidgets.QDialog):
    """
    This dialogs ask the user to choose an encoding from a combo box.

    You can use it if you're not using the encoding panel when there is a
    decoding error when opening a file.

    """
    def __init__(self, parent, path, encoding):
        super(DlgEncodingsChoice, self).__init__(parent)
        self.setWindowTitle('Choose encoding')
        # avoid circular references with CodeEdit
        from pyqode.core._forms import dlg_encodings_ui
        self.ui = dlg_encodings_ui.Ui_Dialog()
        self.ui.setupUi(self)
        self.ui.comboBoxEncodings.current_encoding = encoding
        self.ui.lblDescription.setText(
            self.ui.lblDescription.text() %
            (_('There was a problem opening the file %r with encoding: %s') %
             (path, encoding)))

    @classmethod
    def choose_encoding(cls, parent, path, encoding):
        """
        Show the encodings dialog and returns the user choice.

        :param parent: parent widget.
        :param path: file path
        :param encoding: current file encoding
        :return: selected encoding
        """
        dlg = cls(parent, path, encoding)
        dlg.exec_()
        return dlg.ui.comboBoxEncodings.current_encoding


if __name__ == '__main__':
    import sys
    app = QtWidgets.QApplication(sys.argv)
    new_encoding = DlgEncodingsChoice.choose_encoding(None, __file__, 'utf-8')
    print(new_encoding)
