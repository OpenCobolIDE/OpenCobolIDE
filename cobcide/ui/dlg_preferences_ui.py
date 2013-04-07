# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'dlg_preferences.ui'
#
# Created: Sun Apr  7 22:58:53 2013
#      by: pyside-uic 0.2.13 running on PySide 1.1.1
#
# WARNING! All changes made in this file will be lost!

from PySide import QtCore, QtGui

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(665, 482)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/ide-icons/rc/Preferences-system.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        Dialog.setWindowIcon(icon)
        self.gridLayout = QtGui.QGridLayout(Dialog)
        self.gridLayout.setObjectName("gridLayout")
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.listWidget = QtGui.QListWidget(Dialog)
        self.listWidget.setMaximumSize(QtCore.QSize(72, 16777215))
        self.listWidget.setIconSize(QtCore.QSize(64, 64))
        self.listWidget.setTextElideMode(QtCore.Qt.ElideMiddle)
        self.listWidget.setViewMode(QtGui.QListView.IconMode)
        self.listWidget.setObjectName("listWidget")
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(":/ide-icons/rc/Preferences-system-64.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        item = QtGui.QListWidgetItem(self.listWidget)
        item.setIcon(icon1)
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(":/ide-icons/rc/Mypaint-icon.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        item = QtGui.QListWidgetItem(self.listWidget)
        item.setIcon(icon2)
        self.horizontalLayout.addWidget(self.listWidget)
        self.stackedWidget = QtGui.QStackedWidget(Dialog)
        self.stackedWidget.setObjectName("stackedWidget")
        self.pageGeneral = QtGui.QWidget()
        self.pageGeneral.setObjectName("pageGeneral")
        self.formLayout = QtGui.QFormLayout(self.pageGeneral)
        self.formLayout.setObjectName("formLayout")
        self.checkBox = QtGui.QCheckBox(self.pageGeneral)
        self.checkBox.setObjectName("checkBox")
        self.formLayout.setWidget(0, QtGui.QFormLayout.LabelRole, self.checkBox)
        self.checkBox_2 = QtGui.QCheckBox(self.pageGeneral)
        self.checkBox_2.setObjectName("checkBox_2")
        self.formLayout.setWidget(1, QtGui.QFormLayout.LabelRole, self.checkBox_2)
        self.stackedWidget.addWidget(self.pageGeneral)
        self.pageStyle = QtGui.QWidget()
        self.pageStyle.setObjectName("pageStyle")
        self.stackedWidget.addWidget(self.pageStyle)
        self.horizontalLayout.addWidget(self.stackedWidget)
        self.gridLayout.addLayout(self.horizontalLayout, 0, 0, 1, 1)
        self.buttonBox = QtGui.QDialogButtonBox(Dialog)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.gridLayout.addWidget(self.buttonBox, 1, 0, 1, 1)

        self.retranslateUi(Dialog)
        self.stackedWidget.setCurrentIndex(0)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("accepted()"), Dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("rejected()"), Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(QtGui.QApplication.translate("Dialog", "Preferences", None, QtGui.QApplication.UnicodeUTF8))
        __sortingEnabled = self.listWidget.isSortingEnabled()
        self.listWidget.setSortingEnabled(False)
        self.listWidget.item(0).setText(QtGui.QApplication.translate("Dialog", "General", None, QtGui.QApplication.UnicodeUTF8))
        self.listWidget.item(1).setText(QtGui.QApplication.translate("Dialog", "Style", None, QtGui.QApplication.UnicodeUTF8))
        self.listWidget.setSortingEnabled(__sortingEnabled)
        self.checkBox.setText(QtGui.QApplication.translate("Dialog", "Show line numbers", None, QtGui.QApplication.UnicodeUTF8))
        self.checkBox_2.setText(QtGui.QApplication.translate("Dialog", "Show whitespaces", None, QtGui.QApplication.UnicodeUTF8))

import ide_rc
