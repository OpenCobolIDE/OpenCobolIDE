# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'dlg_preferences.ui'
#
# Created: Thu Feb 20 16:00:45 2014
#      by: pyside-uic 0.2.14 running on PySide 1.1.2
#
# WARNING! All changes made in this file will be lost!

from pyqode.qt import QtCore, QtGui

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(843, 761)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        Dialog.setWindowIcon(icon)
        self.verticalLayout = QtGui.QVBoxLayout(Dialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.horizontalLayout_2 = QtGui.QHBoxLayout()
        self.horizontalLayout_2.setContentsMargins(0, -1, -1, -1)
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.lwMenu = QtGui.QListWidget(Dialog)
        self.lwMenu.setMaximumSize(QtCore.QSize(72, 16777215))
        self.lwMenu.setDragDropMode(QtGui.QAbstractItemView.NoDragDrop)
        self.lwMenu.setIconSize(QtCore.QSize(64, 64))
        self.lwMenu.setTextElideMode(QtCore.Qt.ElideMiddle)
        self.lwMenu.setMovement(QtGui.QListView.Static)
        self.lwMenu.setViewMode(QtGui.QListView.IconMode)
        self.lwMenu.setUniformItemSizes(False)
        self.lwMenu.setObjectName("lwMenu")
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(":/ide-icons/rc/Preferences-system-64.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        item = QtGui.QListWidgetItem(self.lwMenu)
        item.setIcon(icon1)
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(":/ide-icons/rc/Mypaint-icon.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        item = QtGui.QListWidgetItem(self.lwMenu)
        item.setIcon(icon2)
        self.horizontalLayout_2.addWidget(self.lwMenu)
        self.line_3 = QtGui.QFrame(Dialog)
        self.line_3.setFrameShape(QtGui.QFrame.VLine)
        self.line_3.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_3.setObjectName("line_3")
        self.horizontalLayout_2.addWidget(self.line_3)
        self.stackedWidget = QtGui.QStackedWidget(Dialog)
        self.stackedWidget.setObjectName("stackedWidget")
        self.page_3 = QtGui.QWidget()
        self.page_3.setObjectName("page_3")
        self.gridLayout = QtGui.QGridLayout(self.page_3)
        self.gridLayout.setObjectName("gridLayout")
        self.tabWidgetSettings = QtGui.QTabWidget(self.page_3)
        self.tabWidgetSettings.setObjectName("tabWidgetSettings")
        self.tabEditorsettings = QtGui.QWidget()
        self.tabEditorsettings.setObjectName("tabEditorsettings")
        self.gridLayout_2 = QtGui.QGridLayout(self.tabEditorsettings)
        self.gridLayout_2.setObjectName("gridLayout_2")
        self.propGridSettings = QPropertyGrid(self.tabEditorsettings)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.propGridSettings.sizePolicy().hasHeightForWidth())
        self.propGridSettings.setSizePolicy(sizePolicy)
        self.propGridSettings.setObjectName("propGridSettings")
        self.gridLayout_2.addWidget(self.propGridSettings, 1, 0, 1, 1)
        self.tabWidgetSettings.addTab(self.tabEditorsettings, "")
        self.gridLayout.addWidget(self.tabWidgetSettings, 1, 0, 1, 1)
        self.stackedWidget.addWidget(self.page_3)
        self.page_4 = QtGui.QWidget()
        self.page_4.setObjectName("page_4")
        self.gridLayout_3 = QtGui.QGridLayout(self.page_4)
        self.gridLayout_3.setObjectName("gridLayout_3")
        self.tabWidgetStyle = QtGui.QTabWidget(self.page_4)
        self.tabWidgetStyle.setObjectName("tabWidgetStyle")
        self.tab = QtGui.QWidget()
        self.tab.setObjectName("tab")
        self.verticalLayout_2 = QtGui.QVBoxLayout(self.tab)
        self.verticalLayout_2.setObjectName("verticalLayout_2")
        self.horizontalLayout_3 = QtGui.QHBoxLayout()
        self.horizontalLayout_3.setObjectName("horizontalLayout_3")
        spacerItem = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem)
        self.rbLightStyle = QtGui.QRadioButton(self.tab)
        self.rbLightStyle.setChecked(True)
        self.rbLightStyle.setObjectName("rbLightStyle")
        self.horizontalLayout_3.addWidget(self.rbLightStyle)
        self.rbDarkStyle = QtGui.QRadioButton(self.tab)
        self.rbDarkStyle.setObjectName("rbDarkStyle")
        self.horizontalLayout_3.addWidget(self.rbDarkStyle)
        spacerItem1 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem1)
        self.verticalLayout_2.addLayout(self.horizontalLayout_3)
        self.propGridStyle = QPropertyGrid(self.tab)
        self.propGridStyle.setObjectName("propGridStyle")
        self.verticalLayout_2.addWidget(self.propGridStyle)
        self.line = QtGui.QFrame(self.tab)
        self.line.setFrameShape(QtGui.QFrame.HLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName("line")
        self.verticalLayout_2.addWidget(self.line)
        self.codeEdit = QCobolCodeEdit(self.tab)
        self.codeEdit.setObjectName("codeEdit")
        self.verticalLayout_2.addWidget(self.codeEdit)
        self.tabWidgetStyle.addTab(self.tab, "")
        self.tab_2 = QtGui.QWidget()
        self.tab_2.setObjectName("tab_2")
        self.verticalLayout_3 = QtGui.QVBoxLayout(self.tab_2)
        self.verticalLayout_3.setObjectName("verticalLayout_3")
        self.formLayout = QtGui.QFormLayout()
        self.formLayout.setObjectName("formLayout")
        self.label = QtGui.QLabel(self.tab_2)
        self.label.setObjectName("label")
        self.formLayout.setWidget(0, QtGui.QFormLayout.LabelRole, self.label)
        self.colorButtonConsoleBck = QColorButton(self.tab_2)
        self.colorButtonConsoleBck.setProperty("color", QtGui.QColor(255, 255, 255))
        self.colorButtonConsoleBck.setObjectName("colorButtonConsoleBck")
        self.formLayout.setWidget(0, QtGui.QFormLayout.FieldRole, self.colorButtonConsoleBck)
        self.label_2 = QtGui.QLabel(self.tab_2)
        self.label_2.setObjectName("label_2")
        self.formLayout.setWidget(1, QtGui.QFormLayout.LabelRole, self.label_2)
        self.colorButtonConsoleFore = QColorButton(self.tab_2)
        self.colorButtonConsoleFore.setProperty("color", QtGui.QColor(64, 64, 64))
        self.colorButtonConsoleFore.setObjectName("colorButtonConsoleFore")
        self.formLayout.setWidget(1, QtGui.QFormLayout.FieldRole, self.colorButtonConsoleFore)
        self.label_3 = QtGui.QLabel(self.tab_2)
        self.label_3.setObjectName("label_3")
        self.formLayout.setWidget(2, QtGui.QFormLayout.LabelRole, self.label_3)
        self.label_4 = QtGui.QLabel(self.tab_2)
        self.label_4.setObjectName("label_4")
        self.formLayout.setWidget(3, QtGui.QFormLayout.LabelRole, self.label_4)
        self.colorButtonConsoleApp = QColorButton(self.tab_2)
        self.colorButtonConsoleApp.setProperty("color", QtGui.QColor(64, 64, 255))
        self.colorButtonConsoleApp.setObjectName("colorButtonConsoleApp")
        self.formLayout.setWidget(3, QtGui.QFormLayout.FieldRole, self.colorButtonConsoleApp)
        self.colorButtonConsoleUsr = QColorButton(self.tab_2)
        self.colorButtonConsoleUsr.setProperty("color", QtGui.QColor(34, 170, 34))
        self.colorButtonConsoleUsr.setObjectName("colorButtonConsoleUsr")
        self.formLayout.setWidget(2, QtGui.QFormLayout.FieldRole, self.colorButtonConsoleUsr)
        self.verticalLayout_3.addLayout(self.formLayout)
        self.line_2 = QtGui.QFrame(self.tab_2)
        self.line_2.setFrameShape(QtGui.QFrame.HLine)
        self.line_2.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_2.setObjectName("line_2")
        self.verticalLayout_3.addWidget(self.line_2)
        self.console = QInteractiveConsole(self.tab_2)
        self.console.setObjectName("console")
        self.verticalLayout_3.addWidget(self.console)
        self.tabWidgetStyle.addTab(self.tab_2, "")
        self.tab_3 = QtGui.QWidget()
        self.tab_3.setObjectName("tab_3")
        self.verticalLayout_4 = QtGui.QVBoxLayout(self.tab_3)
        self.verticalLayout_4.setObjectName("verticalLayout_4")
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setContentsMargins(-1, 0, -1, -1)
        self.horizontalLayout.setObjectName("horizontalLayout")
        spacerItem2 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem2)
        self.radioButtonWhite = QtGui.QRadioButton(self.tab_3)
        self.radioButtonWhite.setChecked(True)
        self.radioButtonWhite.setObjectName("radioButtonWhite")
        self.horizontalLayout.addWidget(self.radioButtonWhite)
        self.radioButtonDark = QtGui.QRadioButton(self.tab_3)
        self.radioButtonDark.setObjectName("radioButtonDark")
        self.horizontalLayout.addWidget(self.radioButtonDark)
        spacerItem3 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem3)
        self.verticalLayout_4.addLayout(self.horizontalLayout)
        self.homeWidget = QHomeWidget(self.tab_3)
        icon3 = QtGui.QIcon()
        icon3.addPixmap(QtGui.QPixmap(":/ide-icons/rc/silex-64x64.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.homeWidget.setProperty("icon", icon3)
        self.homeWidget.setObjectName("homeWidget")
        self.verticalLayout_4.addWidget(self.homeWidget)
        self.tabWidgetStyle.addTab(self.tab_3, "")
        self.gridLayout_3.addWidget(self.tabWidgetStyle, 0, 0, 1, 1)
        self.stackedWidget.addWidget(self.page_4)
        self.horizontalLayout_2.addWidget(self.stackedWidget)
        self.verticalLayout.addLayout(self.horizontalLayout_2)
        self.buttonBox = QtGui.QDialogButtonBox(Dialog)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(Dialog)
        self.stackedWidget.setCurrentIndex(1)
        self.tabWidgetSettings.setCurrentIndex(0)
        self.tabWidgetStyle.setCurrentIndex(0)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("accepted()"), Dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("rejected()"), Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)
        Dialog.setTabOrder(self.lwMenu, self.tabWidgetStyle)
        Dialog.setTabOrder(self.tabWidgetStyle, self.codeEdit)
        Dialog.setTabOrder(self.codeEdit, self.console)
        Dialog.setTabOrder(self.console, self.radioButtonWhite)
        Dialog.setTabOrder(self.radioButtonWhite, self.radioButtonDark)
        Dialog.setTabOrder(self.radioButtonDark, self.tabWidgetSettings)
        Dialog.setTabOrder(self.tabWidgetSettings, self.buttonBox)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(QtGui.QApplication.translate("Dialog", "Preferences", None, QtGui.QApplication.UnicodeUTF8))
        __sortingEnabled = self.lwMenu.isSortingEnabled()
        self.lwMenu.setSortingEnabled(False)
        self.lwMenu.item(0).setText(QtGui.QApplication.translate("Dialog", "General", None, QtGui.QApplication.UnicodeUTF8))
        self.lwMenu.item(1).setText(QtGui.QApplication.translate("Dialog", "Style", None, QtGui.QApplication.UnicodeUTF8))
        self.lwMenu.setSortingEnabled(__sortingEnabled)
        self.tabWidgetSettings.setTabText(self.tabWidgetSettings.indexOf(self.tabEditorsettings), QtGui.QApplication.translate("Dialog", "Editor settings", None, QtGui.QApplication.UnicodeUTF8))
        self.rbLightStyle.setText(QtGui.QApplication.translate("Dialog", "White style", None, QtGui.QApplication.UnicodeUTF8))
        self.rbDarkStyle.setText(QtGui.QApplication.translate("Dialog", "Dark style", None, QtGui.QApplication.UnicodeUTF8))
        self.codeEdit.setPlainText(QtGui.QApplication.translate("Dialog", "      *******************************************************************\n"
"      ** Example taken from http://progopedia.com/version/opencobol-1.0/*\n"
"      *******************************************************************\n"
"       IDENTIFICATION DIVISION.\n"
"      **************************************\n"
"       PROGRAM-ID. SAMPLE.\n"
"      **\n"
"       DATA DIVISION.\n"
"      **************************************\n"
"       WORKING-STORAGE SECTION.\n"
"      *-*-*-*-*-*-*-*-*-*-*-*-*-*\n"
"       77 FACT      PIC 9(15) comp  .\n"
"       77 N         PIC 99          .\n"
"       77 I         PIC 99          .\n"
"       77 IST       PIC XX          .\n"
"       77 FACTTST   PIC X(18)       .\n"
"      **\n"
"       PROCEDURE DIVISION.\n"
"      **************************************\n"
"       MAIN-PROCECURE.\n"
"           MOVE 16 to N\n"
"           MOVE 0 to I\n"
"           MOVE 1 to FACT\n"
"           PERFORM UNTIL I GREATER THAN N\n"
"               MOVE I to IST\n"
"               MOVE FACT to FACTTST\n"
"               DISPLAY IST \"! = \" FACTTST\n"
"               ADD 1 to I\n"
"               MULTIPLY I BY FACT\n"
"                    ON SIZE ERROR DISPLAY \"value too big\"\n"
"               END-MULTIPLY\n"
"           END-PERFORM\n"
"           EXIT PROGRAM.\n"
"       END PROGRAM SAMPLE.\n"
"      **\n"
"", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidgetStyle.setTabText(self.tabWidgetStyle.indexOf(self.tab), QtGui.QApplication.translate("Dialog", "Editor", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("Dialog", "Background", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("Dialog", "Foreground", None, QtGui.QApplication.UnicodeUTF8))
        self.label_3.setText(QtGui.QApplication.translate("Dialog", "User input", None, QtGui.QApplication.UnicodeUTF8))
        self.label_4.setText(QtGui.QApplication.translate("Dialog", "Application output", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidgetStyle.setTabText(self.tabWidgetStyle.indexOf(self.tab_2), QtGui.QApplication.translate("Dialog", "Console", None, QtGui.QApplication.UnicodeUTF8))
        self.radioButtonWhite.setText(QtGui.QApplication.translate("Dialog", "White", None, QtGui.QApplication.UnicodeUTF8))
        self.radioButtonDark.setText(QtGui.QApplication.translate("Dialog", "Dark", None, QtGui.QApplication.UnicodeUTF8))
        self.homeWidget.setProperty("title", QtGui.QApplication.translate("Dialog", "Welcome to OpenCoblIDE", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidgetStyle.setTabText(self.tabWidgetStyle.indexOf(self.tab_3), QtGui.QApplication.translate("Dialog", "Home page", None, QtGui.QApplication.UnicodeUTF8))

from oci.editor import QCobolCodeEdit
from pyqode.widgets import QHomeWidget, QColorButton, QPropertyGrid, QInteractiveConsole
from . import ide_rc