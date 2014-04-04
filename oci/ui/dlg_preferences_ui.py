# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'dlg_preferences.ui'
#
# Created: Fri Apr  4 23:02:44 2014
#      by: PyQt4 UI code generator 4.10.4
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    def _fromUtf8(s):
        return s

try:
    _encoding = QtGui.QApplication.UnicodeUTF8
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig, _encoding)
except AttributeError:
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig)

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName(_fromUtf8("Dialog"))
        Dialog.resize(739, 594)
        icon = QtGui.QIcon.fromTheme(_fromUtf8("preferences-system"))
        Dialog.setWindowIcon(icon)
        self.verticalLayout = QtGui.QVBoxLayout(Dialog)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.horizontalLayout_2 = QtGui.QHBoxLayout()
        self.horizontalLayout_2.setContentsMargins(0, -1, -1, -1)
        self.horizontalLayout_2.setObjectName(_fromUtf8("horizontalLayout_2"))
        self.lwMenu = QtGui.QListWidget(Dialog)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.lwMenu.sizePolicy().hasHeightForWidth())
        self.lwMenu.setSizePolicy(sizePolicy)
        self.lwMenu.setMaximumSize(QtCore.QSize(86, 16777215))
        self.lwMenu.setDragDropMode(QtGui.QAbstractItemView.NoDragDrop)
        self.lwMenu.setIconSize(QtCore.QSize(64, 64))
        self.lwMenu.setTextElideMode(QtCore.Qt.ElideMiddle)
        self.lwMenu.setMovement(QtGui.QListView.Static)
        self.lwMenu.setFlow(QtGui.QListView.TopToBottom)
        self.lwMenu.setLayoutMode(QtGui.QListView.SinglePass)
        self.lwMenu.setViewMode(QtGui.QListView.IconMode)
        self.lwMenu.setUniformItemSizes(True)
        self.lwMenu.setObjectName(_fromUtf8("lwMenu"))
        item = QtGui.QListWidgetItem()
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/ide-icons/rc/Preferences-system.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        item.setIcon(icon)
        self.lwMenu.addItem(item)
        item = QtGui.QListWidgetItem()
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(_fromUtf8(":/ide-icons/rc/applications-graphics.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        item.setIcon(icon1)
        self.lwMenu.addItem(item)
        item = QtGui.QListWidgetItem()
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(_fromUtf8(":/ide-icons/rc/media-playback-start.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        item.setIcon(icon2)
        self.lwMenu.addItem(item)
        self.horizontalLayout_2.addWidget(self.lwMenu)
        self.line_3 = QtGui.QFrame(Dialog)
        self.line_3.setFrameShape(QtGui.QFrame.VLine)
        self.line_3.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_3.setObjectName(_fromUtf8("line_3"))
        self.horizontalLayout_2.addWidget(self.line_3)
        self.stackedWidget = QtGui.QStackedWidget(Dialog)
        self.stackedWidget.setObjectName(_fromUtf8("stackedWidget"))
        self.pageSettings = QtGui.QWidget()
        self.pageSettings.setObjectName(_fromUtf8("pageSettings"))
        self.gridLayout = QtGui.QGridLayout(self.pageSettings)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.tabWidgetSettings = QtGui.QTabWidget(self.pageSettings)
        self.tabWidgetSettings.setObjectName(_fromUtf8("tabWidgetSettings"))
        self.tabEditorsettings = QtGui.QWidget()
        self.tabEditorsettings.setObjectName(_fromUtf8("tabEditorsettings"))
        self.gridLayout_2 = QtGui.QGridLayout(self.tabEditorsettings)
        self.gridLayout_2.setObjectName(_fromUtf8("gridLayout_2"))
        self.propGridSettings = QPropertyGrid(self.tabEditorsettings)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.propGridSettings.sizePolicy().hasHeightForWidth())
        self.propGridSettings.setSizePolicy(sizePolicy)
        self.propGridSettings.setObjectName(_fromUtf8("propGridSettings"))
        self.gridLayout_2.addWidget(self.propGridSettings, 1, 0, 1, 1)
        self.tabWidgetSettings.addTab(self.tabEditorsettings, _fromUtf8(""))
        self.gridLayout.addWidget(self.tabWidgetSettings, 1, 0, 1, 1)
        self.stackedWidget.addWidget(self.pageSettings)
        self.pageStyle = QtGui.QWidget()
        self.pageStyle.setObjectName(_fromUtf8("pageStyle"))
        self.gridLayout_3 = QtGui.QGridLayout(self.pageStyle)
        self.gridLayout_3.setObjectName(_fromUtf8("gridLayout_3"))
        self.tabWidgetStyle = QtGui.QTabWidget(self.pageStyle)
        self.tabWidgetStyle.setObjectName(_fromUtf8("tabWidgetStyle"))
        self.tab = QtGui.QWidget()
        self.tab.setObjectName(_fromUtf8("tab"))
        self.verticalLayout_2 = QtGui.QVBoxLayout(self.tab)
        self.verticalLayout_2.setObjectName(_fromUtf8("verticalLayout_2"))
        self.horizontalLayout_3 = QtGui.QHBoxLayout()
        self.horizontalLayout_3.setObjectName(_fromUtf8("horizontalLayout_3"))
        spacerItem = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem)
        self.rbLightStyle = QtGui.QRadioButton(self.tab)
        self.rbLightStyle.setChecked(True)
        self.rbLightStyle.setObjectName(_fromUtf8("rbLightStyle"))
        self.horizontalLayout_3.addWidget(self.rbLightStyle)
        self.rbDarkStyle = QtGui.QRadioButton(self.tab)
        self.rbDarkStyle.setObjectName(_fromUtf8("rbDarkStyle"))
        self.horizontalLayout_3.addWidget(self.rbDarkStyle)
        spacerItem1 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem1)
        self.verticalLayout_2.addLayout(self.horizontalLayout_3)
        self.propGridStyle = QPropertyGrid(self.tab)
        self.propGridStyle.setObjectName(_fromUtf8("propGridStyle"))
        self.verticalLayout_2.addWidget(self.propGridStyle)
        self.line = QtGui.QFrame(self.tab)
        self.line.setFrameShape(QtGui.QFrame.HLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName(_fromUtf8("line"))
        self.verticalLayout_2.addWidget(self.line)
        self.codeEdit = QCobolCodeEdit(self.tab)
        self.codeEdit.setObjectName(_fromUtf8("codeEdit"))
        self.verticalLayout_2.addWidget(self.codeEdit)
        self.tabWidgetStyle.addTab(self.tab, _fromUtf8(""))
        self.tab_2 = QtGui.QWidget()
        self.tab_2.setObjectName(_fromUtf8("tab_2"))
        self.verticalLayout_3 = QtGui.QVBoxLayout(self.tab_2)
        self.verticalLayout_3.setObjectName(_fromUtf8("verticalLayout_3"))
        self.formLayout = QtGui.QFormLayout()
        self.formLayout.setObjectName(_fromUtf8("formLayout"))
        self.label = QtGui.QLabel(self.tab_2)
        self.label.setObjectName(_fromUtf8("label"))
        self.formLayout.setWidget(0, QtGui.QFormLayout.LabelRole, self.label)
        self.colorButtonConsoleBck = QColorButton(self.tab_2)
        self.colorButtonConsoleBck.setProperty("color", QtGui.QColor(255, 255, 255))
        self.colorButtonConsoleBck.setObjectName(_fromUtf8("colorButtonConsoleBck"))
        self.formLayout.setWidget(0, QtGui.QFormLayout.FieldRole, self.colorButtonConsoleBck)
        self.label_2 = QtGui.QLabel(self.tab_2)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.formLayout.setWidget(1, QtGui.QFormLayout.LabelRole, self.label_2)
        self.colorButtonConsoleFore = QColorButton(self.tab_2)
        self.colorButtonConsoleFore.setProperty("color", QtGui.QColor(64, 64, 64))
        self.colorButtonConsoleFore.setObjectName(_fromUtf8("colorButtonConsoleFore"))
        self.formLayout.setWidget(1, QtGui.QFormLayout.FieldRole, self.colorButtonConsoleFore)
        self.label_3 = QtGui.QLabel(self.tab_2)
        self.label_3.setObjectName(_fromUtf8("label_3"))
        self.formLayout.setWidget(2, QtGui.QFormLayout.LabelRole, self.label_3)
        self.label_4 = QtGui.QLabel(self.tab_2)
        self.label_4.setObjectName(_fromUtf8("label_4"))
        self.formLayout.setWidget(3, QtGui.QFormLayout.LabelRole, self.label_4)
        self.colorButtonConsoleApp = QColorButton(self.tab_2)
        self.colorButtonConsoleApp.setProperty("color", QtGui.QColor(64, 64, 255))
        self.colorButtonConsoleApp.setObjectName(_fromUtf8("colorButtonConsoleApp"))
        self.formLayout.setWidget(3, QtGui.QFormLayout.FieldRole, self.colorButtonConsoleApp)
        self.colorButtonConsoleUsr = QColorButton(self.tab_2)
        self.colorButtonConsoleUsr.setProperty("color", QtGui.QColor(34, 170, 34))
        self.colorButtonConsoleUsr.setObjectName(_fromUtf8("colorButtonConsoleUsr"))
        self.formLayout.setWidget(2, QtGui.QFormLayout.FieldRole, self.colorButtonConsoleUsr)
        self.verticalLayout_3.addLayout(self.formLayout)
        self.line_2 = QtGui.QFrame(self.tab_2)
        self.line_2.setFrameShape(QtGui.QFrame.HLine)
        self.line_2.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_2.setObjectName(_fromUtf8("line_2"))
        self.verticalLayout_3.addWidget(self.line_2)
        self.console = QInteractiveConsole(self.tab_2)
        self.console.setObjectName(_fromUtf8("console"))
        self.verticalLayout_3.addWidget(self.console)
        self.tabWidgetStyle.addTab(self.tab_2, _fromUtf8(""))
        self.tab_3 = QtGui.QWidget()
        self.tab_3.setObjectName(_fromUtf8("tab_3"))
        self.verticalLayout_4 = QtGui.QVBoxLayout(self.tab_3)
        self.verticalLayout_4.setObjectName(_fromUtf8("verticalLayout_4"))
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setContentsMargins(-1, 0, -1, -1)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        spacerItem2 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem2)
        self.radioButtonWhite = QtGui.QRadioButton(self.tab_3)
        self.radioButtonWhite.setChecked(True)
        self.radioButtonWhite.setObjectName(_fromUtf8("radioButtonWhite"))
        self.horizontalLayout.addWidget(self.radioButtonWhite)
        self.radioButtonDark = QtGui.QRadioButton(self.tab_3)
        self.radioButtonDark.setObjectName(_fromUtf8("radioButtonDark"))
        self.horizontalLayout.addWidget(self.radioButtonDark)
        spacerItem3 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem3)
        self.verticalLayout_4.addLayout(self.horizontalLayout)
        self.homeWidget = QHomeWidget(self.tab_3)
        icon3 = QtGui.QIcon()
        icon3.addPixmap(QtGui.QPixmap(_fromUtf8(":/ide-icons/rc/silex-64x64.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.homeWidget.setProperty("icon", icon3)
        self.homeWidget.setObjectName(_fromUtf8("homeWidget"))
        self.verticalLayout_4.addWidget(self.homeWidget)
        self.tabWidgetStyle.addTab(self.tab_3, _fromUtf8(""))
        self.gridLayout_3.addWidget(self.tabWidgetStyle, 0, 0, 1, 1)
        self.stackedWidget.addWidget(self.pageStyle)
        self.pageBuildAndRun = QtGui.QWidget()
        self.pageBuildAndRun.setObjectName(_fromUtf8("pageBuildAndRun"))
        self.formLayout_2 = QtGui.QFormLayout(self.pageBuildAndRun)
        self.formLayout_2.setFieldGrowthPolicy(QtGui.QFormLayout.AllNonFixedFieldsGrow)
        self.formLayout_2.setObjectName(_fromUtf8("formLayout_2"))
        self.label_6 = QtGui.QLabel(self.pageBuildAndRun)
        self.label_6.setObjectName(_fromUtf8("label_6"))
        self.formLayout_2.setWidget(1, QtGui.QFormLayout.LabelRole, self.label_6)
        self.checkBoxExtTerm = QtGui.QCheckBox(self.pageBuildAndRun)
        self.checkBoxExtTerm.setText(_fromUtf8(""))
        self.checkBoxExtTerm.setObjectName(_fromUtf8("checkBoxExtTerm"))
        self.formLayout_2.setWidget(1, QtGui.QFormLayout.FieldRole, self.checkBoxExtTerm)
        self.labelShellCmd = QtGui.QLabel(self.pageBuildAndRun)
        self.labelShellCmd.setObjectName(_fromUtf8("labelShellCmd"))
        self.formLayout_2.setWidget(2, QtGui.QFormLayout.LabelRole, self.labelShellCmd)
        self.horizontalLayout_5 = QtGui.QHBoxLayout()
        self.horizontalLayout_5.setObjectName(_fromUtf8("horizontalLayout_5"))
        self.lineEditShellCmd = QtGui.QLineEdit(self.pageBuildAndRun)
        self.lineEditShellCmd.setObjectName(_fromUtf8("lineEditShellCmd"))
        self.horizontalLayout_5.addWidget(self.lineEditShellCmd)
        spacerItem4 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_5.addItem(spacerItem4)
        self.formLayout_2.setLayout(2, QtGui.QFormLayout.FieldRole, self.horizontalLayout_5)
        self.stackedWidget.addWidget(self.pageBuildAndRun)
        self.horizontalLayout_2.addWidget(self.stackedWidget)
        self.verticalLayout.addLayout(self.horizontalLayout_2)
        self.buttonBox = QtGui.QDialogButtonBox(Dialog)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(Dialog)
        self.stackedWidget.setCurrentIndex(2)
        self.tabWidgetSettings.setCurrentIndex(0)
        self.tabWidgetStyle.setCurrentIndex(0)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("accepted()")), Dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("rejected()")), Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)
        Dialog.setTabOrder(self.lwMenu, self.tabWidgetStyle)
        Dialog.setTabOrder(self.tabWidgetStyle, self.codeEdit)
        Dialog.setTabOrder(self.codeEdit, self.console)
        Dialog.setTabOrder(self.console, self.radioButtonWhite)
        Dialog.setTabOrder(self.radioButtonWhite, self.radioButtonDark)
        Dialog.setTabOrder(self.radioButtonDark, self.tabWidgetSettings)
        Dialog.setTabOrder(self.tabWidgetSettings, self.buttonBox)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(_translate("Dialog", "Preferences", None))
        __sortingEnabled = self.lwMenu.isSortingEnabled()
        self.lwMenu.setSortingEnabled(False)
        item = self.lwMenu.item(0)
        item.setText(_translate("Dialog", "General", None))
        item.setToolTip(_translate("Dialog", "General", None))
        item = self.lwMenu.item(1)
        item.setText(_translate("Dialog", "Style", None))
        item.setToolTip(_translate("Dialog", " Style", None))
        item = self.lwMenu.item(2)
        item.setText(_translate("Dialog", "Build & Run", None))
        self.lwMenu.setSortingEnabled(__sortingEnabled)
        self.tabWidgetSettings.setTabText(self.tabWidgetSettings.indexOf(self.tabEditorsettings), _translate("Dialog", "Editor settings", None))
        self.rbLightStyle.setText(_translate("Dialog", "White style", None))
        self.rbDarkStyle.setText(_translate("Dialog", "Dark style", None))
        self.codeEdit.setPlainText(_translate("Dialog", "      *******************************************************************\n"
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
"", None))
        self.tabWidgetStyle.setTabText(self.tabWidgetStyle.indexOf(self.tab), _translate("Dialog", "Editor", None))
        self.label.setText(_translate("Dialog", "Background", None))
        self.label_2.setText(_translate("Dialog", "Foreground", None))
        self.label_3.setText(_translate("Dialog", "User input", None))
        self.label_4.setText(_translate("Dialog", "Application output", None))
        self.tabWidgetStyle.setTabText(self.tabWidgetStyle.indexOf(self.tab_2), _translate("Dialog", "Console", None))
        self.radioButtonWhite.setText(_translate("Dialog", "White", None))
        self.radioButtonDark.setText(_translate("Dialog", "Dark", None))
        self.homeWidget.setProperty("title", _translate("Dialog", "Welcome to OpenCoblIDE", None))
        self.tabWidgetStyle.setTabText(self.tabWidgetStyle.indexOf(self.tab_3), _translate("Dialog", "Home page", None))
        self.label_6.setText(_translate("Dialog", "Run program in external terminal:", None))
        self.labelShellCmd.setText(_translate("Dialog", "External terminal:", None))

from oci.editor import QCobolCodeEdit
from pyqode.widgets import QHomeWidget, QColorButton, QPropertyGrid, QInteractiveConsole
from . import ide_rc