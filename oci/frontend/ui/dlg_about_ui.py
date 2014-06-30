# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'dlg_about.ui'
#
# Created: Sun May 25 17:55:18 2014
#      by: PyQt5 UI code generator 5.2.1
#
# WARNING! All changes made in this file will be lost!

from pyqode.core.qt import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(650, 700)
        Dialog.setMinimumSize(QtCore.QSize(650, 700))
        Dialog.setMaximumSize(QtCore.QSize(700, 16777215))
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/ide-icons/rc/dialog-information.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        Dialog.setWindowIcon(icon)
        self.verticalLayout = QtWidgets.QVBoxLayout(Dialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.verticalLayout_6 = QtWidgets.QVBoxLayout()
        self.verticalLayout_6.setContentsMargins(-1, 0, -1, -1)
        self.verticalLayout_6.setObjectName("verticalLayout_6")
        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.horizontalLayout.setContentsMargins(0, 0, -1, -1)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.verticalLayout_3 = QtWidgets.QVBoxLayout()
        self.verticalLayout_3.setObjectName("verticalLayout_3")
        self.labelIcon = QtWidgets.QLabel(Dialog)
        self.labelIcon.setObjectName("labelIcon")
        self.verticalLayout_3.addWidget(self.labelIcon)
        spacerItem = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.verticalLayout_3.addItem(spacerItem)
        self.horizontalLayout.addLayout(self.verticalLayout_3)
        self.line = QtWidgets.QFrame(Dialog)
        self.line.setFrameShape(QtWidgets.QFrame.VLine)
        self.line.setFrameShadow(QtWidgets.QFrame.Sunken)
        self.line.setObjectName("line")
        self.horizontalLayout.addWidget(self.line)
        self.verticalLayout_4 = QtWidgets.QVBoxLayout()
        self.verticalLayout_4.setObjectName("verticalLayout_4")
        self.labelMain = QtWidgets.QLabel(Dialog)
        self.labelMain.setObjectName("labelMain")
        self.verticalLayout_4.addWidget(self.labelMain)
        self.line_2 = QtWidgets.QFrame(Dialog)
        self.line_2.setFrameShape(QtWidgets.QFrame.HLine)
        self.line_2.setFrameShadow(QtWidgets.QFrame.Sunken)
        self.line_2.setObjectName("line_2")
        self.verticalLayout_4.addWidget(self.line_2)
        self.textBrowser = QtWidgets.QTextBrowser(Dialog)
        self.textBrowser.setReadOnly(True)
        self.textBrowser.setTextInteractionFlags(QtCore.Qt.LinksAccessibleByMouse|QtCore.Qt.TextSelectableByMouse)
        self.textBrowser.setOpenExternalLinks(True)
        self.textBrowser.setOpenLinks(True)
        self.textBrowser.setObjectName("textBrowser")
        self.verticalLayout_4.addWidget(self.textBrowser)
        self.line_3 = QtWidgets.QFrame(Dialog)
        self.line_3.setFrameShape(QtWidgets.QFrame.HLine)
        self.line_3.setFrameShadow(QtWidgets.QFrame.Sunken)
        self.line_3.setObjectName("line_3")
        self.verticalLayout_4.addWidget(self.line_3)
        self.label = QtWidgets.QLabel(Dialog)
        self.label.setObjectName("label")
        self.verticalLayout_4.addWidget(self.label)
        self.tbwVersions = QtWidgets.QTableWidget(Dialog)
        self.tbwVersions.setMaximumSize(QtCore.QSize(16777215, 212))
        self.tbwVersions.setFocusPolicy(QtCore.Qt.NoFocus)
        self.tbwVersions.setEditTriggers(QtWidgets.QAbstractItemView.NoEditTriggers)
        self.tbwVersions.setSelectionMode(QtWidgets.QAbstractItemView.NoSelection)
        self.tbwVersions.setGridStyle(QtCore.Qt.SolidLine)
        self.tbwVersions.setCornerButtonEnabled(False)
        self.tbwVersions.setObjectName("tbwVersions")
        self.tbwVersions.setColumnCount(1)
        self.tbwVersions.setRowCount(6)
        item = QtWidgets.QTableWidgetItem()
        self.tbwVersions.setVerticalHeaderItem(0, item)
        item = QtWidgets.QTableWidgetItem()
        self.tbwVersions.setVerticalHeaderItem(1, item)
        item = QtWidgets.QTableWidgetItem()
        self.tbwVersions.setVerticalHeaderItem(2, item)
        item = QtWidgets.QTableWidgetItem()
        self.tbwVersions.setVerticalHeaderItem(3, item)
        item = QtWidgets.QTableWidgetItem()
        self.tbwVersions.setVerticalHeaderItem(4, item)
        item = QtWidgets.QTableWidgetItem()
        self.tbwVersions.setVerticalHeaderItem(5, item)
        item = QtWidgets.QTableWidgetItem()
        self.tbwVersions.setHorizontalHeaderItem(0, item)
        self.tbwVersions.horizontalHeader().setStretchLastSection(True)
        self.tbwVersions.verticalHeader().setSortIndicatorShown(False)
        self.tbwVersions.verticalHeader().setStretchLastSection(False)
        self.verticalLayout_4.addWidget(self.tbwVersions)
        self.horizontalLayout.addLayout(self.verticalLayout_4)
        self.verticalLayout_6.addLayout(self.horizontalLayout)
        self.verticalLayout.addLayout(self.verticalLayout_6)
        self.buttonBox = QtWidgets.QDialogButtonBox(Dialog)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtWidgets.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(Dialog)
        self.buttonBox.accepted.connect(Dialog.accept)
        self.buttonBox.rejected.connect(Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "About OpenCobolIDE"))
        self.labelIcon.setText(_translate("Dialog", "<html><head/><body><p><img src=\":/ide-icons/rc/silex-64x64.png\"/></p></body></html>"))
        self.labelMain.setText(_translate("Dialog", "<html><head/><body><p><span style=\" font-size:14pt; font-weight:600;\">OpenCobolIDE</span></p><p><span style=\" text-decoration: underline;\">Version</span>: <span style=\" font-weight:600;\">%s</span></p></body></html>"))
        self.textBrowser.setHtml(_translate("Dialog", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Cantarell\'; font-size:11pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; color:#414141;\">OpenCobol IDE is a simple cobol IDE written in python based on OpenCobol and pyQode.</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; font-weight:600; text-decoration: underline; color:#414141;\">Authors:</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; color:#414141;\">Colin Duquesnoy  &lt;</span><a href=\"mailto:colin.duquesnoy@gmail\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; text-decoration: underline; color:#2768ff;\">colin.duquesnoy@gmail</span></a><a href=\"mailto:colin.duquesnoy@gmail\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; text-decoration: underline; color:#414141;\">&gt;</span></a></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; font-weight:600; text-decoration: underline; color:#414141;\">License:</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; color:#414141;\">This software is licensed under the </span><span style=\" font-family:\'Ubuntu\'; font-size:9pt; font-weight:600; color:#414141;\">GPL v3.</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; font-weight:600; text-decoration: underline; color:#414141;\">Web:</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a href=\"https://github.com/OpenCobolIDE/OpenCobolIDE\"><span style=\" font-family:\'Sans\'; font-size:9pt; text-decoration: underline; color:#2768ff;\">Github repository</span></a></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; font-weight:600; text-decoration: underline; color:#414141;\">Contributors: </span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; color:#414141;\">- </span><a href=\"mailto:thiry.celi@gmail.com\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; text-decoration: underline; color:#2768ff;\">Céline Thiry</span></a><span style=\" font-family:\'Ubuntu\'; font-size:9pt; color:#414141;\"> : application icon designer</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; font-weight:600; text-decoration: underline; color:#414141;\">Credits: </span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a href=\"http://python.org/\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; text-decoration: underline; color:#2768ff;\">Python</span></a><span style=\" font-family:\'Ubuntu\'; font-size:9pt; color:#414141;\"> , </span><a href=\"http://qt-project.org/\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; text-decoration: underline; color:#2768ff;\">Qt</span></a><span style=\" font-family:\'Ubuntu\'; font-size:9pt; color:#2768ff;\">/</span><a href=\"http://www.riverbankcomputing.com/software/pyqt/intro\"><span style=\" font-family:\'Sans\'; font-size:9pt; text-decoration: underline; color:#2768ff;\">PyQt</span></a><span style=\" font-family:\'Ubuntu\'; font-size:9pt; color:#414141;\"> , </span><a href=\"https://github.com/ColinDuquesnoy/pyqode-core\"><span style=\" font-family:\'Sans\'; font-size:9pt; text-decoration: underline; color:#2768ff;\">PyQode</span></a><span style=\" font-family:\'Ubuntu\'; font-size:9pt; color:#414141;\"> , </span><a href=\"https://pypi.python.org/pypi/chardet\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; text-decoration: underline; color:#2768ff;\">chardet</span></a><span style=\" font-family:\'Ubuntu\'; font-size:9pt; color:#414141;\"> , </span><a href=\"http://oxygen-icons.org\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; text-decoration: underline; color:#2768ff;\">Oxygen icons</span></a><span style=\" font-family:\'Ubuntu\'; font-size:9pt; font-style:italic; color:#414141;\"> , </span><a href=\"http://www.opencobol.org/\"><span style=\" font-family:\'Ubuntu\'; font-size:9pt; text-decoration: underline; color:#2768ff;\">OpenCobol</span></a><span style=\" font-family:\'Sans\'; font-size:9pt; color:#414141;\"> , </span><a href=\"https://github.com/ColinDuquesnoy/QDarkStyleSheet\"><span style=\" font-family:\'Sans\'; font-size:9pt; text-decoration: underline; color:#2768ff;\">QDarkStyle</span></a></p></body></html>"))
        self.label.setText(_translate("Dialog", "Third party libraries versions:"))
        item = self.tbwVersions.verticalHeaderItem(0)
        item.setText(_translate("Dialog", "OpenCobol"))
        item = self.tbwVersions.verticalHeaderItem(1)
        item.setText(_translate("Dialog", "Qt"))
        item = self.tbwVersions.verticalHeaderItem(2)
        item.setText(_translate("Dialog", "PyQt"))
        item = self.tbwVersions.verticalHeaderItem(3)
        item.setText(_translate("Dialog", "pyqode.core"))
        item = self.tbwVersions.verticalHeaderItem(4)
        item.setText(_translate("Dialog", "Pygments"))
        item = self.tbwVersions.verticalHeaderItem(5)
        item.setText(_translate("Dialog", "QDarkStyle"))
        item = self.tbwVersions.horizontalHeaderItem(0)
        item.setText(_translate("Dialog", "Version"))

from . import ide_rc