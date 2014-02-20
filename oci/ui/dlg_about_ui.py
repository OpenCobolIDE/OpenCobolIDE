# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'dlg_about.ui'
#
# Created: Thu Feb 20 16:00:45 2014
#      by: pyside-uic 0.2.14 running on PySide 1.1.2
#
# WARNING! All changes made in this file will be lost!

from pyqode.qt import QtCore, QtGui

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(650, 700)
        Dialog.setMinimumSize(QtCore.QSize(650, 700))
        Dialog.setMaximumSize(QtCore.QSize(700, 16777215))
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/ide-icons/rc/dialog-information.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        Dialog.setWindowIcon(icon)
        self.verticalLayout = QtGui.QVBoxLayout(Dialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.verticalLayout_6 = QtGui.QVBoxLayout()
        self.verticalLayout_6.setContentsMargins(-1, 0, -1, -1)
        self.verticalLayout_6.setObjectName("verticalLayout_6")
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setContentsMargins(0, 0, -1, -1)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.verticalLayout_3 = QtGui.QVBoxLayout()
        self.verticalLayout_3.setObjectName("verticalLayout_3")
        self.labelIcon = QtGui.QLabel(Dialog)
        self.labelIcon.setObjectName("labelIcon")
        self.verticalLayout_3.addWidget(self.labelIcon)
        spacerItem = QtGui.QSpacerItem(20, 40, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.verticalLayout_3.addItem(spacerItem)
        self.horizontalLayout.addLayout(self.verticalLayout_3)
        self.line = QtGui.QFrame(Dialog)
        self.line.setFrameShape(QtGui.QFrame.VLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName("line")
        self.horizontalLayout.addWidget(self.line)
        self.verticalLayout_4 = QtGui.QVBoxLayout()
        self.verticalLayout_4.setObjectName("verticalLayout_4")
        self.labelMain = QtGui.QLabel(Dialog)
        self.labelMain.setObjectName("labelMain")
        self.verticalLayout_4.addWidget(self.labelMain)
        self.line_2 = QtGui.QFrame(Dialog)
        self.line_2.setFrameShape(QtGui.QFrame.HLine)
        self.line_2.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_2.setObjectName("line_2")
        self.verticalLayout_4.addWidget(self.line_2)
        self.textBrowser = QtGui.QTextBrowser(Dialog)
        self.textBrowser.setReadOnly(True)
        self.textBrowser.setTextInteractionFlags(QtCore.Qt.LinksAccessibleByMouse|QtCore.Qt.TextSelectableByMouse)
        self.textBrowser.setOpenExternalLinks(True)
        self.textBrowser.setOpenLinks(True)
        self.textBrowser.setObjectName("textBrowser")
        self.verticalLayout_4.addWidget(self.textBrowser)
        self.line_3 = QtGui.QFrame(Dialog)
        self.line_3.setFrameShape(QtGui.QFrame.HLine)
        self.line_3.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_3.setObjectName("line_3")
        self.verticalLayout_4.addWidget(self.line_3)
        self.label = QtGui.QLabel(Dialog)
        self.label.setObjectName("label")
        self.verticalLayout_4.addWidget(self.label)
        self.tbwVersions = QtGui.QTableWidget(Dialog)
        self.tbwVersions.setMaximumSize(QtCore.QSize(16777215, 240))
        self.tbwVersions.setFocusPolicy(QtCore.Qt.NoFocus)
        self.tbwVersions.setEditTriggers(QtGui.QAbstractItemView.NoEditTriggers)
        self.tbwVersions.setSelectionMode(QtGui.QAbstractItemView.NoSelection)
        self.tbwVersions.setGridStyle(QtCore.Qt.SolidLine)
        self.tbwVersions.setCornerButtonEnabled(False)
        self.tbwVersions.setObjectName("tbwVersions")
        self.tbwVersions.setColumnCount(1)
        self.tbwVersions.setRowCount(7)
        item = QtGui.QTableWidgetItem()
        self.tbwVersions.setVerticalHeaderItem(0, item)
        item = QtGui.QTableWidgetItem()
        self.tbwVersions.setVerticalHeaderItem(1, item)
        item = QtGui.QTableWidgetItem()
        self.tbwVersions.setVerticalHeaderItem(2, item)
        item = QtGui.QTableWidgetItem()
        self.tbwVersions.setVerticalHeaderItem(3, item)
        item = QtGui.QTableWidgetItem()
        self.tbwVersions.setVerticalHeaderItem(4, item)
        item = QtGui.QTableWidgetItem()
        self.tbwVersions.setVerticalHeaderItem(5, item)
        item = QtGui.QTableWidgetItem()
        self.tbwVersions.setVerticalHeaderItem(6, item)
        item = QtGui.QTableWidgetItem()
        self.tbwVersions.setHorizontalHeaderItem(0, item)
        self.tbwVersions.horizontalHeader().setStretchLastSection(True)
        self.tbwVersions.verticalHeader().setSortIndicatorShown(False)
        self.tbwVersions.verticalHeader().setStretchLastSection(False)
        self.verticalLayout_4.addWidget(self.tbwVersions)
        self.horizontalLayout.addLayout(self.verticalLayout_4)
        self.verticalLayout_6.addLayout(self.horizontalLayout)
        self.verticalLayout.addLayout(self.verticalLayout_6)
        self.buttonBox = QtGui.QDialogButtonBox(Dialog)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(Dialog)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("accepted()"), Dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("rejected()"), Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(QtGui.QApplication.translate("Dialog", "About OpenCobolIDE", None, QtGui.QApplication.UnicodeUTF8))
        self.labelIcon.setText(QtGui.QApplication.translate("Dialog", "<html><head/><body><p><img src=\":/ide-icons/rc/silex-64x64.png\"/></p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.labelMain.setText(QtGui.QApplication.translate("Dialog", "<html><head/><body><p><span style=\" font-size:14pt; font-weight:600;\">OpenCobolIDE</span></p><p><span style=\" text-decoration: underline;\">Version</span>: <span style=\" font-weight:600;\">%s</span></p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.textBrowser.setHtml(QtGui.QApplication.translate("Dialog", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Sans\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; color:#414141;\">OpenCobol IDE is a simple cobol IDE written in python based on OpenCobol and pyQode.</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-weight:600; text-decoration: underline; color:#414141;\">Authors:</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; color:#414141;\">Colin Duquesnoy  &lt;</span><a href=\"mailto:colin.duquesnoy@gmail\"><span style=\" font-family:\'Ubuntu\'; text-decoration: underline; color:#2768ff;\">colin.duquesnoy@gmail</span></a><a href=\"mailto:colin.duquesnoy@gmail\"><span style=\" font-family:\'Ubuntu\'; text-decoration: underline; color:#414141;\">&gt;</span></a></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-weight:600; text-decoration: underline; color:#414141;\">License:</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; color:#414141;\">This software is licensed under the </span><span style=\" font-family:\'Ubuntu\'; font-weight:600; color:#414141;\">GPL v3.</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-weight:600; text-decoration: underline; color:#414141;\">Web:</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a href=\"https://github.com/ColinDuquesnoy/OpenCobolIDE\"><span style=\" text-decoration: underline; color:#2768ff;\">Github repository</span></a></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-weight:600; text-decoration: underline; color:#414141;\">Contributors: </span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; color:#414141;\">- </span><a href=\"mailto:thiry.celi@gmail.com\"><span style=\" font-family:\'Ubuntu\'; text-decoration: underline; color:#2768ff;\">Céline Thiry</span></a><span style=\" font-family:\'Ubuntu\'; color:#414141;\"> : application icon designer</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'Ubuntu\'; font-weight:600; text-decoration: underline; color:#414141;\">Credits: </span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a href=\"http://python.org/\"><span style=\" font-family:\'Ubuntu\'; text-decoration: underline; color:#2768ff;\">Python</span></a><span style=\" font-family:\'Ubuntu\'; color:#414141;\"> , </span><a href=\"http://qt-project.org/\"><span style=\" font-family:\'Ubuntu\'; text-decoration: underline; color:#2768ff;\">Qt</span></a><span style=\" font-family:\'Ubuntu\'; color:#2768ff;\">/</span><a href=\"http://www.riverbankcomputing.com/software/pyqt/intro\"><span style=\" text-decoration: underline; color:#2768ff;\">PyQt4</span></a><span style=\" font-family:\'Ubuntu\'; color:#414141;\"> , </span><a href=\"https://github.com/ColinDuquesnoy/pyqode-core\"><span style=\" text-decoration: underline; color:#2768ff;\">PyQode</span></a><span style=\" font-family:\'Ubuntu\'; color:#414141;\"> , </span><a href=\"https://pypi.python.org/pypi/chardet\"><span style=\" font-family:\'Ubuntu\'; text-decoration: underline; color:#2768ff;\">chardet</span></a><span style=\" font-family:\'Ubuntu\'; color:#414141;\"> , </span><a href=\"http://tango.freedesktop.org/\"><span style=\" font-family:\'Ubuntu\'; text-decoration: underline; color:#2768ff;\">Tango Desktop Project </span></a><span style=\" font-family:\'Ubuntu\'; font-style:italic; color:#414141;\"> , </span><a href=\"http://www.opencobol.org/\"><span style=\" font-family:\'Ubuntu\'; text-decoration: underline; color:#2768ff;\">OpenCobol</span></a><span style=\" color:#414141;\"> , </span><a href=\"https://github.com/ColinDuquesnoy/QDarkStyleSheet\"><span style=\" text-decoration: underline; color:#2768ff;\">QDarkStyle</span></a></p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("Dialog", "Third party libraries versions:", None, QtGui.QApplication.UnicodeUTF8))
        self.tbwVersions.verticalHeaderItem(0).setText(QtGui.QApplication.translate("Dialog", "OpenCobol", None, QtGui.QApplication.UnicodeUTF8))
        self.tbwVersions.verticalHeaderItem(1).setText(QtGui.QApplication.translate("Dialog", "Qt", None, QtGui.QApplication.UnicodeUTF8))
        self.tbwVersions.verticalHeaderItem(2).setText(QtGui.QApplication.translate("Dialog", "PyQt", None, QtGui.QApplication.UnicodeUTF8))
        self.tbwVersions.verticalHeaderItem(3).setText(QtGui.QApplication.translate("Dialog", "pyqode.core", None, QtGui.QApplication.UnicodeUTF8))
        self.tbwVersions.verticalHeaderItem(4).setText(QtGui.QApplication.translate("Dialog", "pyqode.widgets", None, QtGui.QApplication.UnicodeUTF8))
        self.tbwVersions.verticalHeaderItem(5).setText(QtGui.QApplication.translate("Dialog", "Pygments", None, QtGui.QApplication.UnicodeUTF8))
        self.tbwVersions.verticalHeaderItem(6).setText(QtGui.QApplication.translate("Dialog", "QDarkStyle", None, QtGui.QApplication.UnicodeUTF8))
        self.tbwVersions.horizontalHeaderItem(0).setText(QtGui.QApplication.translate("Dialog", "Version", None, QtGui.QApplication.UnicodeUTF8))

from . import ide_rc