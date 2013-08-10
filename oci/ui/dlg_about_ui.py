# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'dlg_about.ui'
#
# Created: Sun Aug 11 00:00:28 2013
#      by: PyQt4 UI code generator 4.10
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
        Dialog.resize(850, 750)
        Dialog.setMinimumSize(QtCore.QSize(850, 750))
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/ide-icons/rc/dialog-information.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        Dialog.setWindowIcon(icon)
        self.verticalLayout = QtGui.QVBoxLayout(Dialog)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.verticalLayout_6 = QtGui.QVBoxLayout()
        self.verticalLayout_6.setContentsMargins(-1, 0, -1, -1)
        self.verticalLayout_6.setObjectName(_fromUtf8("verticalLayout_6"))
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setContentsMargins(0, 0, -1, -1)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.verticalLayout_3 = QtGui.QVBoxLayout()
        self.verticalLayout_3.setObjectName(_fromUtf8("verticalLayout_3"))
        self.labelIcon = QtGui.QLabel(Dialog)
        self.labelIcon.setObjectName(_fromUtf8("labelIcon"))
        self.verticalLayout_3.addWidget(self.labelIcon)
        spacerItem = QtGui.QSpacerItem(20, 40, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.verticalLayout_3.addItem(spacerItem)
        self.horizontalLayout.addLayout(self.verticalLayout_3)
        self.line = QtGui.QFrame(Dialog)
        self.line.setFrameShape(QtGui.QFrame.VLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName(_fromUtf8("line"))
        self.horizontalLayout.addWidget(self.line)
        self.verticalLayout_4 = QtGui.QVBoxLayout()
        self.verticalLayout_4.setObjectName(_fromUtf8("verticalLayout_4"))
        self.labelMain = QtGui.QLabel(Dialog)
        self.labelMain.setObjectName(_fromUtf8("labelMain"))
        self.verticalLayout_4.addWidget(self.labelMain)
        self.line_2 = QtGui.QFrame(Dialog)
        self.line_2.setFrameShape(QtGui.QFrame.HLine)
        self.line_2.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_2.setObjectName(_fromUtf8("line_2"))
        self.verticalLayout_4.addWidget(self.line_2)
        self.textBrowser = QtGui.QTextBrowser(Dialog)
        self.textBrowser.setReadOnly(True)
        self.textBrowser.setTextInteractionFlags(QtCore.Qt.LinksAccessibleByMouse|QtCore.Qt.TextSelectableByMouse)
        self.textBrowser.setOpenExternalLinks(True)
        self.textBrowser.setOpenLinks(True)
        self.textBrowser.setObjectName(_fromUtf8("textBrowser"))
        self.verticalLayout_4.addWidget(self.textBrowser)
        self.line_3 = QtGui.QFrame(Dialog)
        self.line_3.setFrameShape(QtGui.QFrame.HLine)
        self.line_3.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_3.setObjectName(_fromUtf8("line_3"))
        self.verticalLayout_4.addWidget(self.line_3)
        self.label = QtGui.QLabel(Dialog)
        self.label.setObjectName(_fromUtf8("label"))
        self.verticalLayout_4.addWidget(self.label)
        self.tbwVersions = QtGui.QTableWidget(Dialog)
        self.tbwVersions.setMaximumSize(QtCore.QSize(16777215, 207))
        self.tbwVersions.setFocusPolicy(QtCore.Qt.NoFocus)
        self.tbwVersions.setEditTriggers(QtGui.QAbstractItemView.NoEditTriggers)
        self.tbwVersions.setSelectionMode(QtGui.QAbstractItemView.NoSelection)
        self.tbwVersions.setGridStyle(QtCore.Qt.SolidLine)
        self.tbwVersions.setCornerButtonEnabled(False)
        self.tbwVersions.setObjectName(_fromUtf8("tbwVersions"))
        self.tbwVersions.setColumnCount(1)
        self.tbwVersions.setRowCount(6)
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
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(Dialog)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("accepted()")), Dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("rejected()")), Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(_translate("Dialog", "About OpenCobolIDE", None))
        self.labelIcon.setText(_translate("Dialog", "<html><head/><body><p><img src=\":/ide-icons/rc/silex-64x64.png\"/></p></body></html>", None))
        self.labelMain.setText(_translate("Dialog", "<html><head/><body><p><span style=\" font-size:14pt; font-weight:600;\">OpenCobolIDE</span></p><p><span style=\" text-decoration: underline;\">Version</span>: <span style=\" font-weight:600;\">%s</span></p></body></html>", None))
        self.textBrowser.setHtml(_translate("Dialog", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Ubuntu\'; font-size:11pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">OpenCobol IDE is a simple cobol IDE based on OpenCobol.</p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600; color:#000000;\">Authors:</span></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" color:#000000;\">Colin Duquesnoy: </span><a href=\"mailto:colin.duquesnoy@gmail\"><span style=\" text-decoration: underline; color:#0000ff;\">colin.duquesnoy@gmail</span></a></p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; text-decoration: underline; color:#0000ff;\"><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">Web:</span></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a href=\"https://launchpad.net/cobcide\"><span style=\" text-decoration: underline; color:#0000ff;\">Launchpad repository</span></a></p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-weight:600;\"><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">Contributors: </span></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">- <a href=\"mailto:thiry.celi@gmail.com\"><span style=\" text-decoration: underline; color:#0000ff;\">Céline Thiry</span></a> : silex icon  and code completion icons designer</p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">Credits: </span></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">- <a href=\"http://python.org/\"><span style=\" text-decoration: underline; color:#0000ff;\">Python</span></a></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">- <a href=\"http://qt-project.org/\"><span style=\" text-decoration: underline; color:#0000ff;\">Qt</span></a>/<a href=\"http://qt-project.org/wiki/Category:LanguageBindings::PySide\"><span style=\" text-decoration: underline; color:#0000ff;\">PySide</span></a></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">- <a href=\"https://github.com/ColinDuquesnoy/PCEF\"><span style=\" text-decoration: underline; color:#0000ff;\">PCEF</span></a></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">- <a href=\"https://pypi.python.org/pypi/chardet\"><span style=\" text-decoration: underline; color:#0000ff;\">chardet</span></a></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">- <a href=\"http://tango.freedesktop.org/\"><span style=\" text-decoration: underline; color:#0000ff;\">Tango Desktop Project</span></a></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:9pt; font-style:italic;\">- </span><a href=\"http://www.opencobol.org/\"><span style=\" text-decoration: underline; color:#0000ff;\">OpenCobol</span></a></p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; text-decoration: underline; color:#0000ff;\"><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">License:</span></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">This software is licensed under the <span style=\" font-weight:600;\">GPL v3.</span></p></body></html>", None))
        self.label.setText(_translate("Dialog", "Third party libraries versions:", None))
        item = self.tbwVersions.verticalHeaderItem(0)
        item.setText(_translate("Dialog", "OpenCobol", None))
        item = self.tbwVersions.verticalHeaderItem(1)
        item.setText(_translate("Dialog", "Qt", None))
        item = self.tbwVersions.verticalHeaderItem(2)
        item.setText(_translate("Dialog", "PySide", None))
        item = self.tbwVersions.verticalHeaderItem(3)
        item.setText(_translate("Dialog", "PCEF", None))
        item = self.tbwVersions.verticalHeaderItem(4)
        item.setText(_translate("Dialog", "Pygments", None))
        item = self.tbwVersions.verticalHeaderItem(5)
        item.setText(_translate("Dialog", "QWelcomeWindow", None))
        item = self.tbwVersions.horizontalHeaderItem(0)
        item.setText(_translate("Dialog", "Version", None))

import ide_rc
