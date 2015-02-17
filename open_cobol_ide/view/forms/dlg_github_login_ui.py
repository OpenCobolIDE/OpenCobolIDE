# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file '/home/colin/OpenCobolIDE/forms/dlg_github_login.ui'
#
# Created: Tue Feb 17 22:55:32 2015
#      by: PyQt5 UI code generator 5.4
#
# WARNING! All changes made in this file will be lost!

from pyqode.qt import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(689, 173)
        self.gridLayout = QtWidgets.QGridLayout(Dialog)
        self.gridLayout.setObjectName("gridLayout")
        self.formLayout = QtWidgets.QFormLayout()
        self.formLayout.setFieldGrowthPolicy(QtWidgets.QFormLayout.ExpandingFieldsGrow)
        self.formLayout.setObjectName("formLayout")
        self.label_2 = QtWidgets.QLabel(Dialog)
        self.label_2.setObjectName("label_2")
        self.formLayout.setWidget(0, QtWidgets.QFormLayout.LabelRole, self.label_2)
        self.label_3 = QtWidgets.QLabel(Dialog)
        self.label_3.setOpenExternalLinks(True)
        self.label_3.setObjectName("label_3")
        self.formLayout.setWidget(1, QtWidgets.QFormLayout.LabelRole, self.label_3)
        self.lineEditPassword = QtWidgets.QLineEdit(Dialog)
        self.lineEditPassword.setEchoMode(QtWidgets.QLineEdit.Password)
        self.lineEditPassword.setObjectName("lineEditPassword")
        self.formLayout.setWidget(1, QtWidgets.QFormLayout.FieldRole, self.lineEditPassword)
        self.lineEditUser = QtWidgets.QLineEdit(Dialog)
        self.lineEditUser.setObjectName("lineEditUser")
        self.formLayout.setWidget(0, QtWidgets.QFormLayout.FieldRole, self.lineEditUser)
        self.gridLayout.addLayout(self.formLayout, 2, 0, 1, 1)
        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        spacerItem = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem)
        self.pushButton = QtWidgets.QPushButton(Dialog)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/ide-icons/rc/GitHub-Mark.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.pushButton.setIcon(icon)
        self.pushButton.setObjectName("pushButton")
        self.horizontalLayout.addWidget(self.pushButton)
        self.gridLayout.addLayout(self.horizontalLayout, 5, 0, 1, 1)
        spacerItem1 = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.gridLayout.addItem(spacerItem1, 4, 0, 1, 1)
        self.label_4 = QtWidgets.QLabel(Dialog)
        self.label_4.setOpenExternalLinks(True)
        self.label_4.setObjectName("label_4")
        self.gridLayout.addWidget(self.label_4, 0, 0, 1, 1)
        self.label = QtWidgets.QLabel(Dialog)
        self.label.setOpenExternalLinks(True)
        self.label.setObjectName("label")
        self.gridLayout.addWidget(self.label, 1, 0, 1, 1)

        self.retranslateUi(Dialog)
        QtCore.QMetaObject.connectSlotsByName(Dialog)
        Dialog.setTabOrder(self.lineEditUser, self.lineEditPassword)
        Dialog.setTabOrder(self.lineEditPassword, self.pushButton)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "Login to github"))
        self.label_2.setText(_translate("Dialog", "Username or Email "))
        self.label_3.setText(_translate("Dialog", "<html><head/><body><p>Password <a href=\"https://github.com/password_reset\"><span style=\" text-decoration: underline; color:#4c6b8a;\">(forgot password)</span></a></p></body></html>"))
        self.pushButton.setText(_translate("Dialog", "Generate Personal Access Token"))
        self.label_4.setText(_translate("Dialog", "<html><head/><body><p align=\"center\">You need to login to <a href=\"http://github.com\"><span style=\" text-decoration: underline; color:#4c6b8a;\">Github</span></a> to authorize OpenCobolIDE Bug Report Tool to submit complete bug reports automatically.</p></body></html>"))
        self.label.setText(_translate("Dialog", "<html><head/><body><p align=\"center\"><a href=\"http://github.com/signup/free\"><span style=\" font-size:8pt; font-style:italic; text-decoration: underline; color:#4c6b8a;\">Create an account</span></a><span style=\" font-size:8pt; font-style:italic;\"> at github.com for free!</span></p></body></html>"))

from . import ide_rc