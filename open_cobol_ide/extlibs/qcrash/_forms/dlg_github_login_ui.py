# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file '/home/colin/dev/QCrash/forms/dlg_github_login.ui'
#
# Created by: PyQt5 UI code generator 5.5.1
#
# WARNING! All changes made in this file will be lost!

from qcrash.qt import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(366, 248)
        Dialog.setMinimumSize(QtCore.QSize(350, 0))
        self.verticalLayout = QtWidgets.QVBoxLayout(Dialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.lbl_html = QtWidgets.QLabel(Dialog)
        self.lbl_html.setObjectName("lbl_html")
        self.verticalLayout.addWidget(self.lbl_html)
        self.formLayout = QtWidgets.QFormLayout()
        self.formLayout.setContentsMargins(-1, 0, -1, -1)
        self.formLayout.setObjectName("formLayout")
        self.label_2 = QtWidgets.QLabel(Dialog)
        self.label_2.setObjectName("label_2")
        self.formLayout.setWidget(0, QtWidgets.QFormLayout.LabelRole, self.label_2)
        self.le_username = QtWidgets.QLineEdit(Dialog)
        self.le_username.setObjectName("le_username")
        self.formLayout.setWidget(0, QtWidgets.QFormLayout.FieldRole, self.le_username)
        self.label_3 = QtWidgets.QLabel(Dialog)
        self.label_3.setObjectName("label_3")
        self.formLayout.setWidget(1, QtWidgets.QFormLayout.LabelRole, self.label_3)
        self.le_password = QtWidgets.QLineEdit(Dialog)
        self.le_password.setEchoMode(QtWidgets.QLineEdit.Password)
        self.le_password.setObjectName("le_password")
        self.formLayout.setWidget(1, QtWidgets.QFormLayout.FieldRole, self.le_password)
        self.verticalLayout.addLayout(self.formLayout)
        self.cb_remember = QtWidgets.QCheckBox(Dialog)
        self.cb_remember.setObjectName("cb_remember")
        self.verticalLayout.addWidget(self.cb_remember)
        self.cb_remember_password = QtWidgets.QCheckBox(Dialog)
        self.cb_remember_password.setObjectName("cb_remember_password")
        self.verticalLayout.addWidget(self.cb_remember_password)
        self.bt_sign_in = QtWidgets.QPushButton(Dialog)
        self.bt_sign_in.setObjectName("bt_sign_in")
        self.verticalLayout.addWidget(self.bt_sign_in)

        self.retranslateUi(Dialog)
        self.cb_remember.toggled['bool'].connect(self.cb_remember_password.setEnabled)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "Sign in to github"))
        self.lbl_html.setText(_translate("Dialog", "<html><head/><body><p align=\"center\"><img src=\":/rc/GitHub-Mark.png\"/></p><p align=\"center\">Sign in to GitHub</p></body></html>"))
        self.label_2.setText(_translate("Dialog", "Username:"))
        self.label_3.setText(_translate("Dialog", "Password: "))
        self.cb_remember.setText(_translate("Dialog", "Remember me"))
        self.cb_remember_password.setText(_translate("Dialog", "Remember password"))
        self.bt_sign_in.setText(_translate("Dialog", "Sign in"))

from . import qcrash_rc