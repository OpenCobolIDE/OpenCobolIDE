# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file '/home/colin/OpenCobolIDE/forms/dlg_report_bug.ui'
#
# Created by: PyQt5 UI code generator 5.4.1
#
# WARNING! All changes made in this file will be lost!

from pyqode.qt import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(544, 283)
        self.verticalLayout = QtWidgets.QVBoxLayout(Dialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.formLayout = QtWidgets.QFormLayout()
        self.formLayout.setFieldGrowthPolicy(QtWidgets.QFormLayout.ExpandingFieldsGrow)
        self.formLayout.setObjectName("formLayout")
        self.label = QtWidgets.QLabel(Dialog)
        self.label.setObjectName("label")
        self.formLayout.setWidget(0, QtWidgets.QFormLayout.LabelRole, self.label)
        self.lineEditTitle = QtWidgets.QLineEdit(Dialog)
        self.lineEditTitle.setObjectName("lineEditTitle")
        self.formLayout.setWidget(0, QtWidgets.QFormLayout.FieldRole, self.lineEditTitle)
        self.label_2 = QtWidgets.QLabel(Dialog)
        self.label_2.setObjectName("label_2")
        self.formLayout.setWidget(1, QtWidgets.QFormLayout.LabelRole, self.label_2)
        self.plainTextEditDesc = QtWidgets.QPlainTextEdit(Dialog)
        self.plainTextEditDesc.setObjectName("plainTextEditDesc")
        self.formLayout.setWidget(1, QtWidgets.QFormLayout.FieldRole, self.plainTextEditDesc)
        self.label_3 = QtWidgets.QLabel(Dialog)
        self.label_3.setObjectName("label_3")
        self.formLayout.setWidget(2, QtWidgets.QFormLayout.LabelRole, self.label_3)
        self.horizontalLayout_3 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_3.setContentsMargins(0, -1, -1, -1)
        self.horizontalLayout_3.setObjectName("horizontalLayout_3")
        self.radioButtonBug = QtWidgets.QRadioButton(Dialog)
        self.radioButtonBug.setChecked(True)
        self.radioButtonBug.setObjectName("radioButtonBug")
        self.horizontalLayout_3.addWidget(self.radioButtonBug)
        self.radioButtonWish = QtWidgets.QRadioButton(Dialog)
        self.radioButtonWish.setObjectName("radioButtonWish")
        self.horizontalLayout_3.addWidget(self.radioButtonWish)
        spacerItem = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem)
        self.formLayout.setLayout(2, QtWidgets.QFormLayout.FieldRole, self.horizontalLayout_3)
        self.verticalLayout.addLayout(self.formLayout)
        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.horizontalLayout.setContentsMargins(-1, 0, -1, -1)
        self.horizontalLayout.setObjectName("horizontalLayout")
        spacerItem1 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem1)
        self.pushButtonSubmit = QtWidgets.QPushButton(Dialog)
        self.pushButtonSubmit.setObjectName("pushButtonSubmit")
        self.horizontalLayout.addWidget(self.pushButtonSubmit)
        self.verticalLayout.addLayout(self.horizontalLayout)

        self.retranslateUi(Dialog)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "Report a bug"))
        self.label.setText(_translate("Dialog", "Title:"))
        self.label_2.setText(_translate("Dialog", "Description:"))
        self.label_3.setText(_translate("Dialog", "Report type:"))
        self.radioButtonBug.setText(_translate("Dialog", "Bu&g"))
        self.radioButtonWish.setText(_translate("Dialog", "E&nhancement"))
        self.pushButtonSubmit.setText(_translate("Dialog", "Submit"))
