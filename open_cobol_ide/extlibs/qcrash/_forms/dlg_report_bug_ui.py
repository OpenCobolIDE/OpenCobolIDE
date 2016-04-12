# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file '/home/colin/dev/QCrash/forms/dlg_report_bug.ui'
#
# Created by: PyQt5 UI code generator 5.5.1
#
# WARNING! All changes made in this file will be lost!

from qcrash.qt import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(544, 272)
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
        self.plainTextEditDesc.setTabChangesFocus(True)
        self.plainTextEditDesc.setObjectName("plainTextEditDesc")
        self.formLayout.setWidget(1, QtWidgets.QFormLayout.FieldRole, self.plainTextEditDesc)
        self.cb_include_sys_info = QtWidgets.QCheckBox(Dialog)
        self.cb_include_sys_info.setObjectName("cb_include_sys_info")
        self.formLayout.setWidget(2, QtWidgets.QFormLayout.FieldRole, self.cb_include_sys_info)
        self.cb_include_application_log = QtWidgets.QCheckBox(Dialog)
        self.cb_include_application_log.setObjectName("cb_include_application_log")
        self.formLayout.setWidget(3, QtWidgets.QFormLayout.FieldRole, self.cb_include_application_log)
        self.verticalLayout.addLayout(self.formLayout)
        self.layout_buttons = QtWidgets.QHBoxLayout()
        self.layout_buttons.setContentsMargins(-1, 0, -1, -1)
        self.layout_buttons.setObjectName("layout_buttons")
        spacerItem = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.layout_buttons.addItem(spacerItem)
        self.verticalLayout.addLayout(self.layout_buttons)

        self.retranslateUi(Dialog)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "Report an issue..."))
        self.label.setText(_translate("Dialog", "Title:"))
        self.lineEditTitle.setToolTip(_translate("Dialog", "Title of the issue"))
        self.label_2.setText(_translate("Dialog", "Description:"))
        self.plainTextEditDesc.setToolTip(_translate("Dialog", "Description of the issue (mandatory)"))
        self.cb_include_sys_info.setToolTip(_translate("Dialog", "Include system information"))
        self.cb_include_sys_info.setText(_translate("Dialog", "Include system &information"))
        self.cb_include_application_log.setToolTip(_translate("Dialog", "Include application log"))
        self.cb_include_application_log.setText(_translate("Dialog", "Include application &log"))

from . import qcrash_rc