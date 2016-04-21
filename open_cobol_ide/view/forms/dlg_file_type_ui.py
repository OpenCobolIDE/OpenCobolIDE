# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'C:\Dev\OpenCobolIDE\forms\dlg_file_type.ui'
#
# Created by: PyQt5 UI code generator 5.4.1
#
# WARNING! All changes made in this file will be lost!

from pyqode.qt import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(356, 180)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/ide-icons/rc/silex-64x64.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        Dialog.setWindowIcon(icon)
        self.verticalLayout = QtWidgets.QVBoxLayout(Dialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.formLayout = QtWidgets.QFormLayout()
        self.formLayout.setObjectName("formLayout")
        self.label = QtWidgets.QLabel(Dialog)
        self.label.setObjectName("label")
        self.formLayout.setWidget(0, QtWidgets.QFormLayout.LabelRole, self.label)
        self.comboBoxType = QtWidgets.QComboBox(Dialog)
        self.comboBoxType.setObjectName("comboBoxType")
        self.comboBoxType.addItem("")
        self.comboBoxType.addItem("")
        self.comboBoxType.addItem("")
        self.formLayout.setWidget(0, QtWidgets.QFormLayout.FieldRole, self.comboBoxType)
        self.labelDir = QtWidgets.QLabel(Dialog)
        self.labelDir.setObjectName("labelDir")
        self.formLayout.setWidget(2, QtWidgets.QFormLayout.LabelRole, self.labelDir)
        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.lineEditPath = QtWidgets.QLineEdit(Dialog)
        self.lineEditPath.setObjectName("lineEditPath")
        self.horizontalLayout.addWidget(self.lineEditPath)
        self.toolButton = QtWidgets.QToolButton(Dialog)
        self.toolButton.setObjectName("toolButton")
        self.horizontalLayout.addWidget(self.toolButton)
        self.formLayout.setLayout(2, QtWidgets.QFormLayout.FieldRole, self.horizontalLayout)
        self.labelName = QtWidgets.QLabel(Dialog)
        self.labelName.setObjectName("labelName")
        self.formLayout.setWidget(1, QtWidgets.QFormLayout.LabelRole, self.labelName)
        self.horizontalLayout_2 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.lineEditName = QtWidgets.QLineEdit(Dialog)
        self.lineEditName.setObjectName("lineEditName")
        self.horizontalLayout_2.addWidget(self.lineEditName)
        self.comboBoxExtension = QtWidgets.QComboBox(Dialog)
        self.comboBoxExtension.setObjectName("comboBoxExtension")
        self.horizontalLayout_2.addWidget(self.comboBoxExtension)
        self.formLayout.setLayout(1, QtWidgets.QFormLayout.FieldRole, self.horizontalLayout_2)
        self.verticalLayout.addLayout(self.formLayout)
        self.buttonBox = QtWidgets.QDialogButtonBox(Dialog)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtWidgets.QDialogButtonBox.Cancel|QtWidgets.QDialogButtonBox.Ok)
        self.buttonBox.setCenterButtons(False)
        self.buttonBox.setObjectName("buttonBox")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(Dialog)
        self.buttonBox.accepted.connect(Dialog.accept)
        self.buttonBox.rejected.connect(Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)
        Dialog.setTabOrder(self.buttonBox, self.comboBoxType)
        Dialog.setTabOrder(self.comboBoxType, self.lineEditName)
        Dialog.setTabOrder(self.lineEditName, self.comboBoxExtension)
        Dialog.setTabOrder(self.comboBoxExtension, self.lineEditPath)
        Dialog.setTabOrder(self.lineEditPath, self.toolButton)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "New file"))
        self.label.setText(_translate("Dialog", "Template:"))
        self.comboBoxType.setToolTip(_translate("Dialog", "New file template (program, module or empty)"))
        self.comboBoxType.setStatusTip(_translate("Dialog", "New file template (program, module or empty)"))
        self.comboBoxType.setItemText(0, _translate("Dialog", "Program"))
        self.comboBoxType.setItemText(1, _translate("Dialog", "Module"))
        self.comboBoxType.setItemText(2, _translate("Dialog", "Empty"))
        self.labelDir.setText(_translate("Dialog", "Directory"))
        self.lineEditPath.setToolTip(_translate("Dialog", "Directory of the file to create"))
        self.lineEditPath.setStatusTip(_translate("Dialog", "Directory of the file to create"))
        self.toolButton.setToolTip(_translate("Dialog", "Browse file system"))
        self.toolButton.setStatusTip(_translate("Dialog", "Browse file system"))
        self.toolButton.setText(_translate("Dialog", "..."))
        self.labelName.setText(_translate("Dialog", "Name"))
        self.lineEditName.setToolTip(_translate("Dialog", "Program name (without path and without extension)"))
        self.lineEditName.setStatusTip(_translate("Dialog", "Program name (without path and without extension)"))

from . import ide_rc