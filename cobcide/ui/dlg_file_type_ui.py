# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'dlg_file_type.ui'
#
# Created: Sun Apr  7 22:22:35 2013
#      by: pyside-uic 0.2.13 running on PySide 1.1.1
#
# WARNING! All changes made in this file will be lost!

from PySide import QtCore, QtGui

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(475, 133)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/ide-icons/rc/silex-64x64.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        Dialog.setWindowIcon(icon)
        self.gridLayout = QtGui.QGridLayout(Dialog)
        self.gridLayout.setObjectName("gridLayout")
        self.buttonBox = QtGui.QDialogButtonBox(Dialog)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setCenterButtons(True)
        self.buttonBox.setObjectName("buttonBox")
        self.gridLayout.addWidget(self.buttonBox, 2, 0, 1, 1)
        self.formLayout = QtGui.QFormLayout()
        self.formLayout.setContentsMargins(0, -1, -1, -1)
        self.formLayout.setObjectName("formLayout")
        self.label = QtGui.QLabel(Dialog)
        self.label.setObjectName("label")
        self.formLayout.setWidget(0, QtGui.QFormLayout.LabelRole, self.label)
        self.verticalLayout = QtGui.QVBoxLayout()
        self.verticalLayout.setObjectName("verticalLayout")
        self.radioButtonProgram = QtGui.QRadioButton(Dialog)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(":/ide-icons/rc/application-x-executable.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.radioButtonProgram.setIcon(icon1)
        self.radioButtonProgram.setObjectName("radioButtonProgram")
        self.verticalLayout.addWidget(self.radioButtonProgram)
        self.radioButtonSubprogram = QtGui.QRadioButton(Dialog)
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(":/ide-icons/rc/application-x-sharedlib.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.radioButtonSubprogram.setIcon(icon2)
        self.radioButtonSubprogram.setObjectName("radioButtonSubprogram")
        self.verticalLayout.addWidget(self.radioButtonSubprogram)
        self.radioButtonText = QtGui.QRadioButton(Dialog)
        icon3 = QtGui.QIcon()
        icon3.addPixmap(QtGui.QPixmap(":/ide-icons/rc/text-x-generic.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.radioButtonText.setIcon(icon3)
        self.radioButtonText.setObjectName("radioButtonText")
        self.verticalLayout.addWidget(self.radioButtonText)
        self.formLayout.setLayout(0, QtGui.QFormLayout.FieldRole, self.verticalLayout)
        self.gridLayout.addLayout(self.formLayout, 1, 0, 1, 1)

        self.retranslateUi(Dialog)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("accepted()"), Dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("rejected()"), Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)
        Dialog.setTabOrder(self.radioButtonProgram, self.radioButtonSubprogram)
        Dialog.setTabOrder(self.radioButtonSubprogram, self.radioButtonText)
        Dialog.setTabOrder(self.radioButtonText, self.buttonBox)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(QtGui.QApplication.translate("Dialog", "Choose a file type", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("Dialog", "<html><head/><body><p>What kind of file do you want to  <span style=\" font-weight:600;\">open</span>?</p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.radioButtonProgram.setText(QtGui.QApplication.translate("Dialog", "A cobol program", None, QtGui.QApplication.UnicodeUTF8))
        self.radioButtonSubprogram.setText(QtGui.QApplication.translate("Dialog", "A cobol sub program", None, QtGui.QApplication.UnicodeUTF8))
        self.radioButtonText.setText(QtGui.QApplication.translate("Dialog", "A regular text file", None, QtGui.QApplication.UnicodeUTF8))

import ide_rc
