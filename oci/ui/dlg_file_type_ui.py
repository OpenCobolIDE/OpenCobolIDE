# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'dlg_file_type.ui'
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
        Dialog.resize(475, 133)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/ide-icons/rc/silex-64x64.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        Dialog.setWindowIcon(icon)
        self.gridLayout = QtGui.QGridLayout(Dialog)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.buttonBox = QtGui.QDialogButtonBox(Dialog)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setCenterButtons(True)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.gridLayout.addWidget(self.buttonBox, 2, 0, 1, 1)
        self.formLayout = QtGui.QFormLayout()
        self.formLayout.setContentsMargins(0, -1, -1, -1)
        self.formLayout.setObjectName(_fromUtf8("formLayout"))
        self.label = QtGui.QLabel(Dialog)
        self.label.setObjectName(_fromUtf8("label"))
        self.formLayout.setWidget(0, QtGui.QFormLayout.LabelRole, self.label)
        self.verticalLayout = QtGui.QVBoxLayout()
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.radioButtonProgram = QtGui.QRadioButton(Dialog)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(_fromUtf8(":/ide-icons/rc/application-x-executable.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.radioButtonProgram.setIcon(icon1)
        self.radioButtonProgram.setObjectName(_fromUtf8("radioButtonProgram"))
        self.verticalLayout.addWidget(self.radioButtonProgram)
        self.radioButtonSubprogram = QtGui.QRadioButton(Dialog)
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(_fromUtf8(":/ide-icons/rc/application-x-sharedlib.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.radioButtonSubprogram.setIcon(icon2)
        self.radioButtonSubprogram.setObjectName(_fromUtf8("radioButtonSubprogram"))
        self.verticalLayout.addWidget(self.radioButtonSubprogram)
        self.radioButtonText = QtGui.QRadioButton(Dialog)
        icon3 = QtGui.QIcon()
        icon3.addPixmap(QtGui.QPixmap(_fromUtf8(":/ide-icons/rc/text-x-generic.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.radioButtonText.setIcon(icon3)
        self.radioButtonText.setObjectName(_fromUtf8("radioButtonText"))
        self.verticalLayout.addWidget(self.radioButtonText)
        self.formLayout.setLayout(0, QtGui.QFormLayout.FieldRole, self.verticalLayout)
        self.gridLayout.addLayout(self.formLayout, 1, 0, 1, 1)

        self.retranslateUi(Dialog)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("accepted()")), Dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("rejected()")), Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)
        Dialog.setTabOrder(self.radioButtonProgram, self.radioButtonSubprogram)
        Dialog.setTabOrder(self.radioButtonSubprogram, self.radioButtonText)
        Dialog.setTabOrder(self.radioButtonText, self.buttonBox)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(_translate("Dialog", "Choose a file type", None))
        self.label.setText(_translate("Dialog", "<html><head/><body><p>What kind of file do you want to  <span style=\" font-weight:600;\">open</span>?</p></body></html>", None))
        self.radioButtonProgram.setText(_translate("Dialog", "A cobol program", None))
        self.radioButtonSubprogram.setText(_translate("Dialog", "A cobol sub program", None))
        self.radioButtonText.setText(_translate("Dialog", "A regular text file", None))

import ide_rc
