import locale
import os
import sys
from pyqode.core.api.syntax_highlighter import PYGMENTS_STYLES, ColorScheme
from pyqode.qt import QtCore, QtGui, QtWidgets
from open_cobol_ide import system, compilers
from open_cobol_ide.enums import GnuCobolStandard
from open_cobol_ide.settings import Settings
from open_cobol_ide.view.forms import dlg_preferences_ui
from open_cobol_ide.view.dialogs.check_compiler import DlgCheckCompiler

DEFAULT_TEMPLATE = '''      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
            DISPLAY "Hello world"
            STOP RUN.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.

'''


class DlgPreferences(QtWidgets.QDialog, dlg_preferences_ui.Ui_Dialog):
    flags_in_checkbox = [
        '-g', '-ftrace', '-ftraceall', '-fdebugging-line', '-static', '-debug'
    ]

    def __init__(self, parent):
        super().__init__(parent, QtCore.Qt.WindowSystemMenuHint |
                         QtCore.Qt.WindowTitleHint |
                         QtCore.Qt.WindowCloseButtonHint)
        self.setupUi(self)
        themes = system.icon_themes()
        if themes:
            self.comboBoxIconTheme.addItems(themes)
        else:
            self.comboBoxIconTheme.hide()
            self.lblIconTheme.hide()
        self.tabWidget.setTabIcon(2, QtGui.QIcon.fromTheme(
            'application-x-executable',
            QtGui.QIcon(':/ide-icons/rc/application-x-executable.png')))
        self.tabWidget.setTabIcon(1, QtGui.QIcon.fromTheme(
            'applications-graphics',
            QtGui.QIcon(':/ide-icons/rc/applications-graphics.png')))
        self.tabWidget.setTabIcon(3, QtGui.QIcon.fromTheme(
            'media-playback-start', QtGui.QIcon(
                ':/ide-icons/rc/media-playback-start.png')))
        self.buttonBox.button(self.buttonBox.Reset).clicked.connect(self.reset)
        self.buttonBox.button(self.buttonBox.RestoreDefaults).clicked.connect(
            self.restore_defaults)
        self.checkBoxRunExtTerm.stateChanged.connect(
            self.lineEditRunTerm.setEnabled)
        self.listWidgetColorSchemes.currentItemChanged.connect(
            self.update_color_scheme_preview)
        self.plainTextEdit.setPlainText(DEFAULT_TEMPLATE, '', '')
        self.lineEditDbpre.setReadOnly(True)
        self.lineEditDbpreFramework.setReadOnly(True)
        self.lineEditCobmysqlapi.setReadOnly(True)
        self.toolButtonDbpre.clicked.connect(self._select_dbpre)
        self.toolButtonDbpreFramework.clicked.connect(
            self._select_dbpre_framework)
        self.toolButtonCobMySqlApiPath.clicked.connect(
            self._select_cobmysqlapi)
        self.checkBoxShowDbPass.stateChanged.connect(
            self._on_show_pass_state_changed)
        self.toolButtonVCVARS.clicked.connect(self._select_vcvarsall)
        self.toolButtonCustomCompilerPath.clicked.connect(
            self._select_custom_compiler_path)
        self.toolButtonAddLibPath.clicked.connect(self._add_lib_path)
        self.toolButtonAddRelativeLibPath.clicked.connect(
            self._add_rel_lib_path)
        self.toolButtonRemoveLibPath.clicked.connect(self._rm_lib_path)
        self.btAddAbsoluteCopyPath.clicked.connect(self._add_copy_path)
        self.btAddRelativeCopyPath.clicked.connect(
            self._add_rel_copy_path)
        self.btRemoveCopyPath.clicked.connect(self._rm_copy_path)
        self.toolButtonESQLOC.clicked.connect(self._select_esqloc)
        self.reset(all_tabs=True)
        if not system.windows:
            self.labelVCVARS.hide()
            self.lineEditVCVARS.hide()
            self.toolButtonVCVARS.hide()
            self.stackedWidgetSQL.setCurrentIndex(0)
        else:
            self.stackedWidgetSQL.setCurrentIndex(1)
        self.toolButtonCheckCompiler.clicked.connect(self._check_compiler)
        self.cbPATH.stateChanged.connect(self.PATH.setEnabled)
        self.cbCOB_CONFIG_DIR.stateChanged.connect(self.COB_CONFIG_DIR.setEnabled)
        self.cbCOB_COPY_DIR.stateChanged.connect(self.COB_COPY_DIR.setEnabled)
        self.cbCOB_INCLUDE_PATH.stateChanged.connect(self.COB_INCLUDE_PATH.setEnabled)
        self.cbCOB_LIB_PATH.stateChanged.connect(self.COB_LIB_PATH.setEnabled)

        self.PATH.setEnabled(self.cbPATH.isChecked())
        self.COB_CONFIG_DIR.setEnabled(self.cbCOB_CONFIG_DIR.isChecked())
        self.COB_COPY_DIR.setEnabled(self.cbCOB_COPY_DIR.isChecked())
        self.COB_INCLUDE_PATH.setEnabled(self.cbCOB_INCLUDE_PATH.isChecked())
        self.COB_LIB_PATH.setEnabled(self.cbCOB_LIB_PATH.isChecked())

    def _check_compiler(self, *_):
        self.apply()
        pth = self.lineEditCompilerPath.text()
        if os.path.exists(pth) or system.which(pth) is not None:
            p = QtCore.QProcess()
            p.start(pth, ['--version'])
            p.waitForFinished()
            output = bytes(p.readAllStandardOutput()).decode(
                locale.getpreferredencoding())
            DlgCheckCompiler.check(self, pth, output)
        else:
            QtWidgets.QMessageBox.warning(
                self, 'Invalid compiler path',
                'Not a valid compiler path, path does not exists: %s!' % pth)

    def _add_lib_path(self):
        path = QtWidgets.QFileDialog.getExistingDirectory(
            self, 'Select a library directory')
        if path:
            self.listWidgetLibPaths.addItem(os.path.normpath(path))

    def _add_rel_lib_path(self):
        path, status = QtWidgets.QInputDialog.getText(
            self, 'Add relative library path', 'Path:')
        if status:
            self.listWidgetLibPaths.addItem(os.path.normpath(path))

    def _rm_lib_path(self):
        items = self.listWidgetLibPaths.selectedItems()
        for item in items:
            self.listWidgetLibPaths.takeItem(self.listWidgetLibPaths.row(item))

    def _add_copy_path(self):
        path = QtWidgets.QFileDialog.getExistingDirectory(
            self, 'Select a copybook path')
        if path:
            self.listWidgetCopyPaths.addItem(os.path.normpath(path))

    def _add_rel_copy_path(self):
        path, status = QtWidgets.QInputDialog.getText(
            self, 'Add relative copybook path', 'Path:')
        if status:
            self.listWidgetCopyPaths.addItem(os.path.normpath(path))

    def _rm_copy_path(self):
        items = self.listWidgetCopyPaths.selectedItems()
        for item in items:
            self.listWidgetCopyPaths.takeItem(
                self.listWidgetCopyPaths.row(item))

    def _select_vcvarsall(self):
        path, _ = QtWidgets.QFileDialog.getOpenFileName(
            self, 'Select vcvarsall.bat', self.lineEditVCVARS.text())
        if path:
            path = os.path.normpath(path)
            if os.path.isfile(path) and path.lower().endswith('vcvarsall.bat'):
                self.lineEditVCVARS.setText(path)
            else:
                QtWidgets.QMessageBox.warning(
                    self, "Invalid vcvarsall batch path",
                    "%r is not a valid vcvarsall batch file" % path)

    def _select_custom_compiler_path(self):
        path, _ = QtWidgets.QFileDialog.getOpenFileName(
            self, 'Select path to a GnuCOBOL compiler executable',
            self.lineEditCompilerPath.text())
        if path:
            self.lineEditCompilerPath.setText(os.path.normpath(path))
            self._check_compiler()

    def _select_esqloc(self):
        path = QtWidgets.QFileDialog.getExistingDirectory(
            self, 'Select esqlOC folder', self.lineEditESQLOC.text())
        if path:
            self.lineEditESQLOC.setText(os.path.normpath(path))

    def _select_dbpre(self):
        path, _ = QtWidgets.QFileDialog.getOpenFileName(
            self, 'Select dbpre executable', self.lineEditDbpre.text())
        if path:
            self.lineEditDbpre.setText(os.path.normpath(path))
            self.labelDbpreVersion.setText(
                compilers.DbpreCompiler(path).get_version()
                if Settings().dbpre != '' else ''
            )

    def _select_cobmysqlapi(self):
        path, _ = QtWidgets.QFileDialog.getOpenFileName(
            self, 'Select cobmysqlapi object file',
            self.lineEditCobmysqlapi.text())
        if path:
            self.lineEditCobmysqlapi.setText(os.path.normpath(path))

    def _select_dbpre_framework(self):
        def bool_to_string(value):
            if value:
                return 'Found'
            else:
                return 'Missing'

        path = QtWidgets.QFileDialog.getExistingDirectory(
            self, 'Select dbpre framework directory',
            self.lineEditDbpreFramework.text())
        if path:
            path = os.path.normpath(path)
            pgctbbat = os.path.exists(os.path.join(path, 'PGCTBBAT'))
            pgctbbatws = os.path.exists(os.path.join(path, 'PGCTBBATWS'))
            sqlca = os.path.exists(os.path.join(path, 'SQLCA'))
            if pgctbbat and pgctbbatws and sqlca:
                self.lineEditDbpreFramework.setText(path)
            else:
                QtWidgets.QMessageBox.warning(
                    self, 'Invalid dpre framework directory',
                    'Missing one of the following files: \n'
                    'PGCTBBAT: %s\n'
                    'PGCTBBATWS: %s\n'
                    'SQLCA: %s\n' % (
                        bool_to_string(pgctbbat), bool_to_string(pgctbbatws),
                        bool_to_string(sqlca)))

    def stop_backend(self):
        self.plainTextEdit.backend.stop()

    def _update_icon_theme(self, c):
        index = self.comboBoxIconTheme.findText(c)
        self.comboBoxIconTheme.setCurrentIndex(index)

    def _on_show_pass_state_changed(self, state):
        if state:
            self.lineEditDBPASSWD.setEchoMode(QtWidgets.QLineEdit.Normal)
        else:
            self.lineEditDBPASSWD.setEchoMode(QtWidgets.QLineEdit.Password)

    @QtCore.Slot(bool)
    def on_radioButtonColorWhite_toggled(self, native):
        # choose an icon them that goes well with the selected style
        if not native:
            candidates = ['Faenza-Darkest', 'nouveGnomeGray', 'matefaenzadark',
                          'gnome', 'oxygen']
            available = system.icon_themes()
            for c in candidates:
                if c in available:
                    self._update_icon_theme(c)
                    break
        else:
            self._update_icon_theme('default')
        # choose a a color scheme that goes well with the selected style
        for i in range(self.listWidgetColorSchemes.count()):
            if (native and self.listWidgetColorSchemes.item(i).text() ==
                    'qt'):
                self.listWidgetColorSchemes.setCurrentRow(i)
                break
            elif (not native and
                    self.listWidgetColorSchemes.item(i).text() == 'darcula'):
                self.listWidgetColorSchemes.setCurrentRow(i)
                break

    def update_color_scheme_preview(self, item):
        if item:
            self.plainTextEdit.syntax_highlighter.color_scheme = ColorScheme(
                item.text())

    def setupUi(self, Dialog):
        super().setupUi(Dialog)
        self.setMinimumWidth(450)
        self.tabWidget.setCurrentIndex(0)

    def reset(self, all_tabs=False):
        settings = Settings()
        # Editor
        if self.tabWidget.currentIndex() == 0 or all_tabs:
            self.cb_cursor_pos_in_bytes.setChecked(
                settings.show_cursor_pos_in_bytes)
            self.checkBoxShowErrors.setChecked(settings.show_errors)
            self.checkBoxViewLineNumber.setChecked(settings.display_lines)
            self.checkBoxHighlightCurrentLine.setChecked(
                settings.highlight_caret)
            self.checkBoxHighlightWhitespaces.setChecked(
                settings.show_whitespaces)
            self.spinBoxEditorTabLen.setValue(settings.tab_len)
            self.checkBoxEditorAutoIndent.setChecked(
                settings.enable_autoindent)
            self.spinBoxEditorCCTriggerLen.setValue(
                settings.code_completion_trigger_len)
            self.rbLowerCaseKwds.setChecked(settings.lower_case_keywords)
            self.rbUpperCaseKwds.setChecked(not settings.lower_case_keywords)
            self.lineEditCommentIndicator.setText(settings.comment_indicator)
            self.checkBoxSmartBackspace.setChecked(
                settings.enable_smart_backspace)
            self.checkBoxAutodetectEOL.setChecked(settings.autodetect_eol)
            self.comboBoxPreferredEOL.setCurrentIndex(settings.preferred_eol)
            self.comboCcFilterMode.setCurrentIndex(
                settings.completion_filter_mode)
        # Style
        if self.tabWidget.currentIndex() == 1 or all_tabs:
            rb = (self.radioButtonColorDark if settings.dark_style else
                  self.radioButtonColorWhite)
            rb.setChecked(True)
            index = self.comboBoxIconTheme.findText(settings.icon_theme)
            if index != -1:
                self.comboBoxIconTheme.setCurrentIndex(index)
            self.fontComboBox.setCurrentFont(QtGui.QFont(settings.font))
            self.spinBoxFontSize.setValue(settings.font_size)
            self.listWidgetColorSchemes.clear()
            current_index = None
            self.listWidgetColorSchemes.clear()
            for style in PYGMENTS_STYLES:
                self.listWidgetColorSchemes.addItem(style)
                if style == settings.color_scheme:
                    current_index = self.listWidgetColorSchemes.count() - 1
            if current_index:
                self.listWidgetColorSchemes.setCurrentRow(current_index)
        # Run
        if self.tabWidget.currentIndex() == 3 or all_tabs:
            self.checkBoxRunExtTerm.setChecked(settings.external_terminal)
            self.lineEditRunTerm.setVisible(sys.platform != 'win32')
            self.lineEditRunTerm.setEnabled(settings.external_terminal)
            self.lineEditRunTerm.setText(settings.external_terminal_command)
        # compiler
        if self.tabWidget.currentIndex() == 2 or all_tabs:
            self.cbAutoDetectSublmodules.setChecked(
                Settings().autodetect_submodules)
            self.lineEditOutputDirectory.setText(Settings().output_directory)
            self.lineEditCobcExts.setText(';'.join(Settings().cobc_extensions))
            self.checkBoxFreeFormat.setChecked(settings.free_format)
            self.comboBoxStandard.setCurrentIndex(
                int(settings.cobol_standard))
            self.lineEditCompilerPath.setText(settings.compiler_path)
            flags = Settings().compiler_flags
            self.cb_debugging_line.setChecked(
                self.cb_debugging_line.text() in flags)
            self.cb_ftrace.setChecked(self.cb_ftrace.text().replace('&', '') in flags)
            self.cb_ftraceall.setChecked(self.cb_ftraceall.text().replace('&', '') in flags)
            self.cb_g.setChecked(self.cb_g.text().replace('&', '') in flags)
            self.cb_static.setChecked(self.cb_static.text().replace('&', '') in flags)
            self.cb_debug.setChecked(self.cb_debug.text().replace('&', '') in flags)
            for v in self.flags_in_checkbox:
                try:
                    flags.remove(v)
                except ValueError:
                    pass
            self.lineEditLibs.setText(settings.libraries)
            self.listWidgetLibPaths.addItems(
                [pth for pth in settings.library_search_path.split(';')
                 if pth])
            self.listWidgetCopyPaths.addItems(
                [pth for pth in settings.copybook_paths.split(';')
                 if pth])
            self.le_compiler_flags.setText(' '.join(flags))
            if system.windows:
                self.lineEditVCVARS.setText(settings.vcvarsall)
                self.combo_arch.setCurrentIndex(
                    1 if settings.vcvarsall_arch == 'x64' else 0)
            self.PATH.setText(settings.path)
            self.cbPATH.setChecked(settings.path_enabled)
            self.COB_CONFIG_DIR.setText(settings.cob_config_dir)
            self.cbCOB_CONFIG_DIR.setChecked(settings.cob_config_dir_enabled)
            self.COB_COPY_DIR.setText(settings.cob_copy_dir)
            self.cbCOB_COPY_DIR.setChecked(settings.cob_copy_dir_enabled)
            self.COB_INCLUDE_PATH.setText(settings.cob_include_path)
            self.cbCOB_INCLUDE_PATH.setChecked(settings.cob_include_path_enabled)
            self.COB_LIB_PATH.setText(settings.cob_lib_path)
            self.cbCOB_LIB_PATH.setChecked(settings.cob_lib_path_enabled)

        # SQL Cobol
        if self.tabWidget.currentIndex() == 4 or all_tabs:
            self.lineEditDbpreExts.setText(';'.join(Settings().dbpre_extensions))
            self.lineEditDbpre.setText(settings.dbpre)
            self.lineEditDbpreFramework.setText(settings.dbpre_framework)
            self.lineEditCobmysqlapi.setText(settings.cobmysqlapi)
            self.lineEditDBHOST.setText(settings.dbhost)
            self.lineEditDBUSER.setText(settings.dbuser)
            self.lineEditDBPASSWD.setText(settings.dbpasswd)
            self.lineEditDBNAME.setText(settings.dbname)
            self.lineEditDBPORT.setText(settings.dbport)
            self.lineEditDBSOCKET.setText(settings.dbsocket)
            self.labelDbpreVersion.setText(
                compilers.DbpreCompiler().get_version()
                if Settings().dbpre != '' else '')
            self.lineEditESQLOC.setText(settings.esqloc)
            self.lineEditesqlOcExts.setText(';'.join(Settings().esqloc_extensions))

    def restore_defaults(self):
        settings = Settings()
        index = self.tabWidget.currentIndex()
        # Editor
        if index == 0:
            settings.show_cursor_pos_in_bytes = False
            settings.show_error = True
            settings.display_lines = True
            settings.highlight_caret = True
            settings.show_whitespaces = False
            settings.tab_len = 4
            settings.enable_autoindent = True
            settings.code_completion_trigger_len = 1
            settings.comment_indicator = '*> '
            settings.enable_smart_backspace = False
            settings.lower_case_keywords = False
            settings.autodetect_eol = True
            settings.preferred_eol = 0
            settings.code_completion_trigger_len = 1
        # Style
        elif index == 1:
            settings.dark_style = False
            settings.icon_theme = 'default'
            settings.font = 'Source Code Pro'
            settings.font_size = 11
            settings.colorScheme = 'qt'
        # run
        elif index == 3:
            settings.external_terminal = False
            settings.external_terminal_command = None
        # compiler
        elif index == 2:
            settings.autodetect_submodules = True
            settings.output_directory = 'bin'
            settings.free_format = False
            settings.cobol_standard = GnuCobolStandard.default
            settings.compiler_path = Settings.default_compiler_path()
            settings.compiler_flags = ['-debug']
            settings.copybook_paths = ''
            settings.library_search_path = ''
            settings.libraries = ''
            settings.cobc_extensions = ['.cob', '.cbl', '.pco', '.cpy', '.lst']
            if system.windows:
                settings.vcvarsall = ''
                settings.vcvarsall_arch = 'x86'
            settings.path = settings.default_path()
            settings.path_enabled = True
            settings.cob_config_dir = settings.default_config_dir()
            settings.cob_config_dir_enabled = True
            settings.cob_copy_dir = settings.default_copy_dir()
            settings.cob_copy_dir_enabled = True
            settings.cob_include_path = settings.default_include_dir()
            settings.cob_include_path_enabled = True
            settings.cob_lib_path = settings.default_lib_path()
            settings.cob_lib_path_enabled = True
        elif index == 4:
            settings.dbpre = ''
            settings.dbpre_extensions = ['.scb']
            settings.dbpre_framework = ''
            settings.cobmysqlapi = ''
            settings.dbhost = 'localhost'
            settings.dbuser = ''
            settings.dbpasswd = ''
            settings.dbname = ''
            settings.dbport = '03306'
            settings.dbsocket = 'null'
            settings.esqloc = ''
            settings.esqloc_extensions = ['.sqb']
        self.reset()

    def apply(self):
        # force next check (otherwise the cached result will be used)
        compilers.GnuCobolCompiler.check_compiler.reset()
        settings = Settings()
        settings.show_cursor_pos_in_bytes = \
            self.cb_cursor_pos_in_bytes.isChecked()
        settings.display_lines = self.checkBoxViewLineNumber.isChecked()
        settings.highlight_caret = self.checkBoxHighlightCurrentLine.isChecked()
        settings.show_whitespaces = \
            self.checkBoxHighlightWhitespaces.isChecked()
        settings.tab_len = self.spinBoxEditorTabLen.value()
        settings.enable_autoindent = self.checkBoxEditorAutoIndent.isChecked()
        settings.code_completion_trigger_len = \
            self.spinBoxEditorCCTriggerLen.value()
        settings.preferred_eol = self.comboBoxPreferredEOL.currentIndex()
        settings.autodetect_eol = self.checkBoxAutodetectEOL.isChecked()
        settings.dark_style = self.radioButtonColorDark.isChecked()
        settings.font = self.fontComboBox.currentFont().family()
        settings.font_size = self.spinBoxFontSize.value()
        settings.color_scheme = self.listWidgetColorSchemes.currentItem().text()
        settings.external_terminal = self.checkBoxRunExtTerm.isChecked()
        settings.external_terminal_command = self.lineEditRunTerm.text()
        settings.lower_case_keywords = self.rbLowerCaseKwds.isChecked()
        settings.compiler_path = self.lineEditCompilerPath.text()
        settings.vcvarsall = self.lineEditVCVARS.text()
        settings.vcvarsall_arch = self.combo_arch.currentText()
        settings.path = self.PATH.text()
        settings.path_enabled = self.cbPATH.isChecked()
        settings.cob_config_dir = self.COB_CONFIG_DIR.text()
        settings.cob_config_dir_enabled = self.cbCOB_CONFIG_DIR.isChecked()
        settings.cob_copy_dir = self.COB_COPY_DIR.text()
        settings.cob_copy_dir_enabled = self.cbCOB_COPY_DIR.isChecked()
        settings.cob_include_path = self.COB_INCLUDE_PATH.text()
        settings.cob_include_path_enabled = self.cbCOB_INCLUDE_PATH.isChecked()
        settings.cob_lib_path = self.COB_LIB_PATH.text()
        settings.cob_lib_path_enabled = self.cbCOB_LIB_PATH.isChecked()
        settings.free_format = self.checkBoxFreeFormat.isChecked()
        settings.comment_indicator = self.lineEditCommentIndicator.text()
        settings.autodetect_submodules = \
            self.cbAutoDetectSublmodules.isChecked()
        settings.cobol_standard = GnuCobolStandard(
            self.comboBoxStandard.currentIndex())
        settings.icon_theme = self.comboBoxIconTheme.currentText()
        settings.show_errors = self.checkBoxShowErrors.isChecked()
        settings.enable_smart_backspace = \
            self.checkBoxSmartBackspace.isChecked()
        paths = []
        for i in range(self.listWidgetLibPaths.count()):
            paths.append(self.listWidgetLibPaths.item(i).text())
        settings.library_search_path = ';'.join(paths)
        paths = []
        for i in range(self.listWidgetCopyPaths.count()):
            paths.append(self.listWidgetCopyPaths.item(i).text())
        settings.copybook_paths = ';'.join(paths)
        settings.libraries = self.lineEditLibs.text()
        settings.output_directory = self.lineEditOutputDirectory.text()
        cb_flags = [self.cb_g, self.cb_ftrace, self.cb_ftraceall,
                    self.cb_debugging_line, self.cb_static, self.cb_debug]
        flags = [cb.text() for cb in cb_flags if cb.isChecked()]
        flags += self.le_compiler_flags.text().split(' ')
        settings.compiler_flags = flags
        # sql
        settings.dbpre = self.lineEditDbpre.text()
        settings.dbpre_framework = self.lineEditDbpreFramework.text()
        settings.cobmysqlapi = self.lineEditCobmysqlapi.text()
        settings.dbhost = self.lineEditDBHOST.text()
        settings.dbuser = self.lineEditDBUSER.text()
        settings.dbpasswd = self.lineEditDBPASSWD.text()
        settings.dbname = self.lineEditDBNAME.text()
        settings.dbport = self.lineEditDBPORT.text()
        settings.dbsocket = self.lineEditDBSOCKET.text()
        settings.esqloc = self.lineEditESQLOC.text()
        settings.completion_filter_mode = \
            self.comboCcFilterMode.currentIndex()
        settings.cobc_extensions = [
            ext.lower() for ext in
            self.lineEditCobcExts.text().split(';') if ext]
        settings.dbpre_extensions = [
            ext.lower() for ext in
            self.lineEditDbpreExts.text().split(';') if ext]
        settings.esqloc_extensions = [
            ext for ext in self.lineEditesqlOcExts.text().split(';') if ext]

    @classmethod
    def edit_preferences(cls, parent):
        def restore_state(dlg):
            s = Settings()
            dlg.resize(s.preferences_width, s.preferences_height)

        def save_state(dlg):
            s = Settings()
            s.preferences_width = dlg.width()
            s.preferences_height = dlg.height()

        dlg = cls(parent)
        restore_state(dlg)
        ret_val = dlg.exec_()
        dlg.stop_backend()
        if ret_val != dlg.Accepted:
            raise ValueError()
        save_state(dlg)
        dlg.apply()
