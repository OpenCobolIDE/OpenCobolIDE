from pyqode.qt import QtGui
from pyqode.core.api import ColorScheme
from .cobol import CobolCodeEdit
from .generic import GenericCodeEdit
from .text import TextEdit


def update_editor_settings(editor):
    from open_cobol_ide.settings import Settings
    settings = Settings()
    # general settings for all editors
    editor.tab_length = settings.tab_len
    editor.font_name = settings.font
    editor.font_size = settings.font_size
    editor.show_whitespaces = settings.show_whitespaces
    editor.syntax_highlighter.color_scheme = ColorScheme(settings.color_scheme)
    editor.panels.get('FoldingPanel').native_look = not settings.dark_style
    try:
        # cobol editor specific settings
        editor.backspace_mode.enabled = settings.enable_smart_backspace
        editor.comment_indicator = settings.comment_indicator
        editor.free_format = settings.free_format
        editor.caret_line_mode.enabled = settings.highlight_caret
        editor.auto_indent_mode.enabled = settings.enable_autoindent
        editor.code_completion_mode.trigger_length = \
            settings.code_completion_trigger_len
        editor.line_nbr_panel.enabled = settings.display_lines
        editor.line_nbr_panel.setVisible(settings.display_lines)
        editor.linter_mode.enabled = settings.show_errors
        editor.lower_case_keywords = settings.lower_case_keywords
        editor.file.autodetect_eol = settings.autodetect_eol
        editor.file.preferred_eol = settings.preferred_eol
        editor.modes.get('CodeCompletionMode').smart_completion = \
            bool(settings.completion_filter_mode)
        editor.margins.positions = settings.margin_positions
        editor.margins.colors = [QtGui.QColor(c)
                                 for c in settings.margin_colors]
    except AttributeError as e:
        print(e)
        editor.syntax_highlighter.pygments_style = settings.color_scheme
