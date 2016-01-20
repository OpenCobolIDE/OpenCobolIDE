"""
This module contains a cobol specific auto indenter mode.
"""
from pyqode.core.api import TextHelper
from pyqode.core.modes import AutoIndentMode
from pyqode.cobol.api import regex, keywords


class CobolAutoIndentMode(AutoIndentMode):
    """
    Implements a smarter (regex based) automatic indentater.

    Automatic indentation is triggered when the user press enter. The base
    implementation is to use the previous line indentation. This works fine
    in most situations but there are cases where the indenter could
    automatically increase indentation (e.g. after an if statement or a
    loop,...). This is what this mode do.

    """
    def _get_indent(self, cursor):
        prev_line_text = TextHelper(self.editor).current_line_text()
        if not self.editor.free_format:
            prev_line_text = ' ' * 7 + prev_line_text[7:]
        diff = len(prev_line_text) - len(prev_line_text.lstrip())
        post_indent = ' ' * diff
        min_column = self.editor.indenter_mode.min_column
        if len(post_indent) < min_column:
            post_indent = min_column * ' '
        # all regex are upper cases
        text = cursor.block().text().upper()
        # raise indentation level
        patterns = [
            regex.PARAGRAPH_PATTERN,
            regex.STRUCT_PATTERN,
            regex.BRANCH_START,
            regex.LOOP_PATTERN
        ]
        for ptrn in patterns:
            if ptrn.indexIn(text) != -1:
                post_indent += self.editor.tab_length * ' '
                return '', post_indent
        # use the previous line indentation
        return '', post_indent
