from pyqode.cobol.api import regex
from pyqode.core.api import FoldDetector, TextBlockHelper


OFFSET_DIVISION = 0
OFFSET_SECTION = 1
OFFSET_OTHER = 2


class CobolFoldDetector(FoldDetector):
    def __init__(self):
        super().__init__()
        self.proc_division = None
        self._proc_div_txt = ""
        self.data_division = None
        self._data_div_txt = ""
        self.variables = set()
        self.divisions = []

    def is_valid(self, block):
        return block is not None and block.isValid()

    def stripped_texts(self, block, prev_block):
        ctext = block.text().rstrip().upper()
        if self.is_valid(prev_block):
            ptext = prev_block.text().rstrip().upper()
        else:
            ptext = ''
        return ctext, ptext

    def is_in_data_division(self, block):
        for div_block, div_type in reversed(self.divisions):
            if div_block.blockNumber() < block.blockNumber():
                return div_type == 'data'
        return False

    def is_in_proc_division(self, block):
        for div_block, div_type in reversed(self.divisions):
            if div_block.blockNumber() < block.blockNumber():
                return div_type == 'procedure'
        return False

    def normalize_text(self, text):
        """
        Normalize text, when fixed format is ON, replace the first 6 chars by a space.
        """
        if not self.editor.free_format:
            text = ' ' * 6 + text[6:]
        return text.upper()

    def get_indent(self, normalized_text):
        indent = len(normalized_text) - len(normalized_text.lstrip())
        return indent + indent % 2

    def detect_fold_level(self, prev_block, block):
        ctext, ptext = self.stripped_texts(block, prev_block)
        if not self.editor.free_format:
            ctext = self.normalize_text(ctext)
            ptext = self.normalize_text(ptext)
        if regex.DIVISION.indexIn(ctext) != -1 and not ctext.lstrip().startswith('*'):
            return OFFSET_DIVISION
        elif regex.SECTION.indexIn(ctext) != -1 and not ctext.lstrip().startswith('*'):
            return OFFSET_SECTION
        else:
            # anywhere else, folding is mostly based on the indentation level
            indent = self.get_indent(ctext)
            pindent = self.get_indent(ptext)

            if ctext.strip().upper().startswith('END-') and self.is_valid(prev_block) and pindent > indent:
                # find previous block with the same indent, use it's fold level + 1 to include
                # the end-branch statement in the fold scope
                pblock = prev_block
                while self.is_valid(pblock) and (pindent != indent or len(ptext.strip()) == 0):
                    pblock = pblock.previous()
                    ptext = self.normalize_text(pblock.text())
                    pindent = self.get_indent(ptext)
                lvl = TextBlockHelper.get_fold_lvl(pblock.next())
            else:
                lvl = OFFSET_OTHER + indent

            # if not self.editor.free_format and (ctext.lstrip().startswith('-') or ctext.lstrip().startswith('*')):
            if not self.editor.free_format and (ctext.lstrip().startswith('-')):
                # use previous fold level
                lvl = TextBlockHelper.get_fold_lvl(prev_block)

            if not self.editor.free_format and ctext.strip().startswith('*'):
                if regex.DIVISION.indexIn(ptext) != -1 and not ptext.lstrip().startswith('*'):
                    lvl = OFFSET_SECTION
                elif regex.SECTION.indexIn(ptext) != -1 and not ptext.lstrip().startswith('*'):
                    return OFFSET_SECTION + 2
                else:
                    lvl = TextBlockHelper.get_fold_lvl(prev_block)

            return lvl
