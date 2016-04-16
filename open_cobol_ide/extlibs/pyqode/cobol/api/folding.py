from pyqode.cobol.api import regex, keywords
from pyqode.core.api import FoldDetector, TextBlockHelper


class CobolFoldDetector(FoldDetector):
    def __init__(self):
        super().__init__()
        self.proc_division = None
        self._proc_div_txt = ""
        self.data_division = None
        self._data_div_txt = ""
        self.variables = set()
        self.divisions = []

    def stripped_texts(self, block, prev_block):
        ctext = block.text().rstrip().upper()
        if ctext.find(' USING ') != -1:
            ctext = ctext[:ctext.find(' USING ')] + '.'
        ptext = prev_block.text().rstrip().upper()
        if ptext.find(' USING ') != -1:
            ptext = ptext[:ptext.find(' USING ')] + '.'
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

    def detect_fold_level(self, prev_block, block):
        if not prev_block:
            return 0
        ctext, ptext = self.stripped_texts(block, prev_block)
        if not self.editor.free_format:
            ctext = ' ' * 6 + ctext[7:]
            ptext = ' ' * 6 + ptext[7:]
        if regex.DIVISION.indexIn(ctext) != -1:
            div_type = 'data'
            if 'PROCEDURE' in ctext:
                div_type = 'procedure'
            self.divisions.append((block, div_type))
            return 0
        elif regex.SECTION.indexIn(ctext) != -1:
            return 1
        elif regex.DIVISION.indexIn(ptext) != -1:
            return 1
        # inside PROCEDURE DIVISION
        if self.is_in_proc_division(block):
            # we only detect outline of paragraphes
            stext = ctext.strip().upper().replace('.', '')
            if regex.PARAGRAPH_PATTERN.indexIn(ctext) != -1 and stext not in keywords.RESERVED:
                # paragraph
                return 2
            else:
                in_keywords = ptext.strip().upper().replace('.', '') in keywords.RESERVED
                prev = prev_block
                while prev.text().strip() == '' and prev.isValid():
                    prev = prev.previous()
                prtext = prev.text()
                if not self.editor.free_format:
                    prtext = ' ' * 6 + ptext[7:]
                if 'SECTION' in prtext or 'DIVISION' in prtext:
                    return 2
                # content of a paragraph
                if regex.PARAGRAPH_PATTERN.indexIn(prtext) != -1 and not in_keywords:
                    return 3
                else:
                    cstxt = ctext.lstrip()
                    pstxt = ptext.lstrip()
                    plvl = TextBlockHelper.get_fold_lvl(prev_block)
                    if regex.LOOP_PATTERN.indexIn(pstxt) != -1:
                        pstxt = '$L$O$OP$'
                    if regex.BRANCH_END.indexIn(pstxt) == 0:
                        if cstxt in ['ELSE']:
                            return plvl - 2
                        return plvl - 1
                    if regex.BRANCH_END.indexIn(cstxt) == 0:
                        nblock = block.next()
                        if nblock.isValid():
                            TextBlockHelper.set_fold_lvl(nblock, plvl - 1)
                        return plvl
                    if 'ELSE' in cstxt:
                        return plvl - 1
                    for token in ['IF', 'ELSE', '$L$O$OP$', 'READ']:
                        if pstxt.startswith(token) and not pstxt.endswith('END-%s' % token):
                            if token == '$L$O$OP$' and ptext.lstrip().startswith('PERFORM'):
                                tokens = [t for t in ptext.strip().split() if t]
                                try:
                                    tag = tokens[1]
                                except IndexError:
                                    pass
                                else:
                                    if tag not in list(self.variables) + ['VARYING', 'WITH']:
                                        # out-of-line perform
                                        continue
                            return plvl + 1
                    return plvl
        # INSIDE  DATA DIVISION
        elif self.is_in_data_division(block):
            # here folding is based on the indentation level
            indent = len(ctext) - len(ctext.lstrip())
            if not ctext.lstrip().startswith('*'):
                tokens = [t for t in ctext.split(' ') if t]
                try:
                    name = tokens[1]
                except IndexError:
                    pass
                else:
                    self.variables.add(name)

            lvl = 3 + indent

            if not ctext.lstrip().startswith('*'):
                # fix fold level of previous comment lines.
                prev = prev_block
                flg_trigger = False
                ptext = prev.text().upper()
                if not self.editor.free_format:
                    ptext = ' ' * 6 + ptext[7:]
                while (ptext.strip().startswith('*') or not ptext.strip()) and prev.isValid():
                    TextBlockHelper.set_fold_lvl(prev, lvl)
                    TextBlockHelper.set_fold_trigger(prev, False)
                    prev = prev.previous()
                    flg_trigger = True
                    ptext = prev.text().upper()
                    if not self.editor.free_format:
                        ptext = ' ' * 6 + ptext[7:]
                if flg_trigger and 'SECTION' in ptext or 'DIVISION' in ptext:
                    TextBlockHelper.set_fold_trigger(prev, True)
                else:
                    TextBlockHelper.set_fold_trigger(prev, False)

            return lvl
        else:
            print("nowhere", block.text())

        # other lines follow their previous fold level
        plvl = TextBlockHelper.get_fold_lvl(prev_block)
        return plvl
