from pyqode.cobol.api import regex
from pyqode.core.api import FoldDetector, TextBlockHelper


class CobolFoldDetector(FoldDetector):
    def __init__(self):
        super().__init__()
        self.proc_division = None
        self._proc_div_txt = ""
        self.data_division = None
        self._data_div_txt = ""
        self.variables = set()

    def stripped_texts(self, block, prev_block):
        ctext = block.text().rstrip().upper()
        if ctext.find(' USING ') != -1:
            ctext = ctext[:ctext.find(' USING ')] + '.'
        ptext = prev_block.text().rstrip().upper()
        if ptext.find(' USING ') != -1:
            ptext = ptext[:ptext.find(' USING ')] + '.'
        return ctext, ptext

    def detect_fold_level(self, prev_block, block):
        if not prev_block:
            return 0

        ctext, ptext = self.stripped_texts(block, prev_block)
        if not self.editor.free_format:
            ctext = ' ' * 6 + ctext[7:]
            ptext = ' ' * 6 + ptext[7:]
        if regex.DIVISION.indexIn(ctext) != -1:
            if 'DATA' in ctext:
                self.data_division = block
                self._data_div_txt = block.text()
            if 'PROCEDURE' in ctext:
                self.proc_division = block
                self._proc_div_txt = block.text()
            return 0
        elif regex.SECTION.indexIn(ctext) != -1:
            return 1
        elif ptext.endswith('DIVISION.'):
            return 1
        # in case of replace all or simply if the user deleted the data or
        # proc div.
        if (self.proc_division and
                self.proc_division.text() != self._proc_div_txt):
            self.proc_division = None
        if (self.data_division and
                self.data_division.text() != self._data_div_txt):
            self.data_division = None
        # inside PROCEDURE DIVISION
        if (self.proc_division and self.proc_division.isValid() and
                block.blockNumber() > self.proc_division.blockNumber()):
            # we only detect outline of paragraphes
            if regex.PARAGRAPH_PATTERN.indexIn(block.text()) != -1:
                # paragraph
                return 1
            else:
                # content of a paragraph
                if regex.PARAGRAPH_PATTERN.indexIn(prev_block.text()) != -1:
                    return 2
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
                    if 'ELSE' in cstxt:
                        return plvl - 1
                    for token in ['IF', 'ELSE', '$L$O$OP$', 'READ']:
                        if pstxt.startswith(token):
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
        elif (self.data_division and self.data_division.isValid() and
                block.blockNumber() > self.data_division.blockNumber() + 1) or ptext.endswith('SECTION.'):
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

            lvl = 2 + indent

            # if not ctext.lstrip().startswith('*'):
            prev = prev_block
            flg_trigger = False
            while (prev.text().strip().startswith('*') or not prev.text().strip()) and prev.isValid():
                TextBlockHelper.set_fold_lvl(prev, lvl)
                TextBlockHelper.set_fold_trigger(prev, False)
                prev = prev.previous()
                flg_trigger = True
            if flg_trigger and 'SECTION' in prev.text().upper():
                TextBlockHelper.set_fold_trigger(prev, True)
            else:
                TextBlockHelper.set_fold_trigger(prev, False)

            return lvl

        # other lines follow their previous fold level
        plvl = TextBlockHelper.get_fold_lvl(prev_block)
        return plvl
