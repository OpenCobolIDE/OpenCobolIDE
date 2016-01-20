from .parsers.pic import process_cobol


class PicFieldInfo(object):
    """
    This structure holds the information about a PIC field.
    """
    offset = 0
    name = ""
    level = 0
    pic = ""
    occurs = None
    redefines = None
    indexed_by = None


def _clean_code(code):
    """
    Cleans the received code (the parser does not like extra spaces not a VALUE
    statement). Returns the cleaned code as a list of lines.

    :param code: The COBOL code to clean

    :return The list of code lines (cleaned)
    """
    lines = []
    # cleanup lines, the parser is very sensitive to extra spaces,...
    for l in code.splitlines():
        # remove last .
        if l.endswith('.'):
            l = l[:-1]
        # the parser doe not like VALUE xxx.
        if "VALUE" in l:
            l = l[:l.find("VALUE")]
        # the parser does not like extra spaces between "PIC X(xxx)" and "."
        indent = len(l) - len(l.lstrip())
        tokens = l.split(" ")
        while "" in tokens:
            tokens.remove("")
        if tokens and not tokens[-1].endswith("."):
            tokens[-1] += "."
        lines.append(" " * indent + " ".join(tokens))

    return lines


def get_field_infos(code, free_format):
    """
    Gets the list of pic fields information from line |start| to line |end|.

    :param code: code to parse

    :returns: the list of pic fields info found in the specified text.
    """
    offset = 0
    field_infos = []
    lines = _clean_code(code)

    previous_offset = 0

    for row in process_cobol(lines, free_format):
        fi = PicFieldInfo()
        fi.name = row["name"]
        fi.level = row["level"]
        fi.pic = row["pic"]
        fi.occurs = row["occurs"]
        fi.redefines = row["redefines"]
        fi.indexed_by = row["indexed_by"]

        # find item that was redefined and use its offset
        if fi.redefines:
            for fib in field_infos:
                if fib.name == fi.redefines:
                    offset = fib.offset

        # level 1 should have their offset set to 1
        if fi.level == 1:
            offset = 1

        # level 78 have no offset
        if fi.level == 78:
            offset = 0

        # level 77 have offset always to 1
        if fi.level == 77:
            offset = 1

        # set item offset
        fi.offset = offset

        # special case: level 88 have the same level as its parent
        if fi.level == 88:
            fi.offset = previous_offset
        else:
            previous_offset = offset

        field_infos.append(fi)

        # compute offset of next PIC field.
        if row['pic']:
            offset += row['pic_info']['length']

    return field_infos
