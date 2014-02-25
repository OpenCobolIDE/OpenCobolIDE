# Copyright (c) <2013-2014> Colin Duquesnoy
#
# This file is part of OpenCobolIDE.
#
# OpenCobolIDE is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# OpenCobolIDE is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# OpenCobolIDE. If not, see http://www.gnu.org/licenses/.
"""
This package contains the Python-Cobol pic parser written by
Brian Peterson (https://github.com/bpeterso2000/pycobol) and enhanced
by Paulus Schoutsen (https://github.com/balloob/Python-COBOL/blob/master/cobol.py)

The only modifications done by the OpenCobolIDE team is to add support
for python3 and replace print statement with call to logging module. Those
modifications have been implemented on a fork of the Python-Cobol project,
maintained by the OpenCobolIDE organization: https://github.com/OpenCobolIDE/Python-COBOL

PLEASE, if you want to do any modification on the cobol.py script, do it
on the fork then copy the script here!!!
"""
from oci.pic_parser.cobol import parse_cobol


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
        l = l[:-1]
        # the parser doe not like VALUE xxx.
        if "VALUE" in l:
            l = l[:l.find("VALUE")]
        # the parser does not like extra spaces between "PIC X(xxx)" and "."
        indent = len(l) - len(l.lstrip())
        tokens = l.split(" ")
        while "" in tokens:
            tokens.remove("")
        if not tokens[-1].endswith("."):
            tokens[-1] += "."
        lines.append(" " * indent + " ".join(tokens))

    return lines


def get_field_infos(code):
    """
    Gets the list of pic fields information from line |start| to line |end|.

    :param code: code to parse

    :returns: the list of pic fields info found in the specified text.
    """
    offset = 0
    field_infos = []
    lines = _clean_code(code)

    for row in cobol.process_cobol(lines):
        fi = PicFieldInfo()
        fi.offset = offset
        fi.name = row["name"]
        fi.level = row["level"]
        fi.pic = row["pic"]
        fi.occurs = row["occurs"]
        fi.redefines = row["redefines"]
        fi.indexed_by = row["indexed_by"]
        field_infos.append(fi)

        # compute offset of next PIC field.
        if row['pic']:
            offset += row['pic_info']['length']
        else:
            offset += 1

    return field_infos
