"""
Tests the pic fields parser
"""
from oci.pic_parser import get_field_infos

source = """       01 INPUT-DATA.
           05 U1-DINRAR PIC X(06)        .
           05 FILLER PIC X(01)  VALUE ';'.
           05 U1-DINR PIC X(07).
           05 FILLER PIC X(01).
           05 U1-ORGNR PIC X(10).
           05 FILLER PIC X(01).
           05 U1-INKDAT PIC X(08).
           05 FILLER PIC X(01).
           05 U1-TOMPER PIC X(08).
           05 FILLER PIC X(01).
           05 U1-RUBR1 PIC X(06).
           05 FILLER PIC X(01).
           05 U1-RUBR2 PIC X(06).
           05 FILLER PIC X(01).
           05 U1-RUBR3 PIC X(06).
           05 FILLER PIC X(01).
           05 U1-HDL1 PIC X(04).
           05 FILLER PIC X(01).
           05 U1-HDL2 PIC X(04).
           05 FILLER PIC X(01).
"""


def test_offset_calculator():
    """
    Test the pic parser with the example supplied in issue #14
    ( https://github.com/OpenCobolIDE/OpenCobolIDE/issues/14) and ensure the
    offset of the last element is 75.
    """
    fi = get_field_infos(source)
    assert fi[-1].offset == 75
