"""
This module contains the application constants (enums or constant definition
e.g. icon filenames, stylesheets)

"""
from enum import IntEnum
from pyqode.cobol.widgets import CobolCodeEdit


COBOL_FILES_FILTER = "Cobol files (%s)" % " ".join(
    [ext.lower() for ext in CobolCodeEdit.extensions] +
    CobolCodeEdit.extensions).replace(".", "*.")
OTHER_FILES_FILTER = "Other text files (*)"
FILTER_SEPARATOR = ";;"


EXE_TEMPLATE = """      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
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

"""

MODULE_TEMPLATE = """      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM.
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
       LINKAGE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       01 PARAMETRES.
      **
      * Input/Output parameters from/to the calling PROGRAM
      **
           02 PA-RETURN-CODE PIC 99 VALUE 0.
       PROCEDURE DIVISION USING PARAMETRES.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
        DISPLAY "Hello world"
        MOVE 0 TO PA-RETURN-CODE
        STOP RUN.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM.

"""

TEMPLATES = [EXE_TEMPLATE, MODULE_TEMPLATE, ""]



class Page(IntEnum):
    """
    Enumerates the different pages of the application.

    """
    HOME = 0
    EDIT = 1


class HomePageWhite:
    frame_recent = """
    QFrame
    {
    border: 1px solid rgb(150, 150, 150);
    border-radius: 5px;
    background-color: rgb(255, 255, 255);
    }
    """

    list_recent = """
    QListWidget
    {
    border: none;
    background-color: transparent;
    }
    """

    label_recent = """border: none;
    border-top-left-radius: 3px;
    border-top-right-radius: 3px;
    border-bottom-left-radius: 0px;
    border-bottom-right-radius: 0px;
    background-color: rgb(206, 206, 206);
    padding: 5px;
    border-bottom: 1px solid rgb(150, 150, 150);
    """


class HomePageDark:
    frame_recent = """border: 1px solid rgb(80, 80, 80);
    border-radius: 5px;
    """

    list_recent = """border: none;
    background-color: transparent;
    """

    label_recent = """border: none;
    border-top-left-radius: 3px;
    border-top-right-radius: 3px;
    border-bottom-left-radius: 0px;
    border-bottom-right-radius: 0px;
    background-color: rgb(64, 64, 64);
    padding: 5px;
    border-bottom: 1px solid rgb(80, 80, 80);
    """