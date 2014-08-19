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