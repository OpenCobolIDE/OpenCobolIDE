"""
This package contains the various controllers used in the application.

A controller controls a view of the application. There is one
controller per view (home and edit) and one controller per main menu.

"""
from .base import Controller
from .cobol import CobolController
from .edit import EditController
from .file import FileController
from .help import HelpController
from .home import HomeController
from .view import ViewController
