"""
Configures the test suite and describe the global fixture that can be used
in functional tests.

"""
import pytest
from open_cobol_ide import __version__
from open_cobol_ide.app import Application
from open_cobol_ide.logger import setup_logging

setup_logging(__version__, debug=True)
_app = Application()


@pytest.fixture(scope="session")
def app(request):
    global _app

    def fin():
        global _app
        _app.exit()
        del _app

    _app.win.show()
    request.addfinalizer(fin)
    return _app
