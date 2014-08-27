import sys
from open_cobol_ide.settings import Settings


def test_parse_args(app):
    argv = sys.argv
    path = 'test/testfiles/HelloWorld.cbl'
    sys.argv = [argv[0], path, '--verbose']
    args = app.parse_args()
    assert args.verbose is True
    assert len(args.files) == 1
    assert args.files[0] == path


def test_update_style(app):
    Settings().dark_style = True
    app.update_app_style()
    assert app.app.styleSheet() != ''
    Settings().dark_style = False
    app.update_app_style()
    assert app.app.styleSheet() == ''
