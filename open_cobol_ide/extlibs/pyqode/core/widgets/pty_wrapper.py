import sys
import os


def pty_wrapper_main():
    """
    Main function of the pty wrapper script
    """
    # make sure we can import _pty even if pyqode is not installed (this is the case in HackEdit where pyqode has
    # been vendored).
    sys.path.insert(0, os.path.dirname(__file__))
    import _pty

    # fixme: find a way to use a pty and keep stdout and stderr as separate channels
    _pty.spawn(sys.argv[1:])


if __name__ == '__main__':
    pty_wrapper_main()
