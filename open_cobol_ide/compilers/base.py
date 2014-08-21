"""
Contains the base class for compilers.

"""


class Compiler:
    """
    Abstract base class for interfacing with a cobol compiler.

    The base class takes care of running the compiler process but does not know
    anything about the compiler command to run or the how to parse its output.

    To implement a compiler, you must implement the two following methods:

        - get_command: return the command that needs to be run to compile the
                       specified file. The command is a tuple made up of the
                       program name and the list of command line arguments.

        - parse: parses the compiler process output and returns a list of
                 tuples with the following format:

            (desc, status, line_nbr, column, icon, color, path)

    """
    extensions = ['.exe', '.dll']

    @classmethod
    def extension_for_type(cls, file_type):
        return cls.extensions[int(file_type)]

    def get_command(self, file_name, file_type):
        raise NotImplementedError()

    def parse(self, compiler_output):
        raise NotADirectoryError()
