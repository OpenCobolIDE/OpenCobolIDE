class BaseFormatter(object):
    """
    Base class for implementing a custom formatter.

    Just implement :meth:`format_body` and :meth:`format_title` functions and
    set your formatter on the backends you created.
    """
    def format_title(self, title):
        """
        Formats the issue title. By default this method does nothing.

        An email formatter might want to append the application to name to
        the object field...
        """
        return title

    def format_body(self, description, sys_info=None, traceback=None):
        """
        Not implemented.

        :param description: Description of the issue, written by the user.
        :param sys_info: Optional system information string
        :param traceback: Optional traceback.
        """
        raise NotImplementedError
