"""
This module contains some definition that can be shared
between the backend and the frontend (e.g. this module can be imported
without requiring any additional libraries).
"""


class Definition(object):
    """
    Represents a definition in a source file.

    This class is used by the OutlineExplorerWidget to show the defined names
    in a source file.

    Definition usually form a tree (a definition may have some child
    definition, e.g. methods of a class).
    """
    def __init__(self, name, line, column=0, icon='', description='',
                 user_data=None, file_path=None):
        #: Icon resource name associated with the definition, can be None
        self.icon = icon
        #: Definition name (name of the class, method, variable)
        self.name = name
        #: The line of the definition in the current editor text
        self.line = line
        #: The column of the definition in the current editor text
        self.column = column
        #: Possible list of children (only classes have children)
        self.children = []
        #: description (tooltip)
        self.description = description
        #: user data
        self.user_data = user_data
        #: File path of the definition
        self.file_path = file_path

    def add_child(self, definition):
        """
        Adds a child definition
        """
        self.children.append(definition)

    def to_dict(self):
        """
        Serializes a definition to a dictionary, ready for json.

        Children are serialised recursively.
        """
        ddict = {'name': self.name, 'icon': self.icon,
                 'line': self.line, 'column': self.column,
                 'children': [], 'description': self.description,
                 'user_data': self.user_data, 'path': self.file_path}
        for child in self.children:
            ddict['children'].append(child.to_dict())
        return ddict

    @staticmethod
    def from_dict(ddict):
        """
        Deserializes a definition from a simple dict.
        """
        d = Definition(ddict['name'], ddict['line'], ddict['column'],
                       ddict['icon'], ddict['description'],
                       ddict['user_data'], ddict['path'])
        for child_dict in ddict['children']:
            d.children.append(Definition.from_dict(child_dict))
        return d

    def __repr__(self):
        return 'Definition(%r, %r, %r, %r)' % (
            self.name, self.line, self.column, self.icon)
