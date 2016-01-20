"""
This module contains the Qt pygments style.
"""
from pygments.style import Style
from pygments.token import Comment, Error, Generic, Keyword, Literal, Name, \
    Operator, Text, Punctuation


class QtStyle(Style):
    """
    Port of the qt style
    """
    default_style = ''

    background_color = '#ffffff'
    highlight_color = '#c7e7f9'

    styles = {
        Comment.Multiline: ' #008000',
        Comment.Preproc: '#000080',
        Comment.Single: ' #808080',
        Comment.Special: 'bold  #000080',
        Comment: ' #808080',
        Error: '#CC0000',
        Generic.Deleted: 'bg:#ffdddd #000000',
        Generic.Emph: ' #000000',
        Generic.Error: '#aa0000',
        Generic.Heading: '#999999',
        Generic.Inserted: 'bg:#ddffdd #000000',
        Generic.Output: '#888888',
        Generic.Prompt: '#555555',
        Generic.Strong: 'bold',
        Generic.Subheading: '#aaaaaa',
        Generic.Traceback: '#aa0000',
        Keyword.Constant: '#808000 ',
        Keyword.Declaration: '#808000',
        Keyword.Namespace: '#000080',
        Keyword.Pseudo: '#808000',
        Keyword.Reserved: '#808000 bold',
        Keyword.Type: '#800080',
        Keyword: '#808000 bold',
        Literal.Number: '#000080',
        Literal.String: '#008000',
        Literal.String.Doc: '#000080',
        Name.Attribute: '#800080',
        Name.Builtin.Pseudo: '#94558D',
        Name.Builtin: '#AA00AA',
        Name.Class: '#800080 bold',
        Name.Constant: '#800080',
        Name.Decorator: '#808000',
        Name.Entity: '#000000',
        Name.Exception: '#800080',
        Name.Function: '#800000 bold',
        Name.Label: '#800000',
        Name.Namespace: '#000000',
        Name.Tag: '#2984C6 bold',
        Name.Variable.Class: '#800080',
        Name.Variable.Global: '#000000',
        Name.Variable.Instance: '#800000',
        Name.Variable: '#000000',
        Operator.Word: '#808000 bold',
        Operator: '#808000 bold',
        Text: '#000000',
        Text.Whitespace: '#BFBFBF',
        Punctuation: '#202020'
    }
