from pyqode.cobol.api import icons
from pyqode.cobol.api import keywords
from pyqode.cobol.api.parsers.names import defined_names


#: Free format cobol: ON/OFF
free_format = None
#: Whether cobol keywords case is lower or UPPER
lower_case_keywords = False


def set_free_format(value):
    """
    Change the free format flag used by the document analyser.
    :param value: True/False
    """
    global free_format
    free_format = value


def set_lower_case_keywords(lower_case):
    """
    Set the lower_case_keywords flags which is used to control the case of the
    proposed cobol_keywords

    :param lower_case: True to propose lower case keywords and False to
        propose upper case KEYWORDS
    """
    global lower_case_keywords
    lower_case_keywords = lower_case


class CobolCodeCompletionProvider:
    def __init__(self):
        self._kwds = []
        for keyword in keywords.RESERVED:
            self._kwds.append(
                {'name': keyword, 'icon': icons.ICON_KEYWORD})
        for keyword in keywords.INTRINSICS:
            self._kwds.append(
                {'name': keyword, 'icon': icons.ICON_FUNC})

    def complete(self, code, *_):
        global free_format, lower_case_keywords
        completions = []
        try:
            root, vars, functions = defined_names(
                code, free_format=free_format)
        except AttributeError:
            vars = []
            functions = []
        for var in vars:
            completions.append({
                'name': var.name,
                'icon': icons.ICON_VAR,
                'tooltip': var.description
            })
        for func in functions:
            completions.append({
                'name': func.name,
                'icon': icons.ICON_FUNC
            })
        if lower_case_keywords:
            completions += [{'name': k['name'].lower(), 'icon': k['icon']}
                            for k in self._kwds]
        else:
            completions += self._kwds
        return completions


def get_outline(data):
    if free_format is None:
        return None
    else:
        root_node, _, _ = defined_names(data['code'], free_format)
        return [ch.to_definition().to_dict() for ch in root_node.children]
