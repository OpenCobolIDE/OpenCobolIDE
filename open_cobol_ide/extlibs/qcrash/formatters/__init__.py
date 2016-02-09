"""
This package contains the formatters used by the different backends:

- html_formatter: create a bug report using HTML
- md_formatter: create a bug report using Markdown

Implement :class:`qcrash.formatters.BaseFormatter` to add your own formatter
and set it the installed backends using
:meth:`qcrash.backends.BaseBackend.set_formatter`.

"""
