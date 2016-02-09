"""
This package contains the backends used to actually send the crash report.

QCrash provides 2 backends:

- email: let the user report the crash/issue via an email
- github: let the user report the crash/issue on you github issue tracker

To use a backend, use :meth:`qcrash.api.install_backend`.

To add your own, just subclass :class:`qcrash.backends.BaseBackend` and
implement `send_report`.
"""
from .base import BaseBackend
from .email import EmailBackend
from .github import GithubBackend


__all__ = [
    'BaseBackend',
    'EmailBackend',
    'GithubBackend'
]
