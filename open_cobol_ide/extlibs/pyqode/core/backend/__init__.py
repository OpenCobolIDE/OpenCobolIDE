"""
The backend package contains the API to use on the server side:
    - server + methods for running it
    - workers definitions

pyQode uses a client-server architecture for running heavy tasks such as code
analysis (lint), code completion,...

Protocol
--------

We use a worker based json messaging server using the TCP/IP transport.

We build our own, very simple protocol where each message is made up of two
parts:

  - a header: simply contains the length of the payload
  - a payload: a json formatted string, the content of the message.

There are two type of json object: a request and a response.

Request
+++++++
For a request, the object will contains the following fields:

  - 'request_id': uuid generated client side
  - 'worker': fully qualified name to the worker callable (class or function),
    e.g. 'pyqode.core.backend.workers.echo_worker'
  - 'data': data specific to the chose worker.

E.g::

    {
        'request_id': 'a97285af-cc88-48a4-ac69-7459b9c7fa66',
        'worker': 'pyqode.core.backend.workers.echo_worker',
        'data': ['some code', 0]
    }

Response
++++++++

For a response, the object will contains the following fields:
    - 'request_id': uuid generated client side that is simply echoed back
    - 'results': worker results (list, tuple, string,...)

E.g::

    {
        'request_id': 'a97285af-cc88-48a4-ac69-7459b9c7fa66',
        'results': ['some code', 0]
    }

Server script
-------------

The server script must be written by the user. Don't worry, it's very simple.
All you have to do is to create and run a JsonTcpServer instance.

We choose to let you write the main script to let you easily configure it.

Some workers will requires some tweaking (for the completion worker,
you will want to add custom completion providers, you might also want to
modify sys.path,...). It also makes the packaging process more consistent,
your script will be packaged on linux using setup.py and will be frozen on
windows using cx_Freeze.

Here is the most simple and basic example of a server script:

.. code-block: python
    from pyqode.core import backend

    if __name__ == '__main__':
        backend.serve_forever()

.. warning:: The user can choose the python interpreter that will run the
    server. That means that classes and functions that run on the server side (
    workers) **should fully support python2 syntax** and that pyqode.core
    should be installed on the target interpreter sys path!!! (even for a
    virtual env). An alternative is to keep pyqode.core package (and all
    dependencies in a zip archive that you mount on the sys path in your
    server script.

.. note:: print statements on the server side will be logged as debug messages
    on the client side. To have your messages logged as error message just
    print to sys.stderr.

"""
from .server import JsonServer
from .server import default_parser
from .server import serve_forever
from .workers import CodeCompletionWorker
from .workers import DocumentWordsProvider
from .workers import echo_worker


class NotConnected(Exception):
    """
    Raised if the client is not connected to the server when an operation
    is requested.

    .. deprecated:: Since v2.3, you should instead use ``NotRunning``.
        This will be removed in v2.5

    """
    def __init__(self):
        super(NotConnected, self).__init__(
            'Client socket not connected or server not started')


class NotRunning(Exception):
    """
    Raise if the backend process is not running and a backend operation
    is requested.

    """
    def __init__(self):
        super(NotRunning, self).__init__(
            'Backend process not running')


__all__ = [
    'JsonServer',
    'default_parser',
    'serve_forever',
    'CodeCompletionWorker',
    'DocumentWordsProvider',
    'echo_worker',
    'NotConnected',
    'NotRunning'
]
