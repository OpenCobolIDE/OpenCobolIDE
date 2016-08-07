"""
This module contains the client socket API. This API is exposed to the
user throught the backend manager (
:class:`pyqode.core.managers.BackendManager`)

"""
import locale
import json
import logging
import socket
import struct
import sys
import uuid
from weakref import ref
from pyqode.qt import QtCore, QtNetwork


def _logger():
    return logging.getLogger(__name__)


#: log level for communication
COMM = 1


def comm(msg, *args):
    _logger().log(COMM, msg, *args)


#: Dictionary of socket errors messages
SOCKET_ERROR_STRINGS = {
    0: 'the connection was refused by the peer (or timed out).',
    1: 'the remote host closed the connection.',
    2: 'the host address was not found.',
    3: 'the socket operation failed because the application lacked the '
       'required privileges.',
    4: 'the local system ran out of resources (e.g., too many sockets).',
    5: 'the socket operation timed out.',
    6: "the datagram was larger than the operating system's limit (which can "
       "be as low as 8192 bytes).",
    7: 'an error occurred with the network (e.g., the network cable was '
       'accidentally plugged out).',
    # 9 and 10 are UDP only, we only care about TCP.
    # all others erros are unlikely to happen in our case (proxy related
    # errors)
    - 1: 'an unidentified error occurred.',
}

#: Dictionary of process errors messages
PROCESS_ERROR_STRING = {
    0: 'the process failed to start. Either the invoked program is missing, '
       'or you may have insufficient permissions to invoke the program.',
    1: 'the process crashed some time after starting successfully.',
    2: 'the last waitFor...() function timed out. The state of QProcess is '
       'unchanged, and you can try calling waitFor...() again.',
    4: 'an error occurred when attempting to write to the process. '
       'For example, the process may not be running, or it may have closed '
       'its input channel.',
    3: 'an error occurred when attempting to read from the process. '
       'For example, the process may not be running.',
    5: 'an unknown error occurred. This is the default return value of '
       'error().'
}


if sys.version_info[0] >= 3:
    class WeakMethod(ref):
        """
        A custom `weakref.ref` subclass which simulates a weak reference to
        a bound method, working around the lifetime problem of bound methods.
        """

        __slots__ = "_func_ref", "_meth_type", "_alive", "__weakref__"

        def __new__(cls, meth, callback=None):
            try:
                obj = meth.__self__
                func = meth.__func__
            except AttributeError:
                raise TypeError("argument should be a bound method, not {}"
                                .format(type(meth)))

            def _cb(arg):
                # The self-weakref trick is needed to avoid creating a
                # reference cycle.
                self = self_wr()
                if self._alive:
                    self._alive = False
                    if callback is not None:
                        callback(self)
            self = ref.__new__(cls, obj, _cb)
            self._func_ref = ref(func, _cb)
            self._meth_type = type(meth)
            self._alive = True
            self_wr = ref(self)
            return self

        def __call__(self):
            obj = super().__call__()
            func = self._func_ref()
            if obj is None or func is None:
                return None
            return self._meth_type(func, obj)

        def __eq__(self, other):
            if isinstance(other, WeakMethod):
                if not self._alive or not other._alive:
                    return self is other
                return ref.__eq__(self, other) and \
                    self._func_ref == other._func_ref
            return False

        def __ne__(self, other):
            if isinstance(other, WeakMethod):
                if not self._alive or not other._alive:
                    return self is not other
                return ref.__ne__(self, other) or \
                    self._func_ref != other._func_ref
            return True

        __hash__ = ref.__hash__
else:
    class _weak_callable:
        def __init__(self, obj, func):
            self._obj = obj
            self._meth = func

        def __call__(self, *args, **kws):
            if self._obj is not None:
                return self._meth(self._obj, *args, **kws)
            else:
                return self._meth(*args, **kws)

        def __getattr__(self, attr):
            if attr == 'im_self':
                return self._obj
            if attr == 'im_func':
                return self._meth
            raise AttributeError(attr)

    class WeakMethod:
        """ Wraps a function or, more importantly, a bound method, in
        a way that allows a bound method's object to be GC'd, while
        providing the same interface as a normal weak reference. """
        def __init__(self, fn):
            try:
                self._obj = ref(fn.im_self)
                self._meth = fn.im_func
            except AttributeError:
                # It's not a bound method.
                self._obj = None
                self._meth = fn

        def __call__(self):
            if self._dead():
                return None
            return _weak_callable(self._obj(), self._meth)

        def _dead(self):
            return self._obj is not None and self._obj() is None


class JsonTcpClient(QtNetwork.QTcpSocket):
    """
    A json tcp client socket used to start and communicate with the pyqode
    backend.

    It uses a simple message protocol. A message is made up of two parts.
    parts:
      - header: contains the length of the payload. (4bytes)
      - payload: data as a json string.

    """
    #: Internal signal emitted when the backend request finished and the
    #: socket can be removed from the list of sockets maintained by the
    #: backend manager
    finished = QtCore.Signal(QtNetwork.QTcpSocket)

    def __init__(self, parent, port, worker_class_or_function, args,
                 on_receive=None):
        super(JsonTcpClient, self).__init__(parent)
        self._port = port
        self._worker = worker_class_or_function
        self._args = args
        self._header_complete = False
        self._header_buf = bytes()
        self._to_read = 0
        self._data_buf = bytes()
        if on_receive:
            try:
                self._callback = WeakMethod(on_receive)
            except TypeError:
                # unbound method (i.e. free function)
                self._callback = ref(on_receive)
        else:
            self._callback = None
        self.is_connected = False
        self._closed = False
        self.connected.connect(self._on_connected)
        self.error.connect(self._on_error)
        self.disconnected.connect(self._on_disconnected)
        self.readyRead.connect(self._on_ready_read)
        self._connect()

    def close(self):
        self._closed = True  # fix issue with QTimer.singleShot
        super(JsonTcpClient, self).close()
        self._callback = None

    def _send_request(self):
        """
        Sends the request to the backend.
        """
        if isinstance(self._worker, str):
            classname = self._worker
        else:
            classname = '%s.%s' % (self._worker.__module__,
                                   self._worker.__name__)
        self.request_id = str(uuid.uuid4())
        self.send({'request_id': self.request_id, 'worker': classname,
                   'data': self._args})

    def send(self, obj, encoding='utf-8'):
        """
        Sends a python object to the backend. The object **must be JSON
        serialisable**.

        :param obj: object to send
        :param encoding: encoding used to encode the json message into a
            bytes array, this should match CodeEdit.file.encoding.
        """
        comm('sending request: %r', obj)
        msg = json.dumps(obj)
        msg = msg.encode(encoding)
        header = struct.pack('=I', len(msg))
        self.write(header)
        self.write(msg)

    @staticmethod
    def pick_free_port():
        """ Picks a free port """
        test_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        test_socket.bind(('127.0.0.1', 0))
        free_port = int(test_socket.getsockname()[1])
        test_socket.close()
        return free_port

    def _connect(self):
        """ Connects our client socket to the backend socket """
        if self is None:
            return
        comm('connecting to 127.0.0.1:%d', self._port)
        address = QtNetwork.QHostAddress('127.0.0.1')
        self.connectToHost(address, self._port)
        if sys.platform == 'darwin':
            self.waitForConnected()

    def _on_connected(self):
        comm('connected to backend: %s:%d', self.peerName(), self.peerPort())
        self.is_connected = True
        self._send_request()

    def _on_error(self, error):
        if error not in SOCKET_ERROR_STRINGS:  # pragma: no cover
            error = -1
        if error == 1 and self.is_connected or (
                not self.is_connected and error == 0 and not self._closed):
            log_fct = comm
        else:
            log_fct = _logger().warning

        if error == 0 and not self.is_connected and not self._closed:
            QtCore.QTimer.singleShot(100, self._connect)

        log_fct(SOCKET_ERROR_STRINGS[error])

    def _on_disconnected(self):
        try:
            comm('disconnected from backend: %s:%d', self.peerName(),
                 self.peerPort())
        except (AttributeError, RuntimeError):
            # logger might be None if for some reason qt deletes the socket
            # after python global exit
            pass
        try:
            self.is_connected = False
        except AttributeError:
            pass

    def _read_header(self):
        comm('reading header')
        self._header_buf += self.read(4)
        if len(self._header_buf) == 4:
            self._header_complete = True
            try:
                header = struct.unpack('=I', self._header_buf)
            except TypeError:
                # pyside
                header = struct.unpack('=I', self._header_buf.data())
            self._to_read = header[0]
            self._header_buf = bytes()
            comm('header content: %d', self._to_read)

    def _read_payload(self):
        """ Reads the payload (=data) """
        comm('reading payload data')
        comm('remaining bytes to read: %d', self._to_read)
        data_read = self.read(self._to_read)
        nb_bytes_read = len(data_read)
        comm('%d bytes read', nb_bytes_read)
        self._data_buf += data_read
        self._to_read -= nb_bytes_read
        if self._to_read <= 0:
            try:
                data = self._data_buf.decode('utf-8')
            except AttributeError:
                data = bytes(self._data_buf.data()).decode('utf-8')
            comm('payload read: %r', data)
            comm('payload length: %r', len(self._data_buf))
            comm('decoding payload as json object')
            obj = json.loads(data)
            comm('response received: %r', obj)
            try:
                results = obj['results']
            except (KeyError, TypeError):
                results = None
            # possible callback
            if self._callback and self._callback():
                self._callback()(results)
            self._header_complete = False
            self._data_buf = bytes()
            self.finished.emit(self)

    def _on_ready_read(self):
        """ Read bytes when ready read """
        while self.bytesAvailable():
            if not self._header_complete:
                self._read_header()
            else:
                self._read_payload()


class BackendProcess(QtCore.QProcess):
    """
    Extends QProcess with methods to easily manipulate the backend process.

    Also logs everything that is written to the process' stdout/stderr.
    """
    def __init__(self, parent):
        super(BackendProcess, self).__init__(parent)
        self.started.connect(self._on_process_started)
        self.error.connect(self._on_process_error)
        self.finished.connect(self._on_process_finished)
        self.readyReadStandardOutput.connect(self._on_process_stdout_ready)
        self.readyReadStandardError.connect(self._on_process_stderr_ready)
        self.running = False
        self.starting = True
        self._srv_logger = logging.getLogger('pyqode.backend')
        self._prevent_logs = False
        self._encoding = locale.getpreferredencoding()

    def _on_process_started(self):
        """ Logs process started """
        comm('backend process started')
        if self is None:
            return
        self.starting = False
        self.running = True

    def _on_process_error(self, error):
        """ Logs process error """
        if self is None:
            return
        if error not in PROCESS_ERROR_STRING:
            error = -1
        if not self._prevent_logs:
            _logger().warning(PROCESS_ERROR_STRING[error])

    def _on_process_finished(self, exit_code):
        """ Logs process exit status """
        comm('backend process finished with exit code %d', exit_code)
        try:
            self.running = False
        except AttributeError:
            pass

    def _on_process_stdout_ready(self):
        """ Logs process output """
        if not self:
            return
        o = self.readAllStandardOutput()
        try:
            output = bytes(o).decode(self._encoding)
        except TypeError:
            output = bytes(o.data()).decode(self._encoding)
        for line in output.splitlines():
            self._srv_logger.log(1, line)

    def _on_process_stderr_ready(self):
        """ Logs process output (stderr) """
        try:
            o = self.readAllStandardError()
        except (TypeError, RuntimeError):
            # widget already deleted
            return
        try:
            output = bytes(o).decode(self._encoding)
        except TypeError:
            output = bytes(o.data()).decode(self._encoding)
        for line in output.splitlines():
            self._srv_logger.error(line)

    def terminate(self):
        """ Terminate the process """
        self.running = False
        super(BackendProcess, self).terminate()
