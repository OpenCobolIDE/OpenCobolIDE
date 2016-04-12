# -*- coding: utf-8 -*-
"""
This module contains the server socket definition.
"""
import argparse
import inspect
import logging
import json
import os
import struct
import sys
import time
import traceback
import threading


try:
    import socketserver
    PY33 = True
except ImportError:
    import SocketServer as socketserver
    PY33 = False


def _logger():
    """ Returns the module's logger """
    return logging.getLogger(__name__)


HEARTBEAT_DELAY = 60  # delay max without heartbeat signal


def import_class(klass):
    """
    Imports a class from a fully qualified name string.

    :param klass: class string, e.g.
        "pyqode.core.backend.workers.CodeCompletionWorker"
    :return: The corresponding class

    """
    path = klass.rfind(".")
    class_name = klass[path + 1: len(klass)]
    try:
        module = __import__(klass[0:path], globals(), locals(), [class_name])
        klass = getattr(module, class_name)
    except ImportError as e:
        raise ImportError('%s: %s' % (klass, str(e)))
    except AttributeError:
        raise ImportError(klass)
    else:
        return klass


class JsonServer(socketserver.TCPServer):
    """
    A server socket based on a json messaging system.
    """

    class _Handler(socketserver.BaseRequestHandler):
        def read_bytes(self, size):
            """
            Read x bytes

            :param size: number of bytes to read.

            """
            if not PY33:
                data = ''
            else:
                data = bytes()
            while len(data) < size:
                tmp = self.request.recv(size - len(data))
                data += tmp
                if tmp == '':
                    raise RuntimeError("socket connection broken")
            return data

        def get_msg_len(self):
            """ Gets message len """
            data = self.read_bytes(4)
            payload = struct.unpack('=I', data)
            return payload[0]

        def read(self):
            """ Reads a json string from socket and load it. """
            size = self.get_msg_len()
            data = self.read_bytes(size).decode('utf-8')
            return json.loads(data)

        def send(self, obj):
            """
            Sends a python obj as a json string on the socket.

            :param obj: The object to send, must be Json serializable.
            """
            msg = json.dumps(obj).encode('utf-8')
            _logger().log(1, 'sending %d bytes for the payload', len(msg))
            header = struct.pack('=I', len(msg))
            self.request.sendall(header)
            self.request.sendall(msg)

        def handle(self):
            """
            Hanlde the request and keep it alive while shutdown signal
            has not been received
            """
            self.srv.reset_heartbeat()
            # make sure to have enough time to handle the request
            self.srv.timeout = HEARTBEAT_DELAY * 10
            data = self.read()
            self._handle(data)
            self.srv.timeout = HEARTBEAT_DELAY
            self.srv.reset_heartbeat()

        def _handle(self, data):
            """
            Handles a work request.
            """
            try:
                _logger().log(1, 'handling request %r', data)
                assert data['worker']
                assert data['request_id']
                assert data['data'] is not None
                response = {'request_id': data['request_id'], 'results': []}
                try:
                    worker = import_class(data['worker'])
                except ImportError:
                    _logger().exception('Failed to import worker class')
                else:
                    if inspect.isclass(worker):
                        worker = worker()
                    _logger().log(1, 'worker: %r', worker)
                    _logger().log(1, 'data: %r', data['data'])
                    try:
                        ret_val = worker(data['data'])
                    except Exception:
                        _logger().exception(
                            'something went bad with worker %r(data=%r)',
                            worker, data['data'])
                        ret_val = None
                    if ret_val is None:
                        ret_val = []
                    response = {'request_id': data['request_id'],
                                'results': ret_val}
                finally:
                    _logger().log(1, 'sending response: %r', response)
                    try:
                        self.send(response)
                    except ConnectionAbortedError:
                        pass
            except:
                _logger().warn('error with data=%r', data)
                exc1, exc2, exc3 = sys.exc_info()
                traceback.print_exception(exc1, exc2, exc3, file=sys.stderr)

    def __init__(self, args=None):
        """
        :param args: Argument parser args. If None, the server will setup and
            use its own argument parser (using
            :meth:`pyqode.core.backend.default_parser`)
        """
        self.reset_heartbeat()
        if not args:
            args = default_parser().parse_args()
        self.port = args.port
        self.timeout = HEARTBEAT_DELAY
        self._Handler.srv = self
        socketserver.TCPServer.__init__(
            self, ('127.0.0.1', int(args.port)), self._Handler)
        print('started on 127.0.0.1:%d' % int(args.port))
        print('running with python %d.%d.%d' % (sys.version_info[:3]))
        self._heartbeat_thread = threading.Thread(target=self.heartbeat)
        self._heartbeat_thread.setDaemon(True)
        self._heartbeat_thread.start()

    def reset_heartbeat(self):
        self.last_time = time.time()
        self.elapsed_time = 0

    def heartbeat(self):
        while True:
            elapsed_time = time.time() - self.last_time
            if elapsed_time > self.timeout:
                self.shutdown()
                sys.exit(1)
            time.sleep(1)


def default_parser():
    """
    Configures and return the default argument parser. You should use this
    parser as a base if you want to add custom arguments.

    The default parser only has one argument, the tcp port used to start the
    server socket. *(CodeEdit picks up a free port and use it to run
    the server and connect its client socket)*

    :returns: The default server argument parser.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("port", help="the local tcp port to use to run "
                        "the server")
    return parser


def serve_forever(args=None):
    """
    Creates the server and serves forever

    :param args: Optional args if you decided to use your own
        argument parser. Default is None to let the JsonServer setup its own
        parser and parse command line arguments.
    """
    class Unbuffered(object):
        def __init__(self, stream):
            self.stream = stream

        def write(self, data):
            self.stream.write(data)
            self.stream.flush()

        def __getattr__(self, attr):
            return getattr(self.stream, attr)

    sys.stdout = Unbuffered(sys.stdout)
    sys.stderr = Unbuffered(sys.stderr)

    server = JsonServer(args=args)
    server.serve_forever()


# Server script example
if __name__ == '__main__':
    from pyqode.core import backend
    backend.CodeCompletionWorker.providers.append(
        backend.DocumentWordsProvider())
    serve_forever()
