import logging
from pyqode.core.api import Mode
from pyqode.core.api import DelayJobRunner
from pyqode.core.backend import NotRunning
from pyqode.core.share import Definition
from pyqode.qt import QtCore


def _logger():
    return logging.getLogger(__name__)


class OutlineMode(Mode, QtCore.QObject):
    """
    Generic mode that provides outline information through the
    document_changed signal and a specialised worker function.

    To use this mode, you need to write a worker function that returns a list
    of pyqode.core.share.Definition (see
    pyqode.python.backend.workers.defined_names() for an example of how to
    implement the worker function).
    """

    #: Signal emitted when the document structure changed.
    document_changed = QtCore.Signal()

    @property
    def definitions(self):
        """
        Gets the list of top level definitions.
        """
        return self._results

    def __init__(self, worker, delay=1000):
        Mode.__init__(self)
        QtCore.QObject.__init__(self)
        self._worker = worker
        self._jobRunner = DelayJobRunner(delay=delay)
        #: The list of definitions found in the file, each item is a
        #: pyqode.core.share.Definition.
        self._results = []

    def on_state_changed(self, state):
        if state:
            self.editor.new_text_set.connect(self._run_analysis)
            self.editor.textChanged.connect(self._request_analysis)
        else:
            self.editor.textChanged.disconnect(self._request_analysis)
            self.editor.new_text_set.disconnect(self._run_analysis)
            self._jobRunner.cancel_requests()

    def _request_analysis(self):
        self._jobRunner.request_job(self._run_analysis)

    def _run_analysis(self):
        try:
            self.editor.file
            self.editor.toPlainText()
        except (RuntimeError, AttributeError):
            # called by the timer after the editor got deleted
            return
        if self.enabled:
            request_data = {
                'code': self.editor.toPlainText(),
                'path': self.editor.file.path,
                'encoding': self.editor.file.encoding
            }
            try:
                self.editor.backend.send_request(
                    self._worker, request_data,
                    on_receive=self._on_results_available)
            except NotRunning:
                QtCore.QTimer.singleShot(100, self._run_analysis)
        else:
            self._results = []
            self.document_changed.emit()

    def _on_results_available(self, results):
        if results:
            results = [Definition.from_dict(ddict) for ddict in results]
        self._results = results
        if self._results is not None:
            _logger().log(5, "Document structure changed")
            self.document_changed.emit()
