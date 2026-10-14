"""Record the files a compilation depends on, so a cached build can be validated.

Every source read and every module-search probe goes through this module. A
probe that found nothing is an input too: creating that file later would change
which module an import resolves to.
"""

import hashlib
from contextlib import contextmanager
from contextvars import ContextVar
from pathlib import Path

_recording: ContextVar[dict[str, str | None] | None] = ContextVar(
    "mule_source_inputs", default=None
)


def digest(data: bytes) -> str:
    """The SHA3-256 digest every Mule artifact and input is identified by."""
    return hashlib.sha3_256(data).hexdigest()


class _FileDigests:
    """Content digests of file versions already read by this process."""

    def __init__(self):
        self._versions = {}

    def version(self, path):
        state = path.stat()
        return (
            state.st_dev,
            state.st_ino,
            state.st_size,
            state.st_mtime_ns,
            state.st_ctime_ns,
        )

    def read(self, path):
        before = self.version(path)
        cached = self._versions.get(path)
        if cached is not None and cached[0] == before:
            return cached[1]
        return self.remember(path, before, path.read_bytes())

    def current(self, path):
        try:
            return self.version(path)
        except OSError:
            return None

    def remember(self, path, before, data):
        after_read = self.current(path)
        cached = self._versions.get(path)
        if before == after_read and cached is not None and cached[0] == before:
            return cached[1]
        state = digest(data)
        if before == after_read and before == self.current(path):
            self._versions[path] = before, state
        return state


_file_digests = _FileDigests()


def file_digest(path):
    return _file_digests.read(Path(path))


def _directory_inventory(path):
    entries = sorted((entry.name, entry.is_dir()) for entry in path.iterdir())
    data = "\0".join(name + ("/" if directory else "") for name, directory in entries)
    return [name for name, _ in entries], "directory:" + digest(data.encode())


def file_state(path):
    """A file digest, directory inventory digest, or None when absent."""
    path = Path(path)
    if path.is_dir():
        return _directory_inventory(path)[1]
    try:
        return file_digest(path)
    except FileNotFoundError, NotADirectoryError, IsADirectoryError:
        return None


def _record(path, state):
    inputs = _recording.get()
    if inputs is not None:
        inputs.setdefault(str(Path(path).resolve()), state)


def read_source(path):
    """Read a UTF-8 source file, recording its contents as a build input."""
    path = Path(path)
    before = _file_digests.version(path)
    data = path.read_bytes()
    _record(path, _file_digests.remember(path, before, data))
    return data.decode("utf-8")


def source_exists(path):
    """Probe for a source file, recording the result as a build input."""
    path = Path(path)
    if not path.is_file():
        _record(path, None)
        return False
    if _recording.get() is not None:
        _record(path, file_state(path))
    return True


def directory_entries(path):
    """Record an immediate package inventory, including future module additions."""
    path = Path(path)
    entries, state = _directory_inventory(path)
    _record(path, state)
    return entries


def directory_exists(path):
    """Record a namespace-package directory probe, including absent paths."""
    path = Path(path)
    exists = path.is_dir()
    _record(path, file_state(path))
    return exists


@contextmanager
def recording():
    """Collect inputs as {resolved path: digest or None} for the enclosed work."""
    inputs = {}
    token = _recording.set(inputs)
    try:
        yield inputs
    finally:
        _recording.reset(token)


def unchanged(inputs):
    return all(file_state(path) == state for path, state in inputs.items())
