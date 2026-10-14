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
        data = path.read_bytes()
    except FileNotFoundError, NotADirectoryError, IsADirectoryError:
        return None
    return digest(data)


def _record(path, state):
    inputs = _recording.get()
    if inputs is not None:
        inputs.setdefault(str(Path(path).resolve()), state)


def read_source(path):
    """Read a UTF-8 source file, recording its contents as a build input."""
    data = Path(path).read_bytes()
    _record(path, digest(data))
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
