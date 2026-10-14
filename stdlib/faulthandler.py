"""Fatal OS fault handling over compiler-provided native activation metadata."""

from _mule_native import fault_enable as _enable
from _mule_native import fault_disable as _disable
from _mule_native import fault_enabled as _enabled
from _mule_native import fault_dump as _dump
from _mule_native import fault_c_stack as _dump_c
from _mule_native import native_thread_id as _thread_id

_enabled_file = None


def _target(file):
    if file is None:
        import sys

        file = sys.stderr
        if file is None:
            raise RuntimeError("sys.stderr is None")
    from posix import _native_fd

    fd = _native_fd(file if isinstance(file, int) else file.fileno())
    if fd < 0:
        raise ValueError(
            "file descriptor cannot be a negative integer (" + str(fd) + ")"
        )
    return file, fd


def _header():
    return (
        "Current thread 0x" + format(_thread_id(), "x") + " (most recent call first):\n"
    ).encode("ascii")


def enable(file=None, all_threads=True, c_stack=True):
    global _enabled_file
    all_threads = True if all_threads else False
    c_stack = True if c_stack else False
    file, fd = _target(file)
    _enable(fd, _header(), c_stack)
    _enabled_file = file


def disable():
    global _enabled_file
    result = _disable()
    _enabled_file = None
    return result


def is_enabled():
    return _enabled()


def dump_traceback(file=None, all_threads=True):
    all_threads = True if all_threads else False
    file, fd = _target(file)
    _dump(fd, _header())


def dump_c_stack(file=None):
    file, fd = _target(file)
    _dump_c(fd)


def dump_traceback_later(timeout, repeat=False, file=None, exit=False):
    raise NotImplementedError("delayed fault traceback scheduling is not implemented")


def cancel_dump_traceback_later():
    pass


def register(signum, file=None, all_threads=True, chain=False):
    raise NotImplementedError(
        "user-signal fault traceback registration is not implemented"
    )


def unregister(signum):
    return False
