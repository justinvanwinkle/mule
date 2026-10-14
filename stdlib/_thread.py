"""Thread primitives, compiled by Mule.

This provides the interface of CPython's ``_thread`` extension module
(``Modules/_threadmodule.c``) for a program that runs on its single main
thread: locks and reentrant locks keep their acquisition and ownership
rules, and the main thread's identity is constant. Starting another thread
raises RuntimeError, because generated programs do not create OS threads yet.
"""

__mule_builtin_implementation__ = True

from _mule_native import thread_ident as _ident
from _mule_native import thread_native_id as _native_id
from _mule_native import main_thread_ident as _main_ident

error = RuntimeError
TIMEOUT_MAX = 9223372036.0
_MAIN_THREAD = _main_ident()


def get_ident():
    return _ident()


def get_native_id():
    return _native_id()


def _get_main_thread_ident():
    return _MAIN_THREAD


def _is_main_interpreter():
    return True


def daemon_threads_allowed():
    return True


def _count():
    return 0


def stack_size(size=0, /):
    if size != 0 and size < 32768:
        raise ValueError("size not valid: 32768 bytes is the minimum")
    return 0


def _would_block(blocking, timeout):
    if not blocking and timeout != -1:
        raise ValueError("can't specify a timeout for a non-blocking call")
    if timeout < 0 and timeout != -1:
        raise ValueError("timeout value must be a non-negative number")


class LockType:
    """A lock: at most one holder, released by anyone."""

    def __init__(self):
        self._locked = False

    def acquire(self, blocking=True, timeout=-1):
        _would_block(blocking, timeout)
        if self._locked:
            if blocking and timeout == -1:
                # The only thread is waiting on itself: CPython would hang.
                raise RuntimeError("deadlock: lock already held by the only thread")
            return False
        self._locked = True
        return True

    def release(self):
        if not self._locked:
            raise RuntimeError("release unlocked lock")
        self._locked = False

    def locked(self):
        return self._locked

    def _at_fork_reinit(self):
        self._locked = False

    acquire_lock = acquire
    release_lock = release
    locked_lock = locked

    def __enter__(self):
        return self.acquire()

    def __exit__(self, kind, value, traceback):
        self.release()

    def __repr__(self):
        return f"<{'locked' if self._locked else 'unlocked'} _thread.lock object>"


lock = LockType


def allocate_lock():
    return LockType()


class RLock:
    """A reentrant lock: its owner may acquire it again."""

    def __init__(self):
        self._owner = None
        self._count = 0

    def acquire(self, blocking=True, timeout=-1):
        _would_block(blocking, timeout)
        self._owner = _MAIN_THREAD
        self._count += 1
        return True

    def release(self):
        if self._count == 0:
            raise RuntimeError("cannot release un-acquired lock")
        self._count -= 1
        if self._count == 0:
            self._owner = None

    def locked(self):
        return self._count > 0

    def _is_owned(self):
        return self._count > 0

    def _recursion_count(self):
        return self._count

    def _release_save(self):
        state = (self._count, self._owner)
        self._count = 0
        self._owner = None
        return state

    def _acquire_restore(self, state):
        self._count, self._owner = state

    def _at_fork_reinit(self):
        self._count = 0
        self._owner = None

    def __enter__(self):
        return self.acquire()

    def __exit__(self, kind, value, traceback):
        self.release()

    def __repr__(self):
        state = "locked" if self._count else "unlocked"
        return f"<{state} _thread.RLock object owner={self._owner or 0} count={self._count}>"


class _ThreadHandle:
    """The handle of the main thread; other threads are never started."""

    def __init__(self):
        self.ident = _MAIN_THREAD

    def is_done(self):
        return False

    def join(self, timeout=None):
        raise RuntimeError("cannot join the current thread")

    def _set_done(self):
        pass


def _make_thread_handle(ident):
    return _ThreadHandle()


def start_new_thread(function, args, kwargs=None):
    raise RuntimeError("starting threads is not supported yet")


start_new = start_new_thread


def start_joinable_thread(function, handle=None, daemon=True):
    raise RuntimeError("starting threads is not supported yet")


def _shutdown():
    pass


def interrupt_main(signum=2, /):
    raise RuntimeError("interrupt_main is not supported yet")


def exit():
    raise SystemExit


exit_thread = exit


class _local:
    """Thread-local data: with one thread, ordinary instance attributes."""
