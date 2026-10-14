"""Compiled descriptor policy over Linux poll and select physical primitives."""

from _mule_native import descriptor_poll as _poll
from _mule_native import descriptor_select as _select
from math import _as_float, ceil, floor, isfinite, isnan
from math.integer import _index

error = OSError
PIPE_BUF = 4096
POLLIN = 1
POLLPRI = 2
POLLOUT = 4
POLLERR = 8
POLLHUP = 16
POLLNVAL = 32
POLLRDNORM = 64
POLLRDBAND = 128
POLLWRNORM = 256
POLLWRBAND = 512
POLLMSG = 1024
POLLREMOVE = 4096
POLLRDHUP = 8192


def _fd(value):
    if not isinstance(value, int):
        try:
            method = value.fileno
        except AttributeError:
            raise TypeError("argument must be an int, or have a fileno() method") from None
        value = method()
        if not isinstance(value, int):
            raise TypeError("fileno() returned a non-integer")
    value = _index(value)
    if value < 0:
        raise ValueError("file descriptor cannot be a negative integer")
    if value >= 1 << 31:
        raise OverflowError("Python int too large to convert to C int")
    return value


def _events(value):
    value = _index(value)
    if value < 0:
        raise ValueError("Cannot convert negative int")
    if value >= 1 << 16:
        raise OverflowError("Python int too large for C unsigned short")
    return value


def _timeout(value, scale, maximum):
    number = _as_float(value)
    if isnan(number):
        raise ValueError("Invalid value NaN (not a number)")
    if not isfinite(number):
        raise OverflowError("timestamp out of range for C PyTime_t")
    number *= scale
    if number > maximum or number < -(1 << 63):
        raise OverflowError("timeout is too large")
    return ceil(number) if number >= 0 else floor(number)


class poll:
    def __init__(self):
        self._registrations = {}

    def register(self, fd, eventmask=POLLIN | POLLPRI | POLLOUT, /):
        self._registrations[_fd(fd)] = _events(eventmask)

    def modify(self, fd, eventmask, /):
        fd = _fd(fd)
        eventmask = _events(eventmask)
        if fd not in self._registrations:
            raise FileNotFoundError(2, 'No such file or directory')
        self._registrations[fd] = eventmask

    def unregister(self, fd, /):
        del self._registrations[_fd(fd)]

    def poll(self, timeout=None, /):
        milliseconds = -1 if timeout is None else _timeout(timeout, 1, (1 << 31) - 1)
        return list(_poll(tuple(self._registrations.items()), milliseconds))


def select(rlist, wlist, xlist, timeout=None, /):
    objects = (list(rlist), list(wlist), list(xlist))
    descriptors = tuple(tuple(_fd(value) for value in group) for group in objects)
    microseconds = -1
    if timeout is not None:
        timeout = _as_float(timeout)
        if timeout < 0:
            raise ValueError("timeout must be non-negative")
        microseconds = _timeout(timeout, 1_000_000, (1 << 63) - 1)
    ready = _select(*descriptors, microseconds)
    return tuple(
        [value for value, fd in zip(group, fds) if fd in selected]
        for group, fds, selected in zip(objects, descriptors, ready)
    )
