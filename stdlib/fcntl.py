"""Linux descriptor controls and advisory locks, compiled over POSIX services."""

from _posix import fcntl_buffer as _fcntl_buffer
from _posix import fcntl_int as _fcntl_int
from _posix import flock as _flock
from _posix import ioctl_buffer as _ioctl_buffer
from _posix import ioctl_integer as _ioctl_integer
from math.integer import _index
from posix import _native_fd, strerror

LOCK_SH, LOCK_EX, LOCK_NB, LOCK_UN = 1, 2, 4, 8
LOCK_MAND, LOCK_READ, LOCK_WRITE, LOCK_RW = 32, 64, 128, 192
F_DUPFD, F_GETFD, F_SETFD, F_GETFL, F_SETFL = 0, 1, 2, 3, 4
F_GETLK, F_SETLK, F_SETLKW = 5, 6, 7
F_SETOWN, F_GETOWN, F_SETSIG, F_GETSIG = 8, 9, 10, 11
F_SETLEASE, F_GETLEASE, F_NOTIFY = 1024, 1025, 1026
F_DUPFD_CLOEXEC = 1030
F_SETPIPE_SZ, F_GETPIPE_SZ = 1031, 1032
F_ADD_SEALS, F_GET_SEALS = 1033, 1034
F_SEAL_SEAL, F_SEAL_SHRINK, F_SEAL_GROW, F_SEAL_WRITE = 1, 2, 4, 8
FD_CLOEXEC = 1
F_RDLCK, F_WRLCK, F_UNLCK = 0, 1, 2


def _descriptor(value):
    if not isinstance(value, int):
        try:
            method = value.fileno
        except AttributeError:
            raise TypeError('argument must be an int, or have a fileno() method.') from None
        value = method()
        if not isinstance(value, int):
            raise TypeError('fileno() returned a non-integer')
    value = _native_fd(value)
    if value < 0:
        raise ValueError('file descriptor cannot be a negative integer (' + str(value) + ')')
    return value


def _operation(function, *arguments):
    while True:
        error, result = function(*arguments)
        if error == 4:
            continue
        if error:
            raise OSError(error, strerror(error))
        return result


def flock(fd, operation, /):
    return _operation(_flock, _descriptor(fd), _native_fd(operation))


def fcntl(fd, cmd, arg=0, /):
    fd, cmd = _descriptor(fd), _native_fd(cmd)
    if isinstance(arg, str):
        arg = arg.encode('utf-8')
    if isinstance(arg, bytes):
        if len(arg) > 1024:
            raise ValueError('fcntl argument 3 is too long')
        return _operation(_fcntl_buffer, fd, cmd, bytes(arg))
    arg = _index(arg) & 0xFFFFFFFF
    return _operation(_fcntl_int, fd, cmd, arg)


def lockf(fd, cmd, len=0, start=0, whence=0, /):
    import struct

    fd, cmd, whence = _descriptor(fd), _native_fd(cmd), _native_fd(whence)
    if cmd == LOCK_UN:
        kind = F_UNLCK
    elif cmd & LOCK_SH:
        kind = F_RDLCK
    elif cmd & LOCK_EX:
        kind = F_WRLCK
    else:
        raise ValueError('unrecognized lockf argument')
    length, start = _index(len), _index(start)
    if not -(1 << 63) <= length < 1 << 63 or not -(1 << 63) <= start < 1 << 63:
        raise OverflowError('Python int too large to convert to C long')
    # Linux's native struct flock: short fields, aligned off_t fields, then pid.
    record = struct.pack('@hhqqi', kind, whence, start, length, 0)
    fcntl(fd, F_SETLK if cmd & LOCK_NB else F_SETLKW, record)


def ioctl(fd, request, arg=0, mutate_flag=True, /):
    fd = _descriptor(fd)
    request = _index(request) & ((1 << 64) - 1)
    if request >= 1 << 63:
        request -= 1 << 64
    if isinstance(arg, int):
        return _operation(_ioctl_integer, fd, request, _native_fd(arg))
    if isinstance(arg, str):
        arg = arg.encode('utf-8')
    with memoryview(arg) as view:
        if not view.c_contiguous:
            raise BufferError('memoryview: underlying buffer is not C-contiguous')
        mutable = not view.readonly and bool(mutate_flag)
        if not mutable and view.nbytes > 1024:
            raise ValueError('ioctl string arg too long')
        returned, output = _operation(_ioctl_buffer, fd, request, view.tobytes())
        if mutable:
            with view.cast('B') as target:
                for index in range(target.nbytes):
                    target[index] = output[index]
            return returned
        return output
