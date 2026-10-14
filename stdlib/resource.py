"""Linux process limits and usage, compiled over POSIX kernel services."""

from _posix import getpagesize as _getpagesize
from _posix import getrlimit as _getrlimit
from _posix import getrusage as _getrusage
from _posix import prlimit as _prlimit
from _posix import setrlimit as _setrlimit
from math.integer import _index
from posix import _native_fd, strerror

RLIM_INFINITY = -1
RLIMIT_CPU, RLIMIT_FSIZE, RLIMIT_DATA, RLIMIT_STACK = 0, 1, 2, 3
RLIMIT_CORE, RLIMIT_RSS, RLIMIT_NPROC, RLIMIT_NOFILE = 4, 5, 6, 7
RLIMIT_MEMLOCK, RLIMIT_AS, RLIMIT_LOCKS, RLIMIT_SIGPENDING = 8, 9, 10, 11
RLIMIT_MSGQUEUE, RLIMIT_NICE, RLIMIT_RTPRIO, RLIMIT_RTTIME = 12, 13, 14, 15
RLIMIT_OFILE = RLIMIT_NOFILE
RUSAGE_SELF, RUSAGE_CHILDREN, RUSAGE_THREAD = 0, -1, 1
error = OSError


def _resource(value):
    value = _native_fd(value)
    if not 0 <= value < 16:
        raise ValueError('invalid resource specified')
    return value


def _limits(values):
    values = tuple(values)
    if len(values) != 2:
        raise ValueError('expected a tuple of 2 integers')
    result = []
    for value in values:
        value = _index(value)
        if value < -1:
            raise ValueError('Cannot convert negative int')
        if value >= 1 << 64:
            raise OverflowError('Python int too large to convert to C rlim_t')
        if value >= 1 << 63:
            value -= 1 << 64
        result.append(value)
    return result[0], result[1]


def _checked(result, setting=False, own=False):
    code, value = result
    if code:
        if setting and code == 22:
            raise ValueError('current limit exceeds maximum limit')
        if own and code == 1:
            raise ValueError('not allowed to raise maximum limit')
        raise OSError(code, strerror(code))
    return value


def getrlimit(resource, /):
    return _checked(_getrlimit(_resource(resource)))


def setrlimit(resource, limits, /):
    resource = _resource(resource)
    soft, hard = _limits(limits)
    return _checked(_setrlimit(resource, soft, hard), True, True)


def prlimit(pid, resource, limits=None, /):
    pid, resource = _native_fd(pid), _resource(resource)
    soft, hard = (0, 0) if limits is None else _limits(limits)
    return _checked(_prlimit(pid, resource, soft, hard, 0 if limits is None else 1), True)


def getpagesize():
    return _getpagesize()


class struct_rusage(tuple):
    n_fields = n_sequence_fields = 16
    n_unnamed_fields = 0

    def __new__(cls, sequence):
        values = tuple(sequence)
        if len(values) != 16:
            raise TypeError('resource.struct_rusage() takes a 16-sequence')
        return tuple.__new__(cls, values)


def _usage_field(index):
    def read(self):
        return tuple.__getitem__(self, index)
    return property(read)


_fields = ('ru_utime', 'ru_stime', 'ru_maxrss', 'ru_ixrss', 'ru_idrss', 'ru_isrss',
           'ru_minflt', 'ru_majflt', 'ru_nswap', 'ru_inblock', 'ru_oublock',
           'ru_msgsnd', 'ru_msgrcv', 'ru_nsignals', 'ru_nvcsw', 'ru_nivcsw')
for _index_, _name in enumerate(_fields):
    setattr(struct_rusage, _name, _usage_field(_index_))
del _index_, _name


def getrusage(who, /):
    result = _getrusage(_native_fd(who))
    if result[0] == 22:
        raise ValueError('invalid who parameter')
    return struct_rusage(_checked(result))
