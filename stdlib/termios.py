"""Linux terminal policy over compiled descriptor and buffer services."""

from _termios_constants import *
from _posix import terminal_break as _break
from _posix import terminal_drain as _drain
from _posix import terminal_flow as _flow
from _posix import terminal_flush as _flush
from _posix import terminal_getattr as _getattr
from _posix import terminal_setattr as _setattr
from fcntl import _descriptor, ioctl
from math.integer import _index
from posix import _native_fd, strerror


class error(Exception):
    pass


def _operation(function, *arguments):
    while True:
        code, value = function(*arguments)
        if code == 4:
            continue
        if code:
            raise error(code, strerror(code))
        return value


def tcgetattr(fd, /):
    record = _operation(_getattr, _descriptor(fd))
    result = list(record[:6])
    canonical = bool(result[3] & ICANON)
    characters = []
    for index, byte in enumerate(record[6]):
        characters.append(byte if not canonical and index in (VMIN, VTIME) else bytes((byte,)))
    result.append(characters)
    return result


def tcsetattr(fd, when, attributes, /):
    fd, when = _descriptor(fd), _native_fd(when)
    if not isinstance(attributes, list) or len(attributes) != 7:
        raise TypeError('tcsetattr, arg 3: must be 7 element list')
    fields = [_index(value) & 0xFFFFFFFF for value in attributes[:6]]
    characters = attributes[6]
    if not isinstance(characters, list) or len(characters) != NCCS:
        raise TypeError('tcsetattr: attributes[6] must be ' + str(NCCS) + ' element list')
    values = []
    for value in characters:
        if isinstance(value, bytes) and len(value) == 1:
            value = value[0]
        else:
            value = _index(value)
            if not 0 <= value < 256:
                raise OverflowError('unsigned byte integer is out of range')
        values.append(value)
    return _operation(_setattr, fd, when, *fields, bytes(values))


def tcdrain(fd, /):
    return _operation(_drain, _descriptor(fd))


def tcflush(fd, queue, /):
    return _operation(_flush, _descriptor(fd), _native_fd(queue))


def tcflow(fd, action, /):
    return _operation(_flow, _descriptor(fd), _native_fd(action))


def tcsendbreak(fd, duration, /):
    return _operation(_break, _descriptor(fd), _native_fd(duration))


def tcgetwinsize(fd, /):
    import struct
    try:
        rows, columns, _, _ = struct.unpack('HHHH', ioctl(fd, TIOCGWINSZ, bytes(8)))
    except OSError as raised:
        raise error(*raised.args) from None
    return rows, columns


def tcsetwinsize(fd, winsize, /):
    import struct
    rows, columns = winsize
    try:
        ioctl(fd, TIOCSWINSZ, struct.pack('HHHH', _index(rows), _index(columns), 0, 0))
    except OSError as raised:
        raise error(*raised.args) from None
