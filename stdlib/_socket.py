"""Compiled socket policy over owned Linux descriptors and libc services."""

from _mule_native import (
    address_info as _address_info,
    address_pack as _address_pack,
    address_text as _address_text,
    host_name as _host_name,
    socket_accept as _accept,
    socket_adopt as _adopt,
    socket_bind as _bind,
    socket_close as _close,
    socket_connect as _connect,
    socket_detach as _detach,
    socket_fd as _fd,
    socket_listen as _listen,
    socket_name as _name,
    socket_new as _new,
    socket_nonblocking as _nonblocking,
    socket_option as _option,
    socket_pair as _pair,
    socket_peer as _peer,
    socket_receive as _receive,
    socket_receive_from as _receive_from,
    socket_send as _send,
    socket_send_to as _send_to,
    socket_set_option as _set_option,
    socket_shutdown as _shutdown,
)
import os
from math import _as_float, isfinite, isnan
from math.integer import _index
from select import select
from time import monotonic

AF_UNSPEC = 0
AF_UNIX = 1
AF_INET = 2
AF_INET6 = 10
SOCK_STREAM = 1
SOCK_DGRAM = 2
SOCK_RAW = 3
SOCK_SEQPACKET = 5
SOCK_NONBLOCK = 2048
SOCK_CLOEXEC = 524288
SOL_SOCKET = 1
SO_REUSEADDR = 2
SO_TYPE = 3
SO_ERROR = 4
SO_BROADCAST = 6
SO_SNDBUF = 7
SO_RCVBUF = 8
SO_KEEPALIVE = 9
SO_REUSEPORT = 15
SO_PROTOCOL = 38
SO_DOMAIN = 39
SHUT_RD = 0
SHUT_WR = 1
SHUT_RDWR = 2
IPPROTO_IP = 0
IPPROTO_TCP = 6
IPPROTO_UDP = 17
IPPROTO_IPV6 = 41
TCP_NODELAY = 1
IPV6_V6ONLY = 26
MSG_OOB = 1
MSG_PEEK = 2
MSG_DONTROUTE = 4
MSG_DONTWAIT = 64
MSG_WAITALL = 256
MSG_NOSIGNAL = 16384
AI_PASSIVE = 1
AI_CANONNAME = 2
AI_NUMERICHOST = 4
AI_V4MAPPED = 8
AI_ALL = 16
AI_ADDRCONFIG = 32
AI_NUMERICSERV = 1024
has_ipv6 = True
error = OSError
timeout = TimeoutError
_default_timeout = None


class gaierror(OSError):
    pass


class herror(OSError):
    pass


def _checked(result):
    code, value = result
    if code:
        exception = OSError
        if code in (11, 114, 115):
            exception = BlockingIOError
        elif code == 32:
            exception = BrokenPipeError
        elif code == 111:
            exception = ConnectionRefusedError
        elif code == 104:
            exception = ConnectionResetError
        elif code == 110:
            exception = TimeoutError
        raise exception(code, os.strerror(code))
    return value


def _integer(value):
    value = _index(value)
    if not -(1 << 31) <= value < 1 << 31:
        raise OverflowError("Python int too large to convert to C int")
    return value


def _timeout(value):
    if value is None:
        return None
    value = _as_float(value)
    if isnan(value):
        raise ValueError("Invalid value NaN (not a number)")
    if not isfinite(value) or value >= (1 << 63) / 1_000_000_000:
        raise OverflowError("timestamp out of range for C PyTime_t")
    if value < 0:
        raise ValueError("Timeout value out of range")
    return value


def getdefaulttimeout():
    return _default_timeout


def setdefaulttimeout(value):
    global _default_timeout
    _default_timeout = _timeout(value)


def _buffer(value):
    view = memoryview(value)
    if not view.c_contiguous:
        raise BufferError("memoryview: underlying buffer is not C-contiguous")
    return view.tobytes()


def inet_pton(family, address):
    return _checked(_address_pack(_integer(family), address))


def inet_ntop(family, packed_ip):
    return _checked(_address_text(_integer(family), _buffer(packed_ip)))


def gethostname():
    return _checked(_host_name())


def _decode_address(raw):
    family = int.from_bytes(raw[:2], "little")
    if family == AF_INET:
        return inet_ntop(family, raw[4:8]), int.from_bytes(raw[2:4], "big")
    if family == AF_INET6:
        return (
            inet_ntop(family, raw[8:24]),
            int.from_bytes(raw[2:4], "big"),
            int.from_bytes(raw[4:8], "big"),
            int.from_bytes(raw[24:28], "little"),
        )
    if family == AF_UNIX:
        path = raw[2:]
        if path[:1] == b"\0":
            return path
        return os.fsdecode(path.rstrip(b"\0"))
    raise NotImplementedError("socket address family is not implemented")


def getaddrinfo(host, port, family=0, type=0, proto=0, flags=0):
    if isinstance(host, bytes):
        host = host.decode("ascii")
    if host is not None and not isinstance(host, str):
        raise TypeError("getaddrinfo() argument 1 must be string or None")
    if isinstance(port, int):
        port = str(_index(port))
    code, rows = _address_info(
        host, port, _integer(family), _integer(type), _integer(proto), _integer(flags)
    )
    if code:
        raise gaierror(code, rows)
    return [
        (af, kind, protocol, name, _decode_address(raw))
        for af, kind, protocol, name, raw in rows
    ]


def _encode_address(family, kind, protocol, address):
    if family == AF_UNIX:
        path = os.fsencode(address)
        if len(path) > 107:
            raise OSError("AF_UNIX path too long")
        return b"\1\0" + path + (b"" if path[:1] == b"\0" else b"\0")
    if family not in (AF_INET, AF_INET6):
        raise NotImplementedError("socket address family is not implemented")
    if not isinstance(address, tuple):
        raise TypeError("address must be a tuple")
    if len(address) < 2 or len(address) > (2 if family == AF_INET else 4):
        raise TypeError("invalid address tuple length")
    host, port = address[:2]
    port = _index(port)
    if not 0 <= port <= 65535:
        raise OverflowError("bind(): port must be 0-65535.")
    if isinstance(host, bytes):
        host = host.decode("ascii")
    if host == "":
        host = "0.0.0.0" if family == AF_INET else "::"
    elif host == "<broadcast>" and family == AF_INET:
        host = "255.255.255.255"
    code, packed = _address_pack(family, host)
    if code:
        host = getaddrinfo(host, port, family, kind, protocol)[0][4][0]
        packed = inet_pton(family, host)
    header = family.to_bytes(2, "little") + port.to_bytes(2, "big")
    if family == AF_INET:
        return header + packed + b"\0" * 8
    flow = _index(address[2]) if len(address) > 2 else 0
    scope = _index(address[3]) if len(address) > 3 else 0
    if not 0 <= flow <= 1048575:
        raise OverflowError("flowinfo must be 0-1048575.")
    return header + flow.to_bytes(4, "big") + packed + scope.to_bytes(4, "little")


class socket:
    def __init__(self, family=AF_INET, type=SOCK_STREAM, proto=0, fileno=None):
        family, type, proto = _integer(family), _integer(type), _integer(proto)
        if fileno is None:
            self._lease = _checked(_new(family, type, proto))
        else:
            descriptor = _integer(fileno)
            if descriptor < 0:
                raise ValueError("negative file descriptor")
            self._lease = _adopt(descriptor)
            if family == -1:
                family = self.getsockopt(SOL_SOCKET, SO_DOMAIN)
            if type == -1:
                type = self.getsockopt(SOL_SOCKET, SO_TYPE)
            if proto == -1:
                proto = self.getsockopt(SOL_SOCKET, SO_PROTOCOL)
        self._family, self._type, self._proto = family, type & 15, proto
        self._timeout_value = None
        self.settimeout(_default_timeout)
        if type & SOCK_NONBLOCK and _default_timeout is None:
            self.setblocking(False)

    @property
    def family(self):
        return self._family

    @property
    def type(self):
        return self._type

    @property
    def proto(self):
        return self._proto

    def fileno(self):
        return _fd(self._lease)

    def close(self):
        _close(self._lease)

    def detach(self):
        return _detach(self._lease)

    def gettimeout(self):
        return self._timeout_value

    def settimeout(self, value):
        value = _timeout(value)
        _checked(_nonblocking(self._lease, value is not None))
        self._timeout_value = value

    def setblocking(self, flag):
        self.settimeout(None if flag else 0.0)

    def getblocking(self):
        return self._timeout_value != 0

    def _operation(self, function, writing, *args, deadline=None):
        if deadline is None and self._timeout_value:
            deadline = monotonic() + self._timeout_value
        while True:
            if deadline is not None:
                remaining = deadline - monotonic()
                if remaining <= 0:
                    raise TimeoutError("timed out")
                reads, writes, _ = select(
                    [] if writing else [self], [self] if writing else [], [], remaining
                )
                if not (reads or writes):
                    raise TimeoutError("timed out")
            code, value = function(self._lease, *args)
            if code == 4:
                continue
            if code == 11 and deadline is not None:
                continue
            return _checked((code, value))

    def bind(self, address):
        _checked(
            _bind(
                self._lease,
                _encode_address(self.family, self.type, self.proto, address),
            )
        )

    def connect(self, address):
        raw = _encode_address(self.family, self.type, self.proto, address)
        code, value = _connect(self._lease, raw)
        if code in (4, 114, 115) and self._timeout_value:
            if not select([], [self], [], self._timeout_value)[1]:
                raise TimeoutError("timed out")
            _checked((self.getsockopt(SOL_SOCKET, SO_ERROR), None))
        else:
            _checked((code, value))

    def connect_ex(self, address):
        try:
            self.connect(address)
        except OSError as error:
            return error.args[0] if isinstance(error.args[0], int) else 11
        return 0

    def listen(self, backlog=128):
        _checked(_listen(self._lease, max(0, _integer(backlog))))

    def _accept(self):
        lease, raw = self._operation(_accept, False)
        return _detach(lease), _decode_address(raw)

    def getsockname(self):
        return _decode_address(_checked(_name(self._lease)))

    def getpeername(self):
        return _decode_address(_checked(_peer(self._lease)))

    def shutdown(self, how):
        _checked(_shutdown(self._lease, _integer(how)))

    def send(self, data, flags=0):
        return self._operation(_send, True, _buffer(data), _integer(flags))

    def sendall(self, data, flags=0):
        data = _buffer(data)
        flags = _integer(flags)
        offset = 0
        deadline = monotonic() + self._timeout_value if self._timeout_value else None
        while offset < len(data):
            offset += self._operation(
                _send, True, data[offset:], flags, deadline=deadline
            )

    def recv(self, bufsize, flags=0):
        bufsize = _index(bufsize)
        if bufsize < 0:
            raise ValueError("negative buffersize in recv")
        return self._operation(_receive, False, bufsize, _integer(flags))

    def recv_into(self, buffer, nbytes=0, flags=0):
        view = memoryview(buffer)
        if view.readonly:
            raise TypeError(
                "recv_into() argument 1 must be read-write bytes-like object"
            )
        view = view.cast("B")
        nbytes = _index(nbytes)
        if nbytes == 0:
            nbytes = view.nbytes
        if nbytes < 0:
            raise ValueError("negative buffersize in recv_into")
        if nbytes > view.nbytes:
            raise ValueError("buffer too small for requested bytes")
        data = self.recv(nbytes, flags)
        view[: len(data)] = data
        return len(data)

    def sendto(self, data, *args):
        if len(args) == 1:
            flags, address = 0, args[0]
        elif len(args) == 2:
            flags, address = _integer(args[0]), args[1]
        else:
            raise TypeError("sendto() takes 2 or 3 arguments")
        raw = _encode_address(self.family, self.type, self.proto, address)
        return self._operation(_send_to, True, _buffer(data), flags, raw)

    def recvfrom(self, bufsize, flags=0):
        bufsize = _index(bufsize)
        if bufsize < 0:
            raise ValueError("negative buffersize in recvfrom")
        data, address = self._operation(_receive_from, False, bufsize, _integer(flags))
        return data, _decode_address(address)

    def getsockopt(self, level, optname, buflen=0):
        buflen = _index(buflen)
        if not 0 <= buflen <= 1024:
            raise OSError("getsockopt buflen out of range")
        result = _checked(
            _option(self._lease, _integer(level), _integer(optname), buflen or 4)
        )
        return result if buflen else int.from_bytes(result, "little", signed=True)

    def setsockopt(self, level, optname, value):
        data = (
            _integer(value).to_bytes(4, "little", signed=True)
            if isinstance(value, int)
            else _buffer(value)
        )
        _checked(_set_option(self._lease, _integer(level), _integer(optname), data))

    def get_inheritable(self):
        return os.get_inheritable(self.fileno())

    def set_inheritable(self, value):
        os.set_inheritable(self.fileno(), value)


SocketType = socket


def socketpair(family=AF_UNIX, type=SOCK_STREAM, proto=0):
    family, type, proto = _integer(family), _integer(type), _integer(proto)
    left, right = _checked(_pair(family, type, proto))
    return (
        socket(family, type, proto, _detach(left)),
        socket(family, type, proto, _detach(right)),
    )


def dup(fd):
    result = os.dup(_integer(fd))
    os.set_inheritable(result, False)
    return result


def close(fd):
    os.close(_integer(fd))


def htonl(value):
    return int.from_bytes(_index(value).to_bytes(4, "little"), "big")


def htons(value):
    return int.from_bytes(_index(value).to_bytes(2, "little"), "big")


ntohl = htonl
ntohs = htons
__all__ = [
    name
    for name in globals()
    if not name.startswith("_")
    and name not in ("os", "isfinite", "isnan", "select", "monotonic")
]
