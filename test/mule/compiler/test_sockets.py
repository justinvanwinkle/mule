"""Real local socket operations compared with Python 3.14."""

import pytest

from test.mule.compiler.test_exception_groups import compare


@pytest.mark.integration
def test_socketpair_buffers_modes_ownership_and_timeout(tmp_path):
    result = compare(
        tmp_path,
        """
import socket
import os
left, right = socket.socketpair()
print(left.family == socket.AF_UNIX, left.type == socket.SOCK_STREAM)
print(left.gettimeout(), left.getblocking(), left.get_inheritable())
left.sendall(memoryview(b'hello'))
target = bytearray(8)
print(right.recv_into(target, 5), target)
right.send(b'bye')
print(left.recv(3, socket.MSG_PEEK), left.recv(3))
copy = left.dup()
left.close()
print(left.fileno(), copy.getblocking(), copy.get_inheritable())
copy.sendall(b'copy')
print(right.recv(4))
right.setblocking(False)
print(right.gettimeout(), right.getblocking())
try:
    right.recv(1)
except BlockingIOError:
    print('would block')
right.settimeout(0.01)
try:
    right.recv(1)
except TimeoutError as error:
    print(str(error))
copy.shutdown(socket.SHUT_WR)
print(right.recv(1))
descriptor = copy.detach()
print(copy.fileno(), os.get_inheritable(descriptor))
os.close(descriptor)
right.close()
for value in (-1, float('nan'), float('inf')):
    try:
        socket.setdefaulttimeout(value)
    except (ValueError, OverflowError) as error:
        print(type(error).__name__, str(error))
print(socket.inet_pton(socket.AF_INET, '127.0.0.1'))
print(socket.inet_ntop(socket.AF_INET6, bytes(15) + b'\\1'))
print(socket.htons(1234), socket.htonl(1234))
""",
    )
    assert result.stderr == ""


@pytest.mark.integration
def test_loopback_stream_datagram_and_numeric_resolution(tmp_path):
    result = compare(
        tmp_path,
        """
import socket
with socket.socket() as listener:
    listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    print(listener.getsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR))
    listener.bind(('127.0.0.1', 0))
    listener.listen(1)
    with socket.socket() as client:
        client.settimeout(1)
        client.connect(listener.getsockname())
        with listener.accept()[0] as accepted:
            print(client.getpeername() == listener.getsockname())
            print(accepted.getpeername() == client.getsockname())
            client.sendall(b'hello')
            print(accepted.recv(5))
with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as server:
    server.bind(('127.0.0.1', 0))
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as client:
        print(client.sendto(b'data', server.getsockname()))
        data, source = server.recvfrom(16)
        print(data, source == client.getsockname())
print(socket.getaddrinfo('127.0.0.1', 80, socket.AF_INET,
                        socket.SOCK_STREAM, flags=socket.AI_NUMERICHOST))
""",
    )
    assert result.stderr == ""
