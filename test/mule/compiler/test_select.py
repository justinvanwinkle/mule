"""Real descriptor multiplexing compared with Python 3.14."""

import pytest

from test.mule.compiler.test_exception_groups import compare


def test_pipe_readiness_registration_original_objects_and_errors(tmp_path):
    compare(
        tmp_path,
        """
import os
import select
class File:
    def __init__(self, fd):
        self.fd = fd
    def fileno(self):
        return self.fd
class Invalid:
    def fileno(self):
        return 'bad'
read, write = os.pipe()
obj = File(read)
poller = select.poll()
poller.register(obj, select.POLLIN)
poller.register(write, select.POLLOUT)
print('constants', select.PIPE_BUF, select.POLLIN, select.POLLOUT, select.POLLHUP)
print('initial', poller.poll(0) == [(write, select.POLLOUT)])
print('select', select.select([obj], [write, write], [], 0) == ([], [write, write], []))
os.write(write, b'x')
ready = select.select([obj, read, obj], [write], [], 0.0001)
print('identity', ready[0][0] is obj, ready[0][1] == read, ready[0][2] is obj)
print('both', sorted(poller.poll(0)) == sorted([(read, select.POLLIN), (write, select.POLLOUT)]))
poller.modify(read, 0)
print('modified', poller.poll(-0.1) == [(write, select.POLLOUT)])
poller.register(read, select.POLLIN)
print('replacement', len(poller.poll(0)))
poller.unregister(obj)
for operation in (lambda: poller.unregister(read), lambda: poller.modify(read, 1)):
    try:
        operation()
    except (KeyError, OSError) as error:
        print(type(error).__name__)
for value in (-1, 1 << 80, 1.5, object(), Invalid()):
    for operation in (lambda: poller.register(value), lambda: select.select([value], [], [], 0)):
        try:
            operation()
        except (ValueError, OverflowError, TypeError) as error:
            print('fd', type(error).__name__)
for mask in (-1, 1 << 16, 1.5):
    try:
        poller.register(read, mask)
    except (TypeError, ValueError, OverflowError) as error:
        print('mask', type(error).__name__)
for timeout in ('1', float('nan'), float('inf'), 1 << 80):
    for operation in (lambda: poller.poll(timeout), lambda: select.select([], [write], [], timeout)):
        try:
            operation()
        except (TypeError, ValueError, OverflowError) as error:
            print('timeout', type(error).__name__)
try:
    select.select([], [], [], -0.1)
except ValueError:
    print('negative select')
try:
    select.select([1024], [], [], 0)
except ValueError:
    print('fd set bound')
os.close(write)
print('hangup', select.select([read], [], [], 0)[0] == [read])
poller.register(read, select.POLLIN)
print('poll hangup', poller.poll(0) == [(write, select.POLLNVAL), (read, select.POLLIN | select.POLLHUP)] or
      sorted(poller.poll(0)) == sorted([(write, select.POLLNVAL), (read, select.POLLIN | select.POLLHUP)]))
os.close(read)
try:
    select.select([read], [], [], 0)
except OSError:
    print('closed select')
""",
    )


@pytest.mark.integration
def test_default_selector_drives_real_pipe(tmp_path):
    compare(
        tmp_path,
        """
import os
import selectors
read, write = os.pipe()
with selectors.DefaultSelector() as selector:
    key = selector.register(read, selectors.EVENT_READ, 'payload')
    print(selector.select(0))
    os.write(write, b'hello')
    ready = selector.select(0.01)
    print(len(ready), ready[0][0] is key, ready[0][0].data, ready[0][1])
    print(os.read(ready[0][0].fd, 5))
    changed = selector.modify(read, selectors.EVENT_READ, 'changed')
    print(changed.data, selector.get_key(read) is changed)
    print(selector.unregister(read) is changed)
os.close(read)
os.close(write)
""",
    )
