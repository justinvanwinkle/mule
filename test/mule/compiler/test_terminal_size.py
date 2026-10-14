"""Terminal tuple fields and the Linux terminal query match Python 3.14."""

import fcntl
import os
import pty
import struct
import subprocess
import termios

from mule.compiler import build
from test.mule.compiler.test_exception_groups import compare
from test.python_host import python_host


def test_terminal_size_tuple_contract(tmp_path):
    compare(
        tmp_path,
        """
import os
size = os.terminal_size([101, 37])
print(size, tuple(size), size.columns, size.lines)
print(size.n_fields, size.n_sequence_fields, size.n_unnamed_fields)
print(isinstance(size, tuple), size == (101, 37))
print(os.terminal_size(iter([5, 6])))
print(os.terminal_size([5, 6], {}))
for values in ([], [1], [1, 2, 3]):
    try:
        os.terminal_size(values)
    except TypeError as error:
        print(str(error))
try:
    size.columns = 5
except AttributeError:
    print('readonly')
for fd in (-1, 1 << 80):
    try:
        os.get_terminal_size(fd)
    except (OSError, OverflowError) as error:
        print(type(error).__name__)
""",
    )


def test_terminal_query_reads_pty_dimensions(tmp_path):
    path = tmp_path / "terminal.py"
    path.write_text("""
import os
import sys
class Descriptor:
    def __index__(self):
        return int(sys.argv[1])
class Integer(int):
    def __index__(self):
        return -1
size = os.get_terminal_size(Descriptor())
print(size, size.columns, size.lines)
print(os.isatty(Descriptor()), os.isatty(Integer(int(sys.argv[1]))))
for fd in (-1, True, 1 << 80, -(1 << 80), 1.5, '1', object()):
    try:
        print('isatty', os.isatty(fd))
    except (TypeError, OverflowError) as error:
        print(type(error).__name__)
read, write = os.pipe()
print('pipe', os.isatty(read), os.isatty(write))
try:
    os.get_terminal_size(read)
except OSError:
    print('pipe has no terminal dimensions')
finally:
    os.close(read)
    os.close(write)
print('closed', os.isatty(read))
""")
    executable = build(path, tmp_path / "program", optimize=False, workers=1)
    master, slave = pty.openpty()
    try:
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 37, 101, 0, 0))
        results = [
            subprocess.run(
                [*command, str(slave)],
                capture_output=True,
                text=True,
                check=False,
                pass_fds=(slave,),
                timeout=10,
            )
            for command in ([python_host(), str(path)], [str(executable)])
        ]
    finally:
        os.close(slave)
        os.close(master)
    for result in results:
        assert result.returncode == 0, result.stderr
    assert results[1].stdout == results[0].stdout
