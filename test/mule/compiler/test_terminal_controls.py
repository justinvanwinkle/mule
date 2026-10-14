"""Terminal controls use a real PTY and preserve typed ioctl buffer behavior."""

from test.mule.compiler.test_exception_groups import compare


def test_pty_attributes_raw_mode_windows_and_buffers_match_python(tmp_path):
    compare(
        tmp_path,
        """
import fcntl
import os
import pty
import struct
import termios
import tty
master, slave = pty.openpty()
try:
    print(os.isatty(master), os.isatty(slave))
    print(os.get_inheritable(master), os.get_inheritable(slave))
    original = termios.tcgetattr(slave)
    print(len(original), len(original[6]), original[3] & termios.ICANON != 0)
    previous = tty.setraw(slave, termios.TCSANOW)
    current = termios.tcgetattr(slave)
    print(previous == original, current[3] & termios.ICANON == 0)
    print(current[6][termios.VMIN], current[6][termios.VTIME])
    termios.tcsetattr(slave, termios.TCSANOW, original)
    print(termios.tcgetattr(slave) == original)
    print(termios.tcsetwinsize(slave, (37, 101)))
    print(termios.tcgetwinsize(slave), tuple(os.get_terminal_size(slave)))
    immutable = fcntl.ioctl(slave, termios.TIOCGWINSZ, bytes(8))
    print(struct.unpack('HHHH', immutable))
    mutable = bytearray(8)
    print(fcntl.ioctl(slave, termios.TIOCGWINSZ, mutable))
    print(struct.unpack('HHHH', mutable))
    unchanged = bytearray(8)
    output = fcntl.ioctl(slave, termios.TIOCGWINSZ, unchanged, False)
    print(unchanged == bytearray(8), output == immutable)
    storage = bytearray(8)
    view = memoryview(storage).cast('H')
    print(fcntl.ioctl(slave, termios.TIOCGWINSZ, view), struct.unpack('HHHH', storage))
    view.release()
    print(termios.tcflush(slave, termios.TCIOFLUSH), termios.tcdrain(slave))
    print(termios.tcflow(slave, termios.TCOOFF), termios.tcflow(slave, termios.TCOON))
    print(termios.tcsendbreak(slave, 0))
finally:
    os.close(master)
    os.close(slave)
read, write = os.pipe()
try:
    termios.tcgetattr(read)
except termios.error as error:
    print(error.args, str(error), isinstance(error, OSError))
finally:
    os.close(read)
    os.close(write)
""",
    )
