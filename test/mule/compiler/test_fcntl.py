"""Descriptor flags and advisory locking use real Linux kernel operations."""

from test.mule.compiler.test_exception_groups import compare


def test_descriptor_controls_and_locking_match_python(tmp_path):
    compare(
        tmp_path,
        """
import fcntl
import os
import tempfile
with tempfile.TemporaryFile() as file:
    fd = file.fileno()
    print(fcntl.fcntl(file, fcntl.F_GETFD) & fcntl.FD_CLOEXEC)
    fcntl.fcntl(fd, fcntl.F_SETFD, 0)
    print(fcntl.fcntl(fd, fcntl.F_GETFD) & fcntl.FD_CLOEXEC)
    fcntl.fcntl(fd, fcntl.F_SETFD, fcntl.FD_CLOEXEC)
    duplicate = fcntl.fcntl(fd, fcntl.F_DUPFD_CLOEXEC, 0)
    print(duplicate != fd, fcntl.fcntl(duplicate, fcntl.F_GETFD) & fcntl.FD_CLOEXEC)
    os.close(duplicate)
    print(fcntl.flock(file, fcntl.LOCK_EX | fcntl.LOCK_NB))
    print(fcntl.flock(fd, fcntl.LOCK_UN))
    print(fcntl.lockf(fd, fcntl.LOCK_EX | fcntl.LOCK_NB, 10, 2))
    print(fcntl.lockf(fd, fcntl.LOCK_UN, 10, 2))
    for value in (-1, 2 ** 40, object()):
        try:
            fcntl.flock(value, fcntl.LOCK_UN)
        except (ValueError, TypeError, OverflowError) as error:
            print(type(error).__name__, str(error))
    try:
        fcntl.fcntl(fd, fcntl.F_GETLK, b'x' * 1025)
    except ValueError as error:
        print(str(error))
    try:
        fcntl.fcntl(fd, -1)
    except OSError as error:
        print(type(error).__name__, error.errno, error.strerror, str(error))
with tempfile.TemporaryDirectory() as directory:
    with open(directory + '/lock', 'w+b') as first:
        with open(directory + '/lock', 'r+b') as second:
            fcntl.flock(first, fcntl.LOCK_EX)
            try:
                fcntl.flock(second, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except BlockingIOError as error:
                print('contended', error.errno)
            fcntl.flock(first, fcntl.LOCK_UN)
            print(fcntl.flock(second, fcntl.LOCK_EX | fcntl.LOCK_NB))
""",
    )


def test_os_errors_expose_errno_selection_and_filename_messages(tmp_path):
    compare(
        tmp_path,
        """
for arguments in ((), ('message',), (9, 'bad'), (2, 'no', 'file'),
                  (2, 'no', None), (2, 'no', 'one', None, 'two'),
                  (2, 'no', 'one', None, 'two', 'six'), (None, 'missing'),
                  (11, 'busy', 3), (13, 'denied'), (110, 'late')):
    error = OSError(*arguments)
    print(type(error).__name__, error.args, error.errno, error.strerror,
          error.filename, error.filename2, str(error),
          getattr(error, 'characters_written', None))
class Custom(OSError):
    pass
print(type(Custom(2, 'no')).__name__)
""",
    )
