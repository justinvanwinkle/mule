"""Timestamp updates and their argument protocols match Python 3.14."""

from test.mule.compiler.test_exception_groups import compare


def test_timestamp_paths_descriptors_directories_symlinks_and_now(tmp_path):
    compare(
        tmp_path,
        f"directory = {str(tmp_path)!r}\n"
        + """
import os
import time
path = directory + '/timestamp-data'
fd = os.open(path, os.O_CREAT | os.O_RDWR, 0o600)
class Path:
    def __fspath__(self): return path
class Descriptor:
    def __index__(self): return fd
for target in (path, path.encode(), Path(), fd, Descriptor()):
    for stamps in ((1234567890123456789, 1234567890987654321), (-100000001, 100000001)):
        print(os.utime(target, ns=stamps))
        info = os.fstat(fd)
        print(info.st_atime_ns, info.st_mtime_ns)
for stamps in ((1, 2), (1.125, -0.125), (1.0000000019, -1.0000000019)):
    os.utime(Path(), stamps)
    info = os.fstat(fd)
    print(info.st_atime_ns, info.st_mtime_ns)
directory_fd = os.open(directory, os.O_RDONLY)
os.utime('timestamp-data', ns=(11, 22), dir_fd=directory_fd)
print(os.fstat(fd).st_mtime_ns)
link = directory + '/timestamp-link'
os.symlink(path, link)
os.utime(link, ns=(33, 44), follow_symlinks=False)
print(os.lstat(link).st_atime_ns, os.lstat(link).st_mtime_ns, os.fstat(fd).st_mtime_ns)
os.utime(fd)
after = time.time_ns()
info = os.fstat(fd)
# Filesystem "now" can use a coarser clock than clock_gettime.
print(info.st_atime_ns == info.st_mtime_ns,
      abs(info.st_mtime_ns - after) < 1_000_000_000)
print(os.utime in os.supports_fd, os.utime in os.supports_dir_fd,
      os.utime in os.supports_follow_symlinks)
with os.scandir(directory) as entries:
    print(all(isinstance(entry, os.DirEntry) for entry in entries))
os.close(directory_fd)
os.close(fd)
os.unlink(link)
os.unlink(path)
""",
    )


def test_timestamp_argument_errors_numeric_protocols_and_os_errors(tmp_path):
    compare(
        tmp_path,
        f"path = {str(tmp_path / 'timestamp-data')!r}\n"
        + """
import os
fd = os.open(path, os.O_CREAT | os.O_RDWR, 0o600)
class Index:
    def __index__(self): return 2
class FloatOnly:
    def __float__(self): raise AssertionError('float override')
class Nanos:
    def __divmod__(self, divisor): return (3, 4)
class Result(tuple):
    def __len__(self): raise AssertionError('length override')
    def __getitem__(self, index): raise AssertionError('index override')
class SubclassNanos:
    def __divmod__(self, divisor): return Result((3, 4))
class BadNanos:
    def __divmod__(self, divisor): return [3, 4]
class Number(float):
    def __float__(self): raise AssertionError('float override')
    def __int__(self): raise AssertionError('int override')
    def __floor__(self): raise AssertionError('floor override')
class Times(tuple): pass
for options in ({'times': (Index(), True)}, {'times': (Number(1.5), 2)},
                {'ns': (Nanos(), 2)}, {'ns': (SubclassNanos(), 2)}):
    print(os.utime(path, **options), os.stat(path).st_atime_ns)
for options in ({'times': [1, 2]}, {'ns': [1, 2]}, {'times': (1,)},
                {'times': Times((1, 2))}, {'ns': Times((1, 2))},
                {'ns': None}, {'times': (1, 2), 'ns': (3, 4)},
                {'times': ('x', 2)}, {'times': (FloatOnly(), 2)},
                {'times': (float('nan'), 2)}, {'times': (float('inf'), 2)},
                {'times': (1 << 100, 2)}, {'ns': (1 << 100, 2)},
                {'ns': (1.5, 2)}, {'ns': ('x', 2)}, {'ns': (Index(), 2)},
                {'ns': (BadNanos(), 2)}):
    try:
        os.utime(path, **options)
    except (TypeError, ValueError, OverflowError) as error:
        print(type(error).__name__, str(error))
for options in ({'dir_fd': fd}, {'follow_symlinks': False}):
    try:
        os.utime(fd, **options)
    except ValueError as error:
        print(type(error).__name__, str(error))
print(os.utime(fd, dir_fd=-100))
try:
    os.utime(-1, follow_symlinks=False)
except OSError as error:
    print(error.errno, error.filename)
for target in (path + '.missing', (path + '.missing').encode(), -1):
    try:
        os.utime(target)
    except OSError as error:
        print(type(error).__name__, error.errno, error.filename, str(error))
os.close(fd)
os.unlink(path)
""",
    )
