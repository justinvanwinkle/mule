"""Descriptor metadata errors are Python exceptions and preserve descriptor ownership."""

from test.mule.compiler.test_math_native import assert_matches_python


def test_fstat_descriptor_errors_fields_and_lifetime_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        """
import posix as os
class Index:
    def __init__(self, value): self.value = value
    def __index__(self): return self.value
class Integer(int):
    def __index__(self): raise AssertionError('index override')
    def __int__(self): raise AssertionError('integer override')
for fd in (-1, -2, -(1 << 31), 999999, Index(-1), Integer(-1)):
    try:
        os.fstat(fd)
    except OSError as error:
        print(type(error).__name__, error.errno, error.args, str(error))
fd = os.open('data', os.O_RDWR | os.O_CREAT | os.O_TRUNC, 0o600)
os.write(fd, b'abcdef')
os.lseek(fd, 2, 0)
for descriptor in (fd, Index(fd), Integer(fd)):
    by_fd = os.fstat(descriptor)
    by_path = os.stat('data')
    for name in ('st_mode', 'st_ino', 'st_dev', 'st_nlink', 'st_uid', 'st_gid',
                 'st_size', 'st_atime_ns', 'st_mtime_ns', 'st_ctime_ns'):
        print(getattr(by_fd, name) == getattr(by_path, name))
    print(os.lseek(fd, 0, 1) == 2)
print(os.read(fd, 2))
os.close(fd)
try:
    os.fstat(fd)
except OSError as error:
    print(error.errno, str(error))
replacement = os.open('data', os.O_RDONLY)
print(os.fstat(replacement).st_size)
os.close(replacement)
""",
    )
