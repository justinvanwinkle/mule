"""POSIX truncation changes file size while preserving seek offsets."""

from test.mule.compiler.test_exception_groups import compare


def test_descriptor_truncation_extends_with_zeroes_and_checks_arguments(tmp_path):
    compare(
        tmp_path,
        f"""
import os
class Index:
    def __init__(self, value):
        self.value = value
    def __index__(self):
        return self.value
path = {str(tmp_path / "file")!r}
fd = os.open(path, os.O_CREAT | os.O_TRUNC | os.O_RDWR, 0o600)
os.write(fd, b'abcdef')
print(os.ftruncate(Index(fd), Index(3)), os.fstat(fd).st_size, os.lseek(fd, 0, os.SEEK_CUR))
os.lseek(fd, 0, os.SEEK_SET)
print(os.read(fd, 10))
os.ftruncate(fd, 8)
os.lseek(fd, 0, os.SEEK_SET)
print(os.read(fd, 10))
os.lseek(fd, 10, os.SEEK_SET)
os.ftruncate(fd, 4)
print(os.lseek(fd, 0, os.SEEK_CUR), os.fstat(fd).st_size)
for descriptor, length in ((fd, -1), (-1, 0), (fd, 1 << 80), (1 << 80, 0), (fd, 1.5)):
    try:
        os.ftruncate(descriptor, length)
    except (OSError, OverflowError, TypeError) as error:
        print(type(error).__name__)
os.close(fd)
os.unlink(path)
""",
    )
