"""Permission changes use real descriptors and directory-relative OS calls."""

from test.mule.compiler.test_exception_groups import compare


def test_chmod_paths_descriptors_directory_relative_and_validation(tmp_path):
    result = compare(
        tmp_path,
        f"""
import os
class Path:
    def __fspath__(self):
        print('path')
        return {str(tmp_path / "permissions")!r}
class Index:
    def __init__(self, value): self.value = value
    def __index__(self): return self.value
path = {str(tmp_path / "permissions")!r}
fd = os.open(path, os.O_CREAT | os.O_RDWR, 0o600)
os.chmod(Path(), Index(0o640))
print(oct(os.stat(path).st_mode & 0o777))
os.fchmod(Index(fd), Index(0o600))
print(oct(os.fstat(fd).st_mode & 0o777))
os.chmod(Index(fd), 0o400)
print(oct(os.fstat(fd).st_mode & 0o777))
directory = os.open({str(tmp_path)!r}, os.O_RDONLY)
os.chmod('permissions', 0o660, dir_fd=Index(directory))
print(oct(os.fstat(fd).st_mode & 0o777))
os.chmod(path, 0o600, follow_symlinks=False)
print(oct(os.fstat(fd).st_mode & 0o777))
for options in ({{'dir_fd': directory}}, {{'follow_symlinks': False}}):
    try:
        os.chmod(fd, 0o600, **options)
    except ValueError as error:
        print(str(error))
os.close(directory)
os.close(fd)
os.unlink(path)
print(os.chmod in os.supports_fd, os.chmod in os.supports_dir_fd)
""",
    )
    assert result.stderr == ""
