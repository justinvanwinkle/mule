"""Filesystem wrappers select PathLike methods before native OS calls."""

from test.mule.compiler.test_exception_groups import compare


def test_posix_and_os_share_fspath_with_descriptor_binding(tmp_path):
    compare(
        tmp_path,
        """
import os
import posix
print(os.fspath is posix.fspath)
class Path:
    def __fspath__(self):
        return b'path'
class DescriptorPath:
    @property
    def __fspath__(self):
        return lambda: 'bound'
class MissingDescriptor:
    @property
    def __fspath__(self):
        raise AttributeError('missing binding')
class Invalid:
    def __fspath__(self):
        return 7
class Broken:
    def __fspath__(self):
        raise AttributeError('inside method')
class Disabled:
    __fspath__ = None
for value in ('text', b'bytes', Path(), DescriptorPath(), MissingDescriptor(), Invalid(), Broken(), Disabled(), object()):
    try:
        print(repr(posix.fspath(value)))
    except (TypeError, AttributeError) as error:
        print(type(error).__name__, str(error))
""",
    )


def test_path_objects_are_converted_once_and_preserve_bytes_results(tmp_path):
    compare(
        tmp_path,
        f"""
import os
class Path:
    def __init__(self, value):
        self.value = value
    def __fspath__(self):
        print('path', type(self.value).__name__)
        return self.value
class Invalid:
    def __fspath__(self):
        return 7
class Broken:
    def __fspath__(self):
        raise AttributeError('inside fspath')
class Index:
    def __init__(self, fd):
        self.fd = fd
    def __index__(self):
        return self.fd
root = {str(tmp_path / "fixture")!r}
os.mkdir(Path(root))
path = root + '/file'
fd = os.open(Path(path), os.O_CREAT | os.O_WRONLY, 0o600)
os.write(fd, b'hello')
print('fd stat', os.stat(Index(fd)).st_size, os.stat(fd).st_size)
os.close(fd)
print('stat', os.stat(Path(path)).st_size, os.lstat(Path(path)).st_size)
print('access', os.access(Path(path), os.R_OK))
print('names', os.listdir(Path(root)), os.listdir(Path(root.encode())))
for entry in os.scandir(Path(root.encode())):
    print('entry', entry.name, entry.path)
link = root + '/link'
os.symlink(Path('file'), Path(link))
print('link', os.readlink(Path(link)), os.readlink(Path(link.encode())))
renamed = root + '/renamed'
os.rename(Path(path), Path(renamed))
os.unlink(Path(renamed))
os.unlink(Path(link))
os.rmdir(Path(root))
for value in (Invalid(), Broken(), object()):
    try:
        os.stat(value)
    except (TypeError, AttributeError) as error:
        print(type(error).__name__, str(error))
""",
    )
