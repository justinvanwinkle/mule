"""POSIX operating-system routines, as the ``posix`` builtin module.

Operating-system services come from the ``_posix`` native module as bytes
and rows; the Python-level types (``stat_result``, ``uname_result``) and
argument handling live here, following CPython's ``Modules/posixmodule.c``.
"""

from _posix import access as _access
from _posix import chdir as _chdir
from _posix import chmod_at as _chmod_at
from _posix import close as _close
from _posix import cwd as _cwd
from _posix import dup as _dup
from _posix import dup2 as _dup2
from _posix import environ_items as _environ_items
from _posix import entropy_bytes as _entropy_bytes
from _posix import fstat as _fstat
from _posix import fchmod as _fchmod
from _posix import fsync as _fsync
from _posix import ftruncate as _ftruncate
from _posix import getegid as _getegid
from _posix import geteuid as _geteuid
from _posix import getgid as _getgid
from _posix import getpid as _getpid
from _posix import getppid as _getppid
from _posix import getuid as _getuid
from _posix import kill as _kill
from _posix import link as _link
from _posix import listdir as _listdir
from _posix import lseek as _lseek
from _posix import mkdir as _mkdir
from _posix import open_fd as _open_fd
from _posix import openpty as _openpty
from _posix import pipe as _pipe
from _posix import read as _read
from _posix import readlink as _readlink
from _posix import rename as _rename
from _posix import rmdir as _rmdir
from _posix import setenv_bytes as _setenv_bytes
from _posix import stat as _stat
from _posix import strerror_bytes as _strerror_bytes
from _posix import symlink as _symlink
from _posix import sysconf_value as _sysconf_value
from _posix import terminal_size as _terminal_dimensions
from _posix import umask as _umask
from _posix import uname_items as _uname_items
from _posix import unlink as _unlink
from _posix import unsetenv_bytes as _unsetenv_bytes
from _posix import write as _write
from _mule_native import waitpid as _waitpid
from _mule_native import get_inheritable as _get_inheritable
from _mule_native import set_inheritable as _set_inheritable
from _mule_native import isatty as _isatty

__all__ = [
    "F_OK", "R_OK", "W_OK", "X_OK", "O_RDONLY", "O_WRONLY", "O_RDWR",
    "O_APPEND", "O_CREAT", "O_EXCL", "O_TRUNC", "SEEK_SET", "SEEK_CUR", "openpty",
    "SEEK_END", "_exit", "access", "chdir", "close", "dup", "dup2",
    "cpu_count", "environ", "fstat", "fsync", "getcwd", "getegid", "geteuid", "getgid", "chmod", "fchmod",
    "getpid", "getppid", "getuid", "kill", "link", "listdir", "lseek",
    "lstat", "mkdir", "open", "pipe", "putenv", "read", "readlink", "scandir",
    "remove", "rename", "rmdir", "stat", "stat_result", "strerror",
    "symlink", "sysconf", "sysconf_names", "umask", "uname",
    "uname_result", "unsetenv", "unlink", "urandom", "write", "terminal_size", "get_terminal_size",
    "waitpid", "waitstatus_to_exitcode", "WNOHANG", "WUNTRACED", "WCONTINUED",
    "WIFEXITED", "WIFSIGNALED", "WIFSTOPPED", "WIFCONTINUED", "WEXITSTATUS",
    "WTERMSIG", "WSTOPSIG", "WCOREDUMP", "get_inheritable", "set_inheritable", "isatty", "readinto", "ftruncate", "fspath",
]

F_OK = 0
R_OK = 4
W_OK = 2
X_OK = 1

O_RDONLY = 0
O_WRONLY = 1
O_RDWR = 2
O_APPEND = 1024
O_CREAT = 64
O_EXCL = 128
O_TRUNC = 512

SEEK_SET = 0
SEEK_CUR = 1
SEEK_END = 2

WNOHANG = 1
WUNTRACED = 2
WCONTINUED = 8


def _native_fd(value):
    from math.integer import _index
    value = _index(value)
    if value < -(1 << 31) or value >= (1 << 31):
        raise OverflowError("Python int too large to convert to C int")
    return value


def fspath(path):
    from _mule_native import class_lookup_default, bind_attribute
    if isinstance(path, (str, bytes)):
        return path
    owner = type(path)
    definition = class_lookup_default(owner, '__fspath__', None)
    try:
        method = bind_attribute(definition, path, owner) if definition is not None else None
    except AttributeError:
        method = None
    if method is None:
        raise TypeError('expected str, bytes or os.PathLike object, not ' + type(path).__name__)
    result = method()
    if not isinstance(result, (str, bytes)):
        raise TypeError('expected ' + type(path).__name__ + '.__fspath__() to return str or bytes, not ' + type(result).__name__)
    return result


def _path(value, operation, descriptors=False, nullable=False):
    if value is None and nullable:
        return '.'
    if not isinstance(value, (str, bytes)) and getattr(type(value), '__fspath__', None) is None:
        expected = 'string, bytes, os.PathLike'
        if descriptors:
            expected += ', integer or None' if nullable else ' or integer'
        else:
            expected = 'string, bytes or os.PathLike'
        raise TypeError(operation + ': path should be ' + expected + ', not ' + type(value).__name__)
    return fspath(value)


def waitpid(pid, options, /):
    return _waitpid(_native_fd(pid), _native_fd(options))


def get_inheritable(fd, /):
    return _get_inheritable(_native_fd(fd))


def isatty(fd, /):
    return _isatty(_native_fd(fd))


def set_inheritable(fd, inheritable, /):
    _set_inheritable(_native_fd(fd), bool(inheritable))


def WIFEXITED(status):
    return (_native_fd(status) & 127) == 0


def WIFSIGNALED(status):
    return 0 < (_native_fd(status) & 127) < 127


def WIFSTOPPED(status):
    return (_native_fd(status) & 255) == 127


def WIFCONTINUED(status):
    return _native_fd(status) == 65535


def WEXITSTATUS(status):
    return (_native_fd(status) >> 8) & 255


def WTERMSIG(status):
    return _native_fd(status) & 127


def WSTOPSIG(status):
    return (_native_fd(status) >> 8) & 255


def WCOREDUMP(status):
    return bool(_native_fd(status) & 128)


def waitstatus_to_exitcode(status):
    status = _native_fd(status)
    if WIFEXITED(status):
        return WEXITSTATUS(status)
    if WIFSIGNALED(status):
        return -WTERMSIG(status)
    if WIFSTOPPED(status):
        raise ValueError(f"process stopped by delivery of signal {WSTOPSIG(status)}")
    raise ValueError(f"invalid wait status: {status}")


class terminal_size(tuple):
    __slots__ = ()
    n_fields = 2
    n_sequence_fields = 2
    n_unnamed_fields = 0

    def __new__(cls, sequence, fields=None):
        values = list(sequence)
        if len(values) != 2:
            raise TypeError(f"os.terminal_size() takes a 2-sequence ({len(values)}-sequence given)")
        if fields is not None:
            if not isinstance(fields, dict):
                raise TypeError("os.terminal_size() takes a dict as second arg, if any")
            if fields:
                raise TypeError("os.terminal_size() got duplicate or unexpected field name(s)")
        return tuple.__new__(cls, values)

    @property
    def columns(self):
        return self[0]

    @property
    def lines(self):
        return self[1]

    def __repr__(self):
        return f"os.terminal_size(columns={self.columns!r}, lines={self.lines!r})"


terminal_size.__module__ = "os"


def get_terminal_size(fd=1):
    from math.integer import _index
    descriptor = _index(fd)
    if descriptor < -(1 << 31) or descriptor >= (1 << 31):
        raise OverflowError("Python int too large to convert to C int")
    return terminal_size(_terminal_dimensions(descriptor))


def _bytes(value):
    if isinstance(value, bytes):
        return value
    if isinstance(value, str):
        return value.encode("utf-8", "surrogateescape")
    raise TypeError("expected str or bytes, not " + type(value).__name__)


def _create_environ():
    result = {}
    for key, value in _environ_items():
        result[key] = value
    return result


environ = _create_environ()
_have_functions = {"HAVE_FACCESSAT", "HAVE_LSTAT", "HAVE_FSTATAT", "HAVE_OPENAT", "HAVE_FCHMOD", "HAVE_FCHMODAT"}


def putenv(name, value):
    key = _bytes(name)
    item = _bytes(value)
    _setenv_bytes(key, item)


def unsetenv(name):
    key = _bytes(name)
    _unsetenv_bytes(key)


def urandom(size, /):
    from math.integer import _index
    size = _index(size)
    if size < 0:
        raise ValueError("negative argument not allowed")
    return _entropy_bytes(size)


class stat_result:
    """The result of stat: a tuple-like row with named fields."""

    def __init__(self, row):
        self._row = (
            row[0], row[1], row[2], row[3], row[4], row[5], row[6],
            row[7] / 1000000000, row[8] / 1000000000, row[9] / 1000000000,
        )
        self._ns = (row[7], row[8], row[9])

    def __getitem__(self, index):
        return self._row[index]

    def __len__(self):
        return 10

    def __iter__(self):
        return iter(self._row)

    def __eq__(self, other):
        if isinstance(other, stat_result):
            return self._row == other._row
        if isinstance(other, tuple):
            return self._row == other
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        return result if result is NotImplemented else not result

    def __repr__(self):
        return "os.stat_result" + repr(self._row)

    @property
    def st_mode(self):
        return self._row[0]

    @property
    def st_ino(self):
        return self._row[1]

    @property
    def st_dev(self):
        return self._row[2]

    @property
    def st_nlink(self):
        return self._row[3]

    @property
    def st_uid(self):
        return self._row[4]

    @property
    def st_gid(self):
        return self._row[5]

    @property
    def st_size(self):
        return self._row[6]

    @property
    def st_atime(self):
        return self._row[7]

    @property
    def st_mtime(self):
        return self._row[8]

    @property
    def st_ctime(self):
        return self._row[9]

    @property
    def st_atime_ns(self):
        return self._ns[0]

    @property
    def st_mtime_ns(self):
        return self._ns[1]

    @property
    def st_ctime_ns(self):
        return self._ns[2]


class uname_result:
    """The result of uname: a tuple-like row with named fields."""

    def __init__(self, row):
        self._row = tuple(row)

    def __getitem__(self, index):
        return self._row[index]

    def __len__(self):
        return 5

    def __iter__(self):
        return iter(self._row)

    def __eq__(self, other):
        if isinstance(other, uname_result):
            return self._row == other._row
        if isinstance(other, tuple):
            return self._row == other
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        return result if result is NotImplemented else not result

    def __repr__(self):
        return "posix.uname_result" + repr(self._row)

    @property
    def sysname(self):
        return self._row[0]

    @property
    def nodename(self):
        return self._row[1]

    @property
    def release(self):
        return self._row[2]

    @property
    def version(self):
        return self._row[3]

    @property
    def machine(self):
        return self._row[4]


def getcwd():
    return _cwd()


def chdir(path):
    return _chdir(_path(path, 'chdir', descriptors=True))


def listdir(path="."):
    from os import fsencode
    path = _path(path, 'listdir', descriptors=True, nullable=True)
    names = _listdir(path)
    return [fsencode(name) for name in names] if isinstance(path, bytes) else list(names)


class DirEntry:
    """An entry from scandir: a name and its directory's path."""

    def __init__(self, name, path):
        self._name = name
        self._path = path

    @property
    def name(self):
        return self._name

    @property
    def path(self):
        return self._path

    def __fspath__(self):
        return self._path

    def __repr__(self):
        return "<DirEntry '" + self._name + "'>"

    def is_dir(self, *, follow_symlinks=True):
        info = stat(self._path) if follow_symlinks else lstat(self._path)
        return (info.st_mode & 0o170000) == 0o040000

    def is_file(self, *, follow_symlinks=True):
        info = stat(self._path) if follow_symlinks else lstat(self._path)
        return (info.st_mode & 0o170000) == 0o100000

    def is_symlink(self):
        return (lstat(self._path).st_mode & 0o170000) == 0o120000

    def is_junction(self):
        return False

    def stat(self, *, follow_symlinks=True):
        return stat(self._path) if follow_symlinks else lstat(self._path)

    def inode(self):
        return lstat(self._path).st_ino


class _ScandirIterator:
    def __init__(self, entries):
        self._entries = entries
        self._index = 0

    def __iter__(self):
        return self

    def __next__(self):
        if self._index >= len(self._entries):
            raise StopIteration
        entry = self._entries[self._index]
        self._index += 1
        return entry

    def __enter__(self):
        return self

    def __exit__(self, kind, value, traceback):
        self.close()
        return False

    def close(self):
        pass


def scandir(path="."):
    path = _path(path, 'scandir', descriptors=True, nullable=True)
    entries = []
    separator = b'/' if isinstance(path, bytes) else '/'
    for name in listdir(path):
        joined = name if not path else (path + name if path.endswith(separator) else path + separator + name)
        entries.append(DirEntry(name, joined))
    return _ScandirIterator(entries)


def stat(path, *, dir_fd=None, follow_symlinks=True):
    if dir_fd is not None:
        raise NotImplementedError("stat with dir_fd is not supported yet")
    if not isinstance(path, (str, bytes)) and hasattr(type(path), '__index__'):
        if not follow_symlinks:
            raise ValueError('stat: cannot use fd and follow_symlinks together')
        return fstat(_native_fd(path))
    return stat_result(_stat(_path(path, 'stat', descriptors=True), follow_symlinks))


def lstat(path, *, dir_fd=None):
    if dir_fd is not None:
        raise NotImplementedError("lstat with dir_fd is not supported yet")
    return stat_result(_stat(_path(path, 'lstat'), False))


def fstat(fd):
    return stat_result(_fstat(_native_fd(fd)))


def fchmod(fd, mode):
    return _fchmod(_native_fd(fd), _native_fd(mode))


def chmod(path, mode, *, dir_fd=None, follow_symlinks=True):
    mode = _native_fd(mode)
    directory = -100 if dir_fd is None else _native_fd(dir_fd)
    if not isinstance(path, (str, bytes)) and hasattr(type(path), "__index__"):
        return fchmod(path, mode)
    return _chmod_at(_path(path, "chmod", descriptors=True), mode, directory, follow_symlinks)


def mkdir(path, mode=0o777, *, dir_fd=None):
    if dir_fd is not None:
        raise NotImplementedError("mkdir with dir_fd is not supported yet")
    parsed = -mode if False else mode
    return _mkdir(_path(path, 'mkdir'), parsed)


def rmdir(path, *, dir_fd=None):
    if dir_fd is not None:
        raise NotImplementedError("rmdir with dir_fd is not supported yet")
    return _rmdir(_path(path, 'rmdir'))


def unlink(path, *, dir_fd=None):
    if dir_fd is not None:
        raise NotImplementedError("unlink with dir_fd is not supported yet")
    return _unlink(_path(path, 'unlink'))


def remove(path, *, dir_fd=None):
    if dir_fd is not None:
        raise NotImplementedError("remove with dir_fd is not supported yet")
    return _unlink(_path(path, 'remove'))


def rename(source, target, *, src_dir_fd=None, dst_dir_fd=None):
    if src_dir_fd is not None or dst_dir_fd is not None:
        raise NotImplementedError("rename with dir_fd is not supported yet")
    return _rename(_path(source, 'rename'), _path(target, 'rename'))


def link(source, target, *, src_dir_fd=None, dst_dir_fd=None, follow_symlinks=True):
    if src_dir_fd is not None or dst_dir_fd is not None:
        raise NotImplementedError("link with dir_fd is not supported yet")
    return _link(_path(source, 'link'), _path(target, 'link'))


def symlink(source, target, target_is_path=False, *, dir_fd=None):
    if dir_fd is not None:
        raise NotImplementedError("symlink with dir_fd is not supported yet")
    return _symlink(_path(source, 'symlink'), _path(target, 'symlink'))


def readlink(path, *, dir_fd=None):
    if dir_fd is not None:
        raise NotImplementedError("readlink with dir_fd is not supported yet")
    from os import fsencode
    path = _path(path, 'readlink')
    result = _readlink(path)
    return fsencode(result) if isinstance(path, bytes) else result


def access(path, mode, *, dir_fd=None, effective_ids=False, follow_symlinks=True):
    if dir_fd is not None:
        raise NotImplementedError("access with dir_fd is not supported yet")
    return _access(_path(path, 'access'), mode)


def open(path, flags, mode=0o777, *, dir_fd=None):
    if dir_fd is not None:
        raise NotImplementedError("open with dir_fd is not supported yet")
    return _open_fd(_path(path, 'open'), flags, mode)


def read(fd, count):
    return _read(fd, count)


def readinto(fd, buffer, /):
    fd = _native_fd(fd)
    message = 'readinto() argument 2 must be read-write bytes-like object, not ' + type(buffer).__name__
    try:
        view = memoryview(buffer)
    except (TypeError, BufferError):
        raise TypeError(message) from None
    with view:
        if view.readonly or not view.c_contiguous:
            raise TypeError(message)
        with view.cast('B') as byte_view:
            data = _read(fd, byte_view.nbytes)
            byte_view[:len(data)] = data
            return len(data)


def write(fd, data):
    fd = _native_fd(fd)
    try:
        view = memoryview(data)
    except TypeError:
        raise TypeError(f"a bytes-like object is required, not '{type(data).__name__}'") from None
    with view:
        if not view.c_contiguous:
            raise BufferError("memoryview: underlying buffer is not C-contiguous")
        return _write(fd, view.tobytes())


def close(fd):
    return _close(fd)


def lseek(fd, offset, whence):
    return _lseek(fd, offset, whence)


def fsync(fd):
    return _fsync(fd)


def ftruncate(fd, length, /):
    from math.integer import _index
    fd = _native_fd(fd)
    length = _index(length)
    if length < -(1 << 63) or length >= (1 << 63):
        raise OverflowError('Python int too large to convert to C long')
    return _ftruncate(fd, length)


def dup(fd):
    return _dup(fd)


def dup2(fd, fd2, inheritable=True):
    return _dup2(fd, fd2)


def pipe():
    return _pipe()


def getpid():
    return _getpid()


def getppid():
    return _getppid()


def getuid():
    return _getuid()


def geteuid():
    return _geteuid()


def getgid():
    return _getgid()


def getegid():
    return _getegid()


def kill(pid, signal):
    return _kill(pid, signal)


def umask(mask):
    return _umask(mask)


def uname():
    return uname_result(_uname_items())


sysconf_names = {
    "SC_ARG_MAX": 0, "SC_CHILD_MAX": 1, "SC_CLK_TCK": 2,
    "SC_NGROUPS_MAX": 3, "SC_OPEN_MAX": 4, "SC_JOB_CONTROL": 7,
    "SC_SAVED_IDS": 8, "SC_VERSION": 9, "SC_PAGESIZE": 30,
    "SC_PAGE_SIZE": 30, "SC_NPROCESSORS_CONF": 83,
    "SC_NPROCESSORS_ONLN": 84,
}


def sysconf(name):
    if isinstance(name, str):
        if name not in sysconf_names:
            raise ValueError("unrecognized configuration name")
        name = sysconf_names[name]
    return _sysconf_value(name)


def cpu_count():
    try:
        return _sysconf_value(sysconf_names["SC_NPROCESSORS_ONLN"])
    except (OSError, ValueError):
        return None


def strerror(code):
    return _strerror_bytes(_native_fd(code)).decode('utf-8', 'surrogateescape')


def openpty():
    code, result = _openpty()
    if code:
        raise OSError(code, strerror(code))
    return result


def _exit(code):
    raise SystemExit(code)
