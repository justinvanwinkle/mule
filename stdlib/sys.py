"""The system module: interpreter state owned by compiled Python.

Process arguments are decoded from the native startup vector, standard
streams write to the process's descriptors, and ``modules`` is the registry
every compiled module enters before its body runs. Interpreter facilities
Mule does not have (frames, tracing, audit hooks with effects) are present
with their documented "absent" results where CPython defines one.
"""

import _mule_native
from _mule_native import active_exception
from _mule_native import argc
from _mule_native import current_frame
from _mule_native import flush_fd
from _mule_native import image_search_path
from _mule_native import intern_text as _intern_text
from _mule_native import is_interned_text as _is_interned_text
from _mule_native import read_c_string
from _mule_native import read_pointer
from _mule_native import register_modules
from _mule_native import write_fd
from _posix import read as _read
from _posix import environ_items as _environ_items
from _posix import executable_path_bytes as _executable_path_bytes


def _make_argv():
    arguments = []
    index = 0
    while index < argc:
        pointer = read_pointer(_mule_native.argv, index)
        if pointer is None:
            raise SystemError("NULL pointer inside native argument vector")
        arguments.append(read_c_string(pointer).decode('utf-8', 'surrogateescape'))
        index += 1
    return arguments


argv = _make_argv()
orig_argv = list(argv)
try:
    executable = _executable_path_bytes().decode('utf-8', 'surrogateescape')
except OSError:
    executable = ""
_base_executable = executable

modules = {}
register_modules(modules)

import _monitoring as monitoring
modules["sys.monitoring"] = monitoring


def _initial_cpu_count():
    value = None
    for key, item in _environ_items():
        if key == b'PYTHON_CPU_COUNT':
            value = item
            break
    if value is None or value == b'default':
        return -1
    digits = value[1:] if value.startswith((b'+', b'-')) else value
    message = 'PYTHON_CPU_COUNT must be a positive integer fitting C int'
    if not digits or any(byte < 48 or byte > 57 for byte in digits):
        raise ValueError(message)
    count = int(value)
    if count <= 0 or count >= (1 << 31):
        raise ValueError(message)
    return count


_cpu_count_config = _initial_cpu_count()


def _get_cpu_count_config():
    return _cpu_count_config


platform = "linux"
byteorder = "little"
maxsize = 9223372036854775807
maxunicode = 0x10FFFF
api_version = 1013
copyright = "Copyright (c) Python Software Foundation and Mule contributors."
platlibdir = "lib"
pycache_prefix = None
dont_write_bytecode = True
prefix = exec_prefix = base_prefix = base_exec_prefix = ""
abiflags = ""
float_repr_style = "short"
warnoptions = []
_xoptions = {}
path = list(image_search_path())
meta_path = []
path_hooks = []
path_importer_cache = {}
builtin_module_names = ("builtins", "sys", "_mule_native", "posix")
stdlib_module_names = frozenset()
_stdlib_dir = None
_git = ("mule", "", "")
_framework = ""


class _StructSequence:
    """A named tuple of interpreter facts (CPython's structseq)."""

    _fields = ()

    def __init__(self, *values):
        self._values = values
        for index in range(len(self._fields)):
            setattr(self, self._fields[index], values[index])

    def __getitem__(self, index):
        return self._values[index]

    def __len__(self):
        return len(self._values)

    def __iter__(self):
        return iter(self._values)

    def __eq__(self, other):
        return tuple(self) == tuple(other)

    def __ne__(self, other):
        return tuple(self) != tuple(other)

    def __lt__(self, other):
        return tuple(self) < tuple(other)

    def __le__(self, other):
        return tuple(self) <= tuple(other)

    def __gt__(self, other):
        return tuple(self) > tuple(other)

    def __ge__(self, other):
        return tuple(self) >= tuple(other)

    def __hash__(self):
        return hash(tuple(self))

    def __repr__(self):
        fields = ", ".join(
            f"{name}={value!r}" for name, value in zip(self._fields, self._values)
        )
        return f"{self._name}({fields})"


class _VersionInfo(_StructSequence):
    _name = "sys.version_info"
    _fields = ("major", "minor", "micro", "releaselevel", "serial")


version_info = _VersionInfo(3, 14, 0, "final", 0)
hexversion = 0x030E00F0
version = "3.14.0 (Mule native build) [LLVM]"


class _Implementation:
    def __init__(self):
        self.name = "mule"
        self.cache_tag = None
        self.version = version_info
        self.hexversion = hexversion
        self._multiarch = "x86_64-linux-gnu"

    def __repr__(self):
        return (
            f"namespace(name={self.name!r}, cache_tag=None, version={self.version!r}, "
            f"hexversion={self.hexversion})"
        )


implementation = _Implementation()


class _Flags(_StructSequence):
    _name = "sys.flags"
    _fields = (
        "debug",
        "inspect",
        "interactive",
        "optimize",
        "dont_write_bytecode",
        "no_user_site",
        "no_site",
        "ignore_environment",
        "verbose",
        "bytes_warning",
        "quiet",
        "hash_randomization",
        "isolated",
        "dev_mode",
        "utf8_mode",
        "warn_default_encoding",
        "safe_path",
        "int_max_str_digits",
        "gil",
        "thread_inherit_context",
        "context_aware_warnings",
    )


flags = _Flags(0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, False, 1, 0, False, 4300, 1, 0, 0)


class _FloatInfo(_StructSequence):
    _name = "sys.float_info"
    _fields = (
        "max",
        "max_exp",
        "max_10_exp",
        "min",
        "min_exp",
        "min_10_exp",
        "dig",
        "mant_dig",
        "epsilon",
        "radix",
        "rounds",
    )


float_info = _FloatInfo(
    1.7976931348623157e308,
    1024,
    308,
    2.2250738585072014e-308,
    -1021,
    -307,
    15,
    53,
    2.220446049250313e-16,
    2,
    1,
)


class _IntInfo(_StructSequence):
    _name = "sys.int_info"
    _fields = (
        "bits_per_digit",
        "sizeof_digit",
        "default_max_str_digits",
        "str_digits_check_threshold",
    )


int_info = _IntInfo(30, 4, 4300, 640)


class _HashInfo(_StructSequence):
    _name = "sys.hash_info"
    _fields = ("width", "modulus", "inf", "nan", "imag", "algorithm", "hash_bits", "seed_bits", "cutoff")


hash_info = _HashInfo(64, 2305843009213693951, 314159, 0, 1000003, "fnv", 64, 0, 0)


class _ThreadInfo(_StructSequence):
    _name = "sys.thread_info"
    _fields = ("name", "lock", "version")


thread_info = _ThreadInfo(None, None, None)


class _TextStream:
    """A standard stream writing UTF-8 text to a file descriptor."""

    def __init__(self, fd, name, mode):
        self._fd = fd
        self.name = name
        self.mode = mode
        self.encoding = "utf-8"
        self.errors = "backslashreplace" if fd == 2 else "surrogateescape"
        self.closed = False
        self.newlines = None
        self.line_buffering = fd != 0
        self.buffer = _BinaryStream(fd, name)

    def write(self, text):
        if not isinstance(text, str):
            raise TypeError(f"write() argument must be str, not {type(text).__name__}")
        write_fd(self._fd, text.encode(self.encoding, self.errors))
        return len(text)

    def writelines(self, lines):
        for line in lines:
            self.write(line)

    def flush(self):
        flush_fd(self._fd)

    def fileno(self):
        return self._fd

    def isatty(self):
        return _mule_native.isatty(self._fd)

    def readable(self):
        return self._fd == 0

    def writable(self):
        return self._fd != 0

    def seekable(self):
        return False

    def _decode_partial(self, data, final):
        if self.encoding.lower().replace("-", "_") == "utf_8":
            from _codecs import utf_8_decode

            return utf_8_decode(bytes(data), self.errors, final)[0]
        return bytes(data).decode(self.encoding, self.errors)

    def read(self, size=-1):
        if self._fd != 0:
            raise OSError("not readable")
        if size is None or size < 0:
            return self.buffer.read().decode(self.encoding, self.errors)
        if size == 0:
            return ""
        data = bytearray()
        while True:
            text = self._decode_partial(data, False)
            if len(text) >= size:
                return text[:size]
            chunk = self.buffer.read(1)
            if not chunk:
                return self._decode_partial(data, True)
            data.extend(chunk)

    def readline(self, size=-1):
        if self._fd != 0:
            raise OSError("not readable")
        if size is None or size < 0:
            return self.buffer.readline().decode(self.encoding, self.errors)
        if size == 0:
            return ""
        data = bytearray()
        while True:
            text = self._decode_partial(data, False)
            if len(text) >= size:
                return text[:size]
            chunk = self.buffer.read(1)
            if not chunk:
                return self._decode_partial(data, True)
            data.extend(chunk)
            if chunk == b"\n":
                return self._decode_partial(data, True)

    def readlines(self, hint=-1):
        lines = []
        total = 0
        while True:
            line = self.readline()
            if not line:
                break
            lines.append(line)
            total += len(line)
            if hint is not None and hint > 0 and total >= hint:
                break
        return lines

    def __iter__(self):
        return self

    def __next__(self):
        line = self.readline()
        if not line:
            raise StopIteration
        return line

    def close(self):
        self.flush()

    def __repr__(self):
        return f"<_io.TextIOWrapper name={self.name!r} mode={self.mode!r} encoding='utf-8'>"


class _BinaryStream:
    def __init__(self, fd, name):
        self._fd = fd
        self.name = name
        self.closed = False

    def read(self, size=-1):
        if self._fd != 0:
            raise OSError("not readable")
        if size is None or size < 0:
            chunks = []
            while True:
                chunk = _read(self._fd, 65536)
                if not chunk:
                    break
                chunks.append(chunk)
            return b"".join(chunks)
        if size == 0:
            return b""
        return _read(self._fd, size)

    def readline(self, size=-1):
        if self._fd != 0:
            raise OSError("not readable")
        limit = -1 if size is None else size
        chunks = []
        total = 0
        while limit < 0 or total < limit:
            chunk = _read(self._fd, 1)
            if not chunk:
                break
            chunks.append(chunk)
            total += 1
            if chunk == b"\n":
                break
        return b"".join(chunks)

    def readlines(self, hint=-1):
        lines = []
        total = 0
        while True:
            line = self.readline()
            if not line:
                break
            lines.append(line)
            total += len(line)
            if hint is not None and hint > 0 and total >= hint:
                break
        return lines

    def __iter__(self):
        return self

    def __next__(self):
        line = self.readline()
        if not line:
            raise StopIteration
        return line

    def write(self, data):
        return write_fd(self._fd, bytes(data))

    def writelines(self, lines):
        for line in lines:
            self.write(line)

    def flush(self):
        flush_fd(self._fd)

    def fileno(self):
        return self._fd

    def isatty(self):
        return _mule_native.isatty(self._fd)

    def readable(self):
        return self._fd == 0

    def writable(self):
        return self._fd != 0

    def seekable(self):
        return False

    def close(self):
        self.flush()


stdin = __stdin__ = _TextStream(0, "<stdin>", "r")
stdout = __stdout__ = _TextStream(1, "<stdout>", "w")
stderr = __stderr__ = _TextStream(2, "<stderr>", "w")

_recursion_limit = 1000


def getrecursionlimit():
    return _recursion_limit


def setrecursionlimit(limit, /):
    global _recursion_limit
    if limit < 1:
        raise ValueError("recursion limit must be greater or equal than 1")
    _recursion_limit = limit


def getdefaultencoding():
    return "utf-8"


def getfilesystemencoding():
    return "utf-8"


def getfilesystemencodeerrors():
    return "surrogateescape"


def intern(string, /):
    if not isinstance(string, str):
        raise TypeError(f"intern() argument must be str, not {type(string).__name__}")
    if type(string) is not str:
        raise TypeError(f"can't intern {type(string).__name__}")
    return _intern_text(string)


def _is_interned(string, /):
    if not isinstance(string, str):
        raise TypeError(f"_is_interned() argument must be str, not {type(string).__name__}")
    if type(string) is not str:
        return False
    return _is_interned_text(string)


def exit(status=None, /):
    raise SystemExit(status)


def exception():
    return active_exception()


def exc_info():
    value = active_exception()
    if value is None:
        return (None, None, None)
    return (type(value), value, getattr(value, "__traceback__", None))


def getsizeof(object, default=None):
    return 64 if default is None else default


def getrefcount(object, /):
    return 2


def is_finalizing():
    return False


def _is_gil_enabled():
    return True


def get_int_max_str_digits():
    return int_info.default_max_str_digits


def set_int_max_str_digits(maxdigits):
    if maxdigits and maxdigits < 640:
        raise ValueError("maxdigits must be >= 640 or 0 for unlimited")


def getswitchinterval():
    return 0.005


def setswitchinterval(interval, /):
    if interval <= 0:
        raise ValueError("switch interval must be strictly positive")


def audit(event, /, *args):
    for hook in _audit_hooks:
        hook(event, args)


_audit_hooks = []


def addaudithook(hook):
    _audit_hooks.append(hook)


_trace = None
_profile = None


def settrace(function, /):
    global _trace
    _trace = function


def gettrace():
    return _trace


def setprofile(function, /):
    global _profile
    _profile = function


def getprofile():
    return _profile


def _getframe(depth=0, /):
    if depth < 0:
        raise ValueError("depth must be >= 0")
    # This function's own frame is one deeper than CPython's C _getframe.
    frame = current_frame(depth + 1)
    if frame is None:
        raise ValueError("call stack is not deep enough")
    return frame


def _getframemodulename(depth=0):
    return None


def call_tracing(function, args, /):
    return function(*args)


def displayhook(value, /):
    if value is None:
        return
    stdout.write(repr(value) + "\n")


def excepthook(kind, value, traceback, /):
    stderr.write(f"{kind.__name__}: {value}\n")


def unraisablehook(unraisable, /):
    stderr.write(f"Exception ignored in: {unraisable.object!r}\n")


def _clear_type_descriptors(cls, /):
    from _object import _builtin_types
    if not isinstance(cls, type):
        raise TypeError("_clear_type_descriptors() argument must be type, not " + type(cls).__name__)
    if cls in _builtin_types():
        raise TypeError("argument is immutable")
    for name in ('__dict__', '__weakref__'):
        if name in vars(cls):
            delattr(cls, name)


__displayhook__ = displayhook
__excepthook__ = excepthook
__unraisablehook__ = unraisablehook
__breakpointhook__ = None


def breakpointhook(*args, **keywords):
    raise RuntimeError("breakpoint() is not supported by Mule")


def activate_stack_trampoline(backend, /):
    pass


def deactivate_stack_trampoline():
    pass


def is_stack_trampoline_active():
    return False


def getallocatedblocks():
    return 0


def getunicodeinternedsize():
    return 0


def _clear_type_cache():
    pass


def _clear_internal_caches():
    pass


class _AsyncgenHooks(_StructSequence):
    _name = "asyncgen_hooks"
    _fields = ("firstiter", "finalizer")


_asyncgen_hooks = _AsyncgenHooks(None, None)
_MISSING = object()
_coroutine_origin_tracking_depth = 0


def get_asyncgen_hooks():
    return _asyncgen_hooks


def set_asyncgen_hooks(firstiter=_MISSING, finalizer=_MISSING):
    global _asyncgen_hooks
    if firstiter is not _MISSING and firstiter is not None and not callable(firstiter):
        raise TypeError(f"callable firstiter expected, got {type(firstiter).__name__}")
    if finalizer is not _MISSING and finalizer is not None and not callable(finalizer):
        raise TypeError(f"callable finalizer expected, got {type(finalizer).__name__}")
    current = _asyncgen_hooks
    _asyncgen_hooks = _AsyncgenHooks(
        current.firstiter if firstiter is _MISSING else firstiter,
        current.finalizer if finalizer is _MISSING else finalizer,
    )


def get_coroutine_origin_tracking_depth():
    return _coroutine_origin_tracking_depth


def set_coroutine_origin_tracking_depth(depth):
    global _coroutine_origin_tracking_depth
    if not isinstance(depth, int):
        raise TypeError(f"an integer is required (got type {type(depth).__name__})")
    if depth < 0:
        raise ValueError("depth must be >= 0")
    _coroutine_origin_tracking_depth = depth
