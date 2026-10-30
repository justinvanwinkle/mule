"""Native sequence and exception protocols, compiled by Mule.

Underscored storage operations are bound by the compiler. These definitions
execute as native Python functions and classes, in a private module namespace.
"""

from _object import (
    bool,
    complex,
    float,
    globals,
    int,
    isinstance,
    issubclass,
    object,
    type,
    vars,
)
from _object import _globals_dict
from _object import locals as _builtin_locals
from _object import _make_complex
from _object import _bytes_special
from _object import _index_integer
from _mule_native import text_from_codepoints
from _mule_native import typed_value
from _str import TextIterator, TextStorage, ascii, decode, find_in, format, ord, repr
from _str import str as _make_str
from _str import _MISSING as _CODEC_ERRORS_MISSING
from _attributes import delattr, getattr, hasattr, setattr
from _descriptors import classmethod, property, staticmethod, super
from _generators import generator as _generator
from _generators import yield_from as _yield_from
from _generators import aiter, anext
from _generators import async_generator as _async_generator
from _generators import async_generator_yield as _async_generator_yield
from _generators import await_value as _await_value
from _generators import coroutine as _coroutine
from _dict import build_dict as _build_dict
from _dict import build_set as _build_set
from _dict import _MISSING as _DICT_MISSING
from _dict import (
    _DELETED as _TABLE_DELETED,
    _DUMMY as _TABLE_DUMMY,
    _EMPTY as _TABLE_EMPTY,
    _MASK_LIMIT as _TABLE_MASK_LIMIT,
    _PERTURB_MASK as _TABLE_PERTURB_MASK,
    _empty_set_sized,
    _empty_table,
    _set_lookup,
    _set_remove,
    _table_hash_fallback,
    _table_insert,
    _table_lookup,
    _table_remove,
    _table_resize,
    _tuple_hash,
    dict,
    frozenset,
    hash,
    set,
)
from _mule_native import bytes_from_values
from _mule_native import byte_length
from _mule_native import read_byte
from _mule_native import byte_copy
from _mule_native import call_with
from _mule_native import identity
from _mule_native import is_class
from _mule_native import is_subclass
from _mule_native import register_not_implemented
from _mule_native import namespace_items
from _mule_native import new_namespace
from _mule_native import is_instance
from _mule_native import overriding_new
from _mule_native import class_lookup_default
from _mule_native import bind_instance_attribute
from _mule_native import math_fabs
from _mule_native import set_instance_field
from _mule_native import type_of
from _slice import is_slice, positions, slice

__speed__ = 3
__mule_builtin_implementation__ = True

# ruff: noqa: F821

list = _binding_missing()
tuple = _binding_missing()
StorageIterator = _binding_missing()
Enumerate = _binding_missing()


class BaseException:
    # Set when a handler catches the exception; see mule/runtime/traceback.rs.
    __traceback__ = None
    _args = ()
    _cause = None
    _context = None
    _suppress_context = False

    def __new__(cls, *args, **kwargs):
        instance = _allocate_instance(cls)
        if args:
            instance._args = args
        return instance

    def __init__(self, *args):
        self._args = args

    @property
    def args(self):
        return self._args

    @args.setter
    def args(self, value):
        self._args = tuple(value)

    @property
    def __cause__(self):
        return self._cause

    @__cause__.setter
    def __cause__(self, value):
        if value is not None and not isinstance(value, BaseException):
            raise TypeError("exception cause must be None or derive from BaseException")
        self._cause = value
        self.__suppress_context__ = True

    @property
    def __context__(self):
        return self._context

    @__context__.setter
    def __context__(self, value):
        if value is not None and not isinstance(value, BaseException):
            raise TypeError(
                "exception context must be None or derive from BaseException"
            )
        self._context = value

    @property
    def __suppress_context__(self):
        return self._suppress_context

    @__suppress_context__.setter
    def __suppress_context__(self, value):
        if not isinstance(value, bool):
            raise TypeError("attribute value type must be bool")
        self._suppress_context = value

    def with_traceback(self, tb):
        self.__traceback__ = tb
        return self

    def __str__(self):
        if len(self.args) == 0:
            return ""
        if len(self.args) == 1:
            return str(self.args[0])
        return str(self.args)

    def __repr__(self):
        return (
            type(self).__name__ + "(" + ", ".join(repr(arg) for arg in self.args) + ")"
        )

    def add_note(self, note):
        if not isinstance(note, str):
            raise TypeError("note must be a str, not '" + type(note).__name__ + "'")
        if not hasattr(self, "__notes__"):
            self.__notes__ = []
        self.__notes__.append(note)


class Exception(BaseException):
    pass


class ArithmeticError(Exception):
    pass


class LookupError(Exception):
    pass


class IndexError(LookupError):
    pass


class KeyError(LookupError):
    def __str__(self):
        if len(self.args) == 1:
            return repr(self.args[0])
        return BaseException.__str__(self)


class ValueError(Exception):
    pass


class TypeError(Exception):
    pass


class RuntimeError(Exception):
    pass


class NameError(Exception):
    pass


class UnboundLocalError(NameError):
    pass


class ZeroDivisionError(ArithmeticError):
    pass


class OverflowError(ArithmeticError):
    pass


class AssertionError(Exception):
    pass


class StopIteration(Exception):
    # Iteration itself raises StopIteration, so construction must not iterate.
    @property
    def value(self):
        return self.args[0] if self.args else None


class GeneratorExit(BaseException):
    pass


class StopAsyncIteration(Exception):
    @property
    def value(self):
        return self.args[0] if self.args else None


class MemoryError(Exception):
    pass


class SystemError(Exception):
    pass


class AttributeError(Exception):
    pass


class OSError(Exception):
    def __new__(cls, *args):
        if cls is OSError and 2 <= len(args) <= 5 and isinstance(args[0], int):
            cls = {
                1: PermissionError,
                2: FileNotFoundError,
                3: ProcessLookupError,
                4: InterruptedError,
                10: ChildProcessError,
                11: BlockingIOError,
                13: PermissionError,
                17: FileExistsError,
                20: NotADirectoryError,
                21: IsADirectoryError,
                32: BrokenPipeError,
                103: ConnectionAbortedError,
                104: ConnectionResetError,
                108: BrokenPipeError,
                110: TimeoutError,
                111: ConnectionRefusedError,
                114: BlockingIOError,
                115: BlockingIOError,
            }.get(_integer_storage(args[0]), cls)
        return _allocate_instance(cls)

    def __init__(self, *args):
        BaseException.__init__(self, *args)
        self.errno = self.strerror = self.filename = self.filename2 = None
        self._os_error = 2 <= len(args) <= 5
        if self._os_error:
            self.errno, self.strerror = args[:2]
            if len(args) >= 3:
                if isinstance(self, BlockingIOError):
                    if isinstance(args[2], int):
                        self.characters_written = integer_index(args[2])
                elif args[2] is not None:
                    self.filename = args[2]
                    self.args = args[:2]
                    if len(args) == 5:
                        self.filename2 = args[4]

    def __str__(self):
        if not self._os_error:
            return BaseException.__str__(self)
        result = "[Errno " + str(self.errno) + "] " + str(self.strerror)
        if self.filename is not None:
            result += ": " + repr(self.filename)
            if self.filename2 is not None:
                result += " -> " + repr(self.filename2)
        return result


class EOFError(Exception):
    pass


class RecursionError(RuntimeError):
    pass


class UnicodeError(ValueError):
    pass


class UnicodeEncodeError(UnicodeError):
    def __init__(self, encoding, object, start, end, reason, /):
        if not isinstance(encoding, str):
            raise TypeError("argument 1 must be str, not " + type(encoding).__name__)
        if not isinstance(object, str):
            raise TypeError("argument 2 must be str, not " + type(object).__name__)
        _unicode_error_init(self, encoding, object, start, end, reason)

    def __str__(self):
        if self.end == self.start + 1 and 0 <= self.start < TextStorage.__len__(
            self.object
        ):
            code = ord(TextStorage.__getitem__(self.object, self.start))
            if code <= 0xFF:
                character = "\\x" + format(code, "02x")
            elif code <= 0xFFFF:
                character = "\\u" + format(code, "04x")
            else:
                character = "\\U" + format(code, "08x")
            where = "character '" + character + "' in position " + str(self.start)
        else:
            where = (
                "characters in position " + str(self.start) + "-" + str(self.end - 1)
            )
        return (
            "'" + self.encoding + "' codec can't encode " + where + ": " + self.reason
        )


class UnicodeDecodeError(UnicodeError):
    def __init__(self, encoding, object, start, end, reason, /):
        if not isinstance(encoding, str):
            raise TypeError("argument 1 must be str, not " + type(encoding).__name__)
        original = object
        if not isinstance(object, bytes):
            object = memoryview(object).tobytes()
        _unicode_error_init(self, encoding, original, start, end, reason)
        self.object = object

    def __str__(self):
        if self.end == self.start + 1 and 0 <= self.start < byte_length(self.object):
            where = "byte 0x" + format(read_byte(self.object, self.start), "02x")
            where += " in position " + str(self.start)
        else:
            where = "bytes in position " + str(self.start) + "-" + str(self.end - 1)
        return (
            "'" + self.encoding + "' codec can't decode " + where + ": " + self.reason
        )


def _unicode_error_init(error, encoding, object, start, end, reason):
    if not isinstance(encoding, str):
        raise TypeError("argument 1 must be str, not " + type(encoding).__name__)
    error.args = encoding, object, start, end, reason
    error.encoding = encoding
    error.object = object
    error.start = integer_index(start)
    error.end = integer_index(end)
    for value in (error.start, error.end):
        if value < -(1 << 63) or value >= 1 << 63:
            raise OverflowError("Python int too large to convert to C ssize_t")
    if not isinstance(reason, str):
        raise TypeError("argument 5 must be str, not " + type(reason).__name__)
    error.reason = reason


class SystemExit(BaseException):
    def __mule_process_status__(self):
        return self.code.__mule_exit_status__()

    @property
    def code(self):
        if not self.args:
            return None
        return self.args[0] if len(self.args) == 1 else self.args


class KeyboardInterrupt(BaseException):
    pass


class FloatingPointError(ArithmeticError):
    pass


class BufferError(Exception):
    pass


class ImportError(Exception):
    def __init__(self, *args, name=None, path=None):
        BaseException.__init__(self, *args)
        self.name = name
        self.path = path
        self.msg = args[0] if len(args) == 1 else None


class ModuleNotFoundError(ImportError):
    pass


class ReferenceError(Exception):
    pass


class NotImplementedError(RuntimeError):
    pass


class PythonFinalizationError(RuntimeError):
    pass


class SyntaxError(Exception):
    def __init__(self, *args):
        BaseException.__init__(self, *args)
        self.msg = args[0] if args else None
        self.filename = None
        self.lineno = None
        self.offset = None
        self.text = None
        self.end_lineno = None
        self.end_offset = None
        self.print_file_and_line = None
        if len(args) == 2:
            details = args[1]
            self.filename = details[0]
            self.lineno = details[1]
            self.offset = details[2]
            self.text = details[3]
            if len(details) > 4:
                self.end_lineno = details[4]
                self.end_offset = details[5]

    def __str__(self):
        return str(self.msg)


class IndentationError(SyntaxError):
    pass


class TabError(IndentationError):
    pass


class BlockingIOError(OSError):
    pass


class ChildProcessError(OSError):
    pass


class ConnectionError(OSError):
    pass


class BrokenPipeError(ConnectionError):
    pass


class ConnectionAbortedError(ConnectionError):
    pass


class ConnectionRefusedError(ConnectionError):
    pass


class ConnectionResetError(ConnectionError):
    pass


class FileExistsError(OSError):
    pass


class FileNotFoundError(OSError):
    pass


class InterruptedError(OSError):
    pass


class IsADirectoryError(OSError):
    pass


class NotADirectoryError(OSError):
    pass


class PermissionError(OSError):
    pass


class ProcessLookupError(OSError):
    pass


class TimeoutError(OSError):
    pass


class Warning(Exception):
    pass


class BytesWarning(Warning):
    pass


class DeprecationWarning(Warning):
    pass


class EncodingWarning(Warning):
    pass


class FutureWarning(Warning):
    pass


class ImportWarning(Warning):
    pass


class PendingDeprecationWarning(Warning):
    pass


class ResourceWarning(Warning):
    pass


class RuntimeWarning(Warning):
    pass


class SyntaxWarning(Warning):
    pass


class UnicodeWarning(Warning):
    pass


class UserWarning(Warning):
    pass


def exception_matches(value, expected):
    target = _class_identity(expected)
    if target is not None:
        if not _class_is_subclass(target, BaseException):
            raise TypeError("catching classes that do not inherit from BaseException")
        raised = _raised_class(value)
        return raised is not None and _class_is_subclass(raised, target)
    if not _tuple_storage(expected) and not is_instance(expected, tuple):
        raise TypeError("catching classes that do not inherit from BaseException")
    raised = _raised_class(value)
    count = _storage_length(expected)
    index = 0
    while index < count:
        item = _storage_read(expected, index)
        target = _class_identity(item)
        if target is None or not _class_is_subclass(target, BaseException):
            raise TypeError(
                "catching classes that do not inherit from BaseException is not allowed"
            )
        if raised is not None and _class_is_subclass(raised, target):
            return True
        index += 1
    return False


def iteration_exhausted(value):
    return exception_matches(value, StopIteration)


def generator_exception(value):
    """Apply PEP 479 without re-raising unrelated exceptions or their tracebacks."""
    if not iteration_exhausted(value):
        return value
    converted = RuntimeError("generator raised StopIteration")
    converted.__cause__ = value
    converted.__context__ = value
    converted.__suppress_context__ = True
    return converted


def raise_value(value):
    """Prepare an exception; the source raise VOP installs pending state."""
    parent = _raised_class(value)
    if parent is None or not _class_is_subclass(parent, BaseException):
        # Use the physical error primitive here to avoid recursive invalid raises.
        return _invalid_raise()
    instance = _exception_instance(value)
    # An exception raised while another is being handled records it as its
    # context, unless it already has one or is that exception itself.
    active = _active_exception()
    if active is not None:
        if active is instance:
            instance._explicit_raise = True
        elif instance.__context__ is None:
            current = active
            while current is not None:
                nxt = current.__context__
                if nxt is instance:
                    current.__context__ = None
                    break
                current = nxt
            instance.__context__ = active
    return instance


def _assertion_failure(*args):
    instance = (
        AssertionError()
        if _storage_length(args) == 0
        else AssertionError(_storage_read(args, 0))
    )
    return raise_value(instance)


def exception_instance(value):
    """The instance a ``raise`` statement will raise, creating it from a class."""
    parent = _raised_class(value)
    if parent is None or not _class_is_subclass(parent, BaseException):
        raise TypeError("exceptions must derive from BaseException")
    return _exception_instance(value)


def set_exception_cause(instance, cause):
    """``raise instance from cause``: the cause is None or an exception."""
    if cause is not None:
        cause = exception_instance(cause)
    instance.__cause__ = cause
    instance.__suppress_context__ = True


def positive(value):
    method = _pos_method(value)
    if method is None:
        raise TypeError(f"bad operand type for unary +: '{type(value).__name__}'")
    return method()


def negative(value):
    method = _neg_method(value)
    if method is None:
        raise TypeError(f"bad operand type for unary -: '{type(value).__name__}'")
    return method()


def invert(value):
    method = _invert_method(value)
    if method is None:
        raise TypeError(f"bad operand type for unary ~: '{type(value).__name__}'")
    return method()


def abs(value, /):
    cls = _type_of(value)
    if cls is int or cls is bool:
        return -value if value < 0 else +value
    if cls is float:
        return math_fabs(value)
    method = _abs_method(value)
    if method is None:
        raise TypeError(f"bad operand type for abs(): '{type(value).__name__}'")
    return method()


def integer_index(value):
    if _type_of(value) is int:
        return _integer_storage(value)
    if _index_method(value) is None:
        return _integer_storage(value)
    return _index_integer(value)


def chr(value, /):
    value = integer_index(value)
    if value < 0 or value > 0x10FFFF:
        raise ValueError("chr() arg not in range(0x110000)")
    return text_from_codepoints([value])


def getitem(value, index):
    cls = _type_of(value)
    if cls is dict:
        entry = _table_lookup(value, index)
        if entry >= 0:
            return _storage_read(_instance_field(value, "_values"), entry)
        raise KeyError(index)
    if cls is list:
        if is_slice(index):
            return _selected(value, index)
        return _storage_read(value, storage_index(value, index))
    if cls is tuple:
        if is_slice(index):
            return _storage_tuple(_selected(value, index))
        return _storage_read(value, storage_index(value, index))
    if cls is str:
        return TextStorage.__getitem__(value, index)
    if cls is bytes:
        return BytesStorage.__getitem__(value, index)
    if cls is bytearray:
        values = value.values
        if is_slice(index):
            return bytearray(_selected(values, index))
        return _storage_read(values, storage_index(values, index))
    return _getitem_general(value, index)


# Entry points for a receiver the compiler proved to be an exact ``dict``:
# the compiled code calls them directly, without resolving a method,
# binding arguments, or testing the receiver's class. An exact dict has no
# ``__missing__``, ``__getitem__`` override, or other hook.
def dict_get(table, key, default):
    code = _storage_hash(key)
    if code is None:
        code = _table_hash_fallback(key)
    mask = (_storage_length(_instance_field(table, "_indices")) - 1) & _TABLE_MASK_LIMIT
    perturb = code & _TABLE_PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(_instance_field(table, "_indices"), slot)
        if entry >= 0:
            if _storage_read(_instance_field(table, "_hashes"), entry) == code:
                existing = _storage_read(_instance_field(table, "_keys"), entry)
                if existing is key or existing == key:
                    return _storage_read(_instance_field(table, "_values"), entry)
        elif entry is _TABLE_EMPTY:
            return default
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def dict_getitem(table, key):
    code = _storage_hash(key)
    if code is None:
        code = _table_hash_fallback(key)
    mask = (_storage_length(_instance_field(table, "_indices")) - 1) & _TABLE_MASK_LIMIT
    perturb = code & _TABLE_PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(_instance_field(table, "_indices"), slot)
        if entry >= 0:
            if _storage_read(_instance_field(table, "_hashes"), entry) == code:
                existing = _storage_read(_instance_field(table, "_keys"), entry)
                if existing is key or existing == key:
                    return _storage_read(_instance_field(table, "_values"), entry)
        elif entry is _TABLE_EMPTY:
            raise KeyError(key)
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def dict_setitem(table, key, value):
    code = _storage_hash(key)
    if code is None:
        code = _table_hash_fallback(key)
    mask = (_storage_length(_instance_field(table, "_indices")) - 1) & _TABLE_MASK_LIMIT
    perturb = code & _TABLE_PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(_instance_field(table, "_indices"), slot)
        if entry >= 0:
            if _storage_read(_instance_field(table, "_hashes"), entry) == code:
                existing = _storage_read(_instance_field(table, "_keys"), entry)
                if existing is key or existing == key:
                    _storage_write(_instance_field(table, "_values"), entry, value)
                    return
        elif entry is _TABLE_EMPTY:
            break
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask
    if (_storage_length(_instance_field(table, "_keys")) + 1) * 3 > _storage_length(
        _instance_field(table, "_indices")
    ) * 2:
        _table_resize(table, _instance_field(table, "_used") + 1)
        mask = (
            _storage_length(_instance_field(table, "_indices")) - 1
        ) & _TABLE_MASK_LIMIT
        perturb = code & _TABLE_PERTURB_MASK
        slot = perturb & mask
        while (
            _storage_read(_instance_field(table, "_indices"), slot) is not _TABLE_EMPTY
        ):
            perturb >>= 5
            slot = (slot * 5 + perturb + 1) & mask
    _storage_write(
        _instance_field(table, "_indices"),
        slot,
        _storage_length(_instance_field(table, "_keys")),
    )
    _storage_append(_instance_field(table, "_hashes"), code)
    _storage_append(_instance_field(table, "_keys"), key)
    _storage_append(_instance_field(table, "_values"), value)
    next_used = (_instance_field(table, "_used") + 1) & _TABLE_MASK_LIMIT
    _set_instance_field(table, "_used", next_used)
    del next_used


def dict_contains(table, key):
    code = _storage_hash(key)
    if code is None:
        code = _table_hash_fallback(key)
    mask = (_storage_length(_instance_field(table, "_indices")) - 1) & _TABLE_MASK_LIMIT
    perturb = code & _TABLE_PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(_instance_field(table, "_indices"), slot)
        if entry >= 0:
            if _storage_read(_instance_field(table, "_hashes"), entry) == code:
                existing = _storage_read(_instance_field(table, "_keys"), entry)
                if existing is key or existing == key:
                    return True
        elif entry is _TABLE_EMPTY:
            return False
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def dict_not_contains(table, key):
    code = _storage_hash(key)
    if code is None:
        code = _table_hash_fallback(key)
    mask = (_storage_length(_instance_field(table, "_indices")) - 1) & _TABLE_MASK_LIMIT
    perturb = code & _TABLE_PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(_instance_field(table, "_indices"), slot)
        if entry >= 0:
            if _storage_read(_instance_field(table, "_hashes"), entry) == code:
                existing = _storage_read(_instance_field(table, "_keys"), entry)
                if existing is key or existing == key:
                    return False
        elif entry is _TABLE_EMPTY:
            return True
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def dict_len(table):
    return _instance_field(table, "_used")


def table_len(table):
    return _instance_field(table, "_used")


def empty_set():
    return _empty_table(set)


def set_add(table, key):
    code = _storage_hash(key)
    if code is None:
        code = _table_hash_fallback(key)
    mask = (_storage_length(_instance_field(table, "_indices")) - 1) & _TABLE_MASK_LIMIT
    perturb = code & _TABLE_PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(_instance_field(table, "_indices"), slot)
        if entry >= 0:
            if _storage_read(_instance_field(table, "_hashes"), entry) == code:
                existing = _storage_read(_instance_field(table, "_keys"), entry)
                if existing is key or existing == key:
                    return
        elif entry is _TABLE_EMPTY:
            break
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask
    if (_storage_length(_instance_field(table, "_keys")) + 1) * 3 > _storage_length(
        _instance_field(table, "_indices")
    ) * 2:
        _table_resize(table, _instance_field(table, "_used") + 1)
        mask = (
            _storage_length(_instance_field(table, "_indices")) - 1
        ) & _TABLE_MASK_LIMIT
        perturb = code & _TABLE_PERTURB_MASK
        slot = perturb & mask
        while (
            _storage_read(_instance_field(table, "_indices"), slot) is not _TABLE_EMPTY
        ):
            perturb >>= 5
            slot = (slot * 5 + perturb + 1) & mask
    _storage_write(
        _instance_field(table, "_indices"),
        slot,
        _storage_length(_instance_field(table, "_keys")),
    )
    _storage_append(_instance_field(table, "_hashes"), code)
    _storage_append(_instance_field(table, "_keys"), key)
    next_used = (_instance_field(table, "_used") + 1) & _TABLE_MASK_LIMIT
    _set_instance_field(table, "_used", next_used)
    del next_used


def set_discard(table, key):
    code = _storage_hash(key)
    if code is None:
        _set_remove(table, key)
        return
    mask = (_storage_length(_instance_field(table, "_indices")) - 1) & _TABLE_MASK_LIMIT
    perturb = code & _TABLE_PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(_instance_field(table, "_indices"), slot)
        if entry >= 0:
            if _storage_read(_instance_field(table, "_hashes"), entry) == code:
                existing = _storage_read(_instance_field(table, "_keys"), entry)
                if existing is key or existing == key:
                    _storage_write(
                        _instance_field(table, "_indices"), slot, _TABLE_DUMMY
                    )
                    _storage_write(
                        _instance_field(table, "_keys"), entry, _TABLE_DELETED
                    )
                    remaining_used = (
                        _instance_field(table, "_used") - 1
                    ) & _TABLE_MASK_LIMIT
                    _set_instance_field(table, "_used", remaining_used)
                    del remaining_used
                    return
        elif entry is _TABLE_EMPTY:
            return
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def set_remove(table, key):
    code = _storage_hash(key)
    if code is None:
        if _set_remove(table, key) is _DICT_MISSING:
            raise KeyError(key)
        return
    mask = (_storage_length(_instance_field(table, "_indices")) - 1) & _TABLE_MASK_LIMIT
    perturb = code & _TABLE_PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(_instance_field(table, "_indices"), slot)
        if entry >= 0:
            if _storage_read(_instance_field(table, "_hashes"), entry) == code:
                existing = _storage_read(_instance_field(table, "_keys"), entry)
                if existing is key or existing == key:
                    _storage_write(
                        _instance_field(table, "_indices"), slot, _TABLE_DUMMY
                    )
                    _storage_write(
                        _instance_field(table, "_keys"), entry, _TABLE_DELETED
                    )
                    remaining_used = (
                        _instance_field(table, "_used") - 1
                    ) & _TABLE_MASK_LIMIT
                    _set_instance_field(table, "_used", remaining_used)
                    del remaining_used
                    return
        elif entry is _TABLE_EMPTY:
            raise KeyError(key)
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def set_contains(table, key):
    code = _storage_hash(key)
    if code is None:
        return _set_lookup(table, key) >= 0
    mask = (_storage_length(_instance_field(table, "_indices")) - 1) & _TABLE_MASK_LIMIT
    perturb = code & _TABLE_PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(_instance_field(table, "_indices"), slot)
        if entry >= 0:
            if _storage_read(_instance_field(table, "_hashes"), entry) == code:
                existing = _storage_read(_instance_field(table, "_keys"), entry)
                if existing is key or existing == key:
                    return True
        elif entry is _TABLE_EMPTY:
            return False
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def set_not_contains(table, key):
    code = _storage_hash(key)
    if code is None:
        return _set_lookup(table, key) < 0
    mask = (_storage_length(_instance_field(table, "_indices")) - 1) & _TABLE_MASK_LIMIT
    perturb = code & _TABLE_PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(_instance_field(table, "_indices"), slot)
        if entry >= 0:
            if _storage_read(_instance_field(table, "_hashes"), entry) == code:
                existing = _storage_read(_instance_field(table, "_keys"), entry)
                if existing is key or existing == key:
                    return False
        elif entry is _TABLE_EMPTY:
            return True
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def set_and(left, right):
    left_used = _instance_field(left, "_used")
    right_used = _instance_field(right, "_used")
    result = _empty_set_sized(left_used if left_used < right_used else right_used)
    res_indices = _instance_field(result, "_indices")
    res_hashes = _instance_field(result, "_hashes")
    res_keys = _instance_field(result, "_keys")
    res_mask = (_storage_length(res_indices) - 1) & _TABLE_MASK_LIMIT
    count = _storage_length(_instance_field(left, "_keys"))
    index = 0
    while index < count:
        key = _storage_read(_instance_field(left, "_keys"), index)
        if key is not _TABLE_DELETED:
            code = _storage_read(_instance_field(left, "_hashes"), index)
            mask = (
                _storage_length(_instance_field(right, "_indices")) - 1
            ) & _TABLE_MASK_LIMIT
            perturb = code & _TABLE_PERTURB_MASK
            slot = perturb & mask
            while True:
                entry = _storage_read(_instance_field(right, "_indices"), slot)
                if entry >= 0:
                    if _storage_read(_instance_field(right, "_hashes"), entry) == code:
                        existing = _storage_read(_instance_field(right, "_keys"), entry)
                        if existing is key or existing == key:
                            res_perturb = code & _TABLE_PERTURB_MASK
                            res_slot = res_perturb & res_mask
                            while (
                                _storage_read(res_indices, res_slot) is not _TABLE_EMPTY
                            ):
                                res_perturb >>= 5
                                res_slot = (res_slot * 5 + res_perturb + 1) & res_mask
                            _storage_write(
                                res_indices, res_slot, _storage_length(res_keys)
                            )
                            _storage_append(res_hashes, code)
                            _storage_append(res_keys, key)
                            break
                elif entry is _TABLE_EMPTY:
                    break
                perturb >>= 5
                slot = (slot * 5 + perturb + 1) & mask
        index += 1
    used = _storage_length(res_keys)
    _set_instance_field(result, "_used", used)
    del used
    return result


def set_or(left, right):
    result = _empty_set_sized(
        _instance_field(left, "_used") + _instance_field(right, "_used")
    )
    res_indices = _instance_field(result, "_indices")
    res_hashes = _instance_field(result, "_hashes")
    res_keys = _instance_field(result, "_keys")
    res_mask = (_storage_length(res_indices) - 1) & _TABLE_MASK_LIMIT
    count = _storage_length(_instance_field(left, "_keys"))
    index = 0
    while index < count:
        key = _storage_read(_instance_field(left, "_keys"), index)
        if key is not _TABLE_DELETED:
            code = _storage_read(_instance_field(left, "_hashes"), index)
            res_perturb = code & _TABLE_PERTURB_MASK
            res_slot = res_perturb & res_mask
            while _storage_read(res_indices, res_slot) is not _TABLE_EMPTY:
                res_perturb >>= 5
                res_slot = (res_slot * 5 + res_perturb + 1) & res_mask
            _storage_write(res_indices, res_slot, _storage_length(res_keys))
            _storage_append(res_hashes, code)
            _storage_append(res_keys, key)
        index += 1
    right_count = _storage_length(_instance_field(right, "_keys"))
    right_index = 0
    while right_index < right_count:
        key = _storage_read(_instance_field(right, "_keys"), right_index)
        if key is not _TABLE_DELETED:
            code = _storage_read(_instance_field(right, "_hashes"), right_index)
            res_perturb = code & _TABLE_PERTURB_MASK
            res_slot = res_perturb & res_mask
            while True:
                entry = _storage_read(res_indices, res_slot)
                if entry >= 0:
                    if _storage_read(res_hashes, entry) == code:
                        existing = _storage_read(res_keys, entry)
                        if existing is key or existing == key:
                            break
                elif entry is _TABLE_EMPTY:
                    _storage_write(res_indices, res_slot, _storage_length(res_keys))
                    _storage_append(res_hashes, code)
                    _storage_append(res_keys, key)
                    break
                res_perturb >>= 5
                res_slot = (res_slot * 5 + res_perturb + 1) & res_mask
        right_index += 1
    used = _storage_length(res_keys)
    _set_instance_field(result, "_used", used)
    del used
    return result


def set_sub(left, right):
    result = _empty_set_sized(_instance_field(left, "_used"))
    res_indices = _instance_field(result, "_indices")
    res_hashes = _instance_field(result, "_hashes")
    res_keys = _instance_field(result, "_keys")
    res_mask = (_storage_length(res_indices) - 1) & _TABLE_MASK_LIMIT
    count = _storage_length(_instance_field(left, "_keys"))
    index = 0
    while index < count:
        key = _storage_read(_instance_field(left, "_keys"), index)
        if key is not _TABLE_DELETED:
            code = _storage_read(_instance_field(left, "_hashes"), index)
            mask = (
                _storage_length(_instance_field(right, "_indices")) - 1
            ) & _TABLE_MASK_LIMIT
            perturb = code & _TABLE_PERTURB_MASK
            slot = perturb & mask
            while True:
                entry = _storage_read(_instance_field(right, "_indices"), slot)
                if entry >= 0:
                    if _storage_read(_instance_field(right, "_hashes"), entry) == code:
                        existing = _storage_read(_instance_field(right, "_keys"), entry)
                        if existing is key or existing == key:
                            break
                elif entry is _TABLE_EMPTY:
                    res_perturb = code & _TABLE_PERTURB_MASK
                    res_slot = res_perturb & res_mask
                    while _storage_read(res_indices, res_slot) is not _TABLE_EMPTY:
                        res_perturb >>= 5
                        res_slot = (res_slot * 5 + res_perturb + 1) & res_mask
                    _storage_write(res_indices, res_slot, _storage_length(res_keys))
                    _storage_append(res_hashes, code)
                    _storage_append(res_keys, key)
                    break
                perturb >>= 5
                slot = (slot * 5 + perturb + 1) & mask
        index += 1
    used = _storage_length(res_keys)
    _set_instance_field(result, "_used", used)
    del used
    return result


def set_xor(left, right):
    result = _empty_set_sized(
        _instance_field(left, "_used") + _instance_field(right, "_used")
    )
    res_indices = _instance_field(result, "_indices")
    res_hashes = _instance_field(result, "_hashes")
    res_keys = _instance_field(result, "_keys")
    res_mask = (_storage_length(res_indices) - 1) & _TABLE_MASK_LIMIT
    count = _storage_length(_instance_field(left, "_keys"))
    index = 0
    while index < count:
        key = _storage_read(_instance_field(left, "_keys"), index)
        if key is not _TABLE_DELETED:
            code = _storage_read(_instance_field(left, "_hashes"), index)
            mask = (
                _storage_length(_instance_field(right, "_indices")) - 1
            ) & _TABLE_MASK_LIMIT
            perturb = code & _TABLE_PERTURB_MASK
            slot = perturb & mask
            while True:
                entry = _storage_read(_instance_field(right, "_indices"), slot)
                if entry >= 0:
                    if _storage_read(_instance_field(right, "_hashes"), entry) == code:
                        existing = _storage_read(_instance_field(right, "_keys"), entry)
                        if existing is key or existing == key:
                            break
                elif entry is _TABLE_EMPTY:
                    res_perturb = code & _TABLE_PERTURB_MASK
                    res_slot = res_perturb & res_mask
                    while _storage_read(res_indices, res_slot) is not _TABLE_EMPTY:
                        res_perturb >>= 5
                        res_slot = (res_slot * 5 + res_perturb + 1) & res_mask
                    _storage_write(res_indices, res_slot, _storage_length(res_keys))
                    _storage_append(res_hashes, code)
                    _storage_append(res_keys, key)
                    break
                perturb >>= 5
                slot = (slot * 5 + perturb + 1) & mask
        index += 1
    right_count = _storage_length(_instance_field(right, "_keys"))
    right_index = 0
    while right_index < right_count:
        key = _storage_read(_instance_field(right, "_keys"), right_index)
        if key is not _TABLE_DELETED:
            code = _storage_read(_instance_field(right, "_hashes"), right_index)
            mask = (
                _storage_length(_instance_field(left, "_indices")) - 1
            ) & _TABLE_MASK_LIMIT
            perturb = code & _TABLE_PERTURB_MASK
            slot = perturb & mask
            while True:
                entry = _storage_read(_instance_field(left, "_indices"), slot)
                if entry >= 0:
                    if _storage_read(_instance_field(left, "_hashes"), entry) == code:
                        existing = _storage_read(_instance_field(left, "_keys"), entry)
                        if existing is key or existing == key:
                            break
                elif entry is _TABLE_EMPTY:
                    res_perturb = code & _TABLE_PERTURB_MASK
                    res_slot = res_perturb & res_mask
                    while _storage_read(res_indices, res_slot) is not _TABLE_EMPTY:
                        res_perturb >>= 5
                        res_slot = (res_slot * 5 + res_perturb + 1) & res_mask
                    _storage_write(res_indices, res_slot, _storage_length(res_keys))
                    _storage_append(res_hashes, code)
                    _storage_append(res_keys, key)
                    break
                perturb >>= 5
                slot = (slot * 5 + perturb + 1) & mask
        right_index += 1
    used = _storage_length(res_keys)
    _set_instance_field(result, "_used", used)
    del used
    return result


def bytearray_getitem(array, index):
    values = array.values
    index = integer_index(index)
    length = _byte_storage_length(values)
    if index < 0:
        index += length
    if index < 0 or index >= length:
        raise IndexError("sequence index out of range")
    return _byte_storage_read(values, index)


def bytearray_setitem(array, index, item):
    values = array.values
    index = integer_index(index)
    length = _byte_storage_length(values)
    if index < 0:
        index += length
    if index < 0 or index >= length:
        raise IndexError("sequence index out of range")
    item = integer_index(item)
    if item < 0 or item > 255:
        raise ValueError("byte must be in range(0, 256)")
    _byte_storage_write(values, index, item)


def bytearray_append(array, value):
    value = integer_index(value)
    if value < 0 or value > 255:
        raise ValueError("byte must be in range(0, 256)")
    if _byte_storage_exports(array.values):
        raise BufferError("Existing exports of data: object cannot be re-sized")
    _byte_storage_append(array.values, value)


def _getitem_general(value, index):
    method = _getitem_method(value)
    if method is None:
        # Subscripting a class without a metaclass __getitem__ (PEP 560).
        if is_class(value):
            hook = getattr(value, "__class_getitem__", None)
            if hook is not None:
                return hook(index)
            raise TypeError(f"type '{value.__name__}' is not subscriptable")
        raise TypeError(f"'{_type_name(value)}' object is not subscriptable")
    return method(index)


def delitem(value, index):
    cls = _type_of(value)
    if cls is dict:
        if _table_remove(value, index) is _DICT_MISSING:
            raise KeyError(index)
        return
    if cls is list:
        ListStorage.__delitem__(value, index)
        return
    method = _delitem_method(value)
    if method is None:
        if _setitem_method(value) is not None:
            raise AttributeError("__delitem__")
        raise TypeError("object does not support item deletion")
    method(index)


def any(iterable, /):
    owner = _type_of(iterable)
    if owner is bytearray:
        iterable = iterable.values
        owner = list
    if owner is list or owner is tuple or owner is bytes:
        index = 0
        length = _storage_length(iterable)
        while index < length:
            if _storage_read(iterable, index):
                return True
            index += 1
        return False
    for item in iterable:
        if item:
            return True
    return False


def all(iterable, /):
    owner = _type_of(iterable)
    if owner is bytearray:
        iterable = iterable.values
        owner = list
    if owner is list or owner is tuple or owner is bytes:
        index = 0
        length = _storage_length(iterable)
        while index < length:
            if not _storage_read(iterable, index):
                return False
            index += 1
        return True
    for item in iterable:
        if not item:
            return False
    return True


def sum(iterable, /, start=0):
    if is_instance(start, str):
        raise TypeError("sum() can't sum strings [use ''.join(seq) instead]")
    if is_instance(start, bytes):
        raise TypeError("sum() can't sum bytes [use b''.join(seq) instead]")
    if is_instance(start, bytearray):
        raise TypeError("sum() can't sum bytearray [use b''.join(seq) instead]")
    total = start
    comp = 0.0
    in_float = _type_of(total) is float
    for item in iterable:
        if (
            not in_float
            and (_type_of(total) is int or _type_of(total) is bool)
            and _type_of(item) is float
        ):
            total = total + item
            in_float = True
        elif in_float and (
            _type_of(item) is float or _type_of(item) is int or _type_of(item) is bool
        ):
            val = float(item)
            t = total + val
            if t - t == 0.0:
                a_total = -total if total < 0.0 else total
                a_val = -val if val < 0.0 else val
                if a_total >= a_val:
                    comp += (total - t) + val
                else:
                    comp += (val - t) + total
            total = t
        else:
            if in_float and comp != 0.0:
                total = total + comp
                comp = 0.0
            total = total + item
            in_float = _type_of(total) is float
    if in_float and comp != 0.0:
        return total + comp
    return total


class zip:
    def __init__(self, *iterables, strict=False):
        iterators = []
        for iterable in iterables:
            _storage_append(iterators, iterate(iterable))
        self.iterators = iterators
        self.strict = strict

    def __iter__(self):
        return self

    def __next__(self):
        iterators = self.iterators
        if not iterators:
            raise StopIteration
        items = []
        index = 0
        while index < _storage_length(iterators):
            try:
                _storage_append(items, next_item(_storage_read(iterators, index)))
            except StopIteration:
                if self.strict:
                    self._check_exhausted(index)
                raise
            index += 1
        return _storage_tuple(items)

    def _check_exhausted(self, stopped):
        if stopped > 0:
            raise ValueError(f"zip() argument {stopped + 1} is shorter than argument 1")
        index = 1
        while index < _storage_length(self.iterators):
            try:
                next_item(_storage_read(self.iterators, index))
            except StopIteration:
                index += 1
                continue
            raise ValueError(f"zip() argument {index + 1} is longer than argument 1")


class map:
    def __init__(self, function, *iterables, strict=False):
        if not iterables:
            raise TypeError("map() must have at least two arguments.")
        self.function = function
        self.items = zip(*iterables, strict=strict)

    def __iter__(self):
        return self

    def __next__(self):
        return self.function(*next_item(self.items))


class filter:
    def __init__(self, function, iterable, /):
        self.function = function
        self.iterator = iterate(iterable)

    def __iter__(self):
        return self

    def __next__(self):
        while True:
            item = next_item(self.iterator)
            if item if self.function is None else self.function(item):
                return item


class ReverseIterator:
    def __init__(self, sequence, length):
        self.sequence = sequence
        self.index = length - 1

    def __iter__(self):
        return self

    def __next__(self):
        index = self.index
        if index >= 0:
            self.index = index - 1
            try:
                return getitem(self.sequence, index)
            except IndexError, StopIteration:
                self.index = -1
        raise StopIteration

    def __length_hint__(self):
        return self.index + 1 if self.index >= 0 else 0


ReverseIterator.__name__ = "reversed"
ReverseIterator.__qualname__ = "reversed"


def reversed(sequence, /):
    cls = _type_of(sequence)
    if cls is list:
        return ListReverseIterator(sequence)
    if cls is tuple:
        items = list(sequence)
        _reverse_in_place(items)
        return iter(items)
    missing = _binding_missing()
    method = _optional_special(sequence, "__reversed__")
    if method is not missing:
        if method is None:
            raise TypeError(f"'{cls.__name__}' object is not reversible")
        return method()
    len_method = _optional_special(sequence, "__len__")
    getitem_method = _optional_special(sequence, "__getitem__")
    if (
        len_method is missing
        or len_method is None
        or getitem_method is missing
        or getitem_method is None
    ):
        raise TypeError(f"'{cls.__name__}' object is not reversible")
    return ReverseIterator(sequence, len(sequence))


def pow(base, exp, mod=None):
    if mod is None:
        return base**exp
    base_cls = type(base)
    exp_cls = type(exp)
    if (
        base_cls is not exp_cls
        and issubclass(exp_cls, base_cls)
        and class_lookup_default(exp_cls, "__rpow__", None)
        is not class_lookup_default(base_cls, "__rpow__", None)
    ):
        rmethod = getattr(exp_cls, "__rpow__", None)
        if rmethod is not None and rmethod is not getattr(int, "__rpow__", None):
            result = rmethod(exp, base, mod)
            if result is not NotImplemented:
                return result
    if base_cls is not int and base_cls is not bool:
        method = getattr(base_cls, "__pow__", None)
        if method is not None and method is not getattr(int, "__pow__", None):
            result = method(base, exp, mod)
            if result is not NotImplemented:
                return result
    if exp_cls is not base_cls and exp_cls is not int and exp_cls is not bool:
        rmethod = getattr(exp_cls, "__rpow__", None)
        if rmethod is not None and rmethod is not getattr(int, "__rpow__", None):
            result = rmethod(exp, base, mod)
            if result is not NotImplemented:
                return result
    if not (_integer_kind(base) and _integer_kind(exp) and _integer_kind(mod)):
        raise TypeError(
            "pow() 3rd argument not allowed unless all arguments are integers"
        )
    if mod == 0:
        raise ValueError("pow() 3rd argument cannot be 0")
    if exp < 0:
        m = -mod if mod < 0 else mod
        if m == 1:
            return 0
        x0, x1 = 1, 0
        a, b = base % m, m
        while b:
            q = a // b
            a, b = b, a - q * b
            x0, x1 = x1, x0 - q * x1
        if a != 1:
            raise ValueError("base is not invertible for the given modulus")
        base = x0 % mod
        exp = -exp
    result = 1 % mod
    base %= mod
    while exp:
        if exp & 1:
            result = (result * base) % mod
        base = (base * base) % mod
        exp >>= 1
    return result


def _integer_kind(value):
    return isinstance(value, int)


_DIVMOD_MISSING = object()


def divmod(left, right, /):
    lcls, rcls = _type_of(left), _type_of(right)
    if (lcls is int or lcls is bool) and (rcls is int or rcls is bool):
        if right == 0:
            raise ZeroDivisionError("division by zero")
        return (left // right, left % right)
    if (lcls is float or lcls is int or lcls is bool) and (
        rcls is float or rcls is int or rcls is bool
    ):
        if right == 0:
            raise ZeroDivisionError("division by zero")
        return (left // right, left % right)
    left_owner, right_owner = type_of(left), type_of(right)
    forward = class_lookup_default(left_owner, "__divmod__", _DIVMOD_MISSING)
    reflected = class_lookup_default(right_owner, "__rdivmod__", _DIVMOD_MISSING)
    right_first = (
        left_owner is not right_owner
        and reflected is not _DIVMOD_MISSING
        and is_subclass(right_owner, left_owner)
        and reflected
        is not class_lookup_default(left_owner, "__rdivmod__", _DIVMOD_MISSING)
    )
    if right_first:
        result = bind_instance_attribute(reflected, right, right_owner)(left)
        if result is not NotImplemented:
            return result
    if forward is not _DIVMOD_MISSING:
        result = bind_instance_attribute(forward, left, left_owner)(right)
        if result is not NotImplemented:
            return result
    if (
        not right_first
        and left_owner is not right_owner
        and reflected is not _DIVMOD_MISSING
    ):
        result = bind_instance_attribute(reflected, right, right_owner)(left)
        if result is not NotImplemented:
            return result
    raise TypeError(
        "unsupported operand type(s) for divmod(): '"
        + left_owner.__name__
        + "' and '"
        + right_owner.__name__
        + "'"
    )


def round(number, ndigits=None):
    method = getattr(type(number), "__round__", None)
    if method is None:
        raise TypeError(f"type {type(number).__name__} doesn't define __round__ method")
    if ndigits is None:
        return method(number)
    return method(number, ndigits)


def _digits(value, base, prefix):
    if not _text_storage(prefix):
        return ""
    value = integer_index(value)
    if value == 0:
        return prefix + "0"
    sign = "-" if value < 0 else ""
    value = -value if value < 0 else value
    digits = []
    while value:
        _storage_append(digits, "0123456789abcdef"[value % base])
        value //= base
    _reverse_in_place(digits)
    return sign + prefix + "".join(digits)


def hex(number, /):
    return _digits(number, 16, "0x")


def oct(number, /):
    return _digits(number, 8, "0o")


def bin(number, /):
    return _digits(number, 2, "0b")


def callable(obj, /):
    if is_class(obj):
        return True
    kind = _type_name(obj)
    if kind in (
        "function",
        "builtin_function_or_method",
        "wrapper_descriptor",
        "method",
    ):
        return True
    missing = _binding_missing()
    return class_lookup_default(type_of(obj), "__call__", missing) is not missing


def id(obj, /):
    return identity(obj)


def sorted(iterable, /, *, key=None, reverse=False):
    result = _make_list(iterable)
    result.sort(key=key, reverse=reverse)
    return result


def dir(*args):
    if not args:
        raise NotImplementedError("dir() requires compiler local-variable snapshots")
    if len(args) > 1:
        raise TypeError(f"dir expected at most 1 argument, got {len(args)}")
    method = _dir_method(args[0])
    return sorted(method())


def append_argument(positional, value):
    _storage_append(positional, value)


def _length_hint(iterable):
    missing = _binding_missing()
    method = _optional_special(iterable, "__len__")
    if method is not missing and method is not None:
        try:
            size = _index_integer(method())
        except TypeError:
            pass
        else:
            if size < 0:
                raise ValueError("__len__() should return >= 0")
            return
    method = _optional_special(iterable, "__length_hint__")
    if method is not missing and method is not None:
        try:
            hint = method()
        except TypeError:
            return
        if hint is NotImplemented:
            return
        if not is_instance(hint, int):
            raise TypeError(
                f"__length_hint__ must be an integer, not {type_of(hint).__name__}"
            )
        hint = _index_integer(hint)
        if hint < 0:
            raise ValueError("__length_hint__() should return >= 0")


def extend_arguments(positional, iterable):
    owner = type_of(iterable)
    if owner is bytearray:
        iterable = iterable.values
        owner = list
    if owner is tuple or owner is list or owner is bytes:
        # Exact element storage is copied by index: the iteration protocol
        # would end with a StopIteration that costs an exception object.
        count = _storage_length(iterable)
        index = 0
        while index < count:
            _storage_append(positional, _storage_read(iterable, index))
            index += 1
        return
    iterator = iterate(iterable)
    _length_hint(iterable)
    while True:
        try:
            value = next_item(iterator)
        except StopIteration:
            return
        _storage_append(positional, value)


def _argument_tuple(iterable):
    """The positional arguments ``f(*iterable)`` passes, as the tuple the
    call primitive reads. An exact tuple is already that tuple."""
    owner = type_of(iterable)
    if owner is tuple:
        return iterable
    if owner is bytearray:
        return _storage_tuple(
            _byte_sublist(iterable.values, 0, _storage_length(iterable.values))
        )
    if owner is list:
        return _storage_tuple(iterable)
    items = []
    extend_arguments(items, iterable)
    return _storage_tuple(items)


def add_keyword(keywords, name, value):
    if name in keywords:
        raise TypeError(f"got multiple values for keyword argument '{name}'")
    keywords[name] = value


def merge_keywords(keywords, mapping):
    keys = getattr(mapping, "keys", None)
    if keys is None:
        raise TypeError(
            f"argument after ** must be a mapping, not {_type_name(mapping)}"
        )
    for name in keys():
        if not _text_storage(name):
            raise TypeError("keywords must be strings")
        add_keyword(keywords, name, mapping[name])


def tuple_from_list(items):
    return _storage_tuple(items)


def dict_merge(mapping, other):
    if is_instance(other, dict):
        mapping.update(other)
        return
    keys = getattr(other, "keys", None)
    if keys is None:
        raise TypeError(f"'{_type_name(other)}' object is not a mapping")
    for key in keys():
        mapping[key] = other[key]


def call_expanded_positional(function, arguments):
    """``f(*arguments)`` without keyword arguments: no keyword mapping is
    built, and a tuple of arguments is passed through as it is."""
    return call_with(function, _argument_tuple(arguments), (), ())


def call_expanded(function, positional, keywords):
    names, values = [], []
    for name in keywords:
        _storage_append(names, name)
        _storage_append(values, keywords[name])
    return call_with(
        function,
        _storage_tuple(positional),
        _storage_tuple(names),
        _storage_tuple(values),
    )


def delete_attribute(value, name):
    delattr(value, name)


def make_generator(function, arguments, name, qualname):
    return _generator(function, arguments, name, qualname)


def yield_from(iterable):
    return _yield_from(iterable)


def make_coroutine(function, arguments, name, qualname):
    return _coroutine(function, arguments, name, qualname)


def make_async_generator(function, arguments, name, qualname):
    return _async_generator(function, arguments, name, qualname)


def await_value(awaitable):
    return _await_value(awaitable)


def async_generator_yield(value):
    return _async_generator_yield(value)


def _metaclass(bases, explicit):
    """The most derived metaclass among ``explicit`` and the bases' classes."""
    winner = type if explicit is None else explicit
    if not is_class(winner):
        return winner
    for base in bases:
        candidate = type(base)
        if issubclass(candidate, winner):
            winner = candidate
        elif not issubclass(winner, candidate):
            raise TypeError(
                "metaclass conflict: the metaclass of a derived class must be a "
                "(non-strict) subclass of the metaclasses of all its bases"
            )
    return winner


def _direct_bases(bases):
    for base in bases:
        if type(base) is not type:
            return False
    return True


def _resolved_bases(bases):
    """Expand non-class bases through ``__mro_entries__`` (PEP 560).

    Returns the resolved bases and whether any base was replaced; the
    caller keeps the original tuple for ``__orig_bases__`` in that case."""
    resolved = []
    changed = False
    for base in bases:
        if is_class(base):
            _storage_append(resolved, base)
            continue
        method = getattr(base, "__mro_entries__", None)
        if method is None:
            _storage_append(resolved, base)
            continue
        entries = method(bases)
        if not _tuple_storage(entries):
            raise TypeError("__mro_entries__ must return a tuple")
        changed = True
        for entry in entries:
            _storage_append(resolved, entry)
    return _storage_tuple(resolved), changed


def prepare_class(name, bases, keywords):
    """The namespace a class body runs against (``__build_class__``)."""
    if keywords is None and len(bases) <= 1 and _direct_bases(bases):
        return new_namespace()
    keywords = {} if keywords is None else keywords
    explicit = keywords.get("metaclass")
    bases, _changed = _resolved_bases(bases)
    metaclass = _metaclass(bases, explicit)
    if metaclass is type:
        return new_namespace()
    others = {}
    for key in keywords:
        if key != "metaclass":
            others[key] = keywords[key]
    prepare = getattr(metaclass, "__prepare__", None)
    if prepare is None:
        return {}
    return prepare(name, bases, **others)


def build_class(name, bases, namespace, keywords, fields):
    """Create a class through its metaclass after its body has run."""
    keywords = {} if keywords is None else keywords
    explicit = keywords.get("metaclass")
    original = bases
    bases, changed = _resolved_bases(bases)
    metaclass = _metaclass(bases, explicit)
    others = {}
    for key in keywords:
        if key != "metaclass":
            others[key] = keywords[key]
    items = namespace_items(namespace)
    if items is not None:
        namespace = {}
        for key, value in items:
            namespace[key] = value
    if changed:
        namespace["__orig_bases__"] = original
    if is_class(metaclass):
        namespace["__mule_fields__"] = fields
    return metaclass(name, bases, namespace, **others)


def build_template(parts):
    return _build_template(parts)


def make_interpolation(value, expression, conversion, format_spec):
    return _Interpolation(value, expression, conversion, format_spec)


def make_slice(start, stop, step):
    value = _allocate_instance(slice)
    set_instance_field(value, "start", start)
    set_instance_field(value, "stop", stop)
    set_instance_field(value, "step", step)
    return value


def build_dict(flat):
    return _build_dict(flat)


def build_set(items):
    return _build_set(items)


def _list_set_slice(target, index, value):
    items = value if type_of(value) is list and value is not target else list(value)
    start, stop, step, count = positions(index, _storage_length(target))
    if step == 1:
        stop = stop if stop > start else start
        items_length = _storage_length(items)
        if stop - start == items_length:
            offset = 0
            while offset < items_length:
                _storage_write(target, start + offset, _storage_read(items, offset))
                offset += 1
            return
        _storage_delete(target, start, stop)
        offset = 0
        while offset < items_length:
            _storage_insert(target, start + offset, _storage_read(items, offset))
            offset += 1
        return
    if _storage_length(items) != count:
        raise ValueError(
            f"attempt to assign sequence of size {_storage_length(items)} "
            f"to extended slice of size {count}"
        )
    offset = 0
    while offset < count:
        _storage_write(target, start + offset * step, _storage_read(items, offset))
        offset += 1


def setitem(value, index, item):
    cls = _type_of(value)
    if cls is dict:
        _table_insert(value, index, item)
        return
    if cls is list:
        if is_slice(index):
            _list_set_slice(value, index, item)
            return
        _storage_write(value, storage_index(value, index), item)
        return
    if cls is bytearray:
        if is_slice(index):
            bytearray.__setitem__(value, index, item)
            return
        values = value.values
        _storage_write(values, storage_index(values, index), _byte_value(item))
        return
    method = _setitem_method(value)
    if method is None:
        if _delitem_method(value) is not None:
            raise AttributeError("__setitem__")
        raise TypeError("object does not support item assignment")
    method(index, item)


def storage_index(value, index):
    if _type_of(index) is int:
        index = _integer_storage(index)
    elif _index_method(index) is None:
        index = _integer_storage(index)
    else:
        index = _index_integer(index)
    length = _storage_length(value)
    if index < 0:
        index += length
    if index < 0 or index >= length:
        raise IndexError("sequence index out of range")
    return index


class SequenceIterator:
    def __init__(self, sequence):
        self.sequence = sequence
        self.index = 0
        self.exhausted = False

    def __iter__(self):
        return self

    def __next__(self):
        if self.exhausted:
            raise StopIteration
        index = self.index
        try:
            item = getitem(self.sequence, index)
        except IndexError, StopIteration:
            self.exhausted = True
            raise StopIteration
        self.index = index + 1
        return item

    def __length_hint__(self):
        if self.exhausted:
            return 0
        try:
            remaining = len(self.sequence) - self.index
        except TypeError:
            return 0
        return remaining if remaining > 0 else 0


class StorageIterator(SequenceIterator):
    """Builtin sequence iteration reads storage, independently of __getitem__."""

    def __init__(self, sequence):
        self.sequence = sequence
        self.index = 0
        self.exhausted = False

    def __next__(self):
        if self.exhausted:
            raise StopIteration
        index = self.index
        if index >= _storage_length(self.sequence):
            self.exhausted = True
            raise StopIteration
        item = _storage_read(self.sequence, index)
        self.index = index + 1
        return item

    def __length_hint__(self):
        if self.exhausted:
            return 0
        remaining = _storage_length(self.sequence) - self.index
        return remaining if remaining > 0 else 0


class TupleIterator(StorageIterator):
    pass


class BytesIterator(StorageIterator):
    pass


class ListReverseIterator:
    def __init__(self, sequence):
        self.sequence = sequence
        self.index = _storage_length(sequence) - 1

    def __iter__(self):
        return self

    def __next__(self):
        index = self.index
        if index >= 0 and index < _storage_length(self.sequence):
            self.index = index - 1
            return _storage_read(self.sequence, index)
        self.index = -1
        raise StopIteration

    def __length_hint__(self):
        if self.index < 0 or self.index >= _storage_length(self.sequence):
            return 0
        return self.index + 1


SequenceIterator.__name__ = "iterator"
SequenceIterator.__qualname__ = "iterator"
StorageIterator.__name__ = "list_iterator"
StorageIterator.__qualname__ = "list_iterator"
TupleIterator.__name__ = "tuple_iterator"
TupleIterator.__qualname__ = "tuple_iterator"
BytesIterator.__name__ = "bytes_iterator"
BytesIterator.__qualname__ = "bytes_iterator"
ListReverseIterator.__name__ = "list_reverseiterator"
ListReverseIterator.__qualname__ = "list_reverseiterator"


def _selected(sequence, bounds):
    """The elements a slice selects, in order, as a new list."""
    start, stop, step, count = positions(bounds, _storage_length(sequence))
    return _selected_positions(sequence, start, step, count)


def _selected_positions(sequence, start, step, count):
    result = []
    index = 0
    while index < count:
        _storage_append(result, _storage_read(sequence, start + index * step))
        index += 1
    return result


def _list_position(sequence, index):
    """An insertion position: negative indexes count from the end, then clamp."""
    length = _storage_length(sequence)
    index = integer_index(index)
    if index < 0:
        index += length
        if index < 0:
            index = 0
    elif index > length:
        index = length
    return index


def _require_list(value, operation):
    if _type_name(value) != "list":
        raise TypeError(
            f'can only {operation} list (not "{_type_name(value)}") to list'
        )


def _list_fill(item, count):
    """Construct singleton repetition from an exact integer count."""
    count = _integer_storage(count)
    if count >= (1 << 63) or count < -(1 << 63):
        raise OverflowError("cannot fit 'int' into an index-sized integer")
    result = []
    if count <= 0:
        return result
    # Object pointers occupy eight bytes. The allocation's byte size must fit
    # a signed machine word before the storage allocator receives it.
    if count >= (1 << 60):
        raise MemoryError("sequence allocation failed")
    _storage_reserve(result, count)
    index = 0
    while index < count:
        _storage_append(result, item)
        index += 1
    return result


def _repeat(sequence, count):
    count = integer_index(count)
    if count >= (1 << 63) or count < -(1 << 63):
        raise OverflowError("cannot fit 'int' into an index-sized integer")
    result = []
    if count <= 0:
        return result
    length = _storage_length(sequence)
    if length == 0:
        return result
    if length == 1:
        item = _storage_read(sequence, 0)
        while count > 0:
            _storage_append(result, item)
            count -= 1
        return result
    while count > 0:
        index = 0
        while index < length:
            _storage_append(result, _storage_read(sequence, index))
            index += 1
        count -= 1
    return result


def _bytes_repeat(sequence, count):
    count = integer_index(count)
    if count >= (1 << 63) or count < -(1 << 63):
        raise OverflowError("cannot fit 'int' into an index-sized integer")
    length = _storage_length(sequence)
    if length and count > ((1 << 63) - 1) // length:
        raise OverflowError("repeated bytes are too long")
    if count == 1:
        return _bytes_slice_result(sequence, 0, length)
    return bytes_from_values(_repeat(sequence, count))


def _merge_sort(items, keys):
    """Stable merge sort of ``items`` by ``keys``, comparing with ``<`` only."""
    length = _storage_length(items)
    width = 1
    while width < length:
        sorted_items, sorted_keys = [], []
        low = 0
        while low < length:
            middle = low + width if low + width < length else length
            high = low + 2 * width if low + 2 * width < length else length
            left, right = low, middle
            while left < middle and right < high:
                if _storage_read(keys, right) < _storage_read(keys, left):
                    _storage_append(sorted_items, _storage_read(items, right))
                    _storage_append(sorted_keys, _storage_read(keys, right))
                    right += 1
                else:
                    _storage_append(sorted_items, _storage_read(items, left))
                    _storage_append(sorted_keys, _storage_read(keys, left))
                    left += 1
            while left < middle:
                _storage_append(sorted_items, _storage_read(items, left))
                _storage_append(sorted_keys, _storage_read(keys, left))
                left += 1
            while right < high:
                _storage_append(sorted_items, _storage_read(items, right))
                _storage_append(sorted_keys, _storage_read(keys, right))
                right += 1
            low = high
        items, keys = sorted_items, sorted_keys
        width *= 2
    return items


def _reverse_in_place(sequence):
    low, high = 0, _storage_length(sequence) - 1
    while low < high:
        a = _storage_read(sequence, low)
        _storage_write(sequence, low, _storage_read(sequence, high))
        _storage_write(sequence, high, a)
        low += 1
        high -= 1


_repr_active_sequences = []


def _sequence_representation(value, opening, closing, singleton_comma):
    index = 0
    while index < _storage_length(_repr_active_sequences):
        if _storage_read(_repr_active_sequences, index) is value:
            return opening + "..." + closing
        index += 1
    _storage_append(_repr_active_sequences, value)
    try:
        result = opening
        index = 0
        while index < _storage_length(value):
            if index:
                result += ", "
            result += repr(_storage_read(value, index))
            index += 1
        if singleton_comma and index == 1:
            result += ","
        return result + closing
    finally:
        end = _storage_length(_repr_active_sequences)
        _storage_delete(_repr_active_sequences, end - 1, end)


class _OrderedSequence:
    """Equality and lexicographic ordering shared by list and tuple."""

    __slots__ = ()

    def __eq__(self, other):
        if not _storage_compatible(self, other):
            return NotImplemented
        return sequence_equal(self, other)

    def __ne__(self, other):
        if not _storage_compatible(self, other):
            return NotImplemented
        return not sequence_equal(self, other)

    def __lt__(self, other):
        if not _storage_compatible(self, other):
            return NotImplemented
        return sequence_lt(self, other)

    def __le__(self, other):
        if not _storage_compatible(self, other):
            return NotImplemented
        return sequence_le(self, other)

    def __gt__(self, other):
        if not _storage_compatible(self, other):
            return NotImplemented
        return sequence_gt(self, other)

    def __ge__(self, other):
        if not _storage_compatible(self, other):
            return NotImplemented
        return sequence_ge(self, other)

    def __iter__(self):
        cls = TupleIterator if _tuple_storage(self) else StorageIterator
        iterator = _allocate_instance(cls)
        iterator.sequence = self
        iterator.index = 0
        iterator.exhausted = False
        return iterator

    def count(self, value):
        found = 0
        index = 0
        while index < _storage_length(self):
            item = _storage_read(self, index)
            if item is value or item == value:
                found += 1
            index += 1
        return found

    def index(self, value, start=0, stop=None):
        length = _storage_length(self)
        start = _list_position(self, start)
        stop = length if stop is None else _list_position(self, stop)
        while start < stop:
            item = _storage_read(self, start)
            if item is value or item == value:
                return start
            start += 1
        raise ValueError(f"{value!r} is not in {_type_name(self)}")


class ListStorage(_OrderedSequence):
    __slots__ = ()

    def __len__(self):
        if not is_instance(self, list):
            raise TypeError(
                "descriptor '__len__' requires a 'list' object but received a '"
                + _type_name(self)
                + "'"
            )
        return _storage_length(self)

    def __repr__(self):
        return _sequence_representation(self, "[", "]", False)

    def __getitem__(self, index):
        if is_slice(index):
            return _selected(self, index)
        return _storage_read(self, storage_index(self, index))

    def __setitem__(self, index, value):
        if not is_slice(index):
            _storage_write(self, storage_index(self, index), value)
            return
        _list_set_slice(self, index, value)

    def __delitem__(self, index):
        if not is_slice(index):
            position = storage_index(self, index)
            _storage_delete(self, position, position + 1)
            return
        start, stop, step, count = positions(index, _storage_length(self))
        if step < 0:
            start, step = start + (count - 1) * step, -step
        offset = count
        while offset > 0:
            offset -= 1
            position = start + offset * step
            _storage_delete(self, position, position + 1)

    def __add__(self, other):
        _require_list(other, "concatenate")
        result = self.copy()
        result.extend(other)
        return result

    def __iadd__(self, other):
        self.extend(other)
        return self

    def __mul__(self, count):
        return _repeat(self, count)

    def __rmul__(self, count):
        return _repeat(self, count)

    def __imul__(self, count):
        repeated = _repeat(self, count)
        _storage_delete(self, 0, _storage_length(self))
        self.extend(repeated)
        return self

    def __reversed__(self):
        return ListReverseIterator(self)

    def append(self, value, /):
        _storage_append(self, value)

    def extend(self, iterable):
        if iterable is self:
            iterable = self.copy()
        extend_arguments(self, iterable)

    def insert(self, index, value):
        _storage_insert(self, _list_position(self, index), value)

    def pop(self, index=-1, /):
        length = _storage_length(self)
        if length == 0:
            raise IndexError("pop from empty list")
        index = integer_index(index)
        if index < 0:
            index += length
        if index < 0 or index >= length:
            raise IndexError("pop index out of range")
        item = _storage_read(self, index)
        _storage_delete(self, index, index + 1)
        return item

    def remove(self, value):
        index = 0
        while index < _storage_length(self):
            item = _storage_read(self, index)
            if item is value or item == value:
                _storage_delete(self, index, index + 1)
                return
            index += 1
        raise ValueError("list.remove(x): x not in list")

    def clear(self):
        _storage_delete(self, 0, _storage_length(self))

    def copy(self):
        result = []
        index = 0
        while index < _storage_length(self):
            _storage_append(result, _storage_read(self, index))
            index += 1
        return result

    def reverse(self):
        _reverse_in_place(self)

    def sort(self, *, key=None, reverse=False):
        items = self.copy()
        _storage_delete(self, 0, _storage_length(self))
        try:
            if reverse:
                _reverse_in_place(items)
            keys = items
            if key is not None:
                keys = []
                index = 0
                length = _storage_length(items)
                while index < length:
                    _storage_append(keys, key(_storage_read(items, index)))
                    index += 1
            items = _merge_sort(items, keys)
            if reverse:
                _reverse_in_place(items)
            if _storage_length(self) != 0:
                raise ValueError("list modified during sort")
        finally:
            _storage_delete(self, 0, _storage_length(self))
            self.extend(items)


class TupleStorage(_OrderedSequence):
    __slots__ = ()

    def __len__(self):
        if not is_instance(self, tuple):
            raise TypeError(
                "descriptor '__len__' requires a 'tuple' object but received a '"
                + _type_name(self)
                + "'"
            )
        return _storage_length(self)

    def __hash__(self):
        return _tuple_hash(self)

    def __repr__(self):
        return _sequence_representation(self, "(", ")", True)

    def __getitem__(self, index):
        if is_slice(index):
            return _storage_tuple(_selected(self, index))
        return _storage_read(self, storage_index(self, index))

    def __add__(self, other):
        if not _tuple_storage(other):
            raise TypeError(
                f'can only concatenate tuple (not "{_type_name(other)}") to tuple'
            )
        result = []
        index = 0
        left_length = _storage_length(self)
        while index < left_length:
            _storage_append(result, _storage_read(self, index))
            index += 1
        index = 0
        right_length = _storage_length(other)
        while index < right_length:
            _storage_append(result, _storage_read(other, index))
            index += 1
        return _storage_tuple(result)

    def __mul__(self, count):
        return _storage_tuple(_repeat(self, count))

    def __rmul__(self, count):
        return _storage_tuple(_repeat(self, count))


def _byte_sequence_values(value):
    if _integer_storage_kind(value) or _index_method(value) is not None:
        return [_byte_value(value)]
    return _bytes_like_values(value)


def _bytes_like_values(value):
    if is_instance(value, bytes):
        length = _storage_length(value)
        values = []
        index = 0
        while index < length:
            _storage_append(values, _storage_read(value, index))
            index += 1
        return values
    if is_instance(value, bytearray):
        source_values = value.values
        length = _storage_length(source_values)
        values = []
        index = 0
        while index < length:
            _storage_append(values, _storage_read(source_values, index))
            index += 1
        return values
    if is_instance(value, memoryview):
        if not value.c_contiguous:
            raise BufferError("memoryview: underlying buffer is not C-contiguous")
        return _bytes_like_values(value.tobytes())
    raise TypeError("a bytes-like object is required, not '" + _type_name(value) + "'")


def _byte_in_values(values, byte):
    index = 0
    length = _storage_length(values)
    while index < length:
        if _storage_read(values, index) == byte:
            return True
        index += 1
    return False


def _is_ascii_whitespace(byte):
    return byte == 32 or (9 <= byte <= 13)


def _bytes_translated(values, table, delete):
    table_values = None
    if table is not None:
        table_values = _bytes_like_values(table)
        if _storage_length(table_values) != 256:
            raise ValueError("translation table must be 256 characters long")
    delete_values = _bytes_like_values(delete)
    result = []
    index = 0
    while index < _storage_length(values):
        value = _storage_read(values, index)
        if not _byte_in_values(delete_values, value):
            if table_values is not None:
                value = _storage_read(table_values, value)
            _storage_append(result, value)
        index += 1
    return result


def _make_bytes_translation_table(frm, to):
    from_values = _bytes_like_values(frm)
    to_values = _bytes_like_values(to)
    if _storage_length(from_values) != _storage_length(to_values):
        raise ValueError("maketrans arguments must have same length")
    result = []
    value = 0
    while value < 256:
        _storage_append(result, value)
        value += 1
    index = 0
    while index < _storage_length(from_values):
        _storage_write(
            result, _storage_read(from_values, index), _storage_read(to_values, index)
        )
        index += 1
    return bytes_from_values(result)


def _bytes_bounds(length, start, end):
    start = integer_index(start)
    if start < 0:
        start += length
        if start < 0:
            start = 0
    elif start > length:
        start = length + 1
    if end is None:
        end = length
    else:
        end = integer_index(end)
        if end > length:
            end = length
        elif end < 0:
            end += length
            if end < 0:
                end = 0
    return start, end


def _bytes_match(values, needle, position):
    offset = 0
    while offset < _storage_length(needle):
        if _storage_read(values, position + offset) != _storage_read(needle, offset):
            return False
        offset += 1
    return True


def _bytes_find_values(values, needle, start, end):
    if _storage_length(needle) == 0:
        return start if start <= end else -1
    position = start
    last = end - _storage_length(needle)
    while position <= last:
        if _bytes_match(values, needle, position):
            return position
        position += 1
    return -1


def _bytes_rfind_values(values, needle, start, end):
    if _storage_length(needle) == 0:
        return end if start <= end else -1
    position = end - _storage_length(needle)
    while position >= start:
        if _bytes_match(values, needle, position):
            return position
        position -= 1
    return -1


def _bytes_find(values, sub, start, end):
    needle = _byte_sequence_values(sub)
    start, end = _bytes_bounds(_storage_length(values), start, end)
    return _bytes_find_values(values, needle, start, end)


def _bytes_rfind(values, sub, start, end):
    needle = _byte_sequence_values(sub)
    start, end = _bytes_bounds(_storage_length(values), start, end)
    return _bytes_rfind_values(values, needle, start, end)


def _bytes_count(values, sub, start, end):
    needle = _byte_sequence_values(sub)
    start, end = _bytes_bounds(_storage_length(values), start, end)
    if _storage_length(needle) == 0:
        return end - start + 1 if start <= end else 0
    found = 0
    position = start
    while position + _storage_length(needle) <= end:
        if _bytes_match(values, needle, position):
            found += 1
            position += _storage_length(needle)
        else:
            position += 1
    return found


def _bytes_startswith(values, prefix, start, end):
    if _tuple_storage(prefix):
        index = 0
        while index < _storage_length(prefix):
            if _bytes_startswith(values, _storage_read(prefix, index), start, end):
                return True
            index += 1
        return False
    needle = _byte_sequence_values(prefix)
    start, end = _bytes_bounds(_storage_length(values), start, end)
    if end - start < _storage_length(needle):
        return False
    return _bytes_match(values, needle, start)


def _bytes_endswith(values, suffix, start, end):
    if _tuple_storage(suffix):
        index = 0
        while index < _storage_length(suffix):
            if _bytes_endswith(values, _storage_read(suffix, index), start, end):
                return True
            index += 1
        return False
    needle = _byte_sequence_values(suffix)
    start, end = _bytes_bounds(_storage_length(values), start, end)
    if end - start < _storage_length(needle):
        return False
    return _bytes_match(values, needle, end - _storage_length(needle))


def _bytes_strip_bounds(values, chars, left, right):
    chars_values = None if chars is None else _bytes_like_values(chars)
    start = 0
    end = _storage_length(values)
    if left:
        while start < end:
            byte = _storage_read(values, start)
            matched = (
                _is_ascii_whitespace(byte)
                if chars_values is None
                else _byte_in_values(chars_values, byte)
            )
            if not matched:
                break
            start += 1
    if right:
        while end > start:
            byte = _storage_read(values, end - 1)
            matched = (
                _is_ascii_whitespace(byte)
                if chars_values is None
                else _byte_in_values(chars_values, byte)
            )
            if not matched:
                break
            end -= 1
    return start, end


def _bytes_removeprefix_bounds(values, prefix):
    needle = _bytes_like_values(prefix)
    size = _storage_length(needle)
    length = _storage_length(values)
    if size <= length and _bytes_match(values, needle, 0):
        return size, length
    return 0, length


def _bytes_removesuffix_bounds(values, suffix):
    needle = _bytes_like_values(suffix)
    size = _storage_length(needle)
    length = _storage_length(values)
    if size > 0 and size <= length and _bytes_match(values, needle, length - size):
        return 0, length - size
    return 0, length


def _bytes_split_spans(values, sep, maxsplit, from_right):
    maxsplit = integer_index(maxsplit)
    length = _storage_length(values)
    spans = []
    if sep is None:
        if not from_right:
            index = 0
            while maxsplit != 0:
                while index < length and _is_ascii_whitespace(
                    _storage_read(values, index)
                ):
                    index += 1
                if index == length:
                    return spans
                start = index
                index += 1
                while index < length and not _is_ascii_whitespace(
                    _storage_read(values, index)
                ):
                    index += 1
                _storage_append(spans, (start, index))
                maxsplit -= 1
            while index < length and _is_ascii_whitespace(_storage_read(values, index)):
                index += 1
            if index != length:
                _storage_append(spans, (index, length))
            return spans
        index = length - 1
        while maxsplit != 0:
            while index >= 0 and _is_ascii_whitespace(_storage_read(values, index)):
                index -= 1
            if index < 0:
                _reverse_in_place(spans)
                return spans
            end = index + 1
            index -= 1
            while index >= 0 and not _is_ascii_whitespace(_storage_read(values, index)):
                index -= 1
            _storage_append(spans, (index + 1, end))
            maxsplit -= 1
        while index >= 0 and _is_ascii_whitespace(_storage_read(values, index)):
            index -= 1
        if index >= 0:
            _storage_append(spans, (0, index + 1))
        _reverse_in_place(spans)
        return spans
    needle = _bytes_like_values(sep)
    size = _storage_length(needle)
    if size == 0:
        raise ValueError("empty separator")
    if not from_right:
        start = 0
        position = _bytes_find_values(values, needle, 0, length)
        while position >= 0 and maxsplit != 0:
            _storage_append(spans, (start, position))
            start = position + size
            maxsplit -= 1
            position = _bytes_find_values(values, needle, start, length)
        _storage_append(spans, (start, length))
        return spans
    end = length
    position = _bytes_rfind_values(values, needle, 0, end)
    while position >= 0 and maxsplit != 0:
        _storage_append(spans, (position + size, end))
        end = position
        maxsplit -= 1
        position = _bytes_rfind_values(values, needle, 0, end)
    _storage_append(spans, (0, end))
    _reverse_in_place(spans)
    return spans


def _bytes_splitlines_spans(values, keepends):
    keep = integer_index(keepends) != 0
    spans = []
    length = _storage_length(values)
    index = 0
    while index < length:
        start = index
        while index < length:
            byte = _storage_read(values, index)
            if byte == 10 or byte == 13:
                break
            index += 1
        line_end = index
        if index < length:
            if (
                _storage_read(values, index) == 13
                and index + 1 < length
                and _storage_read(values, index + 1) == 10
            ):
                index += 2
            else:
                index += 1
            if keep:
                line_end = index
        _storage_append(spans, (start, line_end))
    return spans


def _bytes_replaced(values, old, new, count):
    old_values = _bytes_like_values(old)
    new_values = _bytes_like_values(new)
    count = integer_index(count)
    length = _storage_length(values)
    if count < 0:
        count = length + 1
    if count == 0:
        return None
    old_size = _storage_length(old_values)
    result = []
    if old_size == 0:
        if _storage_length(new_values) == 0:
            return None
        index = 0
        while index < length and count > 0:
            for item in new_values:
                _storage_append(result, item)
            _storage_append(result, _storage_read(values, index))
            count -= 1
            index += 1
        if count > 0:
            for item in new_values:
                _storage_append(result, item)
        while index < length:
            _storage_append(result, _storage_read(values, index))
            index += 1
        return result
    position = _bytes_find_values(values, old_values, 0, length)
    if position < 0:
        return None
    start = 0
    while position >= 0 and count > 0:
        while start < position:
            _storage_append(result, _storage_read(values, start))
            start += 1
        for item in new_values:
            _storage_append(result, item)
        start = position + old_size
        count -= 1
        position = _bytes_find_values(values, old_values, start, length)
    while start < length:
        _storage_append(result, _storage_read(values, start))
        start += 1
    return result


def _bytes_joined_values(separator_values, iterable):
    values = []
    count = 0
    for item in iterable:
        try:
            view = memoryview(item)
        except TypeError:
            raise TypeError(
                "sequence item "
                + str(count)
                + ": expected a bytes-like object, "
                + type(item).__name__
                + " found"
            ) from None
        with view:
            if not view.c_contiguous:
                raise TypeError(
                    "sequence item "
                    + str(count)
                    + ": expected a bytes-like object, "
                    + type(item).__name__
                    + " found"
                )
            data = view.tobytes()
        if count:
            position = 0
            while position < _storage_length(separator_values):
                _storage_append(values, _storage_read(separator_values, position))
                position += 1
        position = 0
        while position < _storage_length(data):
            _storage_append(values, _storage_read(data, position))
            position += 1
        count += 1
    return values


_HEX_MISSING = _binding_missing()


def _hex_bytes(values, sep, bytes_per_sep):
    group = integer_index(bytes_per_sep)
    if group < -(1 << 31) or group >= (1 << 31):
        raise OverflowError("Python int too large to convert to C int")
    separator = ""
    if sep is not _HEX_MISSING:
        try:
            size = len(sep)
        except TypeError:
            missing = _binding_missing()
            if class_lookup_default(type_of(sep), "__len__", missing) is missing:
                raise TypeError(
                    "object of type '" + type_of(sep).__name__ + "' has no len()"
                ) from None
            raise
        if size != 1:
            raise ValueError("sep must be length 1.")
        if isinstance(sep, str):
            character = ord(sep)
        elif isinstance(sep, bytes):
            character = _storage_read(sep, 0)
        else:
            raise TypeError("sep must be str or bytes.")
        if character > 127:
            raise ValueError("sep must be ASCII.")
        separator = chr(character)
    digits = "0123456789abcdef"
    parts = []
    length = _storage_length(values)
    index = 0
    while index < length:
        if separator and index and group:
            boundary = (
                (length - index) % group == 0 if group > 0 else index % -group == 0
            )
            if boundary:
                _storage_append(parts, separator)
        value = _storage_read(values, index)
        _storage_append(parts, digits[value >> 4])
        _storage_append(parts, digits[value & 15])
        index += 1
    return "".join(parts)


def _hex_nibble(code):
    if 48 <= code <= 57:
        return code - 48
    if 65 <= code <= 70:
        return code - 55
    if 97 <= code <= 102:
        return code - 87
    return -1


def _fromhex_values(string):
    if not _text_storage(string):
        raise TypeError(
            f"fromhex() argument must be str, not {type_of(string).__name__}"
        )
    length = _text_length(string)
    values = []
    index = 0
    while index < length:
        code = _text_codepoint(string, index)
        if code in (9, 10, 11, 12, 13, 32):
            index += 1
            continue
        high = _hex_nibble(code)
        if high < 0:
            raise ValueError(
                f"non-hexadecimal number found in fromhex() arg at position {index}"
            )
        index += 1
        if index >= length:
            raise ValueError(
                f"non-hexadecimal number found in fromhex() arg at position {index}"
            )
        low = _hex_nibble(_text_codepoint(string, index))
        if low < 0:
            raise ValueError(
                f"non-hexadecimal number found in fromhex() arg at position {index}"
            )
        _storage_append(values, (high << 4) | low)
        index += 1
    return values


def _byte_padding(values, width, fillchar, alignment, method):
    width = _index_integer(width)
    if not -(1 << 63) <= width < 1 << 63:
        raise OverflowError("Python int too large to convert to C ssize_t")
    if is_instance(fillchar, bytes):
        fill_values = fillchar
    elif is_instance(fillchar, bytearray):
        fill_values = fillchar.values
    else:
        kind = "None" if fillchar is None else _type_name(fillchar)
        raise TypeError(
            f"{method}() argument 2 must be a byte string of length 1, not {kind}"
        )
    fill_length = _storage_length(fill_values)
    if fill_length != 1:
        raise TypeError(
            f"{method}(): argument 2 must be a byte string of length 1, not a {_type_name(fillchar)} object of length {fill_length}"
        )
    fill = _storage_read(fill_values, 0)
    length = _storage_length(values)
    padding = max(0, width - length)
    left = 0
    if alignment == 1:
        left = padding
    elif alignment == 2:
        left = padding // 2 + (padding & width & 1)
    result = [fill] * left
    index = 0
    while index < length:
        _storage_append(result, _storage_read(values, index))
        index += 1
    for index in range(padding - left):
        _storage_append(result, fill)
    return result


def _bytes_padding_result(source, values):
    if type_of(source) is bytes:
        if _storage_length(source) == _storage_length(values):
            return source
    return bytes_from_values(values)


def _bytes_slice_result(source, start, end):
    if type_of(source) is bytes:
        if start == 0 and end == _storage_length(source):
            return source
    return byte_copy(source, start, end - start)


def _require_bytes(receiver, method):
    if not is_instance(receiver, bytes):
        raise TypeError(
            "descriptor '"
            + method
            + "' requires a 'bytes' object but received a '"
            + _type_name(receiver)
            + "'"
        )


def _require_bytearray(receiver, method):
    if not is_instance(receiver, bytearray):
        raise TypeError(
            "descriptor '"
            + method
            + "' requires a 'bytearray' object but received a '"
            + _type_name(receiver)
            + "'"
        )


def _bytes_upper(values):
    result = []
    index = 0
    length = _storage_length(values)
    while index < length:
        b = _storage_read(values, index)
        _storage_append(result, b - 32 if 97 <= b <= 122 else b)
        index += 1
    return result


def _bytes_lower(values):
    result = []
    index = 0
    length = _storage_length(values)
    while index < length:
        b = _storage_read(values, index)
        _storage_append(result, b + 32 if 65 <= b <= 90 else b)
        index += 1
    return result


def _bytes_swapcase(values):
    result = []
    index = 0
    length = _storage_length(values)
    while index < length:
        b = _storage_read(values, index)
        if 97 <= b <= 122:
            b -= 32
        elif 65 <= b <= 90:
            b += 32
        _storage_append(result, b)
        index += 1
    return result


def _bytes_capitalize(values):
    result = []
    index = 0
    length = _storage_length(values)
    while index < length:
        b = _storage_read(values, index)
        if index == 0:
            if 97 <= b <= 122:
                b -= 32
        elif 65 <= b <= 90:
            b += 32
        _storage_append(result, b)
        index += 1
    return result


def _bytes_title(values):
    result = []
    index = 0
    length = _storage_length(values)
    prev_cased = False
    while index < length:
        b = _storage_read(values, index)
        if 97 <= b <= 122:
            if not prev_cased:
                b -= 32
            prev_cased = True
        elif 65 <= b <= 90:
            if prev_cased:
                b += 32
            prev_cased = True
        else:
            prev_cased = False
        _storage_append(result, b)
        index += 1
    return result


def _bytes_isalpha(values):
    length = _storage_length(values)
    if length == 0:
        return False
    index = 0
    while index < length:
        b = _storage_read(values, index)
        if not ((65 <= b <= 90) or (97 <= b <= 122)):
            return False
        index += 1
    return True


def _bytes_isdigit(values):
    length = _storage_length(values)
    if length == 0:
        return False
    index = 0
    while index < length:
        b = _storage_read(values, index)
        if not (48 <= b <= 57):
            return False
        index += 1
    return True


def _bytes_isalnum(values):
    length = _storage_length(values)
    if length == 0:
        return False
    index = 0
    while index < length:
        b = _storage_read(values, index)
        if not ((65 <= b <= 90) or (97 <= b <= 122) or (48 <= b <= 57)):
            return False
        index += 1
    return True


def _bytes_isspace(values):
    length = _storage_length(values)
    if length == 0:
        return False
    index = 0
    while index < length:
        if not _is_ascii_whitespace(_storage_read(values, index)):
            return False
        index += 1
    return True


def _bytes_isupper(values):
    length = _storage_length(values)
    cased = False
    index = 0
    while index < length:
        b = _storage_read(values, index)
        if 97 <= b <= 122:
            return False
        if 65 <= b <= 90:
            cased = True
        index += 1
    return cased


def _bytes_islower(values):
    length = _storage_length(values)
    cased = False
    index = 0
    while index < length:
        b = _storage_read(values, index)
        if 65 <= b <= 90:
            return False
        if 97 <= b <= 122:
            cased = True
        index += 1
    return cased


def _bytes_istitle(values):
    length = _storage_length(values)
    cased = False
    prev_cased = False
    index = 0
    while index < length:
        b = _storage_read(values, index)
        if 65 <= b <= 90:
            if prev_cased:
                return False
            prev_cased = True
            cased = True
        elif 97 <= b <= 122:
            if not prev_cased:
                return False
            prev_cased = True
            cased = True
        else:
            prev_cased = False
        index += 1
    return cased


def _bytes_isascii(values):
    length = _storage_length(values)
    index = 0
    while index < length:
        if _storage_read(values, index) >= 128:
            return False
        index += 1
    return True


def _bytes_zfill(values, width):
    width = _index_integer(width)
    length = _storage_length(values)
    pad = width - length
    if pad <= 0:
        return None
    result = []
    start = 0
    if length > 0:
        first = _storage_read(values, 0)
        if first == 43 or first == 45:
            _storage_append(result, first)
            start = 1
    while pad > 0:
        _storage_append(result, 48)
        pad -= 1
    while start < length:
        _storage_append(result, _storage_read(values, start))
        start += 1
    return result


def _bytes_expandtabs(values, tabsize):
    tabsize = _index_integer(tabsize)
    result = []
    col = 0
    index = 0
    length = _storage_length(values)
    while index < length:
        b = _storage_read(values, index)
        if b == 9:
            if tabsize > 0:
                spaces = tabsize - (col % tabsize)
                col += spaces
                while spaces > 0:
                    _storage_append(result, 32)
                    spaces -= 1
        else:
            _storage_append(result, b)
            if b == 10 or b == 13:
                col = 0
            else:
                col += 1
        index += 1
    return result


def _bytes_percent_format(values, args):
    original = args
    mapping = None
    if not _tuple_storage(args):
        args = (args,)
    count = _storage_length(args)
    consumed = 0
    index = 0
    size = _storage_length(values)
    result = []
    while index < size:
        b = _storage_read(values, index)
        index += 1
        if b != 37:
            _storage_append(result, b)
            continue
        if index == size:
            raise ValueError("incomplete format")
        if _storage_read(values, index) == 37:
            index += 1
            _storage_append(result, 37)
            continue
        key = None
        if _storage_read(values, index) == 40:
            close = index + 1
            while close < size and _storage_read(values, close) != 41:
                close += 1
            if close == size:
                raise ValueError("incomplete format key")
            key_bytes = []
            while index + 1 < close:
                index += 1
                _storage_append(key_bytes, _storage_read(values, index))
            key = bytes_from_values(key_bytes)
            index = close + 1
            if index == size:
                raise ValueError("incomplete format")
        left = False
        zero = False
        plus = False
        space = False
        alternate = False
        while index < size and _storage_read(values, index) in (45, 48, 43, 32, 35):
            flag = _storage_read(values, index)
            if flag == 45:
                left = True
            elif flag == 48:
                zero = True
            elif flag == 43:
                plus = True
            elif flag == 32:
                space = True
            else:
                alternate = True
            index += 1
        width = 0
        if index < size and _storage_read(values, index) == 42:
            index += 1
            if mapping is not None or consumed == count:
                raise TypeError("not enough arguments for format string")
            width = _index_integer(_storage_read(args, consumed))
            consumed += 1
            if width < 0:
                left = True
                width = -width
        else:
            while index < size and 48 <= _storage_read(values, index) <= 57:
                width = width * 10 + (_storage_read(values, index) - 48)
                index += 1
        precision = -1
        if index < size and _storage_read(values, index) == 46:
            index += 1
            precision = 0
            if index < size and _storage_read(values, index) == 42:
                index += 1
                if mapping is not None or consumed == count:
                    raise TypeError("not enough arguments for format string")
                precision = _index_integer(_storage_read(args, consumed))
                consumed += 1
                if precision < 0:
                    precision = -1
            else:
                while index < size and 48 <= _storage_read(values, index) <= 57:
                    precision = precision * 10 + (_storage_read(values, index) - 48)
                    index += 1
        if index < size and _storage_read(values, index) in (104, 108, 76):
            index += 1
        if index == size:
            raise ValueError("incomplete format")
        conv = chr(_storage_read(values, index))
        index += 1
        if conv not in "diouxXeEfFgGcrsba":
            raise ValueError("unsupported bytes percent format conversion")
        if key is not None:
            mapping = original
            arg = mapping[key]
            consumed = count
        else:
            if consumed == count:
                raise TypeError("not enough arguments for format string")
            arg = _storage_read(args, consumed)
            consumed += 1
        if conv in ("s", "b"):
            piece = _bytes_like_values(arg)
            if precision >= 0 and _storage_length(piece) > precision:
                piece = _byte_sublist(piece, 0, precision)
            pad = width - _storage_length(piece)
            if pad > 0:
                if left:
                    piece = piece + [32] * pad
                else:
                    piece = [32] * pad + piece
            for item in piece:
                _storage_append(result, item)
        elif conv in ("r", "a"):
            text = ascii(arg)
            if precision >= 0:
                text = text[:precision]
            piece = [ord(ch) for ch in text]
            pad = width - _storage_length(piece)
            if pad > 0:
                if left:
                    piece = piece + [32] * pad
                else:
                    piece = [32] * pad + piece
            for item in piece:
                _storage_append(result, item)
        elif conv == "c":
            if is_instance(arg, bytes):
                if _storage_length(arg) != 1:
                    raise TypeError(
                        "%c requires an integer in range(256) or a single byte"
                    )
                byte_val = _storage_read(arg, 0)
            elif is_instance(arg, bytearray):
                if _storage_length(arg.values) != 1:
                    raise TypeError(
                        "%c requires an integer in range(256) or a single byte"
                    )
                byte_val = _storage_read(arg.values, 0)
            else:
                byte_val = _byte_value(arg, "%c arg not in range(256)")
            piece = [byte_val]
            pad = width - 1
            if pad > 0:
                if left:
                    piece = piece + [32] * pad
                else:
                    piece = [32] * pad + piece
            for item in piece:
                _storage_append(result, item)
        else:
            spec = "%"
            if left:
                spec += "-"
            if zero:
                spec += "0"
            if plus:
                spec += "+"
            if space:
                spec += " "
            if alternate:
                spec += "#"
            if width > 0:
                spec += str(width)
            if precision >= 0:
                spec += "." + str(precision)
            spec += conv
            rendered = spec % (arg,)
            for ch in rendered:
                _storage_append(result, ord(ch))
    if consumed != count and mapping is None:
        if (
            _tuple_storage(original)
            or is_instance(original, bytes)
            or is_instance(original, bytearray)
            or _OptionalMapping(original) is None
        ):
            raise TypeError("not all arguments converted during bytes formatting")
    return result


def _OptionalMapping(value):
    missing = _binding_missing()
    if class_lookup_default(type_of(value), "__getitem__", missing) is missing:
        return None
    return value


class BytesStorage:
    def __len__(self):
        _require_bytes(self, "__len__")
        return _storage_length(self)

    def __add__(self, other):
        _require_bytes(self, "__add__")
        values = _bytes_like_values(self)
        for item in _bytes_like_values(other):
            _storage_append(values, item)
        return bytes_from_values(values)

    def __eq__(self, other):
        _require_bytes(self, "__eq__")
        if is_instance(other, bytes):
            if _storage_length(self) != _storage_length(other):
                return False
            index = 0
            while index < _storage_length(self):
                if _storage_read(self, index) != _storage_read(other, index):
                    return False
                index += 1
            return True
        if is_instance(other, bytearray):
            return other == self
        return NotImplemented

    def __ne__(self, other):
        _require_bytes(self, "__ne__")
        res = self.__eq__(other)
        return res if res is NotImplemented else not res

    def __hash__(self):
        _require_bytes(self, "__hash__")
        return _storage_hash(self)

    def __mod__(self, args):
        _require_bytes(self, "__mod__")
        return bytes_from_values(_bytes_percent_format(self, args))

    def upper(self):
        _require_bytes(self, "upper")
        return bytes_from_values(_bytes_upper(self))

    def lower(self):
        _require_bytes(self, "lower")
        return bytes_from_values(_bytes_lower(self))

    def swapcase(self):
        _require_bytes(self, "swapcase")
        return bytes_from_values(_bytes_swapcase(self))

    def capitalize(self):
        _require_bytes(self, "capitalize")
        return bytes_from_values(_bytes_capitalize(self))

    def title(self):
        _require_bytes(self, "title")
        return bytes_from_values(_bytes_title(self))

    def isalpha(self):
        _require_bytes(self, "isalpha")
        return _bytes_isalpha(self)

    def isdigit(self):
        _require_bytes(self, "isdigit")
        return _bytes_isdigit(self)

    def isalnum(self):
        _require_bytes(self, "isalnum")
        return _bytes_isalnum(self)

    def isspace(self):
        _require_bytes(self, "isspace")
        return _bytes_isspace(self)

    def isupper(self):
        _require_bytes(self, "isupper")
        return _bytes_isupper(self)

    def islower(self):
        _require_bytes(self, "islower")
        return _bytes_islower(self)

    def istitle(self):
        _require_bytes(self, "istitle")
        return _bytes_istitle(self)

    def isascii(self):
        _require_bytes(self, "isascii")
        return _bytes_isascii(self)

    def zfill(self, width, /):
        _require_bytes(self, "zfill")
        res = _bytes_zfill(self, width)
        if res is None:
            return _bytes_slice_result(self, 0, _storage_length(self))
        return bytes_from_values(res)

    def expandtabs(self, tabsize=8):
        _require_bytes(self, "expandtabs")
        return bytes_from_values(_bytes_expandtabs(self, tabsize))

    def ljust(self, width, fillchar=b" ", /):
        return _bytes_padding_result(
            self, _byte_padding(self, width, fillchar, 0, "ljust")
        )

    def rjust(self, width, fillchar=b" ", /):
        return _bytes_padding_result(
            self, _byte_padding(self, width, fillchar, 1, "rjust")
        )

    def center(self, width, fillchar=b" ", /):
        return _bytes_padding_result(
            self, _byte_padding(self, width, fillchar, 2, "center")
        )

    def hex(self, sep=_HEX_MISSING, bytes_per_sep=1):
        return _hex_bytes(self, sep, bytes_per_sep)

    @classmethod
    def fromhex(cls, string, /):
        value = bytes_from_values(_fromhex_values(string))
        return cls(value) if cls is not bytes else value

    def join(self, iterable, /):
        return bytes_from_values(_bytes_joined_values(self, iterable))

    __slots__ = ()

    def __getitem__(self, index):
        _require_bytes(self, "__getitem__")
        if is_slice(index):
            start, stop, step, count = positions(index, _storage_length(self))
            if step == 1:
                return byte_copy(self, start, count)
            return bytes_from_values(_selected_positions(self, start, step, count))
        return _storage_read(self, storage_index(self, index))

    def __iter__(self):
        _require_bytes(self, "__iter__")
        return BytesIterator(self)

    def __buffer__(self, flags, /):
        return memoryview(self)

    def __contains__(self, item, /):
        _require_bytes(self, "__contains__")
        return _bytes_find(self, item, 0, None) >= 0

    def decode(self, encoding="utf-8", errors=_CODEC_ERRORS_MISSING):
        _require_bytes(self, "decode")
        return decode(self, encoding, errors)

    def find(self, sub, start=0, end=None):
        return _bytes_find(self, sub, start, end)

    def rfind(self, sub, start=0, end=None):
        return _bytes_rfind(self, sub, start, end)

    def index(self, sub, start=0, end=None):
        result = _bytes_find(self, sub, start, end)
        if result < 0:
            raise ValueError("subsection not found")
        return result

    def rindex(self, sub, start=0, end=None):
        result = _bytes_rfind(self, sub, start, end)
        if result < 0:
            raise ValueError("subsection not found")
        return result

    def count(self, sub, start=0, end=None):
        return _bytes_count(self, sub, start, end)

    def startswith(self, prefix, start=0, end=None):
        return _bytes_startswith(self, prefix, start, end)

    def endswith(self, suffix, start=0, end=None):
        return _bytes_endswith(self, suffix, start, end)

    def strip(self, chars=None, /):
        start, end = _bytes_strip_bounds(self, chars, True, True)
        return _bytes_slice_result(self, start, end)

    def lstrip(self, chars=None, /):
        start, end = _bytes_strip_bounds(self, chars, True, False)
        return _bytes_slice_result(self, start, end)

    def rstrip(self, chars=None, /):
        start, end = _bytes_strip_bounds(self, chars, False, True)
        return _bytes_slice_result(self, start, end)

    def removeprefix(self, prefix, /):
        start, end = _bytes_removeprefix_bounds(self, prefix)
        return _bytes_slice_result(self, start, end)

    def removesuffix(self, suffix, /):
        start, end = _bytes_removesuffix_bounds(self, suffix)
        return _bytes_slice_result(self, start, end)

    def split(self, sep=None, maxsplit=-1):
        return [
            _bytes_slice_result(self, start, end)
            for start, end in _bytes_split_spans(self, sep, maxsplit, False)
        ]

    def rsplit(self, sep=None, maxsplit=-1):
        return [
            _bytes_slice_result(self, start, end)
            for start, end in _bytes_split_spans(self, sep, maxsplit, True)
        ]

    def splitlines(self, keepends=False):
        return [
            _bytes_slice_result(self, start, end)
            for start, end in _bytes_splitlines_spans(self, keepends)
        ]

    def partition(self, sep, /):
        needle = _bytes_like_values(sep)
        size = _storage_length(needle)
        if size == 0:
            raise ValueError("empty separator")
        length = _storage_length(self)
        position = _bytes_find_values(self, needle, 0, length)
        if position < 0:
            return (_bytes_slice_result(self, 0, length), b"", b"")
        return (
            byte_copy(self, 0, position),
            sep,
            byte_copy(self, position + size, length - position - size),
        )

    def rpartition(self, sep, /):
        needle = _bytes_like_values(sep)
        size = _storage_length(needle)
        if size == 0:
            raise ValueError("empty separator")
        length = _storage_length(self)
        position = _bytes_rfind_values(self, needle, 0, length)
        if position < 0:
            return (b"", b"", _bytes_slice_result(self, 0, length))
        return (
            byte_copy(self, 0, position),
            sep,
            byte_copy(self, position + size, length - position - size),
        )

    def replace(self, old, new, count=-1, /):
        replaced = _bytes_replaced(self, old, new, count)
        if replaced is None:
            return _bytes_slice_result(self, 0, _storage_length(self))
        return bytes_from_values(replaced)

    def translate(self, table, delete=b""):
        return bytes_from_values(_bytes_translated(self, table, delete))

    @staticmethod
    def maketrans(frm, to):
        return _make_bytes_translation_table(frm, to)

    def __mul__(self, count):
        return _bytes_repeat(self, count)

    def __rmul__(self, count):
        return _bytes_repeat(self, count)


def sequence_equal(left, right):
    if not _storage_compatible(left, right):
        return False
    if left is right:
        return True
    if _storage_length(left) != _storage_length(right):
        return False
    index = 0
    while index < _storage_length(left) and index < _storage_length(right):
        a = _storage_read(left, index)
        b = _storage_read(right, index)
        if a is not b and not a == b:  # noqa: SIM201 - inequality may be overloaded separately
            return False
        index += 1
    return _storage_length(left) == _storage_length(right)


def sequence_ne(left, right):
    return not sequence_equal(left, right)


def sequence_lt(left, right):
    if left is right:
        return False
    index = 0
    while index < _storage_length(left) and index < _storage_length(right):
        a = _storage_read(left, index)
        b = _storage_read(right, index)
        if not (a is b or a == b):
            return a < b
        index += 1
    return _storage_length(left) < _storage_length(right)


def sequence_le(left, right):
    if left is right:
        return True
    index = 0
    while index < _storage_length(left) and index < _storage_length(right):
        a = _storage_read(left, index)
        b = _storage_read(right, index)
        if not (a is b or a == b):
            return a <= b
        index += 1
    return _storage_length(left) <= _storage_length(right)


def sequence_gt(left, right):
    if left is right:
        return False
    index = 0
    while index < _storage_length(left) and index < _storage_length(right):
        a = _storage_read(left, index)
        b = _storage_read(right, index)
        if not (a is b or a == b):
            return a > b
        index += 1
    return _storage_length(left) > _storage_length(right)


def sequence_ge(left, right):
    if left is right:
        return True
    index = 0
    while index < _storage_length(left) and index < _storage_length(right):
        a = _storage_read(left, index)
        b = _storage_read(right, index)
        if not (a is b or a == b):
            return a >= b
        index += 1
    return _storage_length(left) >= _storage_length(right)


def get_attribute(value, name):
    missing = _binding_missing()
    try:
        found = _attribute_lookup_or(value, name, missing)
    except AttributeError:
        fallback = _getattr_method(value)
        if fallback is None:
            raise
        return fallback(name)
    if found is not missing:
        return found
    fallback = _getattr_method(value)
    if fallback is None:
        # The runtime's lookup failed without raising; raise its error now.
        return _attribute_lookup(value, name)
    return fallback(name)


def iterate(value):
    return _iterate(value, False)


def _optional_special(value, name):
    owner = _type_of(value)
    missing = _binding_missing()
    method = class_lookup_default(owner, name, missing)
    if method is missing:
        return missing
    try:
        return bind_instance_attribute(method, value, owner)
    except AttributeError:
        # Python treats a descriptor's AttributeError as an absent special method.
        return missing


def _iterate(value, membership):
    cls = _type_of(value)
    if cls is dict:
        return dict.__iter__(value)
    if cls is set or cls is frozenset:
        return set.__iter__(value)
    if cls is list or cls is tuple:
        iter_cls = TupleIterator if cls is tuple else StorageIterator
        iterator = _allocate_instance(iter_cls)
        iterator.sequence = value
        iterator.index = 0
        iterator.exhausted = False
        return iterator
    if cls is str:
        iterator = _allocate_instance(TextIterator)
        iterator.text = value
        iterator.index = 0
        return iterator
    if cls is bytes:
        iterator = _allocate_instance(BytesIterator)
        iterator.sequence = value
        iterator.index = 0
        iterator.exhausted = False
        return iterator
    if cls is bytearray:
        iterator = _allocate_instance(StorageIterator)
        iterator.sequence = value
        iterator.index = 0
        iterator.exhausted = False
        return iterator
    if cls is range:
        iterator = _allocate_instance(RangeIterator)
        iterator.current = value.start
        iterator.stop = value.stop
        iterator.step = value.step
        iterator.exhausted = False
        return iterator
    missing = _binding_missing()
    method = _optional_special(value, "__iter__")
    if method is not missing:
        if method is None:
            return _iteration_error(value, membership)
        iterator = method()
        if class_lookup_default(_type_of(iterator), "__next__", missing) is missing:
            raise TypeError("__iter__ returned a non-iterator")
        return iterator
    method = _optional_special(value, "__getitem__")
    if method is not missing and method is not None:
        return SequenceIterator(value)
    return _iteration_error(value, membership)


def _iteration_error(value, membership):
    name = _type_of(value).__name__
    if membership:
        raise TypeError(f"argument of type '{name}' is not a container or iterable")
    raise TypeError(f"'{name}' object is not iterable")


def contains(container, item):
    owner = _type_of(container)
    if owner is dict:
        return _table_lookup(container, item) >= 0
    if owner is set or owner is frozenset:
        return _set_lookup(container, item) >= 0
    if owner is str:
        return TextStorage.__contains__(container, item)
    if owner is bytearray:
        return _bytes_find(container.values, item, 0, None) >= 0
    if owner is list or owner is tuple:
        count = _storage_length(container)
        index = 0
        while index < count:
            existing = _storage_read(container, index)
            if existing is item or existing == item:
                return True
            index += 1
        return False
    missing = _binding_missing()
    method = _optional_special(container, "__contains__")
    if method is not missing:
        if method is None:
            raise TypeError(f"'{owner.__name__}' object is not a container")
        return bool(method(item))
    iterator = _iterate(container, True)
    while True:
        try:
            existing = next_item(iterator)
        except StopIteration:
            return False
        if existing is item or existing == item:
            return True


def not_contains(container, item):
    return not contains(container, item)


def text_contains(text, item):
    """``item in text`` with both operands proven exact ``str``."""
    return find_in(text, item, 0, _text_length(text)) >= 0


def text_not_contains(text, item):
    return find_in(text, item, 0, _text_length(text)) < 0


def next_item(value):
    cls = _type_of(value)
    if cls is StorageIterator or cls is TupleIterator or cls is BytesIterator:
        if value.exhausted:
            raise StopIteration
        index = value.index
        sequence = value.sequence
        if index >= _storage_length(sequence):
            value.exhausted = True
            raise StopIteration
        item = _storage_read(sequence, index)
        value.index = index + 1
        return item
    if cls is TextIterator:
        index = value.index
        text = value.text
        if index >= _text_length(text):
            raise StopIteration
        value.index = index + 1
        return _text_character(text, index)
    if cls is RangeIterator:
        current = value.current
        step = value.step
        if (
            value.exhausted
            or (step > 0 and current >= value.stop)
            or (step < 0 and current <= value.stop)
        ):
            value.exhausted = True
            raise StopIteration
        value.current = current + step
        return current
    if cls is Enumerate:
        item = next_item(value.iterator)
        index = value.index
        value.index = index + 1
        return (index, item)
    method = _next_method(value)
    if method is None:
        raise TypeError("object is not an iterator")
    return method()


def unpack(value, count):
    count = _integer_storage(count)
    owner = _type_of(value)
    if owner is tuple:
        length = _storage_length(value)
        if length < count:
            raise ValueError("not enough values to unpack")
        if length > count:
            raise ValueError("too many values to unpack")
        return value
    if owner is bytearray:
        value = value.values
        owner = list
    if owner is list:
        length = _storage_length(value)
        if length < count:
            raise ValueError("not enough values to unpack")
        if length > count:
            raise ValueError("too many values to unpack")
        return _storage_tuple(_byte_sublist(value, 0, length))
    iterator = iterate(value)
    result = []
    index = 0
    while index < count:
        try:
            item = next_item(iterator)
        except StopIteration:
            raise ValueError("not enough values to unpack")
        _storage_append(result, item)
        index += 1
    try:
        next_item(iterator)
    except StopIteration:
        return _storage_tuple(result)
    raise ValueError("too many values to unpack")


def unpack_starred(value, before, after):
    """``a, *b, c = value``: leading items, a list of the rest, trailing items."""
    items = _make_list(value)
    count = _storage_length(items)
    if count < before + after:
        raise ValueError(
            f"not enough values to unpack (expected at least {before + after}, got {count})"
        )
    result = []
    index = 0
    while index < before:
        _storage_append(result, _storage_read(items, index))
        index += 1
    middle = []
    while index < count - after:
        _storage_append(middle, _storage_read(items, index))
        index += 1
    _storage_append(result, middle)
    while index < count:
        _storage_append(result, _storage_read(items, index))
        index += 1
    return _storage_tuple(result)


class RangeIterator:
    def __init__(self, start, stop, step):
        self.current = start
        self.stop = stop
        self.step = step
        self.exhausted = False

    def __iter__(self):
        return self

    def __next__(self):
        current = self.current
        if (
            self.exhausted
            or (self.step > 0 and current >= self.stop)
            or (self.step < 0 and current <= self.stop)
        ):
            self.exhausted = True
            raise StopIteration
        self.current = current + self.step
        return current

    def __length_hint__(self):
        if self.exhausted:
            return 0
        if self.step > 0:
            if self.current >= self.stop:
                return 0
            return (self.stop - self.current - 1) // self.step + 1
        if self.current <= self.stop:
            return 0
        return (self.current - self.stop - 1) // (-self.step) + 1


RangeIterator.__name__ = "range_iterator"
RangeIterator.__qualname__ = "range_iterator"


class range:
    def __init__(self, start_or_stop, stop=_HEX_MISSING, step=1, /):
        if stop is _HEX_MISSING:
            start = 0
            stop = (
                start_or_stop
                if _type_of(start_or_stop) is int
                else integer_index(start_or_stop)
            )
            step = 1
        else:
            start = (
                start_or_stop
                if _type_of(start_or_stop) is int
                else integer_index(start_or_stop)
            )
            stop = stop if _type_of(stop) is int else integer_index(stop)
            if _type_of(step) is not int:
                step = integer_index(step)
            if step == 0:
                raise ValueError("range step must not be zero")
        self.start = start
        self.stop = stop
        self.step = step

    def __iter__(self):
        iterator = _allocate_instance(RangeIterator)
        iterator.current = self.start
        iterator.stop = self.stop
        iterator.step = self.step
        iterator.exhausted = False
        return iterator

    def __reversed__(self):
        n = self.__len__()
        if n == 0:
            return RangeIterator(self.start, self.start, -self.step)
        last = self.start + (n - 1) * self.step
        return RangeIterator(last, last - n * self.step, -self.step)

    def __bool__(self):
        return self.__len__() > 0

    def __eq__(self, other):
        if _type_of(other) is not range and not is_instance(other, range):
            return NotImplemented
        n = self.__len__()
        if n != other.__len__():
            return False
        if n == 0:
            return True
        if self.start != other.start:
            return False
        if n == 1:
            return True
        return self.step == other.step

    def __ne__(self, other):
        result = self.__eq__(other)
        return result if result is NotImplemented else not result

    def __hash__(self):
        n = self.__len__()
        if n == 0:
            return hash((0, None, None))
        if n == 1:
            return hash((1, self.start, None))
        return hash((n, self.start, self.step))

    def count(self, value):
        value_type = type(value)
        if value_type is int or value_type is bool:
            return 1 if self.__contains__(value) else 0
        found = 0
        for item in self:
            if item is value or item == value:
                found += 1
        return found

    def index(self, value):
        value_type = type(value)
        if value_type is int or value_type is bool:
            if self.__contains__(value):
                return (value - self.start) // self.step
            raise ValueError(f"{value!r} is not in range")
        position = 0
        for item in self:
            if item is value or item == value:
                return position
            position += 1
        raise ValueError(f"{value!r} is not in range")

    def __repr__(self):
        if self.step == 1:
            return f"range({self.start}, {self.stop})"
        return f"range({self.start}, {self.stop}, {self.step})"

    def __contains__(self, value):
        value_type = _type_of(value)
        if value_type is int or value_type is bool:
            if self.step > 0:
                if value < self.start or value >= self.stop:
                    return False
            elif value > self.start or value <= self.stop:
                return False
            return (value - self.start) % self.step == 0
        for item in self:
            if item is value or item == value:
                return True
        return False

    def __len__(self):
        if self.step > 0:
            if self.start >= self.stop:
                return 0
            return (self.stop - self.start - 1) // self.step + 1
        if self.start <= self.stop:
            return 0
        return (self.start - self.stop - 1) // (-self.step) + 1

    def __getitem__(self, index):
        if is_slice(index):
            start, stop, step, count = positions(index, self.__len__())
            first = self.start + start * self.step
            return range(first, first + count * self.step * step, self.step * step)
        index = integer_index(index)
        length = self.__len__()
        if index < 0:
            index += length
        if index < 0 or index >= length:
            raise IndexError("range index out of range")
        return self.start + index * self.step


class Enumerate:
    def __init__(self, iterator, start):
        self.iterator = iterator
        self.index = start

    def __iter__(self):
        return self

    def __next__(self):
        value = next_item(self.iterator)
        index = self.index
        self.index = index + 1
        return (index, value)


def enumerate(iterable, start=0):
    start = integer_index(start)
    iterator = _allocate_instance(Enumerate)
    iterator.iterator = iterate(iterable)
    iterator.index = start
    return iterator


class CallableIterator:
    def __init__(self, function, sentinel):
        self.function = function
        self.sentinel = sentinel
        self.exhausted = False

    def __iter__(self):
        return self

    def exhaust(self):
        self.exhausted = True
        self.function = None
        self.sentinel = None

    def __next__(self):
        if self.exhausted:
            raise StopIteration
        try:
            value = self.function()
        except StopIteration:
            self.exhaust()
            raise StopIteration
        if value is self.sentinel or self.sentinel == value:
            self.exhaust()
            raise StopIteration
        return value


def iter(*args):
    count = _storage_length(args)
    if count == 1:
        return iterate(_storage_read(args, 0))
    if count == 2:
        function = _storage_read(args, 0)
        if not callable(function):
            raise TypeError("iter(v, w): v must be callable")
        return CallableIterator(function, _storage_read(args, 1))
    raise TypeError("iter expected one or two arguments")


def next(*args):
    count = _storage_length(args)
    if count != 1 and count != 2:
        raise TypeError("next expects one or two arguments")
    try:
        return next_item(_storage_read(args, 0))
    except StopIteration:
        if count == 2:
            return _storage_read(args, 1)
        raise


def _make_list(*args):
    count = _storage_length(args)
    if count > 1:
        raise TypeError("list expects at most one argument")
    result = []
    if count == 1:
        extend_arguments(result, _storage_read(args, 0))
    return result


def _make_tuple(*args):
    if _storage_length(args) == 0:
        return ()
    if _storage_length(args) == 1:
        arg = _storage_read(args, 0)
        if _type_of(arg) is tuple:
            return arg
    return _storage_tuple(list_from_arguments(args))


def list_from_arguments(args):
    count = _storage_length(args)
    if count > 1:
        raise TypeError("tuple expects at most one argument")
    result = []
    if count == 1:
        extend_arguments(result, _storage_read(args, 0))
    return result


_missing_default = _binding_missing()


def _better(candidate_key, best_key, greatest):
    if greatest:
        return candidate_key > best_key
    return candidate_key < best_key


def _extreme(args, key, default, greatest):
    count = _storage_length(args)
    if count == 0:
        raise TypeError("min/max expected at least one argument")
    if count > 1:
        if default is not _missing_default:
            raise TypeError("default is only valid with one iterable argument")
        if count == 2 and key is None:
            first = _storage_read(args, 0)
            second = _storage_read(args, 1)
            if greatest:
                return second if second > first else first
            return second if second < first else first
        if count == 3 and key is None:
            first = _storage_read(args, 0)
            second = _storage_read(args, 1)
            third = _storage_read(args, 2)
            if greatest:
                best = second if second > first else first
                return third if third > best else best
            best = second if second < first else first
            return third if third < best else best
        # Positional candidates are read from the argument tuple by index:
        # iterating it would end with a StopIteration exception object.
        best = _storage_read(args, 0)
        best_key = best if key is None else key(best)
        index = 1
        while index < count:
            candidate = _storage_read(args, index)
            candidate_key = candidate if key is None else key(candidate)
            if _better(candidate_key, best_key, greatest):
                best = candidate
                best_key = candidate_key
            index += 1
        return best
    iterable = _storage_read(args, 0)
    owner = _type_of(iterable)
    if owner is bytearray:
        iterable = iterable.values
        owner = list
    if owner is list or owner is tuple or owner is bytes:
        length = _storage_length(iterable)
        if length == 0:
            if default is not _missing_default:
                return default
            raise ValueError("min/max arg is an empty sequence")
        best = _storage_read(iterable, 0)
        best_key = best if key is None else key(best)
        index = 1
        while index < length:
            candidate = _storage_read(iterable, index)
            candidate_key = candidate if key is None else key(candidate)
            if _better(candidate_key, best_key, greatest):
                best = candidate
                best_key = candidate_key
            index += 1
        return best
    iterator = iterate(iterable)
    try:
        best = next_item(iterator)
    except StopIteration:
        if default is not _missing_default:
            return default
        raise ValueError("min/max arg is an empty sequence")
    best_key = best
    if key is not None:
        best_key = key(best)
    while True:
        try:
            candidate = next_item(iterator)
        except StopIteration:
            return best
        candidate_key = candidate
        if key is not None:
            candidate_key = key(candidate)
        if _better(candidate_key, best_key, greatest):
            best = candidate
            best_key = candidate_key


def min(*args, key=None, default=_missing_default):
    return _extreme(args, key, default, False)


def max(*args, key=None, default=_missing_default):
    return _extreme(args, key, default, True)


class list(ListStorage):
    __slots__ = ()

    def __new__(cls, *args, **kwargs):
        if not is_class(cls) or not is_subclass(cls, list):
            raise TypeError("list.__new__: class must be a subtype of list")
        value = []
        if cls is not list:
            return typed_value(value, cls)
        return value

    def __init__(self, *args):
        count = _storage_length(args)
        if count > 1:
            raise TypeError(f"list expected at most 1 argument, got {count}")
        _storage_delete(self, 0, _storage_length(self))
        if count:
            extend_arguments(self, _storage_read(args, 0))


class tuple(TupleStorage):
    __slots__ = ()

    def __new__(cls, *args):
        if cls is not tuple:
            return typed_value(_storage_tuple(list_from_arguments(args)), cls)
        return _make_tuple(*args)


class str(TextStorage):
    __slots__ = ()

    def __new__(
        cls,
        object=_CODEC_ERRORS_MISSING,
        encoding=_CODEC_ERRORS_MISSING,
        errors=_CODEC_ERRORS_MISSING,
    ):
        value = _make_str(object, encoding, errors)
        if cls is not str:
            return typed_value(value, cls)
        return value


class bytes(BytesStorage):
    __slots__ = ()

    def __new__(cls, source=b"", encoding=None, errors="strict"):
        if cls is bytes and encoding is None:
            owner = _type_of(source)
            if owner is bytes:
                return source
            if owner is bytearray:
                return bytes_from_values(source.values)
        if _text_storage(source):
            if encoding is None:
                raise TypeError("string argument without an encoding")
            value = source.encode(encoding, errors)
            return typed_value(value, cls) if cls is not bytes else value
        if encoding is not None:
            raise TypeError("encoding without a string argument")
        converted, result = _bytes_special(source)
        if converted:
            if cls is bytes:
                return result
            values = [
                _storage_read(result, index) for index in range(_storage_length(result))
            ]
            return typed_value(bytes_from_values(values), cls)
        if _integer_storage_kind(source) or _index_method(source) is not None:
            source = _index_integer(source)
            if source < 0:
                raise ValueError("negative count")
            if source >= (1 << 63):
                raise OverflowError("cannot fit 'int' into an index-sized integer")
            values = []
            index = 0
            while index < source:
                _storage_append(values, 0)
                index += 1
            value = bytes_from_values(values)
            return typed_value(value, cls) if cls is not bytes else value
        if is_instance(source, bytes):
            if cls is bytes and type(source) is bytes:
                return source
            values = [
                _storage_read(source, index) for index in range(_storage_length(source))
            ]
            value = bytes_from_values(values)
            return typed_value(value, cls) if cls is not bytes else value
        if is_instance(source, bytearray):
            value = bytes_from_values(source.values)
            return typed_value(value, cls) if cls is not bytes else value
        values = [
            _byte_value(item, "bytes must be in range(0, 256)") for item in source
        ]
        value = bytes_from_values(values)
        return typed_value(value, cls) if cls is not bytes else value


def _integer_storage_kind(value):
    return is_instance(value, int)


def _byte_value(value, range_message="byte must be in range(0, 256)"):
    value = integer_index(value)
    if value < 0 or value > 255:
        raise ValueError(range_message)
    return value


def _byte_values(source, encoding, errors):
    if _text_storage(source):
        if encoding is None:
            raise TypeError("string argument without an encoding")
        source = source.encode(encoding, errors)
    if is_instance(source, bytes):
        length = _storage_length(source)
        values = []
        index = 0
        while index < length:
            _storage_append(values, _storage_read(source, index))
            index += 1
        return values
    if is_instance(source, bytearray):
        source_values = source.values
        length = _storage_length(source_values)
        values = []
        index = 0
        while index < length:
            _storage_append(values, _storage_read(source_values, index))
            index += 1
        return values
    if _integer_storage_kind(source) or _index_method(source) is not None:
        count = _index_integer(source)
        if count < 0:
            raise ValueError("negative count")
        if count >= (1 << 63):
            raise OverflowError("cannot fit 'int' into an index-sized integer")
        values = []
        index = 0
        while index < count:
            _storage_append(values, 0)
            index += 1
        return values
    items = _make_list(source)
    length = _storage_length(items)
    values = []
    index = 0
    while index < length:
        _storage_append(values, _byte_value(_storage_read(items, index)))
        index += 1
    return values


from _mule_native import buffer_lease  # noqa: E402
from _mule_native import buffer_exports  # noqa: E402


def _byte_sublist(values, start, end):
    result = []
    index = start
    while index < end:
        _storage_append(result, _storage_read(values, index))
        index += 1
    return result


def _byte_delete(storage, index):
    if not is_slice(index):
        _storage_delete(storage, index, index + 1)
        return
    start, stop, step, count = positions(index, _storage_length(storage))
    if step == 1:
        _storage_delete(storage, start, start + count)
        return
    if step < 0:
        start += (count - 1) * step
        step = -step
    while count > 0:
        at = start + (count - 1) * step
        _storage_delete(storage, at, at + 1)
        count -= 1


_DEFAULT_BYTES_SOURCE = b""


class bytearray:
    """A mutable sequence backed by compact unsigned-byte storage."""

    def __init__(self, source=_DEFAULT_BYTES_SOURCE, encoding=None, errors="strict"):
        if buffer_exports(self):
            raise BufferError("Existing exports of data: object cannot be re-sized")
        if source is _DEFAULT_BYTES_SOURCE and encoding is None:
            self.values = _byte_storage_new()
            return
        items = _byte_values(source, encoding, errors)
        storage = _byte_storage_new()
        _byte_storage_reserve(storage, _storage_length(items))
        for item in items:
            _byte_storage_append(storage, item)
        if buffer_exports(self):
            raise BufferError("Existing exports of data: object cannot be re-sized")
        self.values = storage

    def hex(self, sep=_HEX_MISSING, bytes_per_sep=1):
        return _hex_bytes(self.values, sep, bytes_per_sep)

    @classmethod
    def fromhex(cls, string, /):
        value = bytearray(_fromhex_values(string))
        return cls(value) if cls is not bytearray else value

    def join(self, iterable, /):
        return bytearray(_bytes_joined_values(self.values, iterable))

    def ljust(self, width, fillchar=b" ", /):
        return bytearray(_byte_padding(self.values, width, fillchar, 0, "ljust"))

    def rjust(self, width, fillchar=b" ", /):
        return bytearray(_byte_padding(self.values, width, fillchar, 1, "rjust"))

    def center(self, width, fillchar=b" ", /):
        return bytearray(_byte_padding(self.values, width, fillchar, 2, "center"))

    def _check_resize(self, size):
        if size != _byte_storage_length(self.values) and _byte_storage_exports(
            self.values
        ):
            raise BufferError("Existing exports of data: object cannot be re-sized")

    def resize(self, size, /):
        size = integer_index(size)
        if size < -(1 << 63) or size >= (1 << 63):
            raise OverflowError("Python int too large to convert to C ssize_t")
        if size < 0:
            raise ValueError(f"Can only resize to positive sizes, got {size}")
        self._check_resize(size)
        length = _byte_storage_length(self.values)
        if size < length:
            _byte_storage_set_length(self.values, size)
            return
        _byte_storage_reserve(self.values, size - length)
        while _byte_storage_length(self.values) < size:
            _byte_storage_append(self.values, 0)

    def __len__(self):
        _require_bytearray(self, "__len__")
        return _byte_storage_length(self.values)

    def __getitem__(self, index):
        _require_bytearray(self, "__getitem__")
        values = self.values
        if is_slice(index):
            return bytearray(_selected(values, index))
        return _byte_storage_read(values, storage_index(values, index))

    def __setitem__(self, index, value):
        _require_bytearray(self, "__setitem__")
        values = self.values
        if is_slice(index):
            replacement = _byte_values(value, None, "strict")
            start, stop, step, count = positions(index, _byte_storage_length(values))
            if step == 1:
                self._check_resize(
                    _byte_storage_length(values) - count + len(replacement)
                )
                if stop < start:
                    stop = start
                tail = []
                at = stop
                while at < _byte_storage_length(values):
                    _storage_append(tail, _byte_storage_read(values, at))
                    at += 1
                while _byte_storage_length(values) > start:
                    _storage_delete(
                        values,
                        _byte_storage_length(values) - 1,
                        _byte_storage_length(values),
                    )
                for item in replacement:
                    _byte_storage_append(values, item)
                for item in tail:
                    _byte_storage_append(values, item)
                return
            if len(replacement) != count:
                raise ValueError(
                    "attempt to assign sequence of wrong size to extended slice"
                )
            at = 0
            while at < count:
                _byte_storage_write(values, start + at * step, replacement[at])
                at += 1
            return
        _byte_storage_write(values, storage_index(values, index), _byte_value(value))

    def __delitem__(self, index):
        count = (
            positions(index, _byte_storage_length(self.values))[3]
            if is_slice(index)
            else 1
        )
        if not is_slice(index):
            index = storage_index(self.values, index)
        self._check_resize(_byte_storage_length(self.values) - count)
        _byte_delete(self.values, index)

    def __iter__(self):
        iterator = _allocate_instance(StorageIterator)
        iterator.sequence = self
        iterator.index = 0
        iterator.exhausted = False
        return iterator

    def __buffer__(self, flags, /):
        return memoryview(self)

    def __contains__(self, item, /):
        return _bytes_find(self.values, item, 0, None) >= 0

    def __eq__(self, other):
        owner = _type_of(other)
        if owner is bytearray or is_instance(other, bytearray):
            left = self.values
            right = other.values
            if _byte_storage_length(left) != _byte_storage_length(right):
                return False
            index = 0
            while index < _byte_storage_length(left):
                if _byte_storage_read(left, index) != _byte_storage_read(right, index):
                    return False
                index += 1
            return True
        if owner is bytes or is_instance(other, bytes):
            values = self.values
            length = _byte_storage_length(values)
            if length != _storage_length(other):
                return False
            index = 0
            while index < length:
                if _byte_storage_read(values, index) != _storage_read(other, index):
                    return False
                index += 1
            return True
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        return result if result is NotImplemented else not result

    def __add__(self, other):
        _require_bytearray(self, "__add__")
        result = self.copy()
        result.extend(other)
        return result

    def __iadd__(self, other):
        _require_bytearray(self, "__iadd__")
        self.extend(other)
        return self

    def __mod__(self, args):
        _require_bytearray(self, "__mod__")
        return bytearray(_bytes_percent_format(self.values, args))

    def __mul__(self, count):
        _require_bytearray(self, "__mul__")
        return bytearray(_repeat(self.values, count))

    def __rmul__(self, count):
        _require_bytearray(self, "__rmul__")
        return bytearray(_repeat(self.values, count))

    def __imul__(self, count):
        _require_bytearray(self, "__imul__")
        repeated = _repeat(self.values, count)
        self._check_resize(len(repeated))
        _storage_delete(self.values, 0, _byte_storage_length(self.values))
        for item in repeated:
            _byte_storage_append(self.values, item)
        return self

    def __repr__(self):
        _require_bytearray(self, "__repr__")
        return "bytearray(" + repr(bytes_from_values(self.values)) + ")"

    def append(self, value):
        _require_bytearray(self, "append")
        value = _byte_value(value)
        if _byte_storage_exports(self.values):
            raise BufferError("Existing exports of data: object cannot be re-sized")
        _byte_storage_append(self.values, value)

    def extend(self, values):
        _require_bytearray(self, "extend")
        target = self.values
        owner = _type_of(values)
        if owner is bytes:
            count = _storage_length(values)
            if count != 0 and _byte_storage_exports(target):
                raise BufferError("Existing exports of data: object cannot be re-sized")
            index = 0
            while index < count:
                _byte_storage_append(target, _storage_read(values, index))
                index += 1
            return
        if owner is bytearray and values is not self:
            source = values.values
            count = _byte_storage_length(source)
            if count != 0 and _byte_storage_exports(target):
                raise BufferError("Existing exports of data: object cannot be re-sized")
            index = 0
            while index < count:
                _byte_storage_append(target, _byte_storage_read(source, index))
                index += 1
            return
        values = _byte_values(values, None, "strict")
        target = self.values
        count = _storage_length(values)
        self._check_resize(_byte_storage_length(target) + count)
        index = 0
        while index < count:
            _byte_storage_append(target, _storage_read(values, index))
            index += 1

    def insert(self, index, value):
        _require_bytearray(self, "insert")
        index = integer_index(index)
        value = _byte_value(value)
        self._check_resize(_byte_storage_length(self.values) + 1)
        position = _list_position(self.values, index)
        length = _byte_storage_length(self.values)
        _byte_storage_append(self.values, 0)
        while length > position:
            _byte_storage_write(
                self.values, length, _byte_storage_read(self.values, length - 1)
            )
            length -= 1
        _byte_storage_write(self.values, position, value)

    def pop(self, index=-1):
        _require_bytearray(self, "pop")
        index = storage_index(self.values, index)
        self._check_resize(_byte_storage_length(self.values) - 1)
        value = _byte_storage_read(self.values, index)
        _storage_delete(self.values, index, index + 1)
        return value

    def remove(self, value):
        _require_bytearray(self, "remove")
        value = _byte_value(value)
        index = 0
        while (
            index < _byte_storage_length(self.values)
            and _byte_storage_read(self.values, index) != value
        ):
            index += 1
        if index == _byte_storage_length(self.values):
            raise ValueError("value not found in bytearray")
        self._check_resize(_byte_storage_length(self.values) - 1)
        _byte_delete(self.values, index)

    def clear(self):
        _require_bytearray(self, "clear")
        self._check_resize(0)
        _storage_delete(self.values, 0, _byte_storage_length(self.values))

    def copy(self):
        _require_bytearray(self, "copy")
        return bytearray(
            _byte_sublist(self.values, 0, _byte_storage_length(self.values))
        )

    def reverse(self):
        _require_bytearray(self, "reverse")
        left = 0
        right = _byte_storage_length(self.values) - 1
        while left < right:
            value = _byte_storage_read(self.values, left)
            _byte_storage_write(
                self.values, left, _byte_storage_read(self.values, right)
            )
            _byte_storage_write(self.values, right, value)
            left += 1
            right -= 1

    def decode(self, encoding="utf-8", errors=_CODEC_ERRORS_MISSING):
        _require_bytearray(self, "decode")
        return decode(bytes_from_values(self.values), encoding, errors)

    def upper(self):
        _require_bytearray(self, "upper")
        return bytearray(_bytes_upper(self.values))

    def lower(self):
        _require_bytearray(self, "lower")
        return bytearray(_bytes_lower(self.values))

    def swapcase(self):
        _require_bytearray(self, "swapcase")
        return bytearray(_bytes_swapcase(self.values))

    def capitalize(self):
        _require_bytearray(self, "capitalize")
        return bytearray(_bytes_capitalize(self.values))

    def title(self):
        _require_bytearray(self, "title")
        return bytearray(_bytes_title(self.values))

    def isalpha(self):
        _require_bytearray(self, "isalpha")
        return _bytes_isalpha(self.values)

    def isdigit(self):
        _require_bytearray(self, "isdigit")
        return _bytes_isdigit(self.values)

    def isalnum(self):
        _require_bytearray(self, "isalnum")
        return _bytes_isalnum(self.values)

    def isspace(self):
        _require_bytearray(self, "isspace")
        return _bytes_isspace(self.values)

    def isupper(self):
        _require_bytearray(self, "isupper")
        return _bytes_isupper(self.values)

    def islower(self):
        _require_bytearray(self, "islower")
        return _bytes_islower(self.values)

    def istitle(self):
        _require_bytearray(self, "istitle")
        return _bytes_istitle(self.values)

    def isascii(self):
        _require_bytearray(self, "isascii")
        return _bytes_isascii(self.values)

    def zfill(self, width, /):
        _require_bytearray(self, "zfill")
        res = _bytes_zfill(self.values, width)
        if res is None:
            return bytearray(
                _byte_sublist(self.values, 0, _byte_storage_length(self.values))
            )
        return bytearray(res)

    def expandtabs(self, tabsize=8):
        _require_bytearray(self, "expandtabs")
        return bytearray(_bytes_expandtabs(self.values, tabsize))

    def find(self, sub, start=0, end=None):
        return _bytes_find(self.values, sub, start, end)

    def rfind(self, sub, start=0, end=None):
        return _bytes_rfind(self.values, sub, start, end)

    def index(self, sub, start=0, end=None):
        result = _bytes_find(self.values, sub, start, end)
        if result < 0:
            raise ValueError("subsection not found")
        return result

    def rindex(self, sub, start=0, end=None):
        result = _bytes_rfind(self.values, sub, start, end)
        if result < 0:
            raise ValueError("subsection not found")
        return result

    def count(self, sub, start=0, end=None):
        return _bytes_count(self.values, sub, start, end)

    def startswith(self, prefix, start=0, end=None):
        return _bytes_startswith(self.values, prefix, start, end)

    def endswith(self, suffix, start=0, end=None):
        return _bytes_endswith(self.values, suffix, start, end)

    def strip(self, chars=None, /):
        start, end = _bytes_strip_bounds(self.values, chars, True, True)
        return bytearray(_byte_sublist(self.values, start, end))

    def lstrip(self, chars=None, /):
        start, end = _bytes_strip_bounds(self.values, chars, True, False)
        return bytearray(_byte_sublist(self.values, start, end))

    def rstrip(self, chars=None, /):
        start, end = _bytes_strip_bounds(self.values, chars, False, True)
        return bytearray(_byte_sublist(self.values, start, end))

    def removeprefix(self, prefix, /):
        start, end = _bytes_removeprefix_bounds(self.values, prefix)
        return bytearray(_byte_sublist(self.values, start, end))

    def removesuffix(self, suffix, /):
        start, end = _bytes_removesuffix_bounds(self.values, suffix)
        return bytearray(_byte_sublist(self.values, start, end))

    def split(self, sep=None, maxsplit=-1):
        return [
            bytearray(_byte_sublist(self.values, start, end))
            for start, end in _bytes_split_spans(self.values, sep, maxsplit, False)
        ]

    def rsplit(self, sep=None, maxsplit=-1):
        return [
            bytearray(_byte_sublist(self.values, start, end))
            for start, end in _bytes_split_spans(self.values, sep, maxsplit, True)
        ]

    def splitlines(self, keepends=False):
        return [
            bytearray(_byte_sublist(self.values, start, end))
            for start, end in _bytes_splitlines_spans(self.values, keepends)
        ]

    def partition(self, sep, /):
        needle = _bytes_like_values(sep)
        size = _storage_length(needle)
        if size == 0:
            raise ValueError("empty separator")
        length = _byte_storage_length(self.values)
        position = _bytes_find_values(self.values, needle, 0, length)
        if position < 0:
            return (bytearray(self), bytearray(), bytearray())
        return (
            bytearray(_byte_sublist(self.values, 0, position)),
            bytearray(_byte_sublist(needle, 0, size)),
            bytearray(_byte_sublist(self.values, position + size, length)),
        )

    def rpartition(self, sep, /):
        needle = _bytes_like_values(sep)
        size = _storage_length(needle)
        if size == 0:
            raise ValueError("empty separator")
        length = _byte_storage_length(self.values)
        position = _bytes_rfind_values(self.values, needle, 0, length)
        if position < 0:
            return (bytearray(), bytearray(), bytearray(self))
        return (
            bytearray(_byte_sublist(self.values, 0, position)),
            bytearray(_byte_sublist(needle, 0, size)),
            bytearray(_byte_sublist(self.values, position + size, length)),
        )

    def replace(self, old, new, count=-1, /):
        replaced = _bytes_replaced(self.values, old, new, count)
        if replaced is None:
            return bytearray(
                _byte_sublist(self.values, 0, _byte_storage_length(self.values))
            )
        return bytearray(replaced)

    def translate(self, table, delete=b""):
        return bytearray(_bytes_translated(self.values, table, delete))

    @staticmethod
    def maketrans(frm, to):
        return _make_bytes_translation_table(frm, to)


def _memoryview_format_size(format):
    if format in "Bbc?":
        return 1
    if format in "hHe":
        return 2
    if format in "iIf":
        return 4
    if format in "lLqQdP":
        return 8
    return 0


def _scale_binary(value, exponent):
    while exponent > 0:
        value *= 2.0
        exponent -= 1
    while exponent < 0:
        value /= 2.0
        exponent += 1
    return value


def _float_from_bits(bits, mantissa_bits, exponent_bits):
    bias = (1 << (exponent_bits - 1)) - 1
    max_exponent = (1 << exponent_bits) - 1
    negative = bits >> (mantissa_bits + exponent_bits)
    exponent = (bits >> mantissa_bits) & max_exponent
    fraction = bits & ((1 << mantissa_bits) - 1)
    if exponent == max_exponent:
        value = float("inf") if fraction == 0 else float("nan")
    elif exponent == 0:
        value = _scale_binary(float(fraction), 1 - bias - mantissa_bits)
    else:
        value = _scale_binary(1.0 + fraction / (2.0**mantissa_bits), exponent - bias)
    return -value if negative else value


def _float_to_bits(value, mantissa_bits, exponent_bits):
    value = float(value)
    bias = (1 << (exponent_bits - 1)) - 1
    max_exponent = (1 << exponent_bits) - 1
    negative = value < 0.0 or (value == 0.0 and str(value).startswith("-"))
    magnitude = -value if value < 0.0 else value
    sign = (1 << (mantissa_bits + exponent_bits)) if negative else 0
    if value != value:
        return sign | (max_exponent << mantissa_bits) | (1 << (mantissa_bits - 1))
    if magnitude == float("inf"):
        return sign | (max_exponent << mantissa_bits)
    if magnitude == 0.0:
        return sign
    exponent = 0
    while magnitude >= 2.0:
        magnitude /= 2.0
        exponent += 1
    while magnitude < 1.0:
        magnitude *= 2.0
        exponent -= 1
    biased = exponent + bias
    if biased >= max_exponent:
        raise OverflowError("float too large to pack with this format")
    if biased <= 0:
        scaled = _scale_binary(magnitude, mantissa_bits + biased - 1)
        fraction = int(scaled)
        if scaled - fraction > 0.5 or (scaled - fraction == 0.5 and fraction % 2):
            fraction += 1
        if fraction >= (1 << mantissa_bits):
            return sign | (1 << mantissa_bits)
        return sign | fraction
    scaled = (magnitude - 1.0) * (2.0**mantissa_bits)
    fraction = int(scaled)
    if scaled - fraction > 0.5 or (scaled - fraction == 0.5 and fraction % 2):
        fraction += 1
    if fraction >= (1 << mantissa_bits):
        fraction = 0
        biased += 1
    return sign | (biased << mantissa_bits) | fraction


class memoryview:
    """A one-dimensional view of a bytes or bytearray object, with format
    ``'B'``. Slices share the underlying storage through an offset and step."""

    def __init__(self, source):
        self.released = False
        self.format_code = "B"
        self._shape = None
        if is_instance(source, memoryview):
            source._check()
            self.source = source.source
            self.start = source.start
            self.step = source.step
            self.length = source.length
            self._readonly = source.readonly
            self.format_code = source.format_code
            self._shape = source._shape
        elif is_instance(source, bytearray):
            self.source = source
            self.start = 0
            self.step = 1
            self.length = _storage_length(source.values)
            self._readonly = False
        elif is_instance(source, bytes):
            self.source = source
            self.start = 0
            self.step = 1
            self.length = _storage_length(source)
            self._readonly = True
        else:
            raise TypeError(
                "memoryview: a bytes-like object is required, not '"
                + _type_name(source)
                + "'"
            )
        self._lease = buffer_lease(self.source)

    def _check(self):
        if self.released:
            raise ValueError("operation forbidden on released memoryview object")

    def _byte_at(self, index):
        at = self.start + index * self.step
        if is_instance(self.source, bytearray):
            return _byte_storage_read(self.source.values, at)
        return _storage_read(self.source, at)

    def _set_byte_at(self, index, value):
        if self.readonly:
            raise TypeError("cannot modify read-only memory")
        at = self.start + index * self.step
        _byte_storage_write(self.source.values, at, _byte_value(value))

    def _itemsize(self):
        return _memoryview_format_size(self.format_code)

    def _raw_byte(self, offset):
        if is_instance(self.source, bytearray):
            return _byte_storage_read(self.source.values, offset)
        return _storage_read(self.source, offset)

    def _value_at(self, index):
        size = self._itemsize()
        base = self.start + index * self.step
        code = self.format_code
        if code == "B":
            return self._raw_byte(base)
        if code == "c":
            return bytes_from_values([self._raw_byte(base)])
        bits = 0
        position = 0
        while position < size:
            bits |= self._raw_byte(base + position) << (position * 8)
            position += 1
        if code == "?":
            return bits != 0
        if code == "e":
            return _float_from_bits(bits, 10, 5)
        if code == "f":
            return _float_from_bits(bits, 23, 8)
        if code == "d":
            return _float_from_bits(bits, 52, 11)
        if code in "bhiq" or code == "l":
            sign = 1 << (size * 8 - 1)
            return bits - (1 << (size * 8)) if bits & sign else bits
        return bits

    def _set_value_at(self, index, value):
        if self.readonly:
            raise TypeError("cannot modify read-only memory")
        size = self._itemsize()
        base = self.start + index * self.step
        code = self.format_code
        if code == "c":
            data = _byte_values(value, None, "strict")
            if len(data) != 1:
                raise TypeError("memoryview: invalid value for format 'c'")
            bits = data[0]
        elif code == "?":
            bits = 1 if value else 0
        elif code == "e":
            bits = _float_to_bits(value, 10, 5)
        elif code == "f":
            bits = _float_to_bits(value, 23, 8)
        elif code == "d":
            bits = _float_to_bits(value, 52, 11)
        else:
            bits = integer_index(value) & ((1 << (size * 8)) - 1)
        position = 0
        while position < size:
            at = base + position
            _byte_storage_write(self.source.values, at, (bits >> (position * 8)) & 0xFF)
            position += 1

    def cast(self, format, shape=None):
        self._check()
        if _memoryview_format_size(format) == 0:
            raise TypeError(
                "memoryview: destination format must be a native single character format"
            )
        if self.step != self._itemsize():
            raise TypeError("memoryview: casts are restricted to C-contiguous views")
        total = self.length * self._itemsize()
        size = _memoryview_format_size(format)
        if total % size:
            raise TypeError("memoryview: length is not a multiple of itemsize")
        result = memoryview(self)
        result.format_code = format
        result.length = total // size
        result.step = size
        result._shape = None
        if shape is not None:
            shape_tuple = tuple(integer_index(dim) for dim in shape)
            if not shape_tuple:
                raise TypeError("memoryview: shape cannot be empty")
            product = 1
            for dim in shape_tuple:
                if dim < 0:
                    raise TypeError("memoryview: shape elements must be >= 0")
                product *= dim
            if product != result.length:
                raise TypeError("memoryview: product(shape) * itemsize != buffer size")
            result._shape = shape_tuple
        return result

    @property
    def format(self):
        self._check()
        return self.format_code

    @property
    def itemsize(self):
        self._check()
        return self._itemsize()

    @property
    def ndim(self):
        self._check()
        return len(self._shape) if self._shape is not None else 1

    @property
    def nbytes(self):
        self._check()
        return self.length * self._itemsize()

    @property
    def shape(self):
        self._check()
        return self._shape if self._shape is not None else (self.length,)

    @property
    def strides(self):
        self._check()
        if self._shape is not None:
            itemsize = self._itemsize()
            strides = []
            stride = itemsize
            index = len(self._shape) - 1
            while index >= 0:
                strides.append(stride)
                stride *= self._shape[index]
                index -= 1
            strides.reverse()
            return tuple(strides)
        return (self.step,)

    @property
    def obj(self):
        self._check()
        return self.source

    @property
    def readonly(self):
        self._check()
        return self._readonly

    @property
    def c_contiguous(self):
        self._check()
        return self.step == self._itemsize()

    def __len__(self):
        self._check()
        if self._shape is not None:
            return self._shape[0]
        return self.length

    def __getitem__(self, index):
        self._check()
        if is_slice(index):
            start, stop, step, count = positions(index, self.length)
            result = memoryview(self)
            result.start = self.start + start * self.step
            result.step = self.step * step
            result.length = count
            return result
        return self._value_at(_view_index(self, index))

    def __setitem__(self, index, value):
        self._check()
        if is_slice(index):
            if self._itemsize() == 1 and is_instance(value, bytes):
                if self.readonly:
                    raise TypeError("cannot modify read-only memory")
                start, stop, step, count = positions(index, self.length)
                if _storage_length(value) != count:
                    raise ValueError(
                        "memoryview assignment: lvalue and rvalue have different structures"
                    )
                target_values = self.source.values
                base_start = self.start
                base_step = self.step
                at = 0
                while at < count:
                    _storage_write(
                        target_values,
                        base_start + (start + at * step) * base_step,
                        _storage_read(value, at),
                    )
                    at += 1
                return
            if self._itemsize() != 1:
                values = _make_list(value)
            else:
                values = (
                    _byte_values(value, None, "strict")
                    if _type_name(value) != "memoryview"
                    else value.tolist()
                )
            start, stop, step, count = positions(index, self.length)
            if len(values) != count:
                raise ValueError(
                    "memoryview assignment: lvalue and rvalue have different structures"
                )
            at = 0
            while at < count:
                if self._itemsize() != 1:
                    self._set_value_at(start + at * step, values[at])
                else:
                    self._set_byte_at(start + at * step, values[at])
                at += 1
            return
        if self._itemsize() != 1:
            self._set_value_at(_view_index(self, index), value)
        else:
            self._set_byte_at(_view_index(self, index), value)

    def __iter__(self):
        return SequenceIterator(self)

    def __buffer__(self, flags, /):
        self._check()
        return memoryview(self)

    def __hash__(self):
        self._check()
        if not self.readonly:
            raise ValueError("cannot hash writable memoryview object")
        return hash(self.tobytes())

    def __eq__(self, other):
        if _type_name(other) in ("bytes", "bytearray", "memoryview"):
            if _type_name(other) == "memoryview":
                return self.tobytes() == other.tobytes()
            return self.tobytes() == bytes(other)
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        return result if result is NotImplemented else not result

    def __repr__(self):
        return "<memory at " + hex(id(self)) + ">"

    def __enter__(self):
        self._check()
        return self

    def __exit__(self, kind, value, traceback):
        self.release()
        return False

    def release(self):
        self.released = True
        self._lease = None
        self.source = None

    def tobytes(self):
        self._check()
        if is_instance(self.source, bytes) and self.step == self._itemsize():
            return byte_copy(self.source, self.start, self.length * self._itemsize())
        values = []
        element = 0
        while element < self.length:
            base = self.start + element * self.step
            position = 0
            while position < self._itemsize():
                _storage_append(values, self._raw_byte(base + position))
                position += 1
            element += 1
        return bytes_from_values(values)

    def tolist(self):
        self._check()
        values = []
        index = 0
        while index < self.length:
            _storage_append(values, self._value_at(index))
            index += 1
        if self._shape is not None and len(self._shape) > 1:
            return _reshape_list(values, self._shape, 0)[0]
        return values

    def hex(self, sep=_HEX_MISSING, bytes_per_sep=1):
        return _hex_bytes(self.tobytes(), sep, bytes_per_sep)


def _reshape_list(values, shape, offset):
    dim = shape[0]
    if len(shape) == 1:
        return values[offset : offset + dim], offset + dim
    rest = shape[1:]
    result = []
    index = 0
    while index < dim:
        sub, offset = _reshape_list(values, rest, offset)
        result.append(sub)
        index += 1
    return result, offset


def _view_index(view, index):
    index = integer_index(index)
    if index < 0:
        index += view.length
    if index < 0 or index >= view.length:
        raise IndexError("index out of bounds on dimension 1")
    return index


def __import__(name, globals=None, locals=None, fromlist=(), level=0):
    from importlib._bootstrap import __import__ as _import

    return _import(name, globals, locals, fromlist, integer_index(level))


def open(
    file,
    mode="r",
    buffering=-1,
    encoding=None,
    errors=None,
    newline=None,
    closefd=True,
    opener=None,
):
    import io as _io

    return _io.open(file, mode, buffering, encoding, errors, newline, closefd, opener)


def compile(
    source,
    filename,
    mode,
    flags=0,
    dont_inherit=False,
    optimize=-1,
    *,
    _feature_version=-1,
):
    import ast as _ast

    return _ast._compile(
        source,
        filename,
        mode,
        flags,
        dont_inherit,
        optimize,
        _feature_version=_feature_version,
    )


def eval(expression, globals=None, locals=None):
    import ast as _ast

    return _ast._eval(expression, globals, locals)


def exec(source, globals=None, locals=None, *, closure=None):
    import ast as _ast

    return _ast._exec(source, globals, locals, closure=closure)


_install_storage(list, tuple, str, bytes, dict)

_install_builtin("open", open)
_install_builtin("compile", compile)
_install_builtin("eval", eval)
_install_builtin("exec", exec)
_install_builtin("__import__", __import__)
_install_builtin("range", range)
_install_builtin("enumerate", enumerate)
_install_builtin("iter", iter)
_install_builtin("next", next)
_install_builtin("aiter", aiter)
_install_builtin("anext", anext)
_install_builtin("list", list)
_install_builtin("tuple", tuple)
_install_builtin("chr", chr)
_install_builtin("ord", ord)
_install_builtin("str", str)
_install_builtin("bytes", bytes)
_install_builtin("bytearray", bytearray)
_install_builtin("memoryview", memoryview)
_install_builtin("repr", repr)
_install_builtin("ascii", ascii)
_install_builtin("format", format)
_install_builtin("min", min)
_install_builtin("max", max)
_install_builtin("dict", dict)
_install_builtin("getattr", getattr)
_install_builtin("hasattr", hasattr)
_install_builtin("setattr", setattr)
_install_builtin("delattr", delattr)
_install_builtin("type", type)
_install_builtin("object", object)
_install_builtin("int", int)
_install_builtin("bool", bool)
_install_builtin("float", float)
_install_builtin("complex", complex)
_install_builtin("isinstance", isinstance)
_install_builtin("issubclass", issubclass)
_install_builtin("vars", vars)
_install_builtin("locals", _builtin_locals)
_install_builtin("globals", globals)
_install_builtin("sorted", sorted)
_install_builtin("dir", dir)
_install_builtin("callable", callable)
_install_builtin("pow", pow)
_install_builtin("divmod", divmod)
_install_builtin("round", round)
_install_builtin("hex", hex)
_install_builtin("oct", oct)
_install_builtin("bin", bin)
_install_builtin("id", id)
_install_builtin("any", any)
_install_builtin("all", all)
_install_builtin("sum", sum)
_install_builtin("zip", zip)
_install_builtin("map", map)
_install_builtin("filter", filter)
_install_builtin("reversed", reversed)
_install_builtin("set", set)
_install_builtin("frozenset", frozenset)
_install_builtin("hash", hash)
_install_builtin("slice", slice)
_install_builtin("staticmethod", staticmethod)
_install_builtin("classmethod", classmethod)
_install_builtin("property", property)
_install_builtin("super", super)
_install_builtin("BaseException", BaseException)
_install_builtin("abs", abs)
_install_builtin("__debug__", True)
_install_builtin("Exception", Exception)
_install_builtin("ArithmeticError", ArithmeticError)
_install_builtin("LookupError", LookupError)
_install_builtin("IndexError", IndexError)
_install_builtin("KeyError", KeyError)
_install_builtin("ValueError", ValueError)
_install_builtin("TypeError", TypeError)
_install_builtin("RuntimeError", RuntimeError)
_install_builtin("NameError", NameError)
_install_builtin("UnboundLocalError", UnboundLocalError)
_install_builtin("ZeroDivisionError", ZeroDivisionError)
_install_builtin("OverflowError", OverflowError)
_install_builtin("AssertionError", AssertionError)
_install_builtin("StopIteration", StopIteration)
_install_builtin("GeneratorExit", GeneratorExit)
_install_builtin("StopAsyncIteration", StopAsyncIteration)
_install_builtin("MemoryError", MemoryError)
_install_builtin("SystemError", SystemError)
_install_builtin("AttributeError", AttributeError)
_install_builtin("OSError", OSError)
_install_builtin("EOFError", EOFError)
_install_builtin("RecursionError", RecursionError)
_install_builtin("UnicodeError", UnicodeError)
_install_builtin("UnicodeEncodeError", UnicodeEncodeError)
_install_builtin("UnicodeDecodeError", UnicodeDecodeError)
_install_builtin("SystemExit", SystemExit)
_install_builtin("KeyboardInterrupt", KeyboardInterrupt)
_install_builtin("FloatingPointError", FloatingPointError)
_install_builtin("BufferError", BufferError)
_install_builtin("ImportError", ImportError)
_install_builtin("ModuleNotFoundError", ModuleNotFoundError)
_install_builtin("ReferenceError", ReferenceError)
_install_builtin("NotImplementedError", NotImplementedError)
_install_builtin("PythonFinalizationError", PythonFinalizationError)
_install_builtin("SyntaxError", SyntaxError)
_install_builtin("IndentationError", IndentationError)
_install_builtin("TabError", TabError)
_install_builtin("BlockingIOError", BlockingIOError)
_install_builtin("ChildProcessError", ChildProcessError)
_install_builtin("ConnectionError", ConnectionError)
_install_builtin("BrokenPipeError", BrokenPipeError)
_install_builtin("ConnectionAbortedError", ConnectionAbortedError)
_install_builtin("ConnectionRefusedError", ConnectionRefusedError)
_install_builtin("ConnectionResetError", ConnectionResetError)
_install_builtin("FileExistsError", FileExistsError)
_install_builtin("FileNotFoundError", FileNotFoundError)
_install_builtin("InterruptedError", InterruptedError)
_install_builtin("IsADirectoryError", IsADirectoryError)
_install_builtin("NotADirectoryError", NotADirectoryError)
_install_builtin("PermissionError", PermissionError)
_install_builtin("ProcessLookupError", ProcessLookupError)
_install_builtin("TimeoutError", TimeoutError)
_install_builtin("Warning", Warning)
_install_builtin("BytesWarning", BytesWarning)
_install_builtin("DeprecationWarning", DeprecationWarning)
_install_builtin("EncodingWarning", EncodingWarning)
_install_builtin("FutureWarning", FutureWarning)
_install_builtin("ImportWarning", ImportWarning)
_install_builtin("PendingDeprecationWarning", PendingDeprecationWarning)
_install_builtin("ResourceWarning", ResourceWarning)
_install_builtin("RuntimeWarning", RuntimeWarning)
_install_builtin("SyntaxWarning", SyntaxWarning)
_install_builtin("UnicodeWarning", UnicodeWarning)
_install_builtin("UserWarning", UserWarning)
_install_builtin("EnvironmentError", OSError)
_install_builtin("IOError", OSError)


_NOT_IMPLEMENTED_SINGLETON = None


class NotImplementedType:
    """The type of the ``NotImplemented`` singleton."""

    __module__ = "builtins"

    def __new__(cls):
        global _NOT_IMPLEMENTED_SINGLETON
        if _NOT_IMPLEMENTED_SINGLETON is None:
            _NOT_IMPLEMENTED_SINGLETON = object.__new__(cls)
        return _NOT_IMPLEMENTED_SINGLETON

    def __repr__(self):
        return "NotImplemented"

    def __bool__(self):
        raise TypeError("NotImplemented should not be used in a boolean context")


NotImplemented = NotImplementedType()
register_not_implemented(NotImplemented)
_install_builtin("NotImplemented", NotImplemented)


_ELLIPSIS_SINGLETON = None


class ellipsis:
    """The type of the ``...`` singleton."""

    __module__ = "builtins"

    def __new__(cls):
        global _ELLIPSIS_SINGLETON
        if _ELLIPSIS_SINGLETON is None:
            _ELLIPSIS_SINGLETON = object.__new__(cls)
        return _ELLIPSIS_SINGLETON

    def __repr__(self):
        return "Ellipsis"


Ellipsis = ellipsis()


def complex_literal(real, imag):
    """Materialize literal components without invoking a rebound constructor."""
    return _make_complex(complex, real, imag)


def ellipsis_value():
    return Ellipsis


_install_builtin("Ellipsis", Ellipsis)


def context_exit_method(manager):
    missing = _binding_missing()
    method = _optional_special(manager, "__exit__")
    if method is missing or method is None:
        raise TypeError(
            f"'{_type_of(manager).__name__}' object does not support the context manager protocol (missed __exit__ method)"
        )
    return method


def context_enter(manager):
    missing = _binding_missing()
    method = _optional_special(manager, "__enter__")
    if method is missing or method is None:
        raise TypeError(
            f"'{_type_of(manager).__name__}' object does not support the context manager protocol"
        )
    return method()


def context_exit(exit_method, value):
    """Report the caught exception and its original traceback to ``__exit__``."""
    return exit_method(_raised_class(value), value, value.__traceback__)


class cell:
    """Shared storage for a variable captured by a nested function, as
    ``types.CellType``."""

    def __init__(self, *contents):
        if len(contents) > 1:
            raise TypeError(f"cell expected at most 1 argument, got {len(contents)}")
        self.bound = len(contents) == 1
        self.value = contents[0] if contents else None

    @property
    def cell_contents(self):
        if not self.bound:
            raise ValueError("Cell is empty")
        return self.value

    @cell_contents.setter
    def cell_contents(self, value):
        cell_set(self, value)

    @cell_contents.deleter
    def cell_contents(self):
        if not self.bound:
            raise ValueError("Cell is empty")
        cell_clear(self)

    def __repr__(self):
        if not self.bound:
            return f"<cell at {id(self):#x}: empty>"
        return f"<cell at {id(self):#x}: {type(self.value).__name__} object>"


def cell_new():
    return cell()


def cell_get(cell):
    if not cell.bound:
        raise NameError("free variable referenced before assignment in enclosing scope")
    return cell.value


def cell_unbound(optional):
    if optional:
        return _HEX_MISSING
    raise NameError("free variable referenced before assignment in enclosing scope")


def cell_get_optional(cell):
    if not cell.bound:
        return _HEX_MISSING
    return cell.value


def cell_set(cell, value):
    cell.value = value
    cell.bound = True


def cell_clear(cell):
    cell.value = None
    cell.bound = False


def snapshot_locals(names, values):
    result = {}
    count = _storage_length(names)
    index = 0
    while index < count:
        value = _storage_read(values, index)
        if value is not _HEX_MISSING:
            result[_storage_read(names, index)] = value
        index += 1
    return result


def call_with_locals(callee, snapshot):
    if callee is _builtin_locals or callee is vars:
        return snapshot
    if callee is dir:
        return sorted(snapshot)
    return callee()


def call_with_eval_context(callee, source, globals_ns, locals_ns):
    if callee is eval or callee is exec:
        return callee(source, globals_ns, locals_ns)
    return callee(source)


def globals_dict(namespace):
    return _globals_dict(namespace)


# Generic aliases are created once the builtins they use are installed.
from _template import Interpolation as _Interpolation  # noqa: E402
from _template import build_template as _build_template  # noqa: E402
from _generic_alias import class_getitem as _class_getitem  # noqa: E402
from _generic_alias import type_or as _type_or  # noqa: E402
from _generic_alias import type_ror as _type_ror  # noqa: E402
from _exceptiongroup import (  # noqa: E402
    BaseExceptionGroup,
    ExceptionGroup,
    except_star_caught as _except_star_caught,
    except_star_finish as _except_star_finish,
    except_star_match as _except_star_match,
    except_star_matched as _except_star_matched,
    except_star_start as _except_star_start,
)
from _object import _install_code_fields  # noqa: E402
from _object import _install_builtin_descriptors  # noqa: E402

# The runtime calls complete_class for classes it builds without type.__new__.
from _object import complete_class  # noqa: E402, F401


def except_star_start(exc):
    return _except_star_start(exc)


def except_star_match(state, targets):
    return _except_star_match(state, targets)


def except_star_matched(state):
    return _except_star_matched(state)


def except_star_caught(state, exc):
    return _except_star_caught(state, exc)


def except_star_finish(state):
    return _except_star_finish(state)


_install_code_fields()
_install_builtin_descriptors()

_install_builtin("BaseExceptionGroup", BaseExceptionGroup)
_install_builtin("ExceptionGroup", ExceptionGroup)
BaseExceptionGroup.__class_getitem__ = classmethod(_class_getitem)

# Builtin classes that accept type arguments, such as ``list[int]``.
list.__class_getitem__ = classmethod(_class_getitem)
tuple.__class_getitem__ = classmethod(_class_getitem)
dict.__class_getitem__ = classmethod(_class_getitem)
set.__class_getitem__ = classmethod(_class_getitem)
frozenset.__class_getitem__ = classmethod(_class_getitem)
type.__class_getitem__ = classmethod(_class_getitem)
type.__or__ = _type_or
type.__ror__ = _type_ror
