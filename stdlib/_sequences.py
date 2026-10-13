"""Native sequence and exception protocols, compiled by Mule.

Underscored storage operations are bound by the compiler. These definitions
execute as native Python functions and classes, in a private module namespace.
"""

from _object import bool, complex, float, globals, int, isinstance, issubclass, object, type, vars
from _object import _make_complex
from _mule_native import text_from_codepoints as _text_from_codepoints
from _mule_native import typed_value as _typed_value
from _str import TextStorage, ascii, decode, format, ord, repr
from _str import str as _make_str
from _attributes import delattr, getattr, hasattr, setattr
from _descriptors import classmethod, property, staticmethod, super
from _generators import generator as _generator
from _generators import yield_from as _yield_from
from _generators import async_generator as _async_generator
from _generators import async_generator_yield as _async_generator_yield
from _generators import await_value as _await_value
from _generators import coroutine as _coroutine
from _dict import build_dict as _build_dict
from _dict import build_set as _build_set
from _dict import dict, frozenset, hash, set
from _mule_native import bytes_from_values as _bytes_from_values
from _mule_native import call_with as _call_with
from _mule_native import identity as _identity
from _mule_native import is_class as _is_class
from _mule_native import register_not_implemented as _register_not_implemented
from _mule_native import namespace_items as _namespace_items
from _mule_native import new_namespace as _new_namespace
from _mule_native import is_instance as _is_instance
from _mule_native import overriding_new as _overriding_new
from _slice import is_slice, positions, slice

# ruff: noqa: F821


class BaseException:
    # Set when a handler catches the exception; see mule/runtime/traceback.rs.
    __traceback__ = None

    def __init__(self, *args):
        self.args = args
        self.__cause__ = None
        self.__context__ = None
        self.__suppress_context__ = False

    def with_traceback(self, tb):
        self.__traceback__ = tb
        return self

    def __str__(self):
        if len(self.args) == 0:
            return ""
        if len(self.args) == 1:
            return str(self.args[0])
        return str(self.args)


class Exception(BaseException):
    pass


class ArithmeticError(Exception):
    pass


class LookupError(Exception):
    pass


class IndexError(LookupError):
    pass


class KeyError(LookupError):
    pass


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
    pass


class EOFError(Exception):
    pass


class RecursionError(RuntimeError):
    pass


class UnicodeError(ValueError):
    pass


class UnicodeEncodeError(UnicodeError):
    pass


class UnicodeDecodeError(UnicodeError):
    pass


class SystemExit(BaseException):
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
    choices = _exception_targets(expected)
    index = 0
    while index < _storage_length(choices):
        target = _storage_read(choices, index)
        parent = _class_identity(target)
        valid = False
        while parent is not None:
            if parent is BaseException:
                valid = True
                break
            parent = _class_parent(parent)
        if not valid:
            raise TypeError("catching classes that do not inherit from BaseException")
        parent = _raised_class(value)
        while parent is not None:
            if parent is target:
                return True
            parent = _class_parent(parent)
        index += 1
    return False


def iteration_exhausted(value):
    return exception_matches(value, StopIteration)


def raise_value(value):
    parent = _raised_class(value)
    valid = False
    while parent is not None:
        if parent is BaseException:
            valid = True
            break
        parent = _class_parent(parent)
    if not valid:
        # Use the physical error primitive here to avoid recursive invalid raises.
        return _invalid_raise()
    instance = _exception_instance(value)
    # An exception raised while another is being handled records it as its
    # context, unless it already has one or is that exception itself.
    active = _active_exception()
    if active is not None and active is not instance and instance.__context__ is None:
        instance.__context__ = active
    return _raise_pending(instance)


def exception_instance(value):
    """The instance a ``raise`` statement will raise, creating it from a class."""
    parent = _raised_class(value)
    valid = False
    while parent is not None:
        if parent is BaseException:
            valid = True
            break
        parent = _class_parent(parent)
    if not valid:
        raise TypeError("exceptions must derive from BaseException")
    return _exception_instance(value)


def set_exception_cause(instance, cause):
    """``raise instance from cause``: the cause is None or an exception."""
    if cause is not None:
        cause = exception_instance(cause)
    instance.__cause__ = cause
    instance.__suppress_context__ = True


def integer_index(value):
    method = _index_method(value)
    if method is not None:
        value = method()
    return _integer_storage(value)


def chr(value, /):
    value = integer_index(value)
    if value < 0 or value > 0x10FFFF:
        raise ValueError("chr() arg not in range(0x110000)")
    return _text_from_codepoints([value])


def getitem(value, index):
    method = _getitem_method(value)
    if method is None:
        # Subscripting a class without a metaclass __getitem__ (PEP 560).
        if _is_class(value):
            hook = getattr(value, "__class_getitem__", None)
            if hook is not None:
                return hook(index)
            raise TypeError(f"type '{value.__name__}' is not subscriptable")
        raise TypeError(f"'{_type_name(value)}' object is not subscriptable")
    return method(index)


def delitem(value, index):
    method = _delitem_method(value)
    if method is None:
        raise TypeError("object does not support item deletion")
    method(index)


def any(iterable, /):
    for item in iterable:
        if item:
            return True
    return False


def all(iterable, /):
    for item in iterable:
        if not item:
            return False
    return True


def sum(iterable, /, start=0):
    if _text_storage(start):
        raise TypeError("sum() can't sum strings [use ''.join(seq) instead]")
    total = start
    for item in iterable:
        total = total + item
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
    def __init__(self, function, *iterables):
        if not iterables:
            raise TypeError("map() must have at least two arguments.")
        self.function = function
        self.items = zip(*iterables)

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
            if (item if self.function is None else self.function(item)):
                return item


def reversed(sequence, /):
    method = _reversed_method(sequence)
    if method is not None:
        return method()
    items = list(sequence)
    _reverse_in_place(items)
    return SequenceIterator(items)


def pow(base, exp, mod=None):
    if mod is None:
        return base ** exp
    method = getattr(type(base), "__pow__", None)
    if method is not None:
        result = method(base, exp, mod)
        if result is not NotImplemented:
            return result
    if not (_integer_kind(base) and _integer_kind(exp) and _integer_kind(mod)):
        raise TypeError("pow() 3rd argument not allowed unless all arguments are integers")
    if mod == 0:
        raise ValueError("pow() 3rd argument cannot be 0")
    if exp < 0:
        raise ValueError("base is not invertible for the given modulus")
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


def divmod(left, right, /):
    method = getattr(type(left), "__divmod__", None)
    if method is not None:
        result = method(left, right)
        if result is not NotImplemented:
            return result
    method = getattr(type(right), "__rdivmod__", None)
    if method is not None:
        result = method(right, left)
        if result is not NotImplemented:
            return result
    return (left // right, left % right)


def round(number, ndigits=None):
    method = getattr(type(number), "__round__", None)
    if method is None:
        raise TypeError(f"type {type(number).__name__} doesn't define __round__ method")
    if ndigits is None:
        return method(number)
    return method(number, ndigits)


def _digits(value, base, prefix):
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
    if _is_class(obj):
        return True
    kind = _type_name(obj)
    if kind in ("function", "builtin_function_or_method", "method"):
        return True
    return getattr(type(obj), "__call__", None) is not None


def id(obj, /):
    return _identity(obj)


def sorted(iterable, /, *, key=None, reverse=False):
    result = list(iterable)
    result.sort(key=key, reverse=reverse)
    return result


def append_argument(positional, value):
    _storage_append(positional, value)


def extend_arguments(positional, iterable):
    for value in iterable:
        _storage_append(positional, value)


def add_keyword(keywords, name, value):
    if name in keywords:
        raise TypeError(f"got multiple values for keyword argument '{name}'")
    keywords[name] = value


def merge_keywords(keywords, mapping):
    for name in mapping.keys():
        if not _text_storage(name):
            raise TypeError("keywords must be strings")
        add_keyword(keywords, name, mapping[name])


def tuple_from_list(items):
    return _storage_tuple(items)


def dict_merge(mapping, other):
    if _is_instance(other, dict):
        mapping.update(other)
        return
    keys = getattr(other, "keys", None)
    if keys is None:
        raise TypeError(f"'{_type_name(other)}' object is not a mapping")
    for key in keys():
        mapping[key] = other[key]


def call_expanded(function, positional, keywords):
    names, values = [], []
    for name in keywords:
        _storage_append(names, name)
        _storage_append(values, keywords[name])
    return _call_with(
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


def prepare_class(name, bases, keywords):
    """The namespace a class body runs against (``__build_class__``)."""
    if keywords is None and len(bases) <= 1 and _direct_bases(bases):
        return _new_namespace()
    keywords = {} if keywords is None else keywords
    explicit = keywords.get("metaclass")
    metaclass = _metaclass(bases, explicit)
    if metaclass is type:
        return _new_namespace()
    others = {}
    for key in keywords:
        if key != "metaclass":
            others[key] = keywords[key]
    return metaclass.__prepare__(name, bases, **others)


def build_class(name, bases, namespace, keywords, fields):
    """Create a class through its metaclass after its body has run."""
    keywords = {} if keywords is None else keywords
    explicit = keywords.get("metaclass")
    metaclass = _metaclass(bases, explicit)
    others = {}
    for key in keywords:
        if key != "metaclass":
            others[key] = keywords[key]
    items = _namespace_items(namespace)
    if items is not None:
        namespace = {}
        for key, value in items:
            namespace[key] = value
    namespace["__mule_fields__"] = fields
    return metaclass(name, bases, namespace, **others)


def build_template(parts):
    return _build_template(parts)


def make_interpolation(value, expression, conversion, format_spec):
    return _Interpolation(value, expression, conversion, format_spec)


def make_slice(start, stop, step):
    return slice(start, stop, step)


def build_dict(flat):
    return _build_dict(flat)


def build_set(items):
    return _build_set(items)


def setitem(value, index, item):
    method = _setitem_method(value)
    if method is None:
        raise TypeError("object does not support item assignment")
    method(index, item)


def storage_index(value, index):
    index = integer_index(index)
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


def _selected(sequence, bounds):
    """The elements a slice selects, in order, as a new list."""
    start, stop, step, count = positions(bounds, _storage_length(sequence))
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


def _repeat(sequence, count):
    count = integer_index(count)
    result = []
    while count > 0:
        index = 0
        length = _storage_length(sequence)
        while index < length:
            _storage_append(result, _storage_read(sequence, index))
            index += 1
        count -= 1
    return result


def _sequence_order(left, right):
    """The pair that orders two sequences lexicographically: the first unequal
    elements, or else the two lengths."""
    index = 0
    left_length, right_length = _storage_length(left), _storage_length(right)
    while index < left_length and index < right_length:
        a = _storage_read(left, index)
        b = _storage_read(right, index)
        if not (a is b or a == b):
            return a, b
        index += 1
    return left_length, right_length


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


class _OrderedSequence:
    """Equality and lexicographic ordering shared by list and tuple."""

    def __eq__(self, other):
        return sequence_equal(self, other)

    def __ne__(self, other):
        return not sequence_equal(self, other)

    def __lt__(self, other):
        a, b = _ordered_pair(self, other)
        return a < b

    def __le__(self, other):
        a, b = _ordered_pair(self, other)
        return a <= b

    def __gt__(self, other):
        a, b = _ordered_pair(self, other)
        return a > b

    def __ge__(self, other):
        a, b = _ordered_pair(self, other)
        return a >= b

    def __iter__(self):
        return SequenceIterator(self)

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


def _ordered_pair(left, right):
    if not _storage_compatible(left, right):
        raise TypeError(
            f"'<' not supported between instances of '{_type_name(left)}' and "
            f"'{_type_name(right)}'"
        )
    return _sequence_order(left, right)


class ListStorage(_OrderedSequence):
    def __getitem__(self, index):
        if is_slice(index):
            return _selected(self, index)
        return _storage_read(self, storage_index(self, index))

    def __setitem__(self, index, value):
        if not is_slice(index):
            _storage_write(self, storage_index(self, index), value)
            return
        items = list(value)
        start, stop, step, count = positions(index, _storage_length(self))
        if step == 1:
            _storage_delete(self, start, stop if stop > start else start)
            offset = 0
            while offset < _storage_length(items):
                _storage_insert(self, start + offset, _storage_read(items, offset))
                offset += 1
            return
        if _storage_length(items) != count:
            raise ValueError(
                f"attempt to assign sequence of size {_storage_length(items)} "
                f"to extended slice of size {count}"
            )
        offset = 0
        while offset < count:
            _storage_write(self, start + offset * step, _storage_read(items, offset))
            offset += 1

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
        result = self.copy()
        _reverse_in_place(result)
        return SequenceIterator(result)

    def append(self, value):
        _storage_append(self, value)

    def extend(self, iterable):
        if iterable is self:
            iterable = self.copy()
        for item in iterable:
            _storage_append(self, item)

    def insert(self, index, value):
        _storage_insert(self, _list_position(self, index), value)

    def pop(self, index=-1):
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
        if reverse:
            _reverse_in_place(items)
        keys = items
        if key is not None:
            keys = []
            for item in items:
                _storage_append(keys, key(item))
        items = _merge_sort(items, keys)
        if reverse:
            _reverse_in_place(items)
        _storage_delete(self, 0, _storage_length(self))
        self.extend(items)


class TupleStorage(_OrderedSequence):
    def __getitem__(self, index):
        if is_slice(index):
            return _storage_tuple(_selected(self, index))
        return _storage_read(self, storage_index(self, index))

    def __add__(self, other):
        if not _tuple_storage(other):
            raise TypeError(
                f'can only concatenate tuple (not "{_type_name(other)}") to tuple'
            )
        result = list(self)
        for item in other:
            _storage_append(result, item)
        return _storage_tuple(result)

    def __mul__(self, count):
        return _storage_tuple(_repeat(self, count))

    def __rmul__(self, count):
        return _storage_tuple(_repeat(self, count))


class BytesStorage:
    def __getitem__(self, index):
        if is_slice(index):
            return _bytes_from_values(_selected(self, index))
        return _storage_read(self, storage_index(self, index))

    def __iter__(self):
        return SequenceIterator(self)

    def decode(self, encoding="utf-8", errors="strict"):
        return decode(self, encoding, errors)


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


def iterate(value):
    method = _iter_method(value)
    if method is not None:
        iterator = method()
        if _next_method(iterator) is None:
            raise TypeError("__iter__ returned a non-iterator")
        return iterator
    if _getitem_method(value) is not None:
        return SequenceIterator(value)
    raise TypeError("object is not iterable")


def next_item(value):
    method = _next_method(value)
    if method is None:
        raise TypeError("object is not an iterator")
    return method()


def unpack(value, count):
    count = _integer_storage(count)
    if _tuple_storage(value):
        length = _storage_length(value)
        if length < count:
            raise ValueError("not enough values to unpack")
        if length > count:
            raise ValueError("too many values to unpack")
        return value
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


class range:
    def __init__(self, *args):
        count = _storage_length(args)
        if count == 1:
            start = 0
            stop = integer_index(_storage_read(args, 0))
            step = 1
        elif count == 2 or count == 3:
            start = integer_index(_storage_read(args, 0))
            stop = integer_index(_storage_read(args, 1))
            step = 1
            if count == 3:
                step = integer_index(_storage_read(args, 2))
        else:
            raise TypeError("range expects one to three arguments")
        if step == 0:
            raise ValueError("range step must not be zero")
        self.start = start
        self.stop = stop
        self.step = step

    def __iter__(self):
        return RangeIterator(self.start, self.stop, self.step)

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


def enumerate(*args):
    count = _storage_length(args)
    if count != 1 and count != 2:
        raise TypeError("enumerate expects one or two arguments")
    start = 0
    if count == 2:
        start = integer_index(_storage_read(args, 1))
    return Enumerate(iterate(_storage_read(args, 0)), start)


def iter(*args):
    if _storage_length(args) != 1:
        raise TypeError("iter currently expects one argument")
    return iterate(_storage_read(args, 0))


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
        for value in _storage_read(args, 0):
            _storage_append(result, value)
    return result


def _make_tuple(*args):
    return _storage_tuple(list_from_arguments(args))


def list_from_arguments(args):
    count = _storage_length(args)
    if count > 1:
        raise TypeError("tuple expects at most one argument")
    result = []
    if count == 1:
        for value in _storage_read(args, 0):
            _storage_append(result, value)
    return result


_missing_default = _binding_missing()


def _extreme(args, key, default, greatest):
    count = _storage_length(args)
    if count == 0:
        raise TypeError("min/max expected at least one argument")
    if count == 1:
        iterator = iterate(_storage_read(args, 0))
    else:
        if default is not _missing_default:
            raise TypeError("default is only valid with one iterable argument")
        iterator = iterate(args)
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
        if greatest:
            better = candidate_key > best_key
        else:
            better = candidate_key < best_key
        if better:
            best = candidate
            best_key = candidate_key


def min(*args, key=None, default=_missing_default):
    return _extreme(args, key, default, False)


def max(*args, key=None, default=_missing_default):
    return _extreme(args, key, default, True)


class list(ListStorage):
    def __new__(cls, *args):
        value = _make_list(*args)
        if cls is not list:
            return _typed_value(value, cls)
        return value


class tuple(TupleStorage):
    def __new__(cls, *args):
        value = _make_tuple(*args)
        if cls is not tuple:
            return _typed_value(value, cls)
        return value


class str(TextStorage):
    def __new__(cls, *args):
        value = _make_str(*args)
        if cls is not str:
            return _typed_value(value, cls)
        return value


class bytes(BytesStorage):
    def __new__(cls, source=b"", encoding=None, errors="strict"):
        if _text_storage(source):
            if encoding is None:
                raise TypeError("string argument without an encoding")
            value = source.encode(encoding, errors)
            return _typed_value(value, cls) if cls is not bytes else value
        if _integer_storage_kind(source):
            values = []
            index = 0
            while index < source:
                _storage_append(values, 0)
                index += 1
            value = _bytes_from_values(values)
            return _typed_value(value, cls) if cls is not bytes else value
        if _type_name(source) == "bytes":
            return _typed_value(source, cls) if cls is not bytes else source
        value = _bytes_from_values(_make_list(source))
        return _typed_value(value, cls) if cls is not bytes else value


def _integer_storage_kind(value):
    return _type_name(value) == "int" or _type_name(value) == "bool"


def _byte_value(value):
    if not _integer_storage_kind(value):
        raise TypeError("'" + _type_name(value) + "' object cannot be interpreted as an integer")
    value = _integer_storage(value)
    if value < 0 or value > 255:
        raise ValueError("byte must be in range(0, 256)")
    return value


def _byte_values(source, encoding, errors):
    if _text_storage(source):
        if encoding is None:
            raise TypeError("string argument without an encoding")
        source = source.encode(encoding, errors)
    if _integer_storage_kind(source):
        count = _integer_storage(source)
        if count < 0:
            raise ValueError("negative count")
        values = []
        index = 0
        while index < count:
            _storage_append(values, 0)
            index += 1
        return values
    values = []
    for item in _make_list(source):
        _storage_append(values, _byte_value(item))
    return values


class bytearray:
    """A mutable sequence of bytes, stored as a list of ints in range(256)."""

    def __init__(self, source=b"", encoding=None, errors="strict"):
        self.values = _byte_values(source, encoding, errors)

    def __len__(self):
        return _storage_length(self.values)

    def __getitem__(self, index):
        if is_slice(index):
            return bytearray(_selected(self.values, index))
        return _storage_read(self.values, storage_index(self.values, index))

    def __setitem__(self, index, value):
        if is_slice(index):
            replacement = _byte_values(value, None, "strict")
            start, stop, step, count = positions(index, _storage_length(self.values))
            if step == 1:
                tail = []
                at = stop
                while at < _storage_length(self.values):
                    _storage_append(tail, _storage_read(self.values, at))
                    at += 1
                while _storage_length(self.values) > start:
                    del self.values[_storage_length(self.values) - 1]
                for item in replacement:
                    _storage_append(self.values, item)
                for item in tail:
                    _storage_append(self.values, item)
                return
            if len(replacement) != count:
                raise ValueError("attempt to assign sequence of wrong size to extended slice")
            at = 0
            while at < count:
                _storage_write(self.values, start + at * step, replacement[at])
                at += 1
            return
        _storage_write(self.values, storage_index(self.values, index), _byte_value(value))

    def __delitem__(self, index):
        del self.values[index]

    def __iter__(self):
        return SequenceIterator(self)

    def __eq__(self, other):
        if _type_name(other) == "bytearray":
            return self.values == other.values
        if _type_name(other) == "bytes":
            return self.values == _make_list(other)
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        return result if result is NotImplemented else not result

    def __add__(self, other):
        return bytearray(self.values + _byte_values(other, None, "strict"))

    def __repr__(self):
        return "bytearray(" + repr(_bytes_from_values(self.values)) + ")"

    def append(self, value):
        _storage_append(self.values, _byte_value(value))

    def extend(self, values):
        for item in _byte_values(values, None, "strict"):
            _storage_append(self.values, item)

    def insert(self, index, value):
        self.values.insert(_list_position(self.values, index), _byte_value(value))

    def pop(self, index=-1):
        return self.values.pop(index)

    def remove(self, value):
        self.values.remove(_byte_value(value))

    def clear(self):
        self.values.clear()

    def copy(self):
        return bytearray(self.values)

    def reverse(self):
        self.values.reverse()

    def decode(self, encoding="utf-8", errors="strict"):
        return decode(_bytes_from_values(self.values), encoding, errors)


class memoryview:
    """A one-dimensional view of a bytes or bytearray object, with format
    ``'B'``. Slices share the underlying storage through an offset and step."""

    def __init__(self, source):
        self.released = False
        if _type_name(source) == "memoryview":
            self.source = source.source
            self.start = source.start
            self.step = source.step
            self.length = source.length
            self.readonly = source.readonly
        elif _type_name(source) == "bytearray":
            self.source = source
            self.start = 0
            self.step = 1
            self.length = _storage_length(source.values)
            self.readonly = False
        elif _type_name(source) == "bytes":
            self.source = source
            self.start = 0
            self.step = 1
            self.length = _storage_length(source)
            self.readonly = True
        else:
            raise TypeError(
                "memoryview: a bytes-like object is required, not '"
                + _type_name(source)
                + "'"
            )

    def _check(self):
        if self.released:
            raise ValueError("operation forbidden on released memoryview object")

    def _byte_at(self, index):
        at = self.start + index * self.step
        if _type_name(self.source) == "bytearray":
            return _storage_read(self.source.values, at)
        return _storage_read(self.source, at)

    def _set_byte_at(self, index, value):
        if self.readonly:
            raise TypeError("cannot modify read-only memory")
        at = self.start + index * self.step
        _storage_write(self.source.values, at, _byte_value(value))

    @property
    def format(self):
        return "B"

    @property
    def itemsize(self):
        return 1

    @property
    def ndim(self):
        return 1

    @property
    def nbytes(self):
        return self.length

    @property
    def shape(self):
        return (self.length,)

    @property
    def strides(self):
        return (self.step,)

    @property
    def obj(self):
        return self.source

    @property
    def c_contiguous(self):
        return self.step == 1

    def __len__(self):
        self._check()
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
        return self._byte_at(_view_index(self, index))

    def __setitem__(self, index, value):
        self._check()
        if is_slice(index):
            values = _byte_values(value, None, "strict") if _type_name(value) != "memoryview" else value.tolist()
            start, stop, step, count = positions(index, self.length)
            if len(values) != count:
                raise ValueError("memoryview assignment: lvalue and rvalue have different structures")
            at = 0
            while at < count:
                self._set_byte_at(start + at * step, values[at])
                at += 1
            return
        self._set_byte_at(_view_index(self, index), value)

    def __iter__(self):
        return SequenceIterator(self)

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

    def tobytes(self):
        self._check()
        return _bytes_from_values(self.tolist())

    def tolist(self):
        self._check()
        values = []
        index = 0
        while index < self.length:
            _storage_append(values, self._byte_at(index))
            index += 1
        return values

    def hex(self):
        digits = "0123456789abcdef"
        result = ""
        for value in self.tolist():
            result += digits[value >> 4] + digits[value & 15]
        return result


def _view_index(view, index):
    index = integer_index(index)
    if index < 0:
        index += view.length
    if index < 0 or index >= view.length:
        raise IndexError("index out of bounds on dimension 1")
    return index


def open(file, mode='r', buffering=-1, encoding=None, errors=None, newline=None, closefd=True, opener=None):
    import io as _io

    return _io.open(file, mode, buffering, encoding, errors, newline, closefd, opener)


_install_storage(list, tuple, str, bytes)

_install_builtin("open", open)
_install_builtin("range", range)
_install_builtin("enumerate", enumerate)
_install_builtin("iter", iter)
_install_builtin("next", next)
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
_install_builtin("globals", globals)
_install_builtin("sorted", sorted)
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


class NotImplementedType:
    """The type of the ``NotImplemented`` singleton."""

    def __repr__(self):
        return "NotImplemented"


NotImplemented = NotImplementedType()
_register_not_implemented(NotImplemented)
_install_builtin("NotImplemented", NotImplemented)


class ellipsis:
    """The type of the ``...`` singleton."""

    def __repr__(self):
        return "Ellipsis"


Ellipsis = ellipsis()


def complex_literal(real, imag):
    """Materialize literal components without invoking a rebound constructor."""
    return _make_complex(complex, real, imag)


def ellipsis_value():
    return Ellipsis


_install_builtin("Ellipsis", Ellipsis)


def context_exit(exit_method, value):
    """Report a pending exception to a context manager's ``__exit__``.

    The caller suppresses the exception when the result is true. Mule has no
    traceback objects yet, so the third argument is None.
    """
    return exit_method(_raised_class(value), value, None)


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


def cell_set(cell, value):
    cell.value = value
    cell.bound = True


def cell_clear(cell):
    cell.value = None
    cell.bound = False


# Generic aliases are created once the builtins they use are installed.
from _template import Interpolation as _Interpolation  # noqa: E402
from _template import build_template as _build_template  # noqa: E402
from _generic_alias import class_getitem as _class_getitem  # noqa: E402
from _generic_alias import type_or as _type_or  # noqa: E402
from _generic_alias import type_ror as _type_ror  # noqa: E402

# Builtin classes that accept type arguments, such as ``list[int]``.
list.__class_getitem__ = classmethod(_class_getitem)
tuple.__class_getitem__ = classmethod(_class_getitem)
dict.__class_getitem__ = classmethod(_class_getitem)
set.__class_getitem__ = classmethod(_class_getitem)
frozenset.__class_getitem__ = classmethod(_class_getitem)
type.__class_getitem__ = classmethod(_class_getitem)
type.__or__ = _type_or
type.__ror__ = _type_ror
