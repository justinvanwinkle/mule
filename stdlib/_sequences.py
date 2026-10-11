"""Native sequence and exception protocols, compiled by Mule.

Underscored storage operations are bound by the compiler. These definitions
execute as native Python functions and classes, in a private module namespace.
"""

from _mule_native import text_from_codepoints as _text_from_codepoints
from _str import TextStorage, ascii, decode, format, ord, repr, str

# ruff: noqa: F821


class BaseException:
    def __init__(self, *args):
        self.args = args
        self.__cause__ = None
        self.__context__ = None
        self.__suppress_context__ = False

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
    pass


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
        raise TypeError("object is not subscriptable")
    return method(index)


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


class ListStorage:
    def __getitem__(self, index):
        return _storage_read(self, storage_index(self, index))

    def __setitem__(self, index, value):
        _storage_write(self, storage_index(self, index), value)

    def append(self, value):
        _storage_append(self, value)

    def __iter__(self):
        return SequenceIterator(self)

    def __eq__(self, other):
        return sequence_equal(self, other)

    def __ne__(self, other):
        return not sequence_equal(self, other)


class TupleStorage:
    def __getitem__(self, index):
        return _storage_read(self, storage_index(self, index))

    def __iter__(self):
        return SequenceIterator(self)

    def __eq__(self, other):
        return sequence_equal(self, other)

    def __ne__(self, other):
        return not sequence_equal(self, other)


class BytesStorage:
    def __getitem__(self, index):
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


class Range:
    def __init__(self, args):
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


def range(*args):
    return Range(args)


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


def list(*args):
    count = _storage_length(args)
    if count > 1:
        raise TypeError("list expects at most one argument")
    result = []
    if count == 1:
        for value in _storage_read(args, 0):
            _storage_append(result, value)
    return result


def tuple(*args):
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


_install_storage(ListStorage, TupleStorage, TextStorage, BytesStorage)
_install_builtin("range", range)
_install_builtin("enumerate", enumerate)
_install_builtin("iter", iter)
_install_builtin("next", next)
_install_builtin("list", list)
_install_builtin("tuple", tuple)
_install_builtin("chr", chr)
_install_builtin("ord", ord)
_install_builtin("str", str)
_install_builtin("repr", repr)
_install_builtin("ascii", ascii)
_install_builtin("format", format)
_install_builtin("min", min)
_install_builtin("max", max)
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
_install_builtin("MemoryError", MemoryError)
_install_builtin("SystemError", SystemError)
_install_builtin("AttributeError", AttributeError)
_install_builtin("OSError", OSError)
_install_builtin("EOFError", EOFError)
_install_builtin("RecursionError", RecursionError)
_install_builtin("UnicodeError", UnicodeError)
_install_builtin("UnicodeEncodeError", UnicodeEncodeError)
_install_builtin("UnicodeDecodeError", UnicodeDecodeError)


def context_exit(exit_method, value):
    """Report a pending exception to a context manager's ``__exit__``.

    The caller suppresses the exception when the result is true. Mule has no
    traceback objects yet, so the third argument is None.
    """
    return exit_method(_raised_class(value), value, None)


class Cell:
    """Shared storage for a variable captured by a nested function."""

    def __init__(self):
        self.bound = False
        self.value = None


def cell_new():
    return Cell()


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
