"""Native sequence and exception protocols, compiled by Mule.

Underscored storage operations are bound by the compiler. These definitions
execute as native Python functions and classes, in a private module namespace.
"""

from _mule_native import text_from_codepoints as _text_from_codepoints
from _str import TextStorage, ascii, decode, format, ord, repr, str
from _attributes import delattr, getattr, hasattr, setattr, type
from _descriptors import classmethod, property, staticmethod, super
from _dict import build_dict as _build_dict
from _dict import build_set as _build_set
from _dict import dict, frozenset, hash, set
from _mule_native import bytes_from_values as _bytes_from_values
from _slice import is_slice, positions, slice

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


def delitem(value, index):
    method = _delitem_method(value)
    if method is None:
        raise TypeError("object does not support item deletion")
    method(index)


def sorted(iterable, /, *, key=None, reverse=False):
    result = list(iterable)
    result.sort(key=key, reverse=reverse)
    return result


def delete_attribute(value, name):
    delattr(value, name)


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
_install_builtin("dict", dict)
_install_builtin("getattr", getattr)
_install_builtin("hasattr", hasattr)
_install_builtin("setattr", setattr)
_install_builtin("delattr", delattr)
_install_builtin("type", type)
_install_builtin("sorted", sorted)
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
_install_builtin("MemoryError", MemoryError)
_install_builtin("SystemError", SystemError)
_install_builtin("AttributeError", AttributeError)
_install_builtin("OSError", OSError)
_install_builtin("EOFError", EOFError)
_install_builtin("RecursionError", RecursionError)
_install_builtin("UnicodeError", UnicodeError)
_install_builtin("UnicodeEncodeError", UnicodeEncodeError)
_install_builtin("UnicodeDecodeError", UnicodeDecodeError)


class ellipsis:
    """The type of the ``...`` singleton."""

    def __repr__(self):
        return "Ellipsis"


Ellipsis = ellipsis()


def ellipsis_value():
    return Ellipsis


_install_builtin("Ellipsis", Ellipsis)


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
