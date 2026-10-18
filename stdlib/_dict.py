"""The ``dict``, ``set`` and ``frozenset`` types and ``hash``, compiled by Mule.

The tables follow CPython's compact layout (``Objects/dictobject.c``): a sparse
index table, sized to a power of two and probed with CPython's perturbation
sequence, refers to a dense list of entries kept in insertion order. Deleting
an entry leaves a marker in both, and resizing compacts them. ``set`` uses the
same table without values; CPython's ``Objects/setobject.c`` uses a separate
open-addressing table, but set iteration order is unspecified, so sharing the
dict layout changes no specified behavior.

``hash`` follows CPython's numeric hashing (``Python/pyhash.c`` and
``Objects/longobject.c``: integers reduce modulo the Mersenne prime 2**61 - 1)
and tuple hashing (``Objects/tupleobject.c``, the xxHash-based combination).
String and bytes hashes come from a native primitive; CPython randomizes them
per process, so their values are not part of the language contract.
"""

from _descriptors import classmethod
from _mule_native import bind_instance_attribute
from _mule_native import class_lookup_default
from _mule_native import class_of
from _mule_native import identity_hash
from _mule_native import is_class
from _mule_native import is_subclass
from _mule_native import math_number_kind
from _mule_native import storage_hash

# Compiler-provided physical primitives are not host Python globals.
# ruff: noqa: F821

_EMPTY = -1
_DUMMY = -2
_MINIMUM_SIZE = 8
_MODULUS = 2305843009213693951
_WORD = 0xFFFFFFFFFFFFFFFF
_PERTURB_MASK = 0x3FFFFFFFFFFFFFFF
_PRIME_1 = 11400714785074694791
_PRIME_2 = 14029467366897019727
_PRIME_5 = 2870177450012600261


class _Deleted:
    """The marker an entry holds after its key is deleted."""


class _Missing:
    """The default of optional arguments that accept any value, None included."""


_DELETED = _Deleted()
_MISSING = _Missing()


def _signed(value):
    value &= _WORD
    return value - (1 << 64) if value >= 1 << 63 else value


def _integer_hash(value):
    result = (-value if value < 0 else value) % _MODULUS
    if value < 0:
        result = -result
    return -2 if result == -1 else result


def _tuple_hash(value):
    accumulator = _PRIME_5
    for index in range(_storage_length(value)):
        item = _storage_read(value, index)
        lane = hash(item) & _WORD
        accumulator = (accumulator + lane * _PRIME_2) & _WORD
        accumulator = ((accumulator << 31) | (accumulator >> 33)) & _WORD
        accumulator = (accumulator * _PRIME_1) & _WORD
    accumulator = (accumulator + (_storage_length(value) ^ (_PRIME_5 ^ 3527539))) & _WORD
    if accumulator == _WORD:
        return 1546275796
    return _signed(accumulator)


def hash(value, /):
    kind = storage_hash(value)
    if kind is not None:
        return kind
    if _integer_storage_kind(value):
        return _integer_hash(_integer_storage(value))
    if _tuple_storage(value):
        return _tuple_hash(value)
    if is_class(value):
        return identity_hash(value)
    owner = _raised_class(value)
    if owner is None:
        if _type_name(value) == "list":
            raise TypeError("unhashable type: 'list'")
        return identity_hash(value)
    missing = _binding_missing()
    method = class_lookup_default(owner, "__hash__", missing)
    if method is missing:
        return identity_hash(value)
    if method is None:
        raise TypeError("unhashable type: '" + _type_name(value) + "'")
    result = bind_instance_attribute(method, value, owner)()
    if not _integer_storage_kind(result):
        raise TypeError("__hash__ method should return an integer")
    result = _integer_storage(result)
    if result < -(1 << 63) or result >= 1 << 63:
        return _integer_hash(result)
    return result


def _integer_storage_kind(value):
    return math_number_kind(value) == 1


class _Table:
    """Hash table storage shared by dict and the set types."""

    def _clear_table(self):
        self._indices = _empty_indices(_MINIMUM_SIZE)
        self._hashes = []
        self._keys = []
        self._values = []
        self._used = 0

    def _find(self, key, code):
        """The index-table slot for ``key`` and its entry, or -1 for none."""
        indices = self._indices
        mask = _storage_length(indices) - 1
        perturb = code & _PERTURB_MASK
        slot = perturb & mask
        while True:
            entry = indices[slot]
            if entry == _EMPTY:
                return slot, -1
            if entry >= 0 and self._hashes[entry] == code:
                existing = self._keys[entry]
                if existing is key or existing == key:
                    return slot, entry
            perturb >>= 5
            slot = (slot * 5 + perturb + 1) & mask

    def _lookup(self, key):
        slot, entry = self._find(key, hash(key))
        return entry

    def _get_value(self, key, default):
        entry = self._lookup(key)
        return self._values[entry] if entry >= 0 else default

    def _insert(self, key, value):
        code = hash(key)
        slot, entry = self._find(key, code)
        if entry >= 0:
            self._values[entry] = value
            return
        if (_storage_length(self._keys) + 1) * 3 > _storage_length(self._indices) * 2:
            self._resize(self._used + 1)
            slot, entry = self._find(key, code)
        self._indices[slot] = _storage_length(self._keys)
        self._hashes.append(code)
        self._keys.append(key)
        self._values.append(value)
        self._used += 1

    def _remove(self, key):
        """Delete ``key``'s entry, returning its value, or _MISSING if absent."""
        slot, entry = self._find(key, hash(key))
        if entry < 0:
            return _MISSING
        value = self._values[entry]
        self._indices[slot] = _DUMMY
        self._keys[entry] = _DELETED
        self._values[entry] = None
        self._used -= 1
        return value

    def _resize(self, minimum):
        size = _MINIMUM_SIZE
        while size * 2 < minimum * 3:
            size *= 2
        hashes, keys, values = [], [], []
        for index in range(_storage_length(self._keys)):
            key = self._keys[index]
            if key is not _DELETED:
                hashes.append(self._hashes[index])
                keys.append(key)
                values.append(self._values[index])
        self._indices = _empty_indices(size * 2)
        self._hashes, self._keys, self._values = hashes, keys, values
        mask = len(self._indices) - 1
        for index in range(len(keys)):
            perturb = hashes[index] & _PERTURB_MASK
            slot = perturb & mask
            while self._indices[slot] != _EMPTY:
                perturb >>= 5
                slot = (slot * 5 + perturb + 1) & mask
            self._indices[slot] = index

    def _live(self):
        """The live entries' indices, in insertion order."""
        positions = []
        for index in range(len(self._keys)):
            if self._keys[index] is not _DELETED:
                positions.append(index)
        return positions

    def __len__(self):
        return self._used


def _empty_indices(size):
    return [_EMPTY] * size


class _TableIterator:
    """Iterates a table's keys, values or items; mutation ends iteration."""

    def __init__(self, table, kind):
        self.table = table
        self.kind = kind
        self.index = 0
        self.used = table._used

    def __iter__(self):
        return self

    def __next__(self):
        table = self.table
        if table._used != self.used:
            self.used = -1
            raise RuntimeError(
                ("dictionary" if self.kind else "set") + " changed size during iteration"
            )
        keys = table._keys
        while self.index < _storage_length(keys):
            index = self.index
            self.index = index + 1
            key = keys[index]
            if key is not _DELETED:
                if self.kind == 2:
                    return table._values[index]
                if self.kind == 3:
                    return (key, table._values[index])
                return key
        raise StopIteration


def _representation(open, items, close):
    return open + ", ".join(items) + close


class dict(_Table):
    def __new__(cls, /, *args, **kwargs):
        if not is_class(cls) or not is_subclass(cls, dict):
            raise TypeError("dict.__new__() requires a subtype of dict")
        return _empty_table(cls)

    def __mule_annotation_mapping__(self):
        return self

    """Mapping with insertion-ordered keys."""

    def __init__(self, /, *args, **kwargs):
        if len(args) > 1:
            raise TypeError(f"dict expected at most 1 argument, got {len(args)}")
        if args:
            self._update_other(args[0])
        if kwargs:
            self._update_table(kwargs)

    @classmethod
    def fromkeys(cls, iterable, value=None):
        result = cls()
        for key in iterable:
            result[key] = value
        return result

    def __getitem__(self, key):
        entry = self._lookup(key)
        if entry >= 0:
            return self._values[entry]
        owner = class_of(self)
        if owner is not dict:
            absent = _binding_missing()
            missing = class_lookup_default(owner, "__missing__", absent)
            if missing is absent:
                raise KeyError(key)
            return bind_instance_attribute(missing, self, owner)(key)
        raise KeyError(key)

    def __setitem__(self, key, value):
        self._insert(key, value)

    def __delitem__(self, key):
        if self._remove(key) is _MISSING:
            raise KeyError(key)

    def __contains__(self, key):
        return self._lookup(key) >= 0

    def __iter__(self):
        return _TableIterator(self, 1)

    def __reversed__(self):
        keys = []
        positions = self._live()
        index = len(positions)
        while index > 0:
            index -= 1
            keys.append(self._keys[positions[index]])
        return iter(keys)

    def __eq__(self, other):
        if _raised_class(other) is None or not _is_dict(other):
            return False
        if self._used != other._used:
            return False
        for index in self._live():
            entry = other._lookup(self._keys[index])
            if entry < 0:
                return False
            mine, theirs = self._values[index], other._values[entry]
            if not (mine is theirs or mine == theirs):
                return False
        return True

    def __ne__(self, other):
        return not self.__eq__(other)

    def __repr__(self):
        items = []
        for index in self._live():
            items.append(repr(self._keys[index]) + ": " + repr(self._values[index]))
        return _representation("{", items, "}")

    def __or__(self, other):
        if not _is_dict(other):
            raise TypeError("unsupported operand type(s) for |")
        result = self.copy()
        result._update_table(other)
        return result

    def __ior__(self, other):
        self._update_other(other)
        return self

    def get(self, key, default=None):
        return self._get_value(key, default)

    def setdefault(self, key, default=None):
        entry = self._lookup(key)
        if entry >= 0:
            return self._values[entry]
        self._insert(key, default)
        return default

    def pop(self, key, default=_MISSING):
        value = self._remove(key)
        if value is _MISSING:
            if default is _MISSING:
                raise KeyError(key)
            return default
        return value

    def popitem(self):
        positions = self._live()
        if not positions:
            raise KeyError("popitem(): dictionary is empty")
        index = positions[len(positions) - 1]
        key, value = self._keys[index], self._values[index]
        self._remove(key)
        return (key, value)

    def keys(self):
        return dict_keys(self)

    def values(self):
        return dict_values(self)

    def items(self):
        return dict_items(self)

    def _update_table(self, other):
        for index in other._live():
            self._insert(other._keys[index], other._values[index])

    def _update_other(self, other):
        if _is_dict(other):
            self._update_table(other)
            return
        owner = _raised_class(other)
        keys = None
        if owner is not None:
            missing = _binding_missing()
            method = class_lookup_default(owner, "keys", missing)
            if method is not missing:
                keys = bind_instance_attribute(method, other, owner)
        if keys is not None:
            for key in keys():
                self._insert(key, other[key])
            return
        for item in other:
            key, value = item
            self._insert(key, value)

    def update(self, /, *args, **kwargs):
        if len(args) > 1:
            raise TypeError(f"update expected at most 1 argument, got {len(args)}")
        if args:
            self._update_other(args[0])
        if kwargs:
            self._update_table(kwargs)

    def clear(self):
        self._clear_table()

    def copy(self):
        result = _empty_dict()
        result._update_table(self)
        return result


def _is_dict(value):
    owner = _raised_class(value)
    while owner is not None:
        if owner is dict:
            return True
        owner = _class_parent(owner)
    return False


class _DictView:
    def __init__(self, mapping):
        self.mapping = mapping

    def __len__(self):
        return self.mapping._used

    def _entries(self):
        result = []
        for item in self:
            result.append(repr(item))
        return result


class _SetView(_DictView):
    """Set behavior of key and item views; values views remain sequences."""

    def __or__(self, other):
        return set(self).union(other)

    def __ror__(self, other):
        return set(other).union(self)

    def __and__(self, other):
        result = set()
        for item in other:
            if item in self:
                result.add(item)
        return result

    def __rand__(self, other):
        return self.__and__(other)

    def __sub__(self, other):
        return set(self).difference(other)

    def __rsub__(self, other):
        return set(other).difference(self)

    def __xor__(self, other):
        return set(self).symmetric_difference(other)

    def __rxor__(self, other):
        return self.__xor__(other)

    def _comparable(self, other):
        return _is_set(other) or isinstance(other, _SetView)

    def __eq__(self, other):
        if not self._comparable(other):
            return NotImplemented
        return len(self) == len(other) and self.__le__(other)

    def __ne__(self, other):
        result = self.__eq__(other)
        return NotImplemented if result is NotImplemented else not result

    def __le__(self, other):
        if not self._comparable(other):
            return NotImplemented
        return len(self) <= len(other) and all(item in other for item in self)

    def __lt__(self, other):
        if not self._comparable(other):
            return NotImplemented
        return len(self) < len(other) and self.__le__(other)

    def __ge__(self, other):
        if not self._comparable(other):
            return NotImplemented
        return len(self) >= len(other) and all(item in self for item in other)

    def __gt__(self, other):
        if not self._comparable(other):
            return NotImplemented
        return len(self) > len(other) and self.__ge__(other)

    def isdisjoint(self, other):
        return not any(item in self for item in other)


class dict_keys(_SetView):
    def __iter__(self):
        return _TableIterator(self.mapping, 1)

    def __contains__(self, key):
        return key in self.mapping

    def __repr__(self):
        return _representation("dict_keys([", self._entries(), "])")


class dict_values(_DictView):
    def __iter__(self):
        return _TableIterator(self.mapping, 2)

    def __repr__(self):
        return _representation("dict_values([", self._entries(), "])")


class dict_items(_SetView):
    def __iter__(self):
        return _TableIterator(self.mapping, 3)

    def __contains__(self, item):
        if not _tuple_storage(item) or len(item) != 2:
            return False
        key, value = _storage_read(item, 0), _storage_read(item, 1)
        entry = self.mapping._lookup(key)
        if entry < 0:
            return False
        stored = self.mapping._values[entry]
        return stored is value or stored == value

    def __repr__(self):
        return _representation("dict_items([", self._entries(), "])")


class _SetBase(_Table):
    """Operations shared by set and frozenset."""

    def _fill(self, iterable):
        self._clear_table()
        for item in iterable:
            self._insert(item, None)

    def _same_kind(self):
        return class_of(self)()

    def __contains__(self, item):
        return self._lookup(item) >= 0

    def __iter__(self):
        return _TableIterator(self, 0)

    def __repr__(self):
        name = _type_name(self)
        if not self._used:
            return name + "()"
        items = []
        for index in self._live():
            items.append(repr(self._keys[index]))
        if name == "set":
            return _representation("{", items, "}")
        return _representation(name + "({", items, "})")

    def __eq__(self, other):
        if not _is_set(other):
            return NotImplemented
        return self._used == other._used and self.issubset(other)

    def __ne__(self, other):
        result = self.__eq__(other)
        return NotImplemented if result is NotImplemented else not result

    def __le__(self, other):
        if not _is_set(other):
            return NotImplemented
        return self.issubset(other)

    def __lt__(self, other):
        if not _is_set(other):
            return NotImplemented
        return self._used < other._used and self.issubset(other)

    def __ge__(self, other):
        if not _is_set(other):
            return NotImplemented
        return self.issuperset(other)

    def __gt__(self, other):
        if not _is_set(other):
            return NotImplemented
        return self._used > other._used and self.issuperset(other)

    def __or__(self, other):
        if not _is_set(other):
            return NotImplemented
        return self.union(other)

    def __and__(self, other):
        if not _is_set(other):
            return NotImplemented
        return self.intersection(other)

    def __sub__(self, other):
        if not _is_set(other):
            return NotImplemented
        return self.difference(other)

    def __xor__(self, other):
        if not _is_set(other):
            return NotImplemented
        return self.symmetric_difference(other)

    def issubset(self, other):
        if not _is_set(other):
            other = set(other)
        for index in self._live():
            if other._lookup(self._keys[index]) < 0:
                return False
        return True

    def issuperset(self, other):
        for item in other:
            if self._lookup(item) < 0:
                return False
        return True

    def isdisjoint(self, other):
        for item in other:
            if self._lookup(item) >= 0:
                return False
        return True

    def union(self, *others):
        result = self._same_kind()
        for index in self._live():
            result._insert(self._keys[index], None)
        for other in others:
            for item in other:
                result._insert(item, None)
        return result

    def intersection(self, *others):
        result = self._same_kind()
        for index in self._live():
            key = self._keys[index]
            kept = True
            for other in others:
                if key not in other:
                    kept = False
                    break
            if kept:
                result._insert(key, None)
        return result

    def difference(self, *others):
        result = self._same_kind()
        for index in self._live():
            key = self._keys[index]
            kept = True
            for other in others:
                if key in other:
                    kept = False
                    break
            if kept:
                result._insert(key, None)
        return result

    def symmetric_difference(self, other):
        if not _is_set(other):
            other = set(other)
        result = self._same_kind()
        for index in self._live():
            key = self._keys[index]
            if other._lookup(key) < 0:
                result._insert(key, None)
        for index in other._live():
            key = other._keys[index]
            if self._lookup(key) < 0:
                result._insert(key, None)
        return result

    def copy(self):
        return self.union()


def _is_set(value):
    owner = _raised_class(value)
    while owner is not None:
        if owner is set or owner is frozenset:
            return True
        owner = _class_parent(owner)
    return False


def _require_set(value):
    if not _is_set(value):
        raise TypeError("unsupported operand type for set operation")


class set(_SetBase):
    """Mutable collection of distinct hashable items."""

    def __init__(self, *args):
        if len(args) > 1:
            raise TypeError(f"set expected at most 1 argument, got {len(args)}")
        self._fill(args[0] if args else ())

    def __ior__(self, other):
        _require_set(other)
        self.update(other)
        return self

    def __iand__(self, other):
        _require_set(other)
        self.intersection_update(other)
        return self

    def __isub__(self, other):
        _require_set(other)
        self.difference_update(other)
        return self

    def __ixor__(self, other):
        _require_set(other)
        self.symmetric_difference_update(other)
        return self

    def add(self, item):
        self._insert(item, None)

    def discard(self, item):
        self._remove(item)

    def remove(self, item):
        if self._remove(item) is _MISSING:
            raise KeyError(item)

    def pop(self):
        positions = self._live()
        if not positions:
            raise KeyError("pop from an empty set")
        key = self._keys[positions[0]]
        self._remove(key)
        return key

    def clear(self):
        self._clear_table()

    def update(self, *others):
        for other in others:
            for item in other:
                self._insert(item, None)

    def intersection_update(self, *others):
        kept = self
        for other in others:
            kept = kept.intersection(other)
        self._fill(kept)

    def difference_update(self, *others):
        for other in others:
            for item in other:
                self._remove(item)

    def symmetric_difference_update(self, other):
        kept = self.symmetric_difference(other)
        self._fill(kept)


class frozenset(_SetBase):
    """Immutable, hashable collection of distinct hashable items."""

    def __init__(self, *args):
        if len(args) > 1:
            raise TypeError(f"frozenset expected at most 1 argument, got {len(args)}")
        self._fill(args[0] if args else ())

    def __hash__(self):
        # CPython's frozenset hash: an order-independent mix of element hashes.
        accumulator = 0
        for index in self._live():
            code = self._hashes[index] & _WORD
            accumulator ^= ((code ^ 89869747) ^ (code << 16)) * 3644798167 & _WORD
        accumulator ^= (self._used + 1) * 1927868237 & _WORD
        accumulator ^= (accumulator >> 11) ^ (accumulator >> 25)
        accumulator = (accumulator * 69069 + 907133923) & _WORD
        if accumulator == _WORD:
            accumulator = 590923713
        return _signed(accumulator)


def _empty_table(cls):
    result = _allocate_instance(cls)
    _Table._clear_table(result)
    return result


def _empty_dict():
    return _empty_table(dict)


def build_dict(flat):
    """A dict display's keys and values, evaluated left to right as pairs."""
    result = _empty_dict()
    index = 0
    while index < _storage_length(flat):
        result._insert(flat[index], flat[index + 1])
        index += 2
    return result


def build_set(items):
    result = set()
    for item in items:
        result._insert(item, None)
    return result
