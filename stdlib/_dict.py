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
_MASK_LIMIT = 0x1FFFFFFFFFFFFFF
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
    value = _integer_storage(value) & _WORD
    return value - (1 << 64) if value >= 1 << 63 else value


def _integer_hash(value):
    value = _integer_storage(value)
    if value > -_MODULUS:
        if value < _MODULUS:
            return -2 if value == -1 else value
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
    kind = _storage_hash(value)
    if kind is not None:
        return kind
    if _integer_storage_kind(value):
        return _integer_hash(_integer_storage(value))
    if _tuple_storage(value):
        return _tuple_hash(value)
    if is_class(value):
        return _identity_hash(value)
    owner = _raised_class(value)
    if owner is None:
        if _type_name(value) == "list":
            raise TypeError("unhashable type: 'list'")
        return _identity_hash(value)
    missing = _binding_missing()
    method = class_lookup_default(owner, "__hash__", missing)
    if method is missing:
        return _identity_hash(value)
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


def _table_clear(self):
    self._indices = _empty_indices(_MINIMUM_SIZE)
    self._hashes = []
    self._keys = []
    self._values = []
    self._used = 0


def _table_find(self, key, code):
    """The index-table slot for ``key`` and its entry, or -1 for none."""
    indices = self._indices
    hashes = self._hashes
    keys = self._keys
    mask = (_storage_length(indices) - 1) & _MASK_LIMIT
    perturb = code & _PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(indices, slot)
        if entry is _EMPTY:
            return slot, -1
        if entry is not _DUMMY and _storage_read(hashes, entry) == code:
            existing = _storage_read(keys, entry)
            if existing is key or existing == key:
                return slot, entry
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def _table_lookup(self, key):
    code = _storage_hash(key)
    if code is None:
        code = hash(key)
    indices = self._indices
    hashes = self._hashes
    keys = self._keys
    mask = (_storage_length(indices) - 1) & _MASK_LIMIT
    perturb = code & _PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(indices, slot)
        if entry is _EMPTY:
            return _EMPTY
        if entry is not _DUMMY and _storage_read(hashes, entry) == code:
            existing = _storage_read(keys, entry)
            if existing is key or existing == key:
                return entry
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def _table_get_value(self, key, default):
    entry = _table_lookup(self, key)
    return _storage_read(self._values, entry) if entry is not _EMPTY else default


def _table_resize(self, minimum):
    size = _MINIMUM_SIZE
    while size * 2 < minimum * 3:
        size *= 2
    hashes, keys, values = [], [], []
    old_hashes = self._hashes
    old_keys = self._keys
    old_values = self._values
    count = _storage_length(old_keys)
    index = 0
    while index < count:
        key = _storage_read(old_keys, index)
        if key is not _DELETED:
            _storage_append(hashes, _storage_read(old_hashes, index))
            _storage_append(keys, key)
            _storage_append(values, _storage_read(old_values, index))
        index += 1
    indices = _empty_indices(size * 2)
    self._indices = indices
    self._hashes, self._keys, self._values = hashes, keys, values
    mask = (_storage_length(indices) - 1) & _MASK_LIMIT
    live_count = _storage_length(keys)
    index = 0
    while index < live_count:
        perturb = _storage_read(hashes, index) & _PERTURB_MASK
        slot = perturb & mask
        while _storage_read(indices, slot) is not _EMPTY:
            perturb >>= 5
            slot = (slot * 5 + perturb + 1) & mask
        _storage_write(indices, slot, index)
        index += 1


def _table_insert(self, key, value):
    code = _storage_hash(key)
    if code is None:
        code = hash(key)
    indices = self._indices
    hashes = self._hashes
    keys = self._keys
    mask = (_storage_length(indices) - 1) & _MASK_LIMIT
    perturb = code & _PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(indices, slot)
        if entry is _EMPTY:
            break
        if entry is not _DUMMY and _storage_read(hashes, entry) == code:
            existing = _storage_read(keys, entry)
            if existing is key or existing == key:
                _storage_write(self._values, entry, value)
                return
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask
    if (_storage_length(self._keys) + 1) * 3 > _storage_length(self._indices) * 2:
        _table_resize(self, self._used + 1)
        indices = self._indices
        mask = (_storage_length(indices) - 1) & _MASK_LIMIT
        perturb = code & _PERTURB_MASK
        slot = perturb & mask
        while _storage_read(indices, slot) is not _EMPTY:
            perturb >>= 5
            slot = (slot * 5 + perturb + 1) & mask
    _storage_write(self._indices, slot, _storage_length(self._keys))
    _storage_append(self._hashes, code)
    _storage_append(self._keys, key)
    _storage_append(self._values, value)
    self._used += 1


def _table_remove(self, key):
    """Delete ``key``'s entry, returning its value, or _MISSING if absent."""
    code = _storage_hash(key)
    if code is None:
        code = hash(key)
    indices = self._indices
    hashes = self._hashes
    keys = self._keys
    mask = (_storage_length(indices) - 1) & _MASK_LIMIT
    perturb = code & _PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(indices, slot)
        if entry is _EMPTY:
            return _MISSING
        if entry is not _DUMMY and _storage_read(hashes, entry) == code:
            existing = _storage_read(keys, entry)
            if existing is key or existing == key:
                value = _storage_read(self._values, entry)
                _storage_write(self._indices, slot, _DUMMY)
                _storage_write(self._keys, entry, _DELETED)
                _storage_write(self._values, entry, None)
                self._used -= 1
                return value
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def _table_live(self):
    """The live entries' indices, in insertion order."""
    positions = []
    keys = self._keys
    count = _storage_length(keys)
    index = 0
    while index < count:
        if _storage_read(keys, index) is not _DELETED:
            _storage_append(positions, index)
        index += 1
    return positions


class _Table:
    """Hash table storage shared by dict and the set types."""

    def _clear_table(self):
        self._indices = _empty_indices(_MINIMUM_SIZE)
        self._hashes = []
        self._keys = []
        self._values = []
        self._used = 0

    def _find(self, key, code):
        return _table_find(self, key, code)

    def _lookup(self, key):
        return _table_lookup(self, key)

    def _get_value(self, key, default):
        return _table_get_value(self, key, default)

    def _insert(self, key, value):
        _table_insert(self, key, value)

    def _remove(self, key):
        return _table_remove(self, key)

    def _resize(self, minimum):
        _table_resize(self, minimum)

    def _live(self):
        return _table_live(self)

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
            key = _storage_read(keys, index)
            if key is not _DELETED:
                if self.kind == 2:
                    return _storage_read(table._values, index)
                if self.kind == 3:
                    return (key, _storage_read(table._values, index))
                return key
        raise StopIteration


def _representation(open, items, close):
    return open + ", ".join(items) + close


def _dict_update_table(self, other):
    for index in _table_live(other):
        _table_insert(self, _storage_read(other._keys, index), _storage_read(other._values, index))


def _dict_update_other(self, other):
    if _is_dict(other):
        _dict_update_table(self, other)
        return
    owner = _type_of(other)
    keys = None
    missing = _binding_missing()
    method = class_lookup_default(owner, "keys", missing)
    if method is not missing:
        keys = bind_instance_attribute(method, other, owner)
    if keys is not None:
        for key in keys():
            _table_insert(self, key, other[key])
        return
    for item in other:
        key, value = item
        _table_insert(self, key, value)


class dict(_Table):
    def __new__(cls, /, *args, **kwargs):
        if not is_class(cls) or not is_subclass(cls, dict):
            raise TypeError("dict.__new__() requires a subtype of dict")
        return _empty_table(cls)

    def __mule_annotation_mapping__(self):
        return self

    """Mapping with insertion-ordered keys."""

    def __init__(self, /, *args, **kwargs):
        count = _storage_length(args)
        if count > 1:
            raise TypeError(f"dict expected at most 1 argument, got {count}")
        if count == 1:
            other = _storage_read(args, 0)
            if _is_dict(other):
                for index in _table_live(other):
                    _table_insert(
                        self,
                        _storage_read(other._keys, index),
                        _storage_read(other._values, index),
                    )
            elif _type_of(other) is list or _type_of(other) is tuple:
                for item in other:
                    key, value = item
                    _table_insert(self, key, value)
            else:
                _dict_update_other(self, other)
        if kwargs:
            for index in _table_live(kwargs):
                _table_insert(
                    self,
                    _storage_read(kwargs._keys, index),
                    _storage_read(kwargs._values, index),
                )

    @classmethod
    def fromkeys(cls, iterable, value=None):
        result = cls()
        for key in iterable:
            result[key] = value
        return result

    def __getitem__(self, key):
        entry = _table_lookup(self, key)
        if entry >= 0:
            return _storage_read(self._values, entry)
        owner = class_of(self)
        if owner is not dict:
            absent = _binding_missing()
            missing = class_lookup_default(owner, "__missing__", absent)
            if missing is absent:
                raise KeyError(key)
            return bind_instance_attribute(missing, self, owner)(key)
        raise KeyError(key)

    def __setitem__(self, key, value):
        _table_insert(self, key, value)

    def __delitem__(self, key):
        if _table_remove(self, key) is _MISSING:
            raise KeyError(key)

    def __contains__(self, key):
        return _table_lookup(self, key) >= 0

    def __iter__(self):
        return _TableIterator(self, 1)

    def __reversed__(self):
        keys = []
        positions = _table_live(self)
        index = _storage_length(positions)
        while index > 0:
            index -= 1
            _storage_append(keys, _storage_read(self._keys, _storage_read(positions, index)))
        return iter(keys)

    def __eq__(self, other):
        if not _is_dict(other):
            return False
        if self._used != other._used:
            return False
        for index in _table_live(self):
            entry = _table_lookup(other, _storage_read(self._keys, index))
            if entry < 0:
                return False
            mine, theirs = _storage_read(self._values, index), _storage_read(other._values, entry)
            if not (mine is theirs or mine == theirs):
                return False
        return True

    def __ne__(self, other):
        return not self.__eq__(other)

    def __repr__(self):
        items = []
        for index in _table_live(self):
            _storage_append(
                items,
                repr(_storage_read(self._keys, index))
                + ": "
                + repr(_storage_read(self._values, index)),
            )
        return _representation("{", items, "}")

    def __or__(self, other):
        if not _is_dict(other):
            raise TypeError("unsupported operand type(s) for |")
        result = self.copy()
        _dict_update_table(result, other)
        return result

    def __ior__(self, other):
        _dict_update_other(self, other)
        return self

    def get(self, key, default=None):
        entry = _table_lookup(self, key)
        return _storage_read(self._values, entry) if entry >= 0 else default

    def setdefault(self, key, default=None):
        entry = _table_lookup(self, key)
        if entry >= 0:
            return _storage_read(self._values, entry)
        _table_insert(self, key, default)
        return default

    def pop(self, key, default=_MISSING):
        value = _table_remove(self, key)
        if value is _MISSING:
            if default is _MISSING:
                raise KeyError(key)
            return default
        return value

    def popitem(self):
        positions = _table_live(self)
        if not positions:
            raise KeyError("popitem(): dictionary is empty")
        index = _storage_read(positions, _storage_length(positions) - 1)
        key, value = _storage_read(self._keys, index), _storage_read(self._values, index)
        _table_remove(self, key)
        return (key, value)

    def keys(self):
        return dict_keys(self)

    def values(self):
        return dict_values(self)

    def items(self):
        return dict_items(self)

    def _update_table(self, other):
        _dict_update_table(self, other)

    def _update_other(self, other):
        _dict_update_other(self, other)

    def update(self, /, *args, **kwargs):
        if len(args) > 1:
            raise TypeError(f"update expected at most 1 argument, got {len(args)}")
        if args:
            _dict_update_other(self, args[0])
        if kwargs:
            _dict_update_table(self, kwargs)

    def clear(self):
        _table_clear(self)

    def copy(self):
        result = _empty_dict()
        _dict_update_table(result, self)
        return result


def _is_dict(value):
    owner = _type_of(value)
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
        entry = _table_lookup(self.mapping, key)
        if entry < 0:
            return False
        stored = self.mapping._values[entry]
        return stored is value or stored == value

    def __repr__(self):
        return _representation("dict_items([", self._entries(), "])")


def _set_fill(self, iterable):
    _table_clear(self)
    for item in iterable:
        _table_insert(self, item, None)


class _SetBase(_Table):
    """Operations shared by set and frozenset."""

    def _fill(self, iterable):
        _set_fill(self, iterable)

    def _same_kind(self):
        return class_of(self)()

    def __contains__(self, item):
        return _table_lookup(self, item) >= 0

    def __iter__(self):
        return _TableIterator(self, 0)

    def __repr__(self):
        name = _type_name(self)
        if not self._used:
            return name + "()"
        items = []
        for index in _table_live(self):
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
        for index in _table_live(self):
            if _table_lookup(other, self._keys[index]) < 0:
                return False
        return True

    def issuperset(self, other):
        for item in other:
            if _table_lookup(self, item) < 0:
                return False
        return True

    def isdisjoint(self, other):
        for item in other:
            if _table_lookup(self, item) >= 0:
                return False
        return True

    def union(self, *others):
        result = self._same_kind()
        for index in _table_live(self):
            _table_insert(result, self._keys[index], None)
        for other in others:
            for item in other:
                _table_insert(result, item, None)
        return result

    def intersection(self, *others):
        result = self._same_kind()
        for index in _table_live(self):
            key = self._keys[index]
            kept = True
            for other in others:
                if key not in other:
                    kept = False
                    break
            if kept:
                _table_insert(result, key, None)
        return result

    def difference(self, *others):
        result = self._same_kind()
        for index in _table_live(self):
            key = self._keys[index]
            kept = True
            for other in others:
                if key in other:
                    kept = False
                    break
            if kept:
                _table_insert(result, key, None)
        return result

    def symmetric_difference(self, other):
        if not _is_set(other):
            other = set(other)
        result = self._same_kind()
        for index in _table_live(self):
            key = self._keys[index]
            if _table_lookup(other, key) < 0:
                _table_insert(result, key, None)
        for index in _table_live(other):
            key = other._keys[index]
            if _table_lookup(self, key) < 0:
                _table_insert(result, key, None)
        return result

    def copy(self):
        return self.union()


def _is_set(value):
    owner = _type_of(value)
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
        _set_fill(self, args[0] if args else ())

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
        _table_insert(self, item, None)

    def discard(self, item):
        _table_remove(self, item)

    def remove(self, item):
        if _table_remove(self, item) is _MISSING:
            raise KeyError(item)

    def pop(self):
        positions = _table_live(self)
        if not positions:
            raise KeyError("pop from an empty set")
        key = self._keys[positions[0]]
        _table_remove(self, key)
        return key

    def clear(self):
        _table_clear(self)

    def update(self, *others):
        for other in others:
            for item in other:
                _table_insert(self, item, None)

    def intersection_update(self, *others):
        kept = self
        for other in others:
            kept = kept.intersection(other)
        _set_fill(self, kept)

    def difference_update(self, *others):
        for other in others:
            for item in other:
                _table_remove(self, item)

    def symmetric_difference_update(self, other):
        kept = self.symmetric_difference(other)
        _set_fill(self, kept)


class frozenset(_SetBase):
    """Immutable, hashable collection of distinct hashable items."""

    def __init__(self, *args):
        if len(args) > 1:
            raise TypeError(f"frozenset expected at most 1 argument, got {len(args)}")
        _set_fill(self, args[0] if args else ())

    def __hash__(self):
        # CPython's frozenset hash: an order-independent mix of element hashes.
        accumulator = 0
        for index in _table_live(self):
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
    _table_clear(result)
    return result


def _empty_dict():
    result = _allocate_instance(dict)
    _table_clear(result)
    return result


def build_dict(flat):
    """A dict display's keys and values, evaluated left to right as pairs."""
    result = _empty_dict()
    index = 0
    while index < _storage_length(flat):
        _table_insert(result, _storage_read(flat, index), _storage_read(flat, index + 1))
        index += 2
    return result


def build_set(items):
    result = _empty_table(set)
    for item in items:
        _table_insert(result, item, None)
    return result
