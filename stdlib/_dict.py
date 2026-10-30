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

__speed__ = 3
__mule_builtin_implementation__ = True

from _descriptors import classmethod
from _object import integer_hash
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
_WORD = 0xFFFFFFFFFFFFFFFF
_FIXNUM_MIN = -0x4000000000000000
_FIXNUM_MAX = 0x3FFFFFFFFFFFFFFF
_PERTURB_MASK = 0x3FFFFFFFFFFFFFFF
_MASK_LIMIT = 0x1FFFFFFFFFFFFFF
_PRIME_1 = 11400714785074694791
_PRIME_2 = 14029467366897019727
_PRIME_5 = 2870177450012600261
_TUPLE_TAIL = 2870177450013471926


class _Deleted:
    """The marker an entry holds after its key is deleted."""


class _Missing:
    """The default of optional arguments that accept any value, None included."""


_DELETED = _Deleted()
_MISSING = _Missing()


def _signed(value):
    value = _integer_storage(value) & _WORD
    return value - (1 << 64) if value >= 1 << 63 else value


def _lanes_to_signed_hash(acc_hi, acc_lo):
    acc_hi = _integer_storage(acc_hi) & 0xFFFFFFFF
    acc_lo = _integer_storage(acc_lo) & 0xFFFFFFFF
    if acc_hi < 0x40000000:
        return (acc_hi << 32) | acc_lo
    if acc_hi >= 0xC0000000:
        return ((acc_hi - 0x100000000) << 32) | acc_lo
    return _signed((acc_hi << 32) | acc_lo)


def _lanes_to_table_hash(acc_hi, acc_lo):
    acc_hi = _integer_storage(acc_hi) & 0xFFFFFFFF
    acc_lo = _integer_storage(acc_lo) & 0xFFFFFFFF
    if acc_hi < 0x40000000:
        return (acc_hi << 32) | acc_lo
    if acc_hi >= 0xC0000000:
        return ((acc_hi - 0x100000000) << 32) | acc_lo
    folded = ((acc_hi & 0x3FFFFFFF) << 32) | acc_lo
    if acc_hi < 0x80000000:
        return folded ^ 1
    return folded ^ 0x3FFFFFFFFFFFFFFE


def _tuple_hash(value):
    count = _storage_length(value)
    acc_hi = 0x27D4EB2F
    acc_lo = 0x165667C5
    index = 0
    while index < count:
        item = _storage_read(value, index)
        item_hash = _storage_hash(item)
        if item_hash is not None:
            lane_lo = item_hash & 0xFFFFFFFF
            lane_hi = (item_hash >> 32) & 0xFFFFFFFF
        else:
            raw_hash = _tuple_hash(item) if _tuple_storage(item) else hash(item)
            lane_lo = raw_hash & 0xFFFFFFFF
            lane_hi = (raw_hash >> 32) & 0xFFFFFFFF
        low_prod = acc_lo + lane_lo * 0x27D4EB4F
        acc_lo = low_prod & 0xFFFFFFFF
        acc_hi = (
            acc_hi
            + (low_prod >> 32)
            + lane_hi * 0x27D4EB4F
            + lane_lo * 0x02B2AE3D
            + (((lane_lo & 3) * 3) << 30)
        ) & 0xFFFFFFFF
        rot_lo = ((acc_lo & 1) << 31) | (acc_hi >> 1)
        rot_hi = ((acc_hi & 1) << 31) | (acc_lo >> 1)
        p1_low = rot_lo * 0x05EBCA87 + ((rot_lo & 1) << 31)
        acc_lo = p1_low & 0xFFFFFFFF
        acc_hi = (
            (p1_low >> 32)
            + (rot_lo >> 1)
            + rot_hi * 0x05EBCA87
            + ((rot_hi & 1) << 31)
            + rot_lo * 0x1E3779B1
            + ((rot_lo & 1) << 31)
        ) & 0xFFFFFFFF
        index += 1
    tail = count ^ _TUPLE_TAIL
    end_lo = acc_lo + (tail & 0xFFFFFFFF)
    acc_lo = end_lo & 0xFFFFFFFF
    acc_hi = (acc_hi + (end_lo >> 32) + (tail >> 32)) & 0xFFFFFFFF
    if acc_hi == 0xFFFFFFFF and acc_lo == 0xFFFFFFFF:
        return 1546275796
    return _lanes_to_signed_hash(acc_hi, acc_lo)


def _tuple_table_hash(value):
    count = _storage_length(value)
    acc_hi = 0x27D4EB2F
    acc_lo = 0x165667C5
    index = 0
    while index < count:
        item = _storage_read(value, index)
        item_hash = _storage_hash(item)
        if item_hash is not None:
            lane_lo = item_hash & 0xFFFFFFFF
            lane_hi = (item_hash >> 32) & 0xFFFFFFFF
        elif _tuple_storage(item):
            sub_hash = _tuple_table_hash(item)
            lane_lo = sub_hash & 0xFFFFFFFF
            lane_hi = (sub_hash >> 32) & 0xFFFFFFFF
        else:
            raw_hash = hash(item)
            lane_lo = raw_hash & 0xFFFFFFFF
            lane_hi = (raw_hash >> 32) & 0xFFFFFFFF
        low_prod = acc_lo + lane_lo * 0x27D4EB4F
        acc_lo = low_prod & 0xFFFFFFFF
        acc_hi = (
            acc_hi
            + (low_prod >> 32)
            + lane_hi * 0x27D4EB4F
            + lane_lo * 0x02B2AE3D
            + (((lane_lo & 3) * 3) << 30)
        ) & 0xFFFFFFFF
        rot_lo = ((acc_lo & 1) << 31) | (acc_hi >> 1)
        rot_hi = ((acc_hi & 1) << 31) | (acc_lo >> 1)
        p1_low = rot_lo * 0x05EBCA87 + ((rot_lo & 1) << 31)
        acc_lo = p1_low & 0xFFFFFFFF
        acc_hi = (
            (p1_low >> 32)
            + (rot_lo >> 1)
            + rot_hi * 0x05EBCA87
            + ((rot_hi & 1) << 31)
            + rot_lo * 0x1E3779B1
            + ((rot_lo & 1) << 31)
        ) & 0xFFFFFFFF
        index += 1
    tail = count ^ _TUPLE_TAIL
    end_lo = acc_lo + (tail & 0xFFFFFFFF)
    acc_lo = end_lo & 0xFFFFFFFF
    acc_hi = (acc_hi + (end_lo >> 32) + (tail >> 32)) & 0xFFFFFFFF
    if acc_hi == 0xFFFFFFFF and acc_lo == 0xFFFFFFFF:
        return 1546275796
    return _lanes_to_table_hash(acc_hi, acc_lo)


def hash(value, /):
    kind = _storage_hash(value)
    if kind is not None:
        return kind
    if _type_of(value) is int or _type_of(value) is bool:
        return integer_hash(value)
    if _tuple_storage(value):
        return _tuple_hash(value)
    if is_class(value):
        return _identity_hash(value)
    owner = _type_of(value)
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
        return integer_hash(result)
    return -2 if result == -1 else result


def _integer_storage_kind(value):
    return math_number_kind(value) == 1


def _table_hash_fallback(key):
    if _tuple_storage(key):
        return _tuple_table_hash(key)
    code = hash(key)
    if code < _FIXNUM_MIN:
        return (code ^ (code >> 62)) & _PERTURB_MASK
    if code > _FIXNUM_MAX:
        return (code ^ (code >> 62)) & _PERTURB_MASK
    return code


def _table_clear(self):
    new_indices = _empty_indices(_MINIMUM_SIZE)
    _set_instance_field(self, "_indices", new_indices)
    del new_indices
    empty_hashes = _empty_hashes(_MINIMUM_SIZE)
    _set_instance_field(self, "_hashes", empty_hashes)
    del empty_hashes
    empty_keys = []
    _set_instance_field(self, "_keys", empty_keys)
    del empty_keys
    empty_values = []
    _set_instance_field(self, "_values", empty_values)
    del empty_values
    cleared_used = 0
    _set_instance_field(self, "_used", cleared_used)
    del cleared_used


def _table_find(self, key, code):
    """The index-table slot for ``key`` and its entry, or -1 for none."""
    if code < _FIXNUM_MIN or code > _FIXNUM_MAX:
        code = (code ^ (code >> 62)) & _PERTURB_MASK
    indices = _instance_field(self, "_indices")
    hashes = _instance_field(self, "_hashes")
    keys = _instance_field(self, "_keys")
    mask = (_storage_length(indices) - 1) & _MASK_LIMIT
    perturb = code & _PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(indices, slot)
        if entry >= 0:
            if _storage_read(hashes, entry) == code:
                existing = _storage_read(keys, entry)
                if existing is key or existing == key:
                    return slot, entry
        elif entry is _EMPTY:
            return slot, -1
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def _table_lookup(self, key):
    code = _storage_hash(key)
    if code is None:
        code = _table_hash_fallback(key)
    indices = _instance_field(self, "_indices")
    hashes = _instance_field(self, "_hashes")
    keys = _instance_field(self, "_keys")
    mask = (_storage_length(indices) - 1) & _MASK_LIMIT
    perturb = code & _PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(indices, slot)
        if entry >= 0:
            if _storage_read(hashes, entry) == code:
                existing = _storage_read(keys, entry)
                if existing is key or existing == key:
                    return entry
        elif entry is _EMPTY:
            return _EMPTY
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def _table_get_value(self, key, default):
    entry = _table_lookup(self, key)
    return (
        _storage_read(_instance_field(self, "_values"), entry)
        if entry >= 0
        else default
    )


def _table_resize(self, minimum):
    size = _MINIMUM_SIZE
    while size * 2 < minimum * 3:
        size *= 2
    hashes = _empty_hashes(size * 2)
    keys, values = [], []
    old_hashes = _instance_field(self, "_hashes")
    old_keys = _instance_field(self, "_keys")
    old_values = _instance_field(self, "_values")
    count = _storage_length(old_keys)
    has_values = _storage_length(old_values) == count
    index = 0
    if has_values:
        while index < count:
            key = _storage_read(old_keys, index)
            if key is not _DELETED:
                _storage_write(
                    hashes, _storage_length(keys), _storage_read(old_hashes, index)
                )
                _storage_append(keys, key)
                _storage_append(values, _storage_read(old_values, index))
            index += 1
    else:
        while index < count:
            key = _storage_read(old_keys, index)
            if key is not _DELETED:
                _storage_write(
                    hashes, _storage_length(keys), _storage_read(old_hashes, index)
                )
                _storage_append(keys, key)
            index += 1
    indices = _empty_indices(size * 2)
    new_indices = indices
    _set_instance_field(self, "_indices", new_indices)
    del new_indices
    compacted_hashes = hashes
    _set_instance_field(self, "_hashes", compacted_hashes)
    del compacted_hashes
    compacted_keys = keys
    _set_instance_field(self, "_keys", compacted_keys)
    del compacted_keys
    compacted_values = values
    _set_instance_field(self, "_values", compacted_values)
    del compacted_values
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
        code = _table_hash_fallback(key)
    indices = _instance_field(self, "_indices")
    hashes = _instance_field(self, "_hashes")
    keys = _instance_field(self, "_keys")
    values = _instance_field(self, "_values")
    mask = (_storage_length(indices) - 1) & _MASK_LIMIT
    perturb = code & _PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(indices, slot)
        if entry >= 0:
            if _storage_read(hashes, entry) == code:
                existing = _storage_read(keys, entry)
                if existing is key or existing == key:
                    _storage_write(values, entry, value)
                    return
        elif entry is _EMPTY:
            break
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask
    if (_storage_length(keys) + 1) * 3 > _storage_length(indices) * 2:
        _table_resize(self, _instance_field(self, "_used") + 1)
        indices = _instance_field(self, "_indices")
        hashes = _instance_field(self, "_hashes")
        keys = _instance_field(self, "_keys")
        values = _instance_field(self, "_values")
        mask = (_storage_length(indices) - 1) & _MASK_LIMIT
        perturb = code & _PERTURB_MASK
        slot = perturb & mask
        while _storage_read(indices, slot) is not _EMPTY:
            perturb >>= 5
            slot = (slot * 5 + perturb + 1) & mask
    _storage_write(
        indices,
        slot,
        _storage_length(keys),
    )
    _storage_write(hashes, _storage_length(keys), code)
    _storage_append(keys, key)
    _storage_append(values, value)
    next_used = (_instance_field(self, "_used") + 1) & _MASK_LIMIT
    _set_instance_field(self, "_used", next_used)
    del next_used


def _table_remove(self, key):
    """Delete ``key``'s entry, returning its value, or _MISSING if absent."""
    code = _storage_hash(key)
    if code is None:
        code = _table_hash_fallback(key)
    indices = _instance_field(self, "_indices")
    hashes = _instance_field(self, "_hashes")
    keys = _instance_field(self, "_keys")
    values = _instance_field(self, "_values")
    mask = (_storage_length(indices) - 1) & _MASK_LIMIT
    perturb = code & _PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(indices, slot)
        if entry >= 0:
            if _storage_read(hashes, entry) == code:
                existing = _storage_read(keys, entry)
                if existing is key or existing == key:
                    value = _storage_read(values, entry)
                    _storage_write(indices, slot, _DUMMY)
                    _storage_write(keys, entry, _DELETED)
                    _storage_write(values, entry, None)
                    remaining_used = (_instance_field(self, "_used") - 1) & _MASK_LIMIT
                    _set_instance_field(self, "_used", remaining_used)
                    del remaining_used
                    return value
        elif entry is _EMPTY:
            return _MISSING
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def _table_live(self):
    """The live entries' indices, in insertion order."""
    positions = []
    keys = _instance_field(self, "_keys")
    count = _storage_length(keys)
    index = 0
    while index < count:
        if _storage_read(keys, index) is not _DELETED:
            _storage_append(positions, index)
        index += 1
    return positions


class _Table:
    """Hash table storage shared by dict and the set types."""

    __mule_slots__ = ("_indices", "_hashes", "_keys", "_values", "_used")

    def _clear_table(self):
        new_indices = _empty_indices(_MINIMUM_SIZE)
        _set_instance_field(self, "_indices", new_indices)
        del new_indices
        empty_hashes = _empty_hashes(_MINIMUM_SIZE)
        _set_instance_field(self, "_hashes", empty_hashes)
        del empty_hashes
        empty_keys = []
        _set_instance_field(self, "_keys", empty_keys)
        del empty_keys
        empty_values = []
        _set_instance_field(self, "_values", empty_values)
        del empty_values
        cleared_used = 0
        _set_instance_field(self, "_used", cleared_used)
        del cleared_used

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
        return _instance_field(self, "_used")


def _empty_indices(size):
    return _word_vector_new(size, _EMPTY)


def _empty_hashes(index_size):
    """Entry hashes for a table of ``index_size`` slots: inserts resize
    before entries exceed two thirds of the slots, as in CPython."""
    return _word_vector_new((index_size * 2) // 3, 0)


class _TableIterator:
    """Iterates a table's keys, values or items; mutation ends iteration."""

    def __init__(self, table, kind):
        self.table = table
        self.kind = kind
        self.index = 0
        self.used = _instance_field(table, "_used")
        self.keys = _instance_field(table, "_keys")
        self.entries = _storage_length(_instance_field(table, "_keys"))

    def __iter__(self):
        return self

    def __next__(self):
        table = self.table
        keys = _instance_field(table, "_keys")
        if (
            _instance_field(table, "_used") != self.used
            or keys is not self.keys
            or _storage_length(keys) != self.entries
        ):
            self.used = -1
            raise RuntimeError(
                ("dictionary" if self.kind else "set")
                + " changed size during iteration"
            )
        while self.index < _storage_length(keys):
            index = self.index
            self.index = index + 1
            key = _storage_read(keys, index)
            if key is not _DELETED:
                if self.kind == 2:
                    return _storage_read(_instance_field(table, "_values"), index)
                if self.kind == 3:
                    return (
                        key,
                        _storage_read(_instance_field(table, "_values"), index),
                    )
                return key
        raise StopIteration


class _TableReverseIterator:
    """Iterates a table's keys, values or items in reverse; mutation ends iteration."""

    def __init__(self, table, kind):
        self.table = table
        self.kind = kind
        self.index = _storage_length(_instance_field(table, "_keys"))
        self.used = _instance_field(table, "_used")
        self.keys = _instance_field(table, "_keys")
        self.entries = _storage_length(_instance_field(table, "_keys"))

    def __iter__(self):
        return self

    def __next__(self):
        table = self.table
        keys = _instance_field(table, "_keys")
        if (
            _instance_field(table, "_used") != self.used
            or keys is not self.keys
            or _storage_length(keys) != self.entries
        ):
            self.used = -1
            raise RuntimeError(
                ("dictionary" if self.kind else "set")
                + " changed size during iteration"
            )
        while self.index > 0:
            index = self.index - 1
            self.index = index
            key = _storage_read(keys, index)
            if key is not _DELETED:
                if self.kind == 2:
                    return _storage_read(_instance_field(table, "_values"), index)
                if self.kind == 3:
                    return (
                        key,
                        _storage_read(_instance_field(table, "_values"), index),
                    )
                return key
        raise StopIteration


def _representation(open, items, close):
    return open + ", ".join(items) + close


def _dict_update_table(self, other):
    count = _storage_length(_instance_field(other, "_keys"))
    index = 0
    while index < count:
        key = _storage_read(_instance_field(other, "_keys"), index)
        if key is not _DELETED:
            _table_insert(
                self,
                key,
                _storage_read(_instance_field(other, "_values"), index),
            )
        index += 1


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


_repr_active_dicts = []


class dict(_Table):
    __mule_slots__ = ()

    def __new__(cls, /, *args, **kwargs):
        if not is_class(cls) or not is_subclass(cls, dict):
            raise TypeError("dict.__new__() requires a subtype of dict")
        return _empty_table(cls)

    def __mule_annotation_mapping__(self):
        return self

    """Mapping with insertion-ordered keys."""

    def __len__(self):
        _require_dict(self)
        return _instance_field(self, "_used")

    def __init__(self, /, *args, **kwargs):
        _require_dict(self)
        count = _storage_length(args)
        if count > 1:
            raise TypeError(f"dict expected at most 1 argument, got {count}")
        if count == 1:
            other = _storage_read(args, 0)
            if _is_dict(other):
                _dict_update_table(self, other)
            elif _type_of(other) is list or _type_of(other) is tuple:
                for item in other:
                    key, value = item
                    _table_insert(self, key, value)
            else:
                _dict_update_other(self, other)
        if kwargs:
            _dict_update_table(self, kwargs)

    @classmethod
    def fromkeys(cls, iterable, value=None):
        result = cls()
        for key in iterable:
            result[key] = value
        return result

    def __getitem__(self, key):
        _require_dict(self)
        entry = _table_lookup(self, key)
        if entry >= 0:
            return _storage_read(_instance_field(self, "_values"), entry)
        owner = class_of(self)
        if owner is not dict:
            absent = _binding_missing()
            missing = class_lookup_default(owner, "__missing__", absent)
            if missing is absent:
                raise KeyError(key)
            return bind_instance_attribute(missing, self, owner)(key)
        raise KeyError(key)

    def __setitem__(self, key, value):
        _require_dict(self)
        _table_insert(self, key, value)

    def __delitem__(self, key):
        _require_dict(self)
        if _table_remove(self, key) is _MISSING:
            raise KeyError(key)

    def __contains__(self, key):
        _require_dict(self)
        return _table_lookup(self, key) >= 0

    def __iter__(self):
        _require_dict(self)
        return _TableIterator(self, 1)

    def __reversed__(self):
        _require_dict(self)
        return _TableReverseIterator(self, 1)

    def __eq__(self, other):
        _require_dict(self)
        if not _is_dict(other):
            return False
        if _instance_field(self, "_used") != _instance_field(other, "_used"):
            return False
        count = _storage_length(_instance_field(self, "_keys"))
        index = 0
        while index < count:
            key = _storage_read(_instance_field(self, "_keys"), index)
            if key is not _DELETED:
                entry = _table_lookup(other, key)
                if entry < 0:
                    return False
                mine, theirs = (
                    _storage_read(_instance_field(self, "_values"), index),
                    _storage_read(_instance_field(other, "_values"), entry),
                )
                if not (mine is theirs or mine == theirs):
                    return False
            index += 1
        return True

    def __ne__(self, other):
        _require_dict(self)
        return not self.__eq__(other)

    def __repr__(self):
        _require_dict(self)
        index = 0
        while index < _storage_length(_repr_active_dicts):
            if _storage_read(_repr_active_dicts, index) is self:
                return "{...}"
            index += 1
        _storage_append(_repr_active_dicts, self)
        try:
            items = []
            for index in _table_live(self):
                _storage_append(
                    items,
                    repr(_storage_read(_instance_field(self, "_keys"), index))
                    + ": "
                    + repr(_storage_read(_instance_field(self, "_values"), index)),
                )
            return _representation("{", items, "}")
        finally:
            end = _storage_length(_repr_active_dicts)
            _storage_delete(_repr_active_dicts, end - 1, end)

    def __or__(self, other):
        _require_dict(self)
        if not _is_dict(other):
            raise TypeError("unsupported operand type(s) for |")
        result = self.copy()
        _dict_update_table(result, other)
        return result

    def __ior__(self, other):
        _require_dict(self)
        _dict_update_other(self, other)
        return self

    def get(self, key, default=None):
        if not _is_dict(self):
            if _type_name(self) in ("_InstanceDict", "_GlobalsDict"):
                return self.get(key, default)
            _require_dict(self)
        entry = _table_lookup(self, key)
        return (
            _storage_read(_instance_field(self, "_values"), entry)
            if entry >= 0
            else default
        )

    def setdefault(self, key, default=None):
        _require_dict(self)
        entry = _table_lookup(self, key)
        if entry >= 0:
            return _storage_read(_instance_field(self, "_values"), entry)
        _table_insert(self, key, default)
        return default

    def pop(self, key, default=_MISSING):
        _require_dict(self)
        value = _table_remove(self, key)
        if value is _MISSING:
            if default is _MISSING:
                raise KeyError(key)
            return default
        return value

    def popitem(self):
        _require_dict(self)
        index = _storage_length(_instance_field(self, "_keys"))
        while index > 0:
            index -= 1
            key = _storage_read(_instance_field(self, "_keys"), index)
            if key is not _DELETED:
                value = _storage_read(_instance_field(self, "_values"), index)
                _table_remove(self, key)
                return (key, value)
        raise KeyError("popitem(): dictionary is empty")

    def keys(self):
        _require_dict(self)
        return dict_keys(self)

    def values(self):
        _require_dict(self)
        return dict_values(self)

    def items(self):
        _require_dict(self)
        return dict_items(self)

    def _update_table(self, other):
        _dict_update_table(self, other)

    def _update_other(self, other):
        _dict_update_other(self, other)

    def update(self, /, *args, **kwargs):
        _require_dict(self)
        if len(args) > 1:
            raise TypeError(f"update expected at most 1 argument, got {len(args)}")
        if args:
            _dict_update_other(self, args[0])
        if kwargs:
            _dict_update_table(self, kwargs)

    def clear(self):
        _require_dict(self)
        _table_clear(self)

    def copy(self):
        _require_dict(self)
        result = _empty_dict()
        _dict_update_table(result, self)
        return result


def _is_dict(value):
    owner = _type_of(value)
    return owner is dict or _class_is_subclass(owner, dict)


def _require_dict(value):
    if not _is_dict(value):
        raise TypeError(
            "descriptor requires a 'dict' object but received a '"
            + _type_name(value)
            + "'"
        )


class _DictView:
    def __init__(self, mapping):
        self.mapping = mapping

    def __len__(self):
        return _instance_field(self.mapping, "_used")

    def _entries(self):
        result = []
        for item in self:
            result.append(repr(item))
        return result

    def _repr_view(self, prefix, suffix):
        index = 0
        while index < _storage_length(_repr_active_dicts):
            if _storage_read(_repr_active_dicts, index) is self:
                return "..."
            index += 1
        _storage_append(_repr_active_dicts, self)
        try:
            return _representation(prefix, self._entries(), suffix)
        finally:
            end = _storage_length(_repr_active_dicts)
            _storage_delete(_repr_active_dicts, end - 1, end)


class _SetView(_DictView):
    """Set behavior of key and item views; values views remain sequences."""

    def __or__(self, other):
        return set(self).union(other)

    def __ror__(self, other):
        return set(other).union(self)

    def __and__(self, other):
        result = set()
        if self._comparable(other) and len(self) <= len(other):
            for item in self:
                if item in other:
                    result.add(item)
            return result
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

    def __reversed__(self):
        return _TableReverseIterator(self.mapping, 1)

    def __contains__(self, key):
        return key in self.mapping

    def __repr__(self):
        return self._repr_view("dict_keys([", "])")


class dict_values(_DictView):
    def __iter__(self):
        return _TableIterator(self.mapping, 2)

    def __reversed__(self):
        return _TableReverseIterator(self.mapping, 2)

    def __repr__(self):
        return self._repr_view("dict_values([", "])")


class dict_items(_SetView):
    def __iter__(self):
        return _TableIterator(self.mapping, 3)

    def __reversed__(self):
        return _TableReverseIterator(self.mapping, 3)

    def __contains__(self, item):
        if not _tuple_storage(item) or len(item) != 2:
            return False
        key, value = _storage_read(item, 0), _storage_read(item, 1)
        entry = _table_lookup(self.mapping, key)
        if entry < 0:
            return False
        stored = _instance_field(self.mapping, "_values")[entry]
        return stored is value or stored == value

    def __repr__(self):
        return self._repr_view("dict_items([", "])")


def _set_insert(self, key):
    code = _storage_hash(key)
    if code is None:
        code = _table_hash_fallback(key)
    indices = _instance_field(self, "_indices")
    hashes = _instance_field(self, "_hashes")
    keys = _instance_field(self, "_keys")
    mask = (_storage_length(indices) - 1) & _MASK_LIMIT
    perturb = code & _PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(indices, slot)
        if entry >= 0:
            if _storage_read(hashes, entry) == code:
                existing = _storage_read(keys, entry)
                if existing is key or existing == key:
                    return
        elif entry is _EMPTY:
            break
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask
    if (_storage_length(keys) + 1) * 3 > _storage_length(indices) * 2:
        _table_resize(self, _instance_field(self, "_used") + 1)
        indices = _instance_field(self, "_indices")
        hashes = _instance_field(self, "_hashes")
        keys = _instance_field(self, "_keys")
        mask = (_storage_length(indices) - 1) & _MASK_LIMIT
        perturb = code & _PERTURB_MASK
        slot = perturb & mask
        while _storage_read(indices, slot) is not _EMPTY:
            perturb >>= 5
            slot = (slot * 5 + perturb + 1) & mask
    _storage_write(
        indices,
        slot,
        _storage_length(keys),
    )
    _storage_write(hashes, _storage_length(keys), code)
    _storage_append(keys, key)
    next_used = (_instance_field(self, "_used") + 1) & _MASK_LIMIT
    _set_instance_field(self, "_used", next_used)
    del next_used


def _set_remove_entry(self, key):
    code = _storage_hash(key)
    if code is None:
        code = _table_hash_fallback(key)
    indices = _instance_field(self, "_indices")
    hashes = _instance_field(self, "_hashes")
    keys = _instance_field(self, "_keys")
    mask = (_storage_length(indices) - 1) & _MASK_LIMIT
    perturb = code & _PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(indices, slot)
        if entry >= 0:
            if _storage_read(hashes, entry) == code:
                existing = _storage_read(keys, entry)
                if existing is key or existing == key:
                    _storage_write(indices, slot, _DUMMY)
                    _storage_write(keys, entry, _DELETED)
                    remaining_used = (_instance_field(self, "_used") - 1) & _MASK_LIMIT
                    _set_instance_field(self, "_used", remaining_used)
                    del remaining_used
                    return key
        elif entry is _EMPTY:
            return _MISSING
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def _set_fill(self, iterable):
    _table_clear(self)
    if _is_set(iterable):
        other_keys = _instance_field(iterable, "_keys")
        count = _storage_length(other_keys)
        index = 0
        while index < count:
            key = _storage_read(other_keys, index)
            if key is not _DELETED:
                _set_insert(self, key)
            index += 1
        return
    for item in iterable:
        _set_insert(self, item)


def _set_lookup_fallback(self, key):
    try:
        return _table_lookup(self, key)
    except TypeError:
        if _is_mutable_set(key):
            return _table_lookup(self, frozenset(key))
        raise


def _set_lookup(self, key):
    code = _storage_hash(key)
    if code is None:
        if _tuple_storage(key):
            code = _tuple_table_hash(key)
        else:
            return _set_lookup_fallback(self, key)
    indices = _instance_field(self, "_indices")
    hashes = _instance_field(self, "_hashes")
    keys = _instance_field(self, "_keys")
    mask = (_storage_length(indices) - 1) & _MASK_LIMIT
    perturb = code & _PERTURB_MASK
    slot = perturb & mask
    while True:
        entry = _storage_read(indices, slot)
        if entry >= 0:
            if _storage_read(hashes, entry) == code:
                existing = _storage_read(keys, entry)
                if existing is key or existing == key:
                    return entry
        elif entry is _EMPTY:
            return _EMPTY
        perturb >>= 5
        slot = (slot * 5 + perturb + 1) & mask


def _set_remove_fallback(self, key):
    try:
        return _set_remove_entry(self, key)
    except TypeError:
        if _is_mutable_set(key):
            return _set_remove_entry(self, frozenset(key))
        raise


def _set_remove(self, key):
    if _storage_hash(key) is not None or _tuple_storage(key):
        return _set_remove_entry(self, key)
    return _set_remove_fallback(self, key)


class _SetBase(_Table):
    """Operations shared by set and frozenset."""

    __mule_slots__ = ()

    def _fill(self, iterable):
        _set_fill(self, iterable)

    def _same_kind(self):
        return class_of(self)()

    def __contains__(self, item):
        _require_any_set(self)
        return _set_lookup(self, item) >= 0

    def __iter__(self):
        _require_any_set(self)
        return _TableIterator(self, 0)

    def __repr__(self):
        _require_any_set(self)
        name = _type_name(self)
        if not _instance_field(self, "_used"):
            return name + "()"
        items = []
        for index in _table_live(self):
            items.append(repr(_instance_field(self, "_keys")[index]))
        if name == "set":
            return _representation("{", items, "}")
        return _representation(name + "({", items, "})")

    def __eq__(self, other):
        _require_any_set(self)
        if not _is_set(other):
            return NotImplemented
        return _instance_field(self, "_used") == _instance_field(
            other, "_used"
        ) and self.issubset(other)

    def __ne__(self, other):
        _require_any_set(self)
        result = self.__eq__(other)
        return NotImplemented if result is NotImplemented else not result

    def __le__(self, other):
        _require_any_set(self)
        if not _is_set(other):
            return NotImplemented
        return self.issubset(other)

    def __lt__(self, other):
        _require_any_set(self)
        if not _is_set(other):
            return NotImplemented
        return _instance_field(self, "_used") < _instance_field(
            other, "_used"
        ) and self.issubset(other)

    def __ge__(self, other):
        _require_any_set(self)
        if not _is_set(other):
            return NotImplemented
        return self.issuperset(other)

    def __gt__(self, other):
        _require_any_set(self)
        if not _is_set(other):
            return NotImplemented
        return _instance_field(self, "_used") > _instance_field(
            other, "_used"
        ) and self.issuperset(other)

    def __or__(self, other):
        _require_any_set(self)
        if not _is_set(other):
            return NotImplemented
        return self.union(other)

    def __and__(self, other):
        _require_any_set(self)
        if not _is_set(other):
            return NotImplemented
        return self.intersection(other)

    def __sub__(self, other):
        _require_any_set(self)
        if not _is_set(other):
            return NotImplemented
        return self.difference(other)

    def __xor__(self, other):
        _require_any_set(self)
        if not _is_set(other):
            return NotImplemented
        return self.symmetric_difference(other)

    def issubset(self, other):
        _require_any_set(self)
        if not _is_set(other):
            other = set(other)
        count = _storage_length(_instance_field(self, "_keys"))
        index = 0
        while index < count:
            key = _storage_read(_instance_field(self, "_keys"), index)
            if key is not _DELETED:
                if _set_lookup(other, key) < 0:
                    return False
            index += 1
        return True

    def issuperset(self, other):
        _require_any_set(self)
        if _is_set(other):
            count = _storage_length(_instance_field(other, "_keys"))
            index = 0
            while index < count:
                key = _storage_read(_instance_field(other, "_keys"), index)
                if key is not _DELETED:
                    if _set_lookup(self, key) < 0:
                        return False
                index += 1
            return True
        for item in other:
            if _set_lookup(self, item) < 0:
                return False
        return True

    def isdisjoint(self, other):
        _require_any_set(self)
        if _is_set(other):
            count = _storage_length(_instance_field(other, "_keys"))
            index = 0
            while index < count:
                key = _storage_read(_instance_field(other, "_keys"), index)
                if key is not _DELETED:
                    if _set_lookup(self, key) >= 0:
                        return False
                index += 1
            return True
        for item in other:
            if _set_lookup(self, item) >= 0:
                return False
        return True

    def union(self, *others):
        _require_any_set(self)
        result = self._same_kind()
        count = _storage_length(_instance_field(self, "_keys"))
        index = 0
        while index < count:
            key = _storage_read(_instance_field(self, "_keys"), index)
            if key is not _DELETED:
                _set_insert(result, key)
            index += 1
        for other in others:
            if _is_set(other):
                other_count = _storage_length(_instance_field(other, "_keys"))
                other_index = 0
                while other_index < other_count:
                    key = _storage_read(_instance_field(other, "_keys"), other_index)
                    if key is not _DELETED:
                        _set_insert(result, key)
                    other_index += 1
            else:
                for item in other:
                    _set_insert(result, item)
        return result

    def intersection(self, *others):
        _require_any_set(self)
        result = self._same_kind()
        if _storage_length(others) == 1 and _is_set(_storage_read(others, 0)):
            other = _storage_read(others, 0)
            count = _storage_length(_instance_field(self, "_keys"))
            index = 0
            while index < count:
                key = _storage_read(_instance_field(self, "_keys"), index)
                if key is not _DELETED and _set_lookup(other, key) >= 0:
                    _set_insert(result, key)
                index += 1
            return result
        count = _storage_length(_instance_field(self, "_keys"))
        index = 0
        while index < count:
            key = _storage_read(_instance_field(self, "_keys"), index)
            if key is not _DELETED:
                kept = True
                for other in others:
                    if key not in other:
                        kept = False
                        break
                if kept:
                    _set_insert(result, key)
            index += 1
        return result

    def difference(self, *others):
        _require_any_set(self)
        result = self._same_kind()
        if _storage_length(others) == 1 and _is_set(_storage_read(others, 0)):
            other = _storage_read(others, 0)
            count = _storage_length(_instance_field(self, "_keys"))
            index = 0
            while index < count:
                key = _storage_read(_instance_field(self, "_keys"), index)
                if key is not _DELETED and _set_lookup(other, key) < 0:
                    _set_insert(result, key)
                index += 1
            return result
        count = _storage_length(_instance_field(self, "_keys"))
        index = 0
        while index < count:
            key = _storage_read(_instance_field(self, "_keys"), index)
            if key is not _DELETED:
                kept = True
                for other in others:
                    if key in other:
                        kept = False
                        break
                if kept:
                    _set_insert(result, key)
            index += 1
        return result

    def symmetric_difference(self, other):
        _require_any_set(self)
        if not _is_set(other):
            other = set(other)
        result = self._same_kind()
        count = _storage_length(_instance_field(self, "_keys"))
        index = 0
        while index < count:
            key = _storage_read(_instance_field(self, "_keys"), index)
            if key is not _DELETED and _set_lookup(other, key) < 0:
                _set_insert(result, key)
            index += 1
        other_count = _storage_length(_instance_field(other, "_keys"))
        other_index = 0
        while other_index < other_count:
            key = _storage_read(_instance_field(other, "_keys"), other_index)
            if key is not _DELETED and _set_lookup(self, key) < 0:
                _set_insert(result, key)
            other_index += 1
        return result

    def copy(self):
        _require_any_set(self)
        return self.union()


def _is_mutable_set(value):
    owner = _type_of(value)
    return owner is set or _class_is_subclass(owner, set)


def _require_mutable_set(value):
    if not _is_mutable_set(value):
        raise TypeError(
            "descriptor requires a 'set' object but received a '"
            + _type_name(value)
            + "'"
        )


def _is_frozenset(value):
    owner = _type_of(value)
    return owner is frozenset or _class_is_subclass(owner, frozenset)


def _require_frozenset(value):
    if not _is_frozenset(value):
        raise TypeError(
            "descriptor requires a 'frozenset' object but received a '"
            + _type_name(value)
            + "'"
        )


def _is_set(value):
    owner = _type_of(value)
    return (
        owner is set
        or owner is frozenset
        or _class_is_subclass(owner, set)
        or _class_is_subclass(owner, frozenset)
    )


def _require_any_set(value):
    if not _is_set(value):
        raise TypeError(
            "descriptor requires a 'set' or 'frozenset' object but received a '"
            + _type_name(value)
            + "'"
        )


def _require_set(value):
    if not _is_set(value):
        raise TypeError("unsupported operand type for set operation")


class set(_SetBase):
    """Mutable collection of distinct hashable items."""

    __mule_slots__ = ()

    def __new__(cls, *args, **kwargs):
        if not is_class(cls) or not is_subclass(cls, set):
            raise TypeError("set.__new__() requires a subtype of set")
        return _empty_table(cls)

    def __len__(self):
        _require_mutable_set(self)
        return _instance_field(self, "_used")

    def __init__(self, *args):
        _require_mutable_set(self)
        if len(args) > 1:
            raise TypeError(f"set expected at most 1 argument, got {len(args)}")
        _set_fill(self, args[0] if args else ())

    def __ior__(self, other):
        _require_mutable_set(self)
        _require_set(other)
        self.update(other)
        return self

    def __iand__(self, other):
        _require_mutable_set(self)
        _require_set(other)
        self.intersection_update(other)
        return self

    def __isub__(self, other):
        _require_mutable_set(self)
        _require_set(other)
        self.difference_update(other)
        return self

    def __ixor__(self, other):
        _require_mutable_set(self)
        _require_set(other)
        self.symmetric_difference_update(other)
        return self

    def add(self, item):
        _require_mutable_set(self)
        _set_insert(self, item)

    def discard(self, item):
        _require_mutable_set(self)
        _set_remove(self, item)

    def remove(self, item):
        _require_mutable_set(self)
        if _set_remove(self, item) is _MISSING:
            raise KeyError(item)

    def pop(self):
        _require_mutable_set(self)
        count = _storage_length(_instance_field(self, "_keys"))
        index = 0
        while index < count:
            key = _storage_read(_instance_field(self, "_keys"), index)
            if key is not _DELETED:
                _set_remove_entry(self, key)
                return key
            index += 1
        raise KeyError("pop from an empty set")

    def clear(self):
        _require_mutable_set(self)
        _table_clear(self)

    def update(self, *others):
        _require_mutable_set(self)
        for other in others:
            if _is_set(other):
                count = _storage_length(_instance_field(other, "_keys"))
                index = 0
                while index < count:
                    key = _storage_read(_instance_field(other, "_keys"), index)
                    if key is not _DELETED:
                        _set_insert(self, key)
                    index += 1
            else:
                for item in other:
                    _set_insert(self, item)

    def intersection_update(self, *others):
        _require_mutable_set(self)
        kept = self
        for other in others:
            kept = kept.intersection(other)
        _set_fill(self, kept)

    def difference_update(self, *others):
        _require_mutable_set(self)
        for other in others:
            if _is_set(other) and other is not self:
                count = _storage_length(_instance_field(other, "_keys"))
                index = 0
                while index < count:
                    key = _storage_read(_instance_field(other, "_keys"), index)
                    if key is not _DELETED:
                        _set_remove(self, key)
                    index += 1
            elif other is self:
                _table_clear(self)
            else:
                for item in other:
                    _set_remove(self, item)

    def symmetric_difference_update(self, other):
        _require_mutable_set(self)
        kept = self.symmetric_difference(other)
        _set_fill(self, kept)


class frozenset(_SetBase):
    """Immutable, hashable collection of distinct hashable items."""

    __mule_slots__ = ()

    def __new__(cls, *args, **kwargs):
        if not is_class(cls) or not is_subclass(cls, frozenset):
            raise TypeError("frozenset.__new__() requires a subtype of frozenset")
        return _empty_table(cls)

    def __len__(self):
        _require_frozenset(self)
        return _instance_field(self, "_used")

    def __init__(self, *args):
        _require_frozenset(self)
        if len(args) > 1:
            raise TypeError(f"frozenset expected at most 1 argument, got {len(args)}")
        _set_fill(self, args[0] if args else ())

    def copy(self):
        _require_frozenset(self)
        if class_of(self) is frozenset:
            return self
        return self.union()

    def __hash__(self):
        _require_frozenset(self)
        # CPython's frozenset hash: an order-independent mix of element hashes.
        acc_hi = 0
        acc_lo = 0
        count = _storage_length(_instance_field(self, "_keys"))
        index = 0
        while index < count:
            if _storage_read(_instance_field(self, "_keys"), index) is not _DELETED:
                code = _storage_read(_instance_field(self, "_hashes"), index)
                c_lo = code & 0xFFFFFFFF
                c_hi = (code >> 32) & 0xFFFFFFFF
                x_lo = ((c_lo ^ 89869747) ^ ((c_lo & 0xFFFF) << 16)) & 0xFFFFFFFF
                x_hi = (c_hi ^ (c_lo >> 16) ^ ((c_hi & 0xFFFF) << 16)) & 0xFFFFFFFF
                m_lo = (x_lo & 0xFFFF) * 3644798167
                m_mid = (x_lo >> 16) * 3644798167 + (m_lo >> 16)
                b_lo = ((m_mid & 0xFFFF) << 16) | (m_lo & 0xFFFF)
                b_hi = (
                    (m_mid >> 16)
                    + (x_hi & 0xFFFF) * 3644798167
                    + (((x_hi >> 16) * 0x34D7) << 16)
                ) & 0xFFFFFFFF
                acc_lo ^= b_lo
                acc_hi ^= b_hi
            index += 1
        u = ((_instance_field(self, "_used") & 0x3FFFFFFF) + 1) * 1927868237
        acc_lo ^= u & 0xFFFFFFFF
        acc_hi ^= (u >> 32) & 0xFFFFFFFF
        s11_lo = ((acc_hi & 0x7FF) << 21) | (acc_lo >> 11)
        s11_hi = acc_hi >> 11
        s25_lo = ((acc_hi & 0x1FFFFFF) << 7) | (acc_lo >> 25)
        s25_hi = acc_hi >> 25
        acc_lo ^= s11_lo ^ s25_lo
        acc_hi ^= s11_hi ^ s25_hi
        fin_lo = acc_lo * 69069 + 907133923
        acc_lo = fin_lo & 0xFFFFFFFF
        acc_hi = (acc_hi * 69069 + (fin_lo >> 32)) & 0xFFFFFFFF
        if acc_hi == 0xFFFFFFFF and acc_lo == 0xFFFFFFFF:
            return 590923713
        return _lanes_to_signed_hash(acc_hi, acc_lo)


def _empty_table(cls):
    result = _allocate_instance(cls)
    _table_clear(result)
    return result


def _empty_set_sized(minimum):
    size = _MINIMUM_SIZE
    while size * 2 < minimum * 3:
        size *= 2
    result = _allocate_instance(set)
    new_indices = _empty_indices(size)
    _set_instance_field(result, "_indices", new_indices)
    del new_indices
    empty_hashes = _empty_hashes(size)
    _set_instance_field(result, "_hashes", empty_hashes)
    del empty_hashes
    empty_keys = []
    _set_instance_field(result, "_keys", empty_keys)
    del empty_keys
    empty_values = []
    _set_instance_field(result, "_values", empty_values)
    del empty_values
    cleared_used = 0
    _set_instance_field(result, "_used", cleared_used)
    del cleared_used
    return result


def _empty_dict():
    result = _allocate_instance(dict)
    _table_clear(result)
    return result


def build_dict(flat):
    """A dict display's keys and values, evaluated left to right as pairs."""
    result = _empty_dict()
    length = _storage_length(flat)
    index = 0
    while index < length:
        _table_insert(
            result, _storage_read(flat, index), _storage_read(flat, index + 1)
        )
        index += 2
    return result


def build_set(items):
    result = _empty_table(set)
    count = _storage_length(items)
    index = 0
    while index < count:
        _set_insert(result, _storage_read(items, index))
        index += 1
    return result
