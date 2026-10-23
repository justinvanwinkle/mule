"""Typed numeric and character arrays, compiled by Mule."""

import _collections_abc

typecodes = "bBuhHiIlLqQfdw"

_ITEMSIZE = {
    "b": 1,
    "B": 1,
    "u": 4,
    "w": 4,
    "h": 2,
    "H": 2,
    "i": 4,
    "I": 4,
    "l": 8,
    "L": 8,
    "q": 8,
    "Q": 8,
    "f": 4,
    "d": 8,
}


def _coerce_item(typecode, value):
    if typecode in ("u", "w"):
        if not isinstance(value, str) or len(value) != 1:
            raise TypeError("array item must be unicode character")
        return value
    if typecode in ("f", "d"):
        return float(value)
    if isinstance(value, float):
        raise TypeError("integer argument expected, got float")
    return int(value)


class array:
    def __init__(self, typecode, initializer=()):
        if not isinstance(typecode, str) or len(typecode) != 1:
            raise TypeError(
                "array() argument 1 must be a unicode character, not "
                + type(typecode).__name__
            )
        if typecode not in _ITEMSIZE:
            raise ValueError(
                "bad typecode (must be b, B, u, h, H, i, I, l, L, q, Q, f, d or w)"
            )
        self._typecode = typecode
        self._items = []
        if isinstance(initializer, (bytes, bytearray)):
            self.frombytes(initializer)
        elif isinstance(initializer, str):
            if typecode not in ("u", "w"):
                raise TypeError(
                    "cannot use a str to initialize an array with typecode '"
                    + typecode
                    + "'"
                )
            self.extend(initializer)
        else:
            self.extend(initializer)

    @property
    def typecode(self):
        return self._typecode

    @property
    def itemsize(self):
        return _ITEMSIZE[self._typecode]

    def __len__(self):
        return len(self._items)

    def __getitem__(self, index):
        if isinstance(index, slice):
            result = array(self._typecode)
            result._items = self._items[index]
            return result
        return self._items[index]

    def __setitem__(self, index, value):
        if isinstance(index, slice):
            if not isinstance(value, array):
                raise TypeError(
                    f"can only assign array (not \"{type(value).__name__}\") to array slice"
                )
            if value._typecode != self._typecode:
                raise TypeError("bad argument type for built-in operation")
            self._items[index] = list(value._items)
        else:
            self._items[index] = _coerce_item(self._typecode, value)

    def __delitem__(self, index):
        del self._items[index]

    def __iter__(self):
        return iter(self._items)

    def __reversed__(self):
        return reversed(self._items)

    def __contains__(self, value):
        return value in self._items

    def __repr__(self):
        if not self._items:
            return f"array({self._typecode!r})"
        if self._typecode in ("u", "w"):
            return f"array({self._typecode!r}, {''.join(self._items)!r})"
        return f"array({self._typecode!r}, {self._items!r})"

    def __eq__(self, other):
        if isinstance(other, array):
            return self._items == other._items
        return NotImplemented

    def __ne__(self, other):
        if isinstance(other, array):
            return self._items != other._items
        return NotImplemented

    def __lt__(self, other):
        if isinstance(other, array):
            return self._items < other._items
        return NotImplemented

    def __le__(self, other):
        if isinstance(other, array):
            return self._items <= other._items
        return NotImplemented

    def __gt__(self, other):
        if isinstance(other, array):
            return self._items > other._items
        return NotImplemented

    def __ge__(self, other):
        if isinstance(other, array):
            return self._items >= other._items
        return NotImplemented

    def __add__(self, other):
        if not isinstance(other, array):
            raise TypeError(
                f'can only append array (not "{type(other).__name__}") to array'
            )
        if self._typecode != other._typecode:
            raise TypeError("bad argument type for built-in operation")
        result = array(self._typecode)
        result._items = self._items + other._items
        return result

    def __iadd__(self, other):
        if not isinstance(other, array):
            raise TypeError(
                f'can only extend array with array (not "{type(other).__name__}")'
            )
        if self._typecode != other._typecode:
            raise TypeError("bad argument type for built-in operation")
        self._items.extend(other._items)
        return self

    def __mul__(self, count):
        result = array(self._typecode)
        result._items = self._items * int(count)
        return result

    def __rmul__(self, count):
        return self.__mul__(count)

    def __imul__(self, count):
        self._items *= int(count)
        return self

    def append(self, value):
        self._items.append(_coerce_item(self._typecode, value))

    def extend(self, iterable):
        if isinstance(iterable, array) and iterable._typecode != self._typecode:
            raise TypeError("can only extend with array of same kind")
        for item in iterable:
            self.append(item)

    def insert(self, index, value):
        self._items.insert(index, _coerce_item(self._typecode, value))

    def pop(self, index=-1):
        return self._items.pop(index)

    def remove(self, value):
        self._items.remove(value)

    def index(self, value, start=0, stop=None):
        if stop is None:
            return self._items.index(value, start)
        return self._items.index(value, start, stop)

    def count(self, value):
        return self._items.count(value)

    def reverse(self):
        self._items.reverse()

    def clear(self):
        self._items.clear()

    def tolist(self):
        return list(self._items)

    def fromlist(self, items):
        if not isinstance(items, list):
            raise TypeError("arg must be list")
        coerced = [_coerce_item(self._typecode, item) for item in items]
        self._items.extend(coerced)

    def tobytes(self):
        import struct

        code = "I" if self._typecode in ("u", "w") else self._typecode
        values = (
            [ord(ch) for ch in self._items]
            if self._typecode in ("u", "w")
            else self._items
        )
        return struct.pack(f"{len(values)}{code}", *values)

    def frombytes(self, buffer):
        import struct

        data = bytes(buffer)
        size = self.itemsize
        if len(data) % size != 0:
            raise ValueError("bytes length not a multiple of item size")
        count = len(data) // size
        code = "I" if self._typecode in ("u", "w") else self._typecode
        unpacked = struct.unpack(f"{count}{code}", data)
        if self._typecode in ("u", "w"):
            self._items.extend(chr(cp) for cp in unpacked)
        else:
            self._items.extend(unpacked)

    def __copy__(self):
        result = array(self._typecode)
        result._items = list(self._items)
        return result

    def __deepcopy__(self, memo=None):
        return self.__copy__()


ArrayType = array
_collections_abc.MutableSequence.register(array)
