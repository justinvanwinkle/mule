"""``deque`` and ``defaultdict``, compiled by Mule.

This provides the containers of CPython's ``_collections`` extension module
(``Modules/_collectionsmodule.c``). The deque keeps its items in a list used
as a ring buffer, so appends and pops at both ends take constant time.
"""


_repr_active_deques = []


class deque:
    """A double-ended queue with an optional maximum length."""

    def __init__(self, iterable=(), maxlen=None):
        if maxlen is not None and maxlen < 0:
            raise ValueError("maxlen must be non-negative")
        self._maxlen = maxlen
        self._items = [None] * 8
        self._head = 0
        self._size = 0
        self._mutations = 0
        self.extend(iterable)

    @property
    def maxlen(self):
        return self._maxlen

    def _slot(self, index):
        return (self._head + index) % len(self._items)

    def _grow(self):
        items = [None] * (len(self._items) * 2)
        for index in range(self._size):
            items[index] = self._items[self._slot(index)]
        self._items = items
        self._head = 0

    def append(self, item):
        if self._maxlen == 0:
            return
        if self._maxlen is not None and self._size == self._maxlen:
            self.popleft()
        if self._size == len(self._items):
            self._grow()
        self._items[self._slot(self._size)] = item
        self._size += 1
        self._mutations += 1

    def appendleft(self, item):
        if self._maxlen == 0:
            return
        if self._maxlen is not None and self._size == self._maxlen:
            self.pop()
        if self._size == len(self._items):
            self._grow()
        self._head = (self._head - 1) % len(self._items)
        self._items[self._head] = item
        self._size += 1
        self._mutations += 1

    def pop(self):
        if not self._size:
            raise IndexError("pop from an empty deque")
        slot = self._slot(self._size - 1)
        item = self._items[slot]
        self._items[slot] = None
        self._size -= 1
        self._mutations += 1
        return item

    def popleft(self):
        if not self._size:
            raise IndexError("pop from an empty deque")
        item = self._items[self._head]
        self._items[self._head] = None
        self._head = (self._head + 1) % len(self._items)
        self._size -= 1
        self._mutations += 1
        return item

    def extend(self, iterable):
        if iterable is self:
            iterable = list(iterable)
        for item in iterable:
            self.append(item)

    def extendleft(self, iterable):
        if iterable is self:
            iterable = list(iterable)
        for item in iterable:
            self.appendleft(item)

    def clear(self):
        self._items = [None] * 8
        self._head = 0
        self._size = 0
        self._mutations += 1

    def copy(self):
        return deque(self, self._maxlen)

    def count(self, value):
        return sum(1 for item in self if item is value or item == value)

    def index(self, value, start=0, stop=None):
        stop = self._size if stop is None else stop
        if start < 0:
            start = max(start + self._size, 0)
        if stop < 0:
            stop = max(stop + self._size, 0)
        for index in range(start, min(stop, self._size)):
            item = self._items[self._slot(index)]
            if item is value or item == value:
                return index
        raise ValueError(f"{value!r} is not in deque")

    def insert(self, index, value):
        if self._maxlen is not None and self._size == self._maxlen:
            raise IndexError("deque already at its maximum size")
        items = list(self)
        items.insert(index, value)
        self.clear()
        self.extend(items)

    def remove(self, value):
        self.__delitem__(self.index(value))

    def reverse(self):
        items = list(self)
        items.reverse()
        self.clear()
        self.extend(items)

    def rotate(self, n=1):
        if not self._size:
            return
        n %= self._size
        for _ in range(n):
            self.appendleft(self.pop())

    def _position(self, index):
        if index < 0:
            index += self._size
        if index < 0 or index >= self._size:
            raise IndexError("deque index out of range")
        return self._slot(index)

    def __getitem__(self, index):
        return self._items[self._position(index)]

    def __setitem__(self, index, value):
        self._items[self._position(index)] = value

    def __delitem__(self, index):
        self._position(index)
        items = list(self)
        del items[index]
        self.clear()
        self.extend(items)

    def __len__(self):
        return self._size

    def __bool__(self):
        return self._size > 0

    def __iter__(self):
        mutations = self._mutations
        for index in range(self._size):
            if self._mutations != mutations:
                raise RuntimeError("deque mutated during iteration")
            yield self._items[self._slot(index)]

    def __reversed__(self):
        for index in range(self._size - 1, -1, -1):
            yield self._items[self._slot(index)]

    def __contains__(self, value):
        for item in self:
            if item is value or item == value:
                return True
        return False

    def __eq__(self, other):
        if not isinstance(other, deque):
            return NotImplemented
        return list(self) == list(other)

    def __add__(self, other):
        if not isinstance(other, deque):
            raise TypeError(f'can only concatenate deque (not "{type(other).__name__}") to deque')
        result = self.copy()
        result.extend(other)
        return result

    def __iadd__(self, other):
        self.extend(other)
        return self

    def __copy__(self):
        return self.copy()

    def __reduce__(self):
        return type(self), (list(self), self._maxlen)

    def __class_getitem__(cls, item):
        return type(list[int])(cls, item)

    def __repr__(self):
        for active in _repr_active_deques:
            if active is self:
                return "[...]"
        _repr_active_deques.append(self)
        try:
            items = ", ".join(repr(item) for item in self)
            if self._maxlen is None:
                return f"deque([{items}])"
            return f"deque([{items}], maxlen={self._maxlen})"
        finally:
            _repr_active_deques.pop()


class defaultdict(dict):
    """A dict that creates missing values with ``default_factory``."""

    def __init__(self, default_factory=None, *args, **keywords):
        if default_factory is not None and not callable(default_factory):
            raise TypeError("first argument must be callable or None")
        dict.__init__(self, *args)
        self.update(keywords)
        self.default_factory = default_factory

    def __missing__(self, key):
        if self.default_factory is None:
            raise KeyError(key)
        value = self.default_factory()
        self[key] = value
        return value

    def copy(self):
        return defaultdict(self.default_factory, self)

    __copy__ = copy

    def __reduce__(self):
        return type(self), (self.default_factory,), None, None, iter(self.items())

    def __repr__(self):
        return f"defaultdict({self.default_factory!r}, {dict.__repr__(self)})"


_deque_iterator = type(iter(deque()))
