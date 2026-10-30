"""Iterator building blocks, compiled by Mule.

This provides CPython's ``itertools`` extension module
(``Modules/itertoolsmodule.c``). Each tool is a class, as in CPython, so
``type()`` and ``isinstance`` behave as with the C types.
"""

__speed__ = 3


class _GeneratedIterator:
    """An iterator class whose items come from ``self._items``."""

    def __iter__(self):
        return self

    def __next__(self):
        return next(self._items)


class count:
    def __init__(self, start=0, step=1):
        self._current = start
        self._step = step

    def __iter__(self):
        return self

    def __next__(self):
        value = self._current
        self._current = value + self._step
        return value

    def __repr__(self):
        return (
            f"count({self._current!r})"
            if self._step == 1
            else f"count({self._current!r}, {self._step!r})"
        )


class cycle:
    def __init__(self, iterable, /):
        self._iterator = iter(iterable)
        self._saved = []
        self._index = 0

    def __iter__(self):
        return self

    def __next__(self):
        iterator = self._iterator
        if iterator is not None:
            try:
                element = next(iterator)
                self._saved.append(element)
                return element
            except StopIteration:
                self._iterator = None
        saved = self._saved
        if not saved:
            raise StopIteration
        index = self._index
        element = saved[index]
        index += 1
        self._index = 0 if index >= len(saved) else index
        return element


class repeat:
    def __init__(self, object, times=None):
        self._object = object
        self._times = times

    def __iter__(self):
        return self

    def __next__(self):
        if self._times is not None:
            if self._times <= 0:
                raise StopIteration
            self._times -= 1
        return self._object

    def __length_hint__(self):
        if self._times is None:
            raise TypeError("len() of unsized object")
        return max(self._times, 0)

    def __repr__(self):
        if self._times is None:
            return f"repeat({self._object!r})"
        return f"repeat({self._object!r}, {self._times})"


def _add(left, right):
    return left + right


class _Missing:
    pass


_MISSING = _Missing()


class accumulate:
    def __init__(self, iterable, func=None, *, initial=None):
        self._iterator = iter(iterable)
        self._function = _add if func is None else func
        self._total = _MISSING if initial is None else initial
        self._started = False

    def __iter__(self):
        return self

    def __next__(self):
        if not self._started:
            self._started = True
            total = self._total
            if total is _MISSING:
                total = next(self._iterator)
                self._total = total
            return total
        element = next(self._iterator)
        total = self._function(self._total, element)
        self._total = total
        return total


class batched:
    def __init__(self, iterable, n, *, strict=False):
        if n < 1:
            raise ValueError("n must be at least one")
        self._iterator = iter(iterable)
        self._n = n
        self._strict = strict
        self._exhausted = False

    def __iter__(self):
        return self

    def __next__(self):
        if self._exhausted:
            raise StopIteration
        iterator = self._iterator
        n = self._n
        items = []
        index = 0
        while index < n:
            try:
                items.append(next(iterator))
            except StopIteration:
                self._exhausted = True
                break
            index += 1
        if not items:
            raise StopIteration
        if self._strict and len(items) != n:
            raise ValueError("batched(): incomplete batch")
        return tuple(items)


class chain:
    def __init__(self, *iterables):
        self._outer = iter(iterables)
        self._inner = None

    @classmethod
    def from_iterable(cls, iterable, /):
        result = cls()
        result._outer = iter(iterable)
        return result

    def __iter__(self):
        return self

    def __next__(self):
        while True:
            inner = self._inner
            if inner is None:
                outer = self._outer
                if outer is None:
                    raise StopIteration
                try:
                    inner = iter(next(outer))
                except StopIteration:
                    self._outer = None
                    raise
                self._inner = inner
            try:
                return next(inner)
            except StopIteration:
                self._inner = None


class compress:
    def __init__(self, data, selectors):
        self._data = iter(data)
        self._selectors = iter(selectors)

    def __iter__(self):
        return self

    def __next__(self):
        data = self._data
        selectors = self._selectors
        while True:
            datum = next(data)
            selector = next(selectors)
            if selector:
                return datum


class dropwhile:
    def __init__(self, predicate, iterable, /):
        self._predicate = predicate
        self._iterator = iter(iterable)
        self._dropping = True

    def __iter__(self):
        return self

    def __next__(self):
        iterator = self._iterator
        if self._dropping:
            predicate = self._predicate
            for element in iterator:
                if not predicate(element):
                    self._dropping = False
                    return element
            self._dropping = False
            raise StopIteration
        return next(iterator)


class filterfalse:
    def __init__(self, function, iterable, /):
        self._predicate = function
        self._iterator = iter(iterable)

    def __iter__(self):
        return self

    def __next__(self):
        iterator = self._iterator
        predicate = self._predicate
        if predicate is None:
            for element in iterator:
                if not element:
                    return element
        else:
            for element in iterator:
                if not predicate(element):
                    return element
        raise StopIteration


class groupby:
    def __init__(self, iterable, key=None):
        self._key = (lambda value: value) if key is None else key
        self._iterator = iter(iterable)
        self._target = _MISSING
        self._current = _MISSING
        self._value = None
        self._exhausted = False

    def __iter__(self):
        return self

    def _advance(self):
        try:
            self._value = next(self._iterator)
        except StopIteration:
            self._exhausted = True
            raise
        self._current = self._key(self._value)

    def __next__(self):
        if self._current is _MISSING:
            self._advance()
        while self._target is not _MISSING and self._current == self._target:
            self._advance()
        self._target = self._current
        return (self._current, self._group(self._target))

    def _group(self, target):
        while self._current == target:
            yield self._value
            try:
                self._advance()
            except StopIteration:
                return


class islice:
    def __init__(self, iterable, *args):
        if len(args) == 1:
            start, stop, step = 0, args[0], 1
        elif len(args) in (2, 3):
            start, stop = args[0], args[1]
            step = args[2] if len(args) == 3 else 1
            start = 0 if start is None else start
            step = 1 if step is None else step
        else:
            raise TypeError("islice expected 2 to 4 arguments")
        for name, value in (("Start", start), ("Stop", stop)):
            if value is not None and (not isinstance(value, int) or value < 0):
                raise ValueError(
                    f"{name} argument for islice() must be None or an integer: 0 <= x <= sys.maxsize."
                )
        if not isinstance(step, int) or step < 1:
            raise ValueError("Step for islice() must be a positive integer or None.")
        self._iterator = iter(iterable)
        self._next_index = start
        self._stop = stop
        self._step = step
        self._index = 0

    def __iter__(self):
        return self

    def __next__(self):
        iterator = self._iterator
        if iterator is None:
            raise StopIteration
        index = self._index
        following = self._next_index
        stop = self._stop
        if stop is not None and following >= stop:
            while index < stop:
                try:
                    next(iterator)
                except StopIteration:
                    break
                index += 1
            self._iterator = None
            raise StopIteration
        while index < following:
            try:
                next(iterator)
            except StopIteration:
                self._iterator = None
                raise
            index += 1
        try:
            element = next(iterator)
        except StopIteration:
            self._iterator = None
            raise
        self._index = index + 1
        self._next_index = following + self._step
        return element


class pairwise:
    def __init__(self, iterable, /):
        self._iterator = iter(iterable)
        self._previous = _MISSING

    def __iter__(self):
        return self

    def __next__(self):
        iterator = self._iterator
        previous = self._previous
        if previous is _MISSING:
            previous = next(iterator)
        current = next(iterator)
        self._previous = current
        return (previous, current)


class starmap:
    def __init__(self, function, iterable, /):
        self._function = function
        self._iterator = iter(iterable)

    def __iter__(self):
        return self

    def __next__(self):
        return self._function(*next(self._iterator))


class takewhile:
    def __init__(self, predicate, iterable, /):
        self._predicate = predicate
        self._iterator = iter(iterable)
        self._exhausted = False

    def __iter__(self):
        return self

    def __next__(self):
        if self._exhausted:
            raise StopIteration
        element = next(self._iterator)
        if not self._predicate(element):
            self._exhausted = True
            raise StopIteration
        return element


class _TeeData:
    def __init__(self, iterator):
        self.iterator = iterator
        self.buffer = []
        self.base = 0
        self.readers = []


class _tee:
    def __init__(self, data):
        self._data = data
        self._position = data.base

    def __iter__(self):
        return self

    def __next__(self):
        data = self._data
        offset = self._position - data.base
        if offset < len(data.buffer):
            value = data.buffer[offset]
        else:
            value = next(data.iterator)
            data.buffer.append(value)
        self._position += 1
        return value

    def __copy__(self):
        copy = _tee(self._data)
        copy._position = self._position
        return copy


def tee(iterable, n=2, /):
    if n < 0:
        raise ValueError("n must be >= 0")
    data = _TeeData(iter(iterable))
    return tuple(_tee(data) for _ in range(n))


class zip_longest:
    def __init__(self, *iterables, fillvalue=None):
        self._iterators = [iter(iterable) for iterable in iterables]
        self._active = len(self._iterators)
        self._fillvalue = fillvalue

    def __iter__(self):
        return self

    def __next__(self):
        if not self._active:
            raise StopIteration
        iterators = self._iterators
        fillvalue = self._fillvalue
        values = []
        index = 0
        count = len(iterators)
        while index < count:
            iterator = iterators[index]
            if iterator is None:
                values.append(fillvalue)
            else:
                try:
                    values.append(next(iterator))
                except StopIteration:
                    self._active -= 1
                    if not self._active:
                        raise
                    iterators[index] = None
                    values.append(fillvalue)
            index += 1
        return tuple(values)


def _product_items(pools):
    result = [[]]
    for pool in pools:
        result = [prefix + [item] for prefix in result for item in pool]
    for combination in result:
        yield tuple(combination)


class product(_GeneratedIterator):
    def __init__(self, *iterables, repeat=1):
        if repeat < 0:
            raise ValueError("repeat argument cannot be negative")
        pools = [tuple(iterable) for iterable in iterables] * repeat
        self._items = _product_items(pools)


def _permutations_items(pool, r):
    n = len(pool)
    if r > n:
        return
    indices = list(range(n))
    cycles = list(range(n, n - r, -1))
    yield tuple(pool[i] for i in indices[:r])
    while n:
        for i in reversed(range(r)):
            cycles[i] -= 1
            if cycles[i] == 0:
                indices[i:] = indices[i + 1 :] + indices[i : i + 1]
                cycles[i] = n - i
            else:
                j = cycles[i]
                indices[i], indices[-j] = indices[-j], indices[i]
                yield tuple(pool[i] for i in indices[:r])
                break
        else:
            return


class permutations(_GeneratedIterator):
    def __init__(self, iterable, r=None):
        pool = tuple(iterable)
        r = len(pool) if r is None else r
        if r < 0:
            raise ValueError("r must be non-negative")
        self._items = _permutations_items(pool, r)


def _combinations_items(pool, r):
    n = len(pool)
    if r > n:
        return
    indices = list(range(r))
    yield tuple(pool[i] for i in indices)
    while True:
        for i in reversed(range(r)):
            if indices[i] != i + n - r:
                break
        else:
            return
        indices[i] += 1
        for j in range(i + 1, r):
            indices[j] = indices[j - 1] + 1
        yield tuple(pool[i] for i in indices)


class combinations(_GeneratedIterator):
    def __init__(self, iterable, r):
        if r < 0:
            raise ValueError("r must be non-negative")
        self._items = _combinations_items(tuple(iterable), r)


def _combinations_with_replacement_items(pool, r):
    n = len(pool)
    if not n and r:
        return
    indices = [0] * r
    yield tuple(pool[i] for i in indices)
    while True:
        for i in reversed(range(r)):
            if indices[i] != n - 1:
                break
        else:
            return
        indices[i:] = [indices[i] + 1] * (r - i)
        yield tuple(pool[i] for i in indices)


class combinations_with_replacement(_GeneratedIterator):
    def __init__(self, iterable, r):
        if r < 0:
            raise ValueError("r must be non-negative")
        self._items = _combinations_with_replacement_items(tuple(iterable), r)
