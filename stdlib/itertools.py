"""Iterator building blocks, compiled by Mule.

This provides CPython's ``itertools`` extension module
(``Modules/itertoolsmodule.c``). Each tool is a class, as in CPython; most
delegate their iteration to a generator implementing the documented
equivalent, so ``type()`` and ``isinstance`` behave as with the C types.
"""


class _GeneratedIterator:
    """An iterator class whose items come from ``self._items``."""

    def __iter__(self):
        return self

    def __next__(self):
        return next(self._items)


def _count_items(start, step):
    value = start
    while True:
        yield value
        value += step


class count(_GeneratedIterator):
    def __init__(self, start=0, step=1):
        self._start = start
        self._step = step
        self._items = _count_items(start, step)

    def __repr__(self):
        return f"count({self._start!r})" if self._step == 1 else f"count({self._start!r}, {self._step!r})"


def _cycle_items(iterable):
    saved = []
    for element in iterable:
        yield element
        saved.append(element)
    while saved:
        for element in saved:
            yield element


class cycle(_GeneratedIterator):
    def __init__(self, iterable, /):
        self._items = _cycle_items(iterable)


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


def _accumulate_items(iterable, function, initial):
    iterator = iter(iterable)
    total = initial
    if initial is None:
        try:
            total = next(iterator)
        except StopIteration:
            return
    yield total
    for element in iterator:
        total = function(total, element)
        yield total


class accumulate(_GeneratedIterator):
    def __init__(self, iterable, func=None, *, initial=None):
        self._items = _accumulate_items(iterable, _add if func is None else func, initial)


def _batched_items(iterable, n, strict):
    iterator = iter(iterable)
    while True:
        batch = tuple(islice(iterator, n))
        if not batch:
            return
        if strict and len(batch) != n:
            raise ValueError("batched(): incomplete batch")
        yield batch


class batched(_GeneratedIterator):
    def __init__(self, iterable, n, *, strict=False):
        if n < 1:
            raise ValueError("n must be at least one")
        self._items = _batched_items(iterable, n, strict)


def _chain_items(iterables):
    for iterable in iterables:
        yield from iterable


class chain(_GeneratedIterator):
    def __init__(self, *iterables):
        self._items = _chain_items(iterables)

    @classmethod
    def from_iterable(cls, iterable, /):
        result = cls()
        result._items = _chain_items(iterable)
        return result


def _compress_items(data, selectors):
    for datum, selector in zip(data, selectors):
        if selector:
            yield datum


class compress(_GeneratedIterator):
    def __init__(self, data, selectors):
        self._items = _compress_items(data, selectors)


def _dropwhile_items(predicate, iterable):
    iterator = iter(iterable)
    for element in iterator:
        if not predicate(element):
            yield element
            break
    yield from iterator


class dropwhile(_GeneratedIterator):
    def __init__(self, predicate, iterable, /):
        self._items = _dropwhile_items(predicate, iterable)


def _filterfalse_items(predicate, iterable):
    for element in iterable:
        if not (bool(element) if predicate is None else predicate(element)):
            yield element


class filterfalse(_GeneratedIterator):
    def __init__(self, function, iterable, /):
        self._items = _filterfalse_items(function, iterable)


class _Missing:
    pass


_MISSING = _Missing()


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


def _islice_items(iterable, start, stop, step):
    iterator = iter(iterable)
    index = 0
    following = start
    while stop is None or following < stop:
        try:
            element = next(iterator)
        except StopIteration:
            return
        if index == following:
            yield element
            following += step
        index += 1
    # Consume up to the stop position, as CPython does.
    while index < following and (stop is None or index < stop):
        try:
            next(iterator)
        except StopIteration:
            return
        index += 1


class islice(_GeneratedIterator):
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
        self._items = _islice_items(iterable, start, stop, step)


def _pairwise_items(iterable):
    iterator = iter(iterable)
    try:
        previous = next(iterator)
    except StopIteration:
        return
    for current in iterator:
        yield (previous, current)
        previous = current


class pairwise(_GeneratedIterator):
    def __init__(self, iterable, /):
        self._items = _pairwise_items(iterable)


def _starmap_items(function, iterable):
    for arguments in iterable:
        yield function(*arguments)


class starmap(_GeneratedIterator):
    def __init__(self, function, iterable, /):
        self._items = _starmap_items(function, iterable)


def _takewhile_items(predicate, iterable):
    for element in iterable:
        if not predicate(element):
            return
        yield element


class takewhile(_GeneratedIterator):
    def __init__(self, predicate, iterable, /):
        self._items = _takewhile_items(predicate, iterable)


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


def _zip_longest_items(iterables, fillvalue):
    iterators = [iter(iterable) for iterable in iterables]
    active = len(iterators)
    if not active:
        return
    while True:
        values = []
        for index in range(len(iterators)):
            iterator = iterators[index]
            if iterator is None:
                values.append(fillvalue)
                continue
            try:
                values.append(next(iterator))
            except StopIteration:
                active -= 1
                if not active:
                    return
                iterators[index] = None
                values.append(fillvalue)
        yield tuple(values)


class zip_longest(_GeneratedIterator):
    def __init__(self, *iterables, fillvalue=None):
        self._items = _zip_longest_items(iterables, fillvalue)


def _product_items(pools):
    result = [[]]
    for pool in pools:
        result = [prefix + [item] for prefix in result for item in pool]
    for combination in result:
        yield tuple(combination)


class product(_GeneratedIterator):
    def __init__(self, *iterables, repeat=1):
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
        self._items = _permutations_items(pool, len(pool) if r is None else r)


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
