"""The ``slice`` type and index adjustment, compiled by Mule.

Follows CPython's ``Objects/sliceobject.c``: ``PySlice_Unpack`` converts the
bounds with ``__index__`` and rejects a zero step, and
``PySlice_AdjustIndices`` clamps them to a sequence length and counts the
selected positions. Sequence types use ``positions`` to read their elements.
"""

# Compiler-provided physical primitives are not host Python globals.
# ruff: noqa: F821

from _mule_native import type_of as _type_of
from _object import int


class slice:
    """Bounds selecting positions of a sequence: ``start:stop:step``."""

    def __init__(self, *args):
        if len(args) == 1:
            self.start, self.stop, self.step = None, args[0], None
        elif len(args) == 2:
            self.start, self.stop, self.step = args[0], args[1], None
        elif len(args) == 3:
            self.start, self.stop, self.step = args[0], args[1], args[2]
        else:
            raise TypeError(f"slice expected 1 to 3 arguments, got {len(args)}")

    def indices(self, length):
        length = _bound(length)
        if length < 0:
            raise ValueError("length should not be negative")
        start, stop, step, _ = positions(self, length)
        return (start, stop, step)

    def __repr__(self):
        return f"slice({self.start!r}, {self.stop!r}, {self.step!r})"


def _bound(value):
    if _type_of(value) is int:
        return value
    method = _index_method(value)
    if method is not None:
        value = method()
    return _integer_storage(value)


def is_slice(value):
    return _raised_class(value) is slice


def positions(bounds, length):
    """The adjusted start, stop and step, and how many positions they select."""
    if bounds.step is None:
        step = 1
    else:
        step = _bound(bounds.step)
        if step == 0:
            raise ValueError("slice step cannot be zero")
    if bounds.start is None:
        start = length - 1 if step < 0 else 0
    else:
        start = _bound(bounds.start)
        if start < 0:
            start += length
            if start < 0:
                start = -1 if step < 0 else 0
        elif start >= length:
            start = length - 1 if step < 0 else length
    if bounds.stop is None:
        stop = -1 if step < 0 else length
    else:
        stop = _bound(bounds.stop)
        if stop < 0:
            stop += length
            if stop < 0:
                stop = -1 if step < 0 else 0
        elif stop >= length:
            stop = length - 1 if step < 0 else length
    if step < 0:
        count = (start - stop - 1) // -step + 1 if stop < start else 0
    else:
        count = (stop - start - 1) // step + 1 if start < stop else 0
    return start, stop, step, count
