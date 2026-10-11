"""Objects with __index__ used as list indices, slice bounds, range arguments, repetition counts and more."""

import operator


class Ix:
    def __init__(self, v, label=None):
        self.v = v
        self.label = label or str(v)

    def __index__(self):
        print(f"  index({self.label})")
        return self.v

    def __repr__(self):
        return f"Ix({self.v})"


xs = list(range(10))
print("list index", xs[Ix(3)])
print("negative index", xs[Ix(-1)])
print("slice", xs[Ix(2):Ix(8):Ix(2)])
print("slice obj indices", slice(Ix(1), Ix(20), Ix(3)).indices(10))
print("range", list(range(Ix(1), Ix(10), Ix(4))))
print("range index", range(100)[Ix(42)])
print("list repeat", [0] * Ix(3))
print("str repeat", "ab" * Ix(2))
print("reflected repeat", Ix(2, "left") * [7])
print("tuple repeat", (1, 2) * Ix(2))
print("bytes repeat", b"z" * Ix(3))
print("bin/hex/oct", bin(Ix(5)), hex(Ix(255)), oct(Ix(8)))
print("operator.index", operator.index(Ix(7)))
print("str index", "hello"[Ix(1)])
print("bytes index", b"hello"[Ix(4)])
print("tuple index", (9, 8, 7)[Ix(-2)])
print("int() does not use __index__ unless no __int__", int(Ix(9)))
print("round ndigits", round(3.14159, Ix(2)))
print("chr", chr(Ix(65)))
print("list.insert", (lambda l: (l.insert(Ix(1), "I"), l)[1])([1, 2]))
print("list.pop", [5, 6, 7].pop(Ix(0)))
print("bytes(n)", bytes(Ix(3)))
print("bytearray item set", (lambda ba: (ba.__setitem__(Ix(0), Ix(66)), ba)[1])(bytearray(b"aa")))
print("str slice with index", "abcdefgh"[Ix(1)::Ix(3)])
print("isinstance int", isinstance(Ix(1), int))

ys = list("abcdef")
ys[Ix(1):Ix(3)] = ["X"]
print("slice assignment", ys)
del ys[Ix(0)]
print("del index", ys)


class BadIx:
    def __index__(self):
        return 1.5


for label, fn in [
    ("float from __index__", lambda: xs[BadIx()]),
    ("float index", lambda: xs[1.0]),
    ("str range", lambda: range("3")),
    ("float repeat", lambda: [1] * 2.0),
]:
    try:
        fn()
    except TypeError as e:
        print(label, type(e).__name__)


class IntSub(int):
    def __index__(self):
        return 99


print("int subclass index ignored by list", xs[IntSub(2)])
print("operator.index on int subclass", operator.index(IntSub(2)))

# __index__ huge values
try:
    xs[Ix(10**30)]
except IndexError as e:
    print("huge index", type(e).__name__)
print("huge slice clamps", xs[Ix(-10**30):Ix(10**30)][:3])
try:
    [0] * Ix(10**30)
except (OverflowError, MemoryError) as e:
    print("huge repeat", type(e).__name__)

# Evaluation order of index calls in slices
print("order", xs[Ix(1, "start"):Ix(9, "stop"):Ix(3, "step")])
d = {Ix(1): "obj"}
print("dict key not converted", list(d.values()), 1 in d)
