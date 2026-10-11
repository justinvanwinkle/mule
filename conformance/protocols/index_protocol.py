"""__index__: used by sequence indexing, slicing, range, bin/hex/oct, operator.index, and int()."""

import operator


class Idx:
    def __init__(self, v):
        self.v = v

    def __index__(self):
        return self.v

    def __repr__(self):
        return f"Idx({self.v})"


lst = list(range(10))
print("index", lst[Idx(3)], lst[Idx(-1)], "abcdef"[Idx(2)], (1, 2, 3)[Idx(0)])
print("slice bounds", lst[Idx(2):Idx(5)], lst[::Idx(3)], "hello"[Idx(1):])
print("range", list(range(Idx(4))), list(range(Idx(1), Idx(10), Idx(4))), range(Idx(5))[Idx(2)], range(10)[Idx(2):Idx(5)])
print("bin/hex/oct", bin(Idx(5)), hex(Idx(255)), oct(Idx(8)))
print("operator.index", operator.index(Idx(42)))
print("int() via __index__", int(Idx(7)))
print("repetition", "ab" * Idx(3), [0] * Idx(2), Idx(2) * "x", b"z" * Idx(2))
print("bytes(n)", bytes(Idx(3)), bytearray(Idx(2)))
print("chr", chr(Idx(65)))
print("round ndigits", round(3.14159, Idx(2)))
print("list insert/pop", (lambda l: (l.insert(Idx(1), "x"), l.pop(Idx(0)), l)[2])([1, 2, 3]))
print("hex on int subclass", hex(True))

# slice.indices with __index__
print("slice.indices", slice(Idx(1), Idx(8), Idx(2)).indices(Idx(6)))


# __index__ must return int
class BadIndex:
    def __index__(self):
        return 1.5


for label, fn in [("subscript", lambda: lst[BadIndex()]), ("operator.index", lambda: operator.index(BadIndex())), ("bin", lambda: bin(BadIndex()))]:
    try:
        fn()
    except TypeError as e:
        print("bad __index__", label, type(e).__name__)


# __int__ alone does not make something usable as an index
class OnlyInt:
    def __int__(self):
        return 2


try:
    lst[OnlyInt()]
except TypeError as e:
    print("__int__ not index", type(e).__name__)
print("int() uses __int__", int(OnlyInt()))
try:
    bin(OnlyInt())
except TypeError as e:
    print("bin needs __index__", type(e).__name__)
try:
    range(OnlyInt())
except TypeError as e:
    print("range needs __index__", type(e).__name__)


# Floats are rejected even though they have __int__
for label, fn in [("list[float]", lambda: lst[1.0]), ("range(float)", lambda: range(2.0)), ("hex(float)", lambda: hex(1.0))]:
    try:
        fn()
    except TypeError as e:
        print("float rejected", label, type(e).__name__)


# Custom container receiving an __index__ object sees the object, not the int
class Echo:
    def __getitem__(self, k):
        return type(k).__name__


print("custom container gets raw key", Echo()[Idx(1)], Echo()[Idx(1):Idx(2)])

# Huge __index__ values
class Huge:
    def __index__(self):
        return 2 ** 80


try:
    lst[Huge()]
except IndexError as e:
    print("huge index", type(e).__name__)
print("huge slice clamps", lst[Huge():], lst[:Huge()][:3])
print("range huge len ok", range(Huge())[-1] == 2 ** 80 - 1)
try:
    len(range(Huge()))
except OverflowError as e:
    print("huge range len", type(e).__name__)

# %-formatting with %x/%o accepts __index__ objects
print("percent formatting", "%x %o" % (Idx(255), Idx(8)))
print("format spec via __index__", f"{operator.index(Idx(255)):#06x}")
