"""Truthiness of builtin types and custom __bool__/__len__ protocols."""

import collections
import decimal
import fractions

values = [
    ("None", None),
    ("True", True),
    ("False", False),
    ("int 0", 0),
    ("int -1", -1),
    ("big int", 10**30),
    ("float 0.0", 0.0),
    ("float -0.0", -0.0),
    ("float nan", float("nan")),
    ("float inf", float("inf")),
    ("complex 0j", 0j),
    ("complex 0+1j", 1j),
    ("str empty", ""),
    ("str space", " "),
    ("str zero", "0"),
    ("bytes empty", b""),
    ("bytes nul", b"\x00"),
    ("bytearray empty", bytearray()),
    ("list empty", []),
    ("list of empty", [[]]),
    ("tuple empty", ()),
    ("tuple of None", (None,)),
    ("dict empty", {}),
    ("dict", {0: 0}),
    ("set empty", set()),
    ("frozenset", frozenset([0])),
    ("range empty", range(0)),
    ("range 5 5", range(5, 5)),
    ("range 1", range(1)),
    ("memoryview empty", memoryview(b"")),
    ("Ellipsis", ...),
    ("NotImplemented-free object", object()),
    ("type", int),
    ("function", len),
    ("decimal 0", decimal.Decimal(0)),
    ("decimal -0", decimal.Decimal("-0")),
    ("fraction 0", fractions.Fraction(0, 5)),
    ("deque empty", collections.deque()),
    ("Counter", collections.Counter("a")),
    ("dict keys empty", {}.keys()),
    ("dict items", {1: 2}.items()),
]
for label, value in values:
    print("bool", label, bool(value), "not", not value)


class AlwaysFalse:
    def __bool__(self):
        return False


class Sized:
    def __init__(self, n):
        self.n = n

    def __len__(self):
        return self.n


class Both:
    def __bool__(self):
        print("  Both.__bool__ called")
        return True

    def __len__(self):
        print("  Both.__len__ called")
        return 0


class NoProtocol:
    pass


class BadBool:
    def __bool__(self):
        return 1


class NegativeLen:
    def __len__(self):
        return -1


class BoolNone:
    __bool__ = None


print("AlwaysFalse", bool(AlwaysFalse()))
print("Sized 0", bool(Sized(0)), "Sized 3", bool(Sized(3)))
print("Both:", bool(Both()))
print("NoProtocol", bool(NoProtocol()))

for label, factory in (("BadBool", BadBool), ("NegativeLen", NegativeLen), ("BoolNone", BoolNone)):
    try:
        print(label, bool(factory()))
    except Exception as e:
        print(label, "raised", type(e).__name__)

# truthiness used implicitly by if, while, and/or, not, conditional expressions
s = Sized(2)
if s:
    print("if Sized(2) taken")
n = Sized(3)
iterations = 0
while n:
    n.n -= 1
    iterations += 1
print("while Sized iterations", iterations)
print("and/or return operands", AlwaysFalse() and 1 or "fallback")
print("ternary", "yes" if Sized(0) else "no")
print("all/any", all([Sized(1), 1, "x"]), any([Sized(0), AlwaysFalse(), 0]))
print("filter None", [type(x).__name__ for x in filter(None, [Sized(0), Sized(1), AlwaysFalse(), 2])])


# subclass of builtin overriding __bool__ / __len__
class FalseInt(int):
    def __bool__(self):
        return False


class LongList(list):
    def __len__(self):
        return 5


print("FalseInt(7)", bool(FalseInt(7)), FalseInt(7) + 1)
print("LongList()", bool(LongList()), len(LongList()))


# __bool__ raising inside a condition
class Raises:
    def __bool__(self):
        raise ValueError("nope")


try:
    if Raises():
        pass
except ValueError as e:
    print("bool raised", e)

# __bool__ set on instance is ignored (lookup on type)
obj = NoProtocol()
obj.__bool__ = lambda: False
print("instance __bool__ ignored", bool(obj))
