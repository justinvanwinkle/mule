"""slice() builtin, Ellipsis/..., and NotImplemented in binary-operator dispatch."""

s = slice(5)
print("one arg", s, s.start, s.stop, s.step)
print("two args", slice(1, 4), "three", slice(1, 9, 2))
print("any objects", slice("a", [1], None))
data = list(range(10))
print("apply", data[slice(2, 8, 3)], data[slice(None, None, -4)], "hello"[slice(1, None)])
print("indices", slice(None, None, None).indices(3), slice(-2, None).indices(5), slice(0, 100, 7).indices(20))
print("indices negative step", slice(None, None, -1).indices(0), slice(10, -10, -3).indices(6))
try:
    slice(1, 2).indices(-1)
except ValueError as e:
    print("indices negative length", type(e).__name__)
print("eq", slice(1, 2) == slice(1, 2, None), slice(0) == slice(None, 0))
print("ordering", slice(1, 2) < slice(1, 3))
print("hashable", hash(slice(1, 2)) == hash(slice(1, 2)), {slice(1, 2): "s"}[slice(1, 2)])
try:
    slice(1).start = 2
except AttributeError as e:
    print("readonly", type(e).__name__)
try:
    slice()
except TypeError as e:
    print("no args", type(e).__name__)

print("ellipsis", ..., Ellipsis, ... is Ellipsis, type(...).__name__)
print("ellipsis truthy", bool(...), str(...), repr(Ellipsis))
print("ellipsis eq", ... == ..., ... == None)


def todo():
    ...


print("body ellipsis returns", todo())
print("ellipsis in container", [..., (..., ...)], {...: 1}[...])


class Grid:
    def __getitem__(self, key):
        return key


print("getitem ellipsis", Grid()[...], Grid()[1, ...], Grid()[..., ::2])

print("notimplemented", NotImplemented, type(NotImplemented).__name__)


class Meters:
    def __init__(self, v):
        self.v = v

    def __add__(self, other):
        if isinstance(other, Meters):
            return Meters(self.v + other.v)
        return NotImplemented

    def __radd__(self, other):
        if isinstance(other, int):
            return Meters(self.v + other)
        return NotImplemented

    def __eq__(self, other):
        if isinstance(other, Meters):
            return self.v == other.v
        return NotImplemented

    def __repr__(self):
        return f"Meters({self.v})"


print("add same", Meters(1) + Meters(2))
print("radd int", 3 + Meters(4))
print("forward returned NotImplemented then reflected", Meters(1).__add__(5) is NotImplemented)
try:
    Meters(1) + "x"
except TypeError as e:
    print("both NotImplemented", type(e).__name__)
print("eq fallback identity", Meters(1) == "x", Meters(1) != "x", Meters(2) == Meters(2))
m = Meters(1)
print("eq NotImplemented both sides identity", m == m)


class Left:
    def __lt__(self, other):
        return NotImplemented


class Right:
    def __gt__(self, other):
        return "Right.__gt__ called"


print("reflected comparison", Left() < Right())
try:
    Left() < Left()
except TypeError as e:
    print("comparison unsupported", type(e).__name__)
print("sum with NotImplemented radd", sum([Meters(1), Meters(2)], Meters(0)))
