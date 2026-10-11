"""hash() equality relations across numeric types and containers, custom __hash__, and identity via `is`."""

print("int float bool", hash(1) == hash(1.0) == hash(True), hash(0) == hash(0.0) == hash(False) == hash(-0.0))
print("complex", hash(2) == hash(2 + 0j), hash(1.5) == hash(1.5 + 0j))
print("small int value", hash(5), hash(-1), hash(-2), hash(0))
print("float integral", hash(3.0) == 3, hash(10**20) == hash(float(10**20)))
print("inf", hash(float("inf")), hash(float("-inf")))
nan = float("nan")
print("nan self", hash(nan) == hash(nan))
print("str eq", hash("abc") == hash("ab" + "c"), hash("") == hash(str()))
print("bytes eq", hash(b"x") == hash(bytes([120])))
print("tuple eq", hash((1, "a")) == hash((1.0, "a")))
print("frozenset order", hash(frozenset([1, 2, 3])) == hash(frozenset([3, 2, 1])))
print("range", hash(range(0)) == hash(range(5, 5)))
print("None hashable", hash(None) == hash(None))
for value in ([], {}, set(), bytearray()):
    try:
        hash(value)
    except TypeError as e:
        print("unhashable", type(value).__name__, type(e).__name__)


class Point:
    def __init__(self, x, y):
        self.x, self.y = x, y

    def __eq__(self, other):
        return isinstance(other, Point) and (self.x, self.y) == (other.x, other.y)

    def __hash__(self):
        return hash((self.x, self.y))

    def __repr__(self):
        return f"Point({self.x}, {self.y})"


print("custom eq hash", hash(Point(1, 2)) == hash(Point(1, 2)), len({Point(1, 2), Point(1, 2), Point(2, 1)}))


class EqOnly:
    def __eq__(self, other):
        return True


try:
    hash(EqOnly())
except TypeError as e:
    print("eq without hash", type(e).__name__)
print("eq only hash attr", EqOnly.__hash__)


class BigHash:
    def __hash__(self):
        return 2**100


print("big hash reduced", type(hash(BigHash())).__name__, hash(BigHash()) == hash(2**100))


class MinusOne:
    def __hash__(self):
        return -1


print("minus one becomes", hash(MinusOne()))


class BadHash:
    def __hash__(self):
        return "x"


try:
    hash(BadHash())
except TypeError as e:
    print("non int hash", type(e).__name__)


class Plain:
    pass


p, q = Plain(), Plain()
print("default hash stable", hash(p) == hash(p), p == p, p != q)

a = [1]
b = a
c = [1]
print("is", a is b, a is c, a is not c, a == c)
print("None singleton", None is type(None)(), ... is Ellipsis, NotImplemented is type(NotImplemented)())
print("bool singletons", (1 == 1) is True, bool(0) is False)
x = object()
y = x
print("object identity", x is y, x is not object())
print("method objects equal not identical", p.__init__ == p.__init__)
print("class identity", type(p) is Plain, Plain is Plain)


def f():
    return []


print("fresh lists", f() is not f())
