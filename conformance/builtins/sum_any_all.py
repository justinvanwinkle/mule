"""sum with start and floats, any/all short-circuiting over generators with side effects."""

print("sum", sum([1, 2, 3]), sum([]), sum(range(101)))
print("sum start", sum([1, 2], 10), sum([1, 2], start=100))
print("sum floats", sum([0.1] * 10), sum([0.1, 0.2, 0.3]))
print("sum mixed", sum([1, 2.5, True]), type(sum([1, True])).__name__)
print("sum lists", sum([[1], [2, 3]], []))
print("sum tuples", sum([(1,), (2,)], ()))
print("sum complex", sum([1j, 2, 3.5]))
print("sum bools", sum([True, True, False]))
print("sum big", sum([10**20, 1, -10**20]))
print("sum precision", sum([1e100, 1.0, -1e100]), sum([1.0, 1e100, 1.0, -1e100]))
print("sum inf", sum([float("inf"), 1.0]), sum([float("inf"), float("-inf")]))
print("sum negative zero", sum([-0.0]), sum([-0.0, -0.0], -0.0))
try:
    sum(["a", "b"], "")
except TypeError as e:
    print("sum strings", type(e).__name__)
try:
    sum([b"a"], b"")
except TypeError as e:
    print("sum bytes", type(e).__name__)
try:
    sum([1, "a"])
except TypeError as e:
    print("sum mixed bad", type(e).__name__)


class Money:
    def __init__(self, cents):
        self.cents = cents

    def __add__(self, other):
        return Money(self.cents + (other.cents if isinstance(other, Money) else other))

    __radd__ = __add__

    def __repr__(self):
        return f"Money({self.cents})"


print("sum custom radd", sum([Money(5), Money(7)]))
print("sum custom start", sum([Money(1)], Money(100)))

print("any", any([]), any([0, ""]), any([0, 1]))
print("all", all([]), all([1, "a"]), all([1, 0]))
print("returns bool", any([0, "x"]), all(["x", 5]))

seen = []


def gen(values):
    for v in values:
        seen.append(v)
        yield v


print("any short", any(gen([0, 3, 0, 9])), seen)
seen.clear()
print("all short", all(gen([1, 0, 1])), seen)
seen.clear()
g = gen([0, 5, 6, 7])
print("any leaves rest", any(g), list(g))
seen.clear()
print("all exhausts when true", all(gen([1, 2])), seen)


class Boom:
    def __bool__(self):
        raise ValueError("boom")


try:
    any([0, Boom()])
except ValueError as e:
    print("bool error propagates", e)
print("short circuit avoids boom", any([1, Boom()]), all([0, Boom()]))
print("any of comparisons", any(x > 2 for x in [1, 2, 3]), all(x > 0 for x in [1, -1]))
print("nested", all(any(row) for row in [[0, 1], [1]]), all(any(row) for row in [[0], [1]]))
