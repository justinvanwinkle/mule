"""Exceptions inside protocol hooks: __hash__ on insert, __eq__ during `in`, sort keys, __iter__ while unpacking, comprehension filters, yield from."""


class BadHash:
    def __init__(self, n):
        self.n = n

    def __hash__(self):
        if self.n == 2:
            raise ValueError("hash 2")
        return self.n

    def __eq__(self, other):
        return isinstance(other, BadHash) and self.n == other.n

    def __repr__(self):
        return f"BH{self.n}"


d = {}
for n in (1, 2, 3):
    try:
        d[BadHash(n)] = n
    except ValueError as e:
        print("hash raised for", n, e)
print("dict after", d)
try:
    {BadHash(1), BadHash(2)}
except ValueError as e:
    print("set literal hash raised", e)
try:
    BadHash(2) in d
except ValueError as e:
    print("membership hash raised", e)


class EqBomb:
    def __init__(self, n):
        self.n = n

    def __eq__(self, other):
        print("  eq called", self.n)
        if self.n == 3:
            raise RuntimeError("eq bomb")
        return False

    __hash__ = None


items = [EqBomb(1), EqBomb(2), EqBomb(3), EqBomb(4)]
try:
    "x" in items
except RuntimeError as e:
    print("in stopped at", e)
sentinel = items[2]
print("identity short-circuits eq", sentinel in items)
try:
    items.index("z")
except RuntimeError as e:
    print("index raised", e)
try:
    items.count("z")
except RuntimeError as e:
    print("count raised", e)


def key(v):
    if v == 5:
        raise KeyError("key 5")
    return -v


data = [3, 1, 5, 2, 4]
try:
    data.sort(key=key)
except KeyError as e:
    print("sort key raised", e.args[0])
print("list unchanged after failed key sort", data)


class Cmp:
    def __init__(self, v):
        self.v = v

    def __lt__(self, other):
        if {self.v, other.v} == {0, 9}:
            raise ArithmeticError("cmp")
        return self.v < other.v

    def __repr__(self):
        return f"C{self.v}"


cs = [Cmp(v) for v in (5, 9, 1, 0, 3)]
try:
    cs.sort()
except ArithmeticError as e:
    print("sort lt raised", e)
print("same elements preserved", sorted(c.v for c in cs), len(cs))


class Iter:
    def __init__(self, n, fail_at):
        self.n, self.fail_at = n, fail_at

    def __iter__(self):
        for i in range(self.n):
            if i == self.fail_at:
                raise LookupError(f"iter at {i}")
            yield i


a = b = c = "unset"
try:
    a, b, c = Iter(3, 2)
except LookupError as e:
    print("unpack raised", e, (a, b, c))
try:
    a, b = Iter(3, 99)
except ValueError as e:
    print("too many values", type(e).__name__, (a, b))
try:
    first, *rest = Iter(5, 4)
except LookupError as e:
    print("star unpack raised", e)


def cond(x):
    if x == 3:
        raise IndexError("cond 3")
    return x % 2


seen = []
try:
    out = [seen.append(x) or x for x in range(6) if cond(x)]
except IndexError as e:
    print("comprehension filter raised", e, "seen", seen)
try:
    gen = {x: 1 / (x - 2) for x in range(4)}
except ZeroDivisionError as e:
    print("dict comprehension raised", type(e).__name__)


def inner():
    yield 1
    raise OSError("inner fail")


def outer():
    try:
        yield from inner()
    except OSError as e:
        yield f"caught {e}"
    yield "after"


print("yield from propagation", list(outer()))


def inner2():
    try:
        yield "a"
        yield "b"
    finally:
        print("  inner2 finally")


def outer2():
    yield from inner2()


g = outer2()
print("first", next(g))
try:
    g.throw(TimeoutError("thrown in"))
except TimeoutError as e:
    print("throw through yield from", e)


class BadLen:
    def __len__(self):
        raise NotImplementedError("len")

    def __iter__(self):
        return iter([1, 2])


print("list() with raising __len__: ", end="")
try:
    print(list(BadLen()))
except NotImplementedError as e:
    print("raised", e)

class NegLen:
    def __len__(self):
        return -1


try:
    len(NegLen())
except ValueError as e:
    print("negative len", type(e).__name__)
