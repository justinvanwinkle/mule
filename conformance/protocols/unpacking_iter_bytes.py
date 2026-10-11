"""__iter__ used by unpacking, star-unpacking, and builtins consuming iterables; plus __bytes__ conversions."""

log = []


class Triple:
    def __init__(self, a, b, c):
        self.vals = (a, b, c)

    def __iter__(self):
        log.append("iter")
        for v in self.vals:
            log.append(f"next {v}")
            yield v


t = Triple(1, 2, 3)
x, y, z = t
print("unpack", x, y, z, log)

log.clear()
first, *rest = t
print("star rest", first, rest, type(rest).__name__)
*init, last = t
print("star init", init, last)
a, *mid, c = Triple("a", "b", "c")
print("star middle", a, mid, c)
p, *empty, q = Triple(1, 2, 3)
print("star middle list", p, empty, q)
(m, (n, o2)) = ("M", Triple("n", "o", "x").vals[:2])
print("nested unpack", m, n, o2)

# Wrong arity errors after consuming
for target in ("two", "four"):
    log.clear()
    try:
        if target == "two":
            u, v = Triple(1, 2, 3)
        else:
            u, v, w, k = Triple(1, 2, 3)
    except ValueError as e:
        print("arity", target, type(e).__name__, log)

try:
    a1, b1, *c1, d1, e1 = Triple(1, 2, 3)
except ValueError as e:
    print("star too few", type(e).__name__)

# Star-unpacking in displays and calls
t = Triple(4, 5, 6)
print("list display", [0, *t, 7])
print("tuple display", (*t,), (*t, *Triple(7, 8, 9)))
print("set display", sorted({*t, 4}))
print("call star", max(*t), (lambda *a: a)(*t, *t))
print("dict-from-pairs", dict([Triple("k", "v", None).vals[:2]]))


class KV:
    def keys(self):
        return ["a", "b"]

    def __getitem__(self, k):
        return k.upper()


print("double-star mapping protocol", {**KV()}, dict(KV()), (lambda **kw: sorted(kw.items()))(**KV()))
try:
    {**Triple(1, 2, 3)}
except TypeError as e:
    print("double-star non-mapping", type(e).__name__)

# for-loop target unpacking
pairs = [Triple(i, i * i, -i) for i in range(3)]
print("loop unpack", [(a, b) for a, b, _ in pairs])
for i, (a, *bc) in enumerate(pairs):
    if i == 2:
        print("enumerate nested star", i, a, bc)

# Builtins consuming iterables
print("builtins", list(Triple(3, 1, 2)), sorted(Triple(3, 1, 2)), sum(Triple(1, 2, 3)), min(Triple(5, 2, 8)), tuple(Triple(1, 1, 2)))
print("zip/enumerate", list(zip(Triple(1, 2, 3), "ab")), list(enumerate(Triple("x", "y", "z"), 1)))
print("join", "-".join(Triple("a", "b", "c")), "map", list(map(str, Triple(1, 2, 3))))
print("dict.fromkeys", dict.fromkeys(Triple("a", "b", "a")))
print("str join star", " ".join([*map(str, Triple(1, 2, 3))]))

# Unpacking strings, dicts, ranges, generators
s1, s2 = "hi"
k1, k2 = {"first": 1, "second": 2}
r1, *rr = range(4)
g1, g2 = (i * 2 for i in range(2))
print("builtin unpack", s1, s2, k1, k2, r1, rr, g1, g2)

# Swapping and evaluation order of unpacking assignment
lst = [0, 1, 2]
i = 0
i, lst[i] = 2, "x"
print("assignment order", i, lst)


# __bytes__
class Packet:
    def __init__(self, *bs):
        self.bs = bs

    def __bytes__(self):
        return bytes(self.bs)


pk = Packet(72, 105)
print("bytes()", bytes(pk), bytes(pk).decode())
print("bytearray needs buffer/iterable", end=" ")
try:
    bytearray(pk)
except TypeError as e:
    print(type(e).__name__)


class BadBytes:
    def __bytes__(self):
        return "not bytes"


try:
    bytes(BadBytes())
except TypeError as e:
    print("bad __bytes__", type(e).__name__)


class IterBytes:
    def __iter__(self):
        return iter([65, 66])


print("bytes from iterable", bytes(IterBytes()), bytearray(IterBytes()))
try:
    bytes(Triple(1, 2, 300))
except ValueError as e:
    print("byte out of range", type(e).__name__)
print("bytes from int/str", bytes(2), bytes("é", "utf-8"), bytes([]))


class BothBytes:
    def __bytes__(self):
        return b"dunder"

    def __iter__(self):
        return iter([1])


print("__bytes__ beats __iter__", bytes(BothBytes()))
