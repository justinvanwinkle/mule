"""Unpacking into attributes, subscripts, slices and starred targets in one statement, plus nested and for-loop targets."""


class NS:
    def __repr__(self):
        return "NS(" + ", ".join(f"{k}={v!r}" for k, v in sorted(vars(self).items())) + ")"


obj = NS()
d = {}
lst = [0, 1, 2, 3]
obj.a, d["k"], *lst[1:2], last = range(6)
print("mixed targets", obj, d, lst, last)

# Starred target bound to a slice assignment of the middle
xs = ["a", "b", "c", "d", "e"]
first, *xs[1:4], tail = "VWXYZ"
print("starred slice target", first, xs, tail)

# Targets are assigned left to right, so later targets can depend on earlier ones
i = 0
arr = [10, 20, 30]
i, arr[i] = 2, "changed"
print("left-to-right targets", i, arr)

# Swapping through attributes and subscripts
obj.x, obj.y = 1, 2
obj.x, obj.y = obj.y, obj.x
print("swap attrs", obj.x, obj.y)
m = [1, 2, 3]
m[0], m[2] = m[2], m[0]
print("swap items", m)

# The classic gotcha: index computed after first target is assigned
a = [1, 0]
j = 0
j, a[j] = 1, 9
print("index uses new j", j, a)

# Nested unpacking with starred in inner levels
(p, *q), [r, (s, *t)] = "abc", [1, (2, 3, 4)]
print("nested", p, q, r, s, t)

# Starred empty and single
*empty, only = [42]
print("star empty", empty, only)
head, *body, foot = (1, 2)
print("star between", head, body, foot)

# For-loop targets can be attributes and subscripts
holder = NS()
store = {}
for holder.val, store["k"] in [(1, "x"), (2, "y")]:
    pass
print("for targets", holder, store)

counts = [0, 0, 0]
for counts[0] in range(3):
    pass
print("for into subscript", counts)

# With-statement targets as tuple of attribute/subscript
class CM:
    def __enter__(self):
        return ("entered", [1, 2, 3])

    def __exit__(self, *a):
        return False


tgt = {}
with CM() as (holder.label, [tgt["a"], *tgt["rest"]]):
    pass
print("with targets", holder.label, tgt)

# Comprehension targets with nesting and starred
pairs = [(a1, b1, rest) for (a1, b1), *rest in [((1, 2), 3, 4), ((5, 6),)]]
print("comprehension targets", pairs)

# Unpacking from generators, dicts, sets(sorted), strings, and dict items
g1, g2, g3 = (n * n for n in range(3))
k1, k2 = {"z": 1, "y": 2}
(ik, iv), *_ = {"one": 1, "two": 2}.items()
print("misc sources", g1, g2, g3, k1, k2, ik, iv)

# Errors: too few, too many, non-iterable; partial assignment does not happen
z1 = z2 = "orig"
for src in ([1], [1, 2, 3], 5):
    try:
        z1, z2 = src
    except (ValueError, TypeError) as e:
        print("unpack error", type(e).__name__, z1, z2)
try:
    w1, *w2, w3, w4 = [1, 2]
except ValueError as e:
    print("starred too few", type(e).__name__)

# Unpacking assigns targets as values are produced? No: iterable consumed first.
order = []


class Tracker:
    def __setattr__(self, name, value):
        order.append(("set", name, value))


def produce():
    for v in range(3):
        order.append(("yield", v))
        yield v


tr = Tracker()
tr.a, tr.b, tr.c = produce()
print("consume then assign", order)

# Starred assignment always makes a list
*star_list, = (1, 2)
print("star from tuple is list", star_list, type(star_list).__name__)
[single] = {"only"}
print("list-pattern target", single)
() = []
[] = ()
print("empty targets ok")

# Chained assignment with unpacking in multiple targets
(c1, c2) = cc = [c3, c4] = "xy"
print("chained unpack", c1, c2, cc, c3, c4)

# Unpack into a dict's slots via dict.__setitem__ while dict is the source
src = {"a": 1}
src["b"], src["c"] = src.values(), len(src)
print("source mutated after evaluation", list(src["b"]), src["c"])
