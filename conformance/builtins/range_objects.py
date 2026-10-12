"""range: len, indexing, slicing, containment, negative steps, empties, equality, and huge ranges."""

r = range(2, 20, 3)
print("repr", r, range(5), range(0, 5, 1), range(5, 0, -1))
print("list", list(r))
print("attrs", r.start, r.stop, r.step, range(4).start, range(4).step)
print("len", len(r), len(range(0)), len(range(5, 0)), len(range(0, 10, -1)), len(range(10, 0, -3)))
print("index", r[0], r[-1], r[2], r[-6])
try:
    r[7]
except IndexError as e:
    print("index out", type(e).__name__)
print("slice", r[1:4], r[::-1], r[::2], r[10:], r[-2:])
print("slice list", list(r[::-2]), list(range(10)[3:1:-1]))
print("contains", 5 in r, 6 in r, 20 in r, 2 in r, -1 in r)
print("contains float", 5.0 in r, 5.5 in r, True in range(2))
print("count index", r.count(8), r.count(9), r.index(11))
try:
    r.index(9)
except ValueError as e:
    print("index missing", type(e).__name__)
print("negative", list(range(5, -5, -3)), list(range(-1, -4, -1)))
print("empty", list(range(3, 3)), list(range(3, 1)), list(range(1, 3, -1)), bool(range(0)), bool(range(1)))
try:
    range(1, 2, 0)
except ValueError as e:
    print("zero step", type(e).__name__)
try:
    range(1.0)
except TypeError as e:
    print("float arg", type(e).__name__)

print("eq same sequence", range(0) == range(5, 5), range(3) == range(0, 4, 2) or "no", range(0, 3, 2) == range(0, 4, 2))
print("eq single element", range(5, 6, 3) == range(5, 7, 10))
print("eq not list", range(3) == [0, 1, 2])
print("hash eq", hash(range(0)) == hash(range(9, 2)), hash(range(1, 4, 2)) == hash(range(1, 5, 2)))
print("as dict key", {range(0, 4, 2): "r"}[range(0, 3, 2)])

big = range(0, 10**18, 3)
print("big len", len(big))
print("big index", big[-1], big[10**17])
print("big contains", 3 * 10**17 in big, 3 * 10**17 + 1 in big)
print("big slice", big[10**17:][:2])
huge = range(10**30)
try:
    len(huge)
except OverflowError as e:
    print("len overflow", type(e).__name__)
print("huge index", huge[-1], huge[5])
print("huge contains", 10**29 in huge)


class I:
    def __index__(self):
        return 4


print("index args", range(I()), list(range(1, I())))
it = iter(range(3))
print("iterator", type(it).__name__, next(it), list(it))
print("reversed", list(reversed(range(1, 10, 4))))
print("sum range", sum(range(1, 101)), max(range(5)), min(range(5, 0, -1)))
print("unpack", *range(3))
print("comparisons", range(3) != range(4))
try:
    range(3) < range(4)
except TypeError as e:
    print("range lt", type(e).__name__)
