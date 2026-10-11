"""Tuple construction, immutability errors, hashing, equality, and lexicographic comparison."""

single = (1,)
not_tuple = (1)
print("single", single, type(single).__name__, len(single))
print("parens only", not_tuple, type(not_tuple).__name__)
bare = 1,
print("bare comma", bare)
empty = ()
print("empty", empty, len(empty), bool(empty), bool((0,)))
print("tuple()", tuple(), tuple("abc"), tuple([1, 2]), tuple(range(3)))
print("repr nested", ((1,), (), ((),)))

t = (1, 2, 3)
try:
    t[0] = 9
except TypeError as e:
    print("item assign", type(e).__name__)
try:
    del t[0]
except TypeError as e:
    print("item delete", type(e).__name__)
try:
    t.append(4)
except AttributeError as e:
    print("no append", type(e).__name__)

orig = t
t += (4,)
print("iadd rebinds", t, orig, t is orig)

holder = ([1], 2)
holder[0].append(5)
print("mutable inside", holder)
try:
    holder[0] += [6]
except TypeError as e:
    print("iadd element error", type(e).__name__, holder)

print("hash eq", hash((1, 2)) == hash((1, 2)))
print("hash numeric eq", hash((1, 2.0)) == hash((1.0, 2)), (1, 2.0) == (1.0, 2))
d = {(1, 2): "a"}
print("lookup by equal", d[(1.0, 2)])
try:
    hash(([1], 2))
except TypeError as e:
    print("unhashable content", type(e).__name__)

print("lt", (1, 2) < (1, 3))
print("prefix shorter less", (1, 2) < (1, 2, 0))
print("longer greater", (1, 2, 0) > (1, 2))
print("first differs", (2,) > (1, 99, 99))
print("le eq", (1, 2) <= (1, 2), (1, 2) >= (1, 2))
print("empty less", () < (0,))
print("nested", ((1, 2), 3) < ((1, 3), 0))
print("eq across", (1, 2) == [1, 2], (1, 2) != [1, 2])
try:
    (1, "a") < (1, 2)
except TypeError as e:
    print("mixed compare", type(e).__name__)
print("short circuit compare", (0, "a") < (1, 2))

print("concat", (1, 2) + (3,))
print("repeat", (1, 2) * 3, 0 * (1,), (1,) * -1)
print("index count", t.index(3), t.count(2), (1, 1, 1.0).count(1))
print("slice", t[1:3], t[::-1], t[10:])
print("in", 2 in t, 9 not in t)
print("min max", min(t), max(t), sum(t))
try:
    (1, 2) + [3]
except TypeError as e:
    print("concat list", type(e).__name__)
a, b = (1, 2)
print("unpack", a, b)
print("tuple of tuple is same", tuple(t) is t)
