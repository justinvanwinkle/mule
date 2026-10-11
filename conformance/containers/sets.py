"""set and frozenset operators, methods, errors, hashing, and frozenset as dict key."""

a = {1, 2, 3, 4}
b = {3, 4, 5}
print("union", sorted(a | b), sorted(a.union(b, [9])))
print("intersection", sorted(a & b), sorted(a.intersection(b, range(4))))
print("difference", sorted(a - b), sorted(a.difference(b, {1})))
print("symmetric", sorted(a ^ b), sorted(a.symmetric_difference([4, 5, 6])))
print("subset", {1, 2} <= a, {1, 2} < a, a < a, a <= a)
print("superset", a >= {1}, a > a, a.issuperset([1, 2]))
print("issubset iterable", {1}.issubset([1, 2]))
print("disjoint", a.isdisjoint({9}), a.isdisjoint([4]))
print("eq", {1, 2} == {2, 1}, {1} == frozenset({1}), {1, 2} != {1})
try:
    a | [5]
except TypeError as e:
    print("operator needs set", type(e).__name__)

s = set()
s.add(1)
s.add(1)
s.add(True)
s.add(1.0)
print("add dup", s, len(s))
s.update([2, 3], "a")
print("update", sorted(s, key=str))
s.discard(99)
s.discard("a")
print("discard", sorted(s))
s.remove(3)
try:
    s.remove(3)
except KeyError as e:
    print("remove missing", type(e).__name__)
popped = {42}.pop()
print("pop single", popped)
try:
    set().pop()
except KeyError as e:
    print("pop empty", type(e).__name__)

m = {1, 2, 3, 4, 5}
m &= {1, 2, 3, 9}
print("iand", sorted(m))
m |= {7}
print("ior", sorted(m))
m -= {1}
print("isub", sorted(m))
m ^= {2, 8}
print("ixor", sorted(m))
m.intersection_update([3, 7, 8], [3, 8])
print("intersection_update", sorted(m))
m.difference_update({3})
print("difference_update", sorted(m))
m.symmetric_difference_update({8, 10})
print("sym_update", sorted(m))
alias = m
m.clear()
print("clear aliased", alias)

fs = frozenset([3, 1, 2])
print("frozenset ops type", type(fs | {4}).__name__, type({4} | fs).__name__)
print("frozenset sorted", sorted(fs & {1, 2}))
try:
    fs.add(4)
except AttributeError as e:
    print("frozen add", type(e).__name__)
d = {fs: "fs", frozenset(): "empty"}
print("frozenset key lookup", d[frozenset({1, 2, 3})], d[frozenset()])
print("set eq frozenset key", d.get(frozenset([2, 1, 3])))
print("hash eq", hash(frozenset({1, 2})) == hash(frozenset({2, 1})))
try:
    hash({1})
except TypeError as e:
    print("set unhashable", type(e).__name__)
print("set in set via frozenset", frozenset({1}) in {frozenset({1})})
print("set lookup of set", {1} in {frozenset({1})})
try:
    {{1}}
except TypeError as e:
    print("set of set", type(e).__name__)
print("frozenset of frozensets", sorted(sorted(x) for x in {frozenset({1}), frozenset({2, 3})}))

tup = {(1, 2), (1, 2), (2, 1), (1, 2.0)}
print("set of tuples", sorted(tup))
words = {"pear", "apple", "fig"}
print("set of str sorted", sorted(words))
print("set comprehension", sorted({w[0] for w in words}))
print("set from string", sorted(set("mississippi")))
print("len bool", len(set()), bool(set()), bool({0}))
print("empty set repr", set(), frozenset())
print("single repr", {5}, frozenset({5}))
print("copy", sorted(a.copy()), a.copy() is a)
print("frozenset copy same", fs.copy() is fs)
print("min max sum", min(a), max(a), sum(a))
