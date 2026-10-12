"""in / not in across container types and custom protocols, and `is` versus `==`."""

print("list in", 2 in [1, 2, 3], 4 not in [1, 2, 3])
print("tuple in", (1, 2) in [(1, 2)], [1, 2] in [(1, 2)])
print("numeric eq in", 1.0 in [1], True in [1], 0 in [False])
print("dict in keys only", "a" in {"a": 1}, 1 in {"a": 1})
print("set in", 3 in {1, 2, 3}, 3.0 in {3})
print("string substring", "ell" in "hello", "" in "abc", "lo" not in "hello")
print("range in", 5 in range(0, 10, 5), 4 in range(0, 10, 5), 5.0 in range(10))
print("nested not deep", 1 in [[1]], [1] in [[1]])
print("generator in consumes")
gen = (n for n in range(10))
print(3 in gen, list(gen))
try:
    [1] in {1: 2}
except TypeError as e:
    print("unhashable dict probe", type(e).__name__)
try:
    1 in 5
except TypeError as e:
    print("in non container", type(e).__name__)
try:
    1 in "abc"
except TypeError as e:
    print("int in str", type(e).__name__)


class Contains:
    def __contains__(self, item):
        return item == "magic"


class IterOnly:
    def __iter__(self):
        return iter([1, 2, 3])


class GetItemOnly:
    def __getitem__(self, i):
        if i >= 3:
            raise IndexError(i)
        return i * 10


class Truthy:
    def __contains__(self, item):
        return 42


print("custom contains", "magic" in Contains(), "other" in Contains())
print("contains result coerced", 1 in Truthy())
print("iter fallback", 2 in IterOnly(), 9 not in IterOnly())
print("getitem fallback", 20 in GetItemOnly(), 25 in GetItemOnly())

a = [1, 2]
b = a
c = [1, 2]
print("is alias", a is b, a == b)
print("is copy", a is c, a == c, a is not c)
print("None is", None is None, [] != None)
e1 = ()
e2 = ()
print("tuple eq", e1 == e2)
s1 = {1}
print("set copy identity", s1 is set(s1), s1 == set(s1))
t = (1, 2)
print("tuple() identity", tuple(t) is t)
lst = [1]
print("list() new", list(lst) is lst)
d = {}
print("dict alias", d is d.copy(), d == d.copy())
print("chained", 1 in [1] in [[1]], 1 < 2 in [True])
print("not in precedence", not 1 in [2], (not 1) in [False])
