"""Copy semantics: list(x), x[:], .copy(), copy.copy, copy.deepcopy with nesting, sharing, and cycles."""

import copy

orig = [[1, 2], {"k": [3]}, (4, [5])]
views = {
    "list()": list(orig),
    "slice": orig[:],
    "method": orig.copy(),
    "copy.copy": copy.copy(orig),
    "deepcopy": copy.deepcopy(orig),
}
orig[0].append("X")
orig[1]["k"].append("Y")
orig[2][1].append("Z")
orig.append("top")
for name, value in views.items():
    print(name, value)

shared = [0]
pair = [shared, shared]
dc = copy.deepcopy(pair)
print("deepcopy preserves sharing", dc[0] is dc[1], dc[0] is shared)

cyc = [1]
cyc.append(cyc)
dcyc = copy.deepcopy(cyc)
print("deepcopy cycle", dcyc[1] is dcyc, dcyc is not cyc, dcyc)

t = (1, 2, (3,))
print("copy immutable tuple same", copy.copy(t) is t, copy.deepcopy(t) is t)
tm = (1, [2])
dtm = copy.deepcopy(tm)
print("deepcopy tuple with list new", dtm is tm, dtm == tm, dtm[1] is tm[1])
print("frozenset copy same", copy.copy(frozenset({1})) == frozenset({1}))

d = {"a": {"b": 1}}
dc1 = copy.copy(d)
dc2 = copy.deepcopy(d)
d["a"]["b"] = 2
print("dict copy", dc1, dc2)
print("dict(d) shallow", dict(d)["a"] is d["a"])

s = {(1, 2)}
print("set copy", copy.copy(s) == s, copy.copy(s) is s)


class Point:
    def __init__(self, x, tags):
        self.x = x
        self.tags = tags

    def __repr__(self):
        return f"Point({self.x}, {self.tags})"


p = Point(1, ["a"])
pc = copy.copy(p)
pd = copy.deepcopy(p)
p.tags.append("b")
p.x = 9
print("object copy", pc, pd, type(pc).__name__)


class Custom:
    def __init__(self, v):
        self.v = v

    def __copy__(self):
        return Custom(("shallow", self.v))

    def __deepcopy__(self, memo):
        return Custom(("deep", copy.deepcopy(self.v, memo)))

    def __repr__(self):
        return f"Custom({self.v!r})"


print("custom hooks", copy.copy(Custom(1)), copy.deepcopy(Custom([1])))
print("list of custom deepcopy", copy.deepcopy([Custom(2)]))
print("replace on list absent", hasattr(copy, "replace"))

a = [1, 2, 3]
b = a
c = a[:]
a[0] = 100
print("alias vs slice", b, c)
a[:] = [7]
print("slice assign keeps alias", b, a is b)
