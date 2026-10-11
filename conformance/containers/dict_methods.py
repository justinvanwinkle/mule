"""dict methods: get, setdefault, pop, popitem, update, fromkeys, copy, clear, and their errors."""

d = {"a": 1, "b": 2}
print("get", d.get("a"), d.get("z"), d.get("z", 0))
try:
    d["z"]
except KeyError as e:
    print("missing key", type(e).__name__, e.args)

print("setdefault existing", d.setdefault("a", 100), d)
print("setdefault new", d.setdefault("c", []), d)
d.setdefault("c", []).append(1)
d.setdefault("c", []).append(2)
print("setdefault accumulate", d["c"])
print("setdefault none", d.setdefault("n"), d)

print("pop", d.pop("n"), d)
print("pop default", d.pop("zz", "dflt"))
print("pop default None", d.pop("zz", None))
try:
    d.pop("zz")
except KeyError as e:
    print("pop missing", type(e).__name__)

p = {"x": 1, "y": 2, "z": 3}
print("popitem", p.popitem(), p)
p["w"] = 4
print("popitem lifo", p.popitem(), p)
p["x"] = 10
print("popitem after overwrite", p.popitem(), p)
p.clear()
try:
    p.popitem()
except KeyError as e:
    print("popitem empty", type(e).__name__)

u = {"a": 1}
u.update({"b": 2})
u.update([("c", 3), ("a", 0)])
u.update(d=4, e=5)
u.update({"f": 6}, g=7, a=-1)
u.update(zip("hi", range(2)))
u.update()
print("update", u)
try:
    u.update([("a", 1, 2)])
except ValueError as e:
    print("update bad pair", type(e).__name__)
try:
    u.update([1])
except TypeError as e:
    print("update non pair", type(e).__name__)


class Mapping:
    def keys(self):
        return ["k1", "k2"]

    def __getitem__(self, k):
        return k.upper()


u2 = {}
u2.update(Mapping())
print("update keys protocol", u2)
print("dict keys protocol", dict(Mapping()))

print("dict kwargs", dict(a=1, b=2))
print("dict pairs kwargs", dict([("a", 1)], a=2, b=3))
print("dict zip", dict(zip(["x", "y", "z"], [1, 2, 3])))
print("dict zip uneven", dict(zip("abc", [1, 2])))
print("dict from dict", dict({1: 2}))
print("dict pairs of strings", dict(["ab", "cd"]))

fk = dict.fromkeys(["a", "b", "c"])
print("fromkeys none", fk)
shared = dict.fromkeys("xyz", [])
shared["x"].append(1)
print("fromkeys aliasing", shared)
print("fromkeys aliasing is", shared["x"] is shared["z"])
print("fromkeys dup", dict.fromkeys([1, 1, 2], 0))

orig = {"k": [1, 2], "n": 1}
cp = orig.copy()
cp["n"] = 99
cp["k"].append(3)
print("copy shallow", orig, cp)
print("len", len(orig), "contains", "k" in orig, 1 in orig, "zz" not in orig)
del orig["n"]
print("del", orig)
try:
    del orig["n"]
except KeyError as e:
    print("del missing", type(e).__name__)
try:
    {[1]: 2}
except TypeError as e:
    print("unhashable key", type(e).__name__)
print("eq ignores order", {"a": 1, "b": 2} == {"b": 2, "a": 1})
print("reversed dict", list(reversed({"a": 1, "b": 2, "c": 3})))
