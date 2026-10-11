"""dict keys/values/items views: liveness, set operations, membership, reversed, and equality."""

d = {"a": 1, "b": 2, "c": 3}
ks = d.keys()
vs = d.values()
its = d.items()
print("views", list(ks), list(vs), list(its))
d["d"] = 4
print("live after add", list(ks), len(vs), len(its))
del d["a"]
print("live after delete", list(ks))

print("keys and set", sorted(ks & {"b", "x"}))
print("keys or", sorted(ks | {"z"}))
print("keys minus", sorted(ks - {"b"}))
print("keys xor", sorted(ks ^ {"b", "q"}))
print("keys rand", sorted({"c", "w"} & ks))
print("keys vs list", sorted(ks & ["c", "d", "e"]))
print("keys disjoint", ks.isdisjoint({"x"}), ks.isdisjoint(["b"]))
print("keys eq set", ks == {"b", "c", "d"}, ks == ["b", "c", "d"])
print("keys lt", {"b"} < ks, ks <= {"b", "c", "d"})
print("other dict keys", sorted(d.keys() & {"b": 0, "c": 0}.keys()))

print("items set ops", sorted(its & {("b", 2), ("c", 99)}))
print("items minus", sorted(its - {("b", 2)}))
print("items contains", ("b", 2) in its, ("b", 3) in its, "b" in its)
print("items eq", d.items() == {("b", 2), ("c", 3), ("d", 4)})

print("values contains", 2 in vs, 99 in vs)
print("values not equal to itself", d.values() == d.values())
try:
    vs & {1}
except TypeError as e:
    print("values set op", type(e).__name__)

print("reversed keys", list(reversed(d.keys())))
print("reversed values", list(reversed(d.values())))
print("reversed items", list(reversed(d.items())))

unhash = {"a": [1]}
try:
    unhash.items() & {("a", 1)}
except TypeError as e:
    print("unhashable items set", type(e).__name__)

it = iter(d.items())
print("next item", next(it))
print("type names", type(ks).__name__, type(vs).__name__, type(its).__name__)
print("view repr", d.keys(), d.values(), d.items())
print("bool view", bool({}.keys()), bool(d.keys()))
print("len view", len(d.items()))
mapping = d.keys().mapping
print("mapping proxy", dict(mapping))

for k, v in d.items():
    print("loop", k, v)
