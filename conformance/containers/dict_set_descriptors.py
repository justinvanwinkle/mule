"""Dict, dict views, set, and frozenset descriptors, subclassing, and errors."""

d = dict([("a", 1), ("b", 2)], c=3)
print("dict_construct:", dict(), d, dict.fromkeys(("x", "y"), 7))
print("dict_len_contains:", len(d), d.__len__(), dict.__len__(d), "b" in d, dict.__contains__(d, "b"))

dict.__setitem__(d, "d", 4)
dict.__delitem__(d, "b")
print(
    "dict_descriptors:",
    dict.__getitem__(d, "a"),
    dict.get(d, "missing", -1),
    dict.setdefault(d, "e", 5),
    dict.setdefault(d, "a", 99),
    dict.pop(d, "c"),
    dict.pop(d, "missing", 0),
)
dict.update(d, [("f", 6)], g=7)
popped_pair = dict.popitem(d)
print("dict_update_popitem:", list(d.items()), popped_pair)
print("dict_merge:", {"a": 1, "b": 2} | {"b": 20, "c": 30}, list(reversed(d)))

keys_view = d.keys()
vals_view = d.values()
items_view = d.items()
d["h"] = 8
print(
    "dict_views:",
    len(keys_view),
    list(keys_view),
    list(vals_view),
    list(items_view),
    "h" in keys_view,
    8 in vals_view,
    ("h", 8) in items_view,
    keys_view.mapping["h"],
)
print(
    "view_set_ops:",
    sorted({"a": 1, "b": 2}.keys() & {"b": 3, "c": 4}.keys()),
    sorted({"a": 1, "b": 2}.keys() | {"b": 3, "c": 4}.keys()),
    sorted({"a": 1, "b": 2}.keys() ^ {"b": 3, "c": 4}.keys()),
)


class DefaultSquareDict(dict):
    def __missing__(self, key):
        val = key * key
        dict.__setitem__(self, key, val)
        return val

    def __getitem__(self, key):
        return ("got", dict.__getitem__(self, key))


dsd = DefaultSquareDict({2: 4})
print(
    "dict_subclass:",
    isinstance(dsd, dict),
    dsd[2],
    dsd[5],
    dict.__getitem__(dsd, 5),
    9 in dsd,
    dsd.get(9, "nope"),
    dict.__len__(dsd),
)

s = set([1, 2, 3, 2])
set.add(s, 4)
set.remove(s, 2)
set.discard(s, 99)
print(
    "set_descriptors:",
    sorted(s),
    3 in s,
    sorted(set.union(s, [5, 6])),
    sorted(set.intersection(s, [3, 4, 7])),
    sorted(set.difference(s, [1, 4])),
    sorted(set.symmetric_difference(s, [3, 5])),
    set.issubset({1, 3}, s),
    set.issuperset(s, {1, 3}),
    set.isdisjoint(s, {8, 9}),
)

fs = frozenset([2, 4, 6])
print(
    "frozenset_descriptors:",
    len(fs),
    4 in fs,
    sorted(frozenset.union(fs, [8])),
    sorted(frozenset.intersection(fs, [4, 6, 10])),
    fs == {2, 4, 6},
    hash(fs) == hash(frozenset([6, 4, 2])),
)


class TaggedSet(set):
    def minimum(self):
        return min(self)


ts = TaggedSet([10, 5, 20])
ts.add(2)
print("set_subclass:", isinstance(ts, set), ts.minimum(), sorted(ts))

for bad_key in (
    lambda: {}["missing"],
    lambda: {}.pop("missing"),
    lambda: {}.popitem(),
    lambda: set().remove(1),
    lambda: set().pop(),
):
    try:
        bad_key()
    except KeyError as e:
        print("key_error:", type(e).__name__)

for bad_type in (
    lambda: {[1, 2]: 3},
    lambda: {[1, 2]},
):
    try:
        bad_type()
    except TypeError as e:
        print("type_error:", type(e).__name__)
