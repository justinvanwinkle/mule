"""match mapping patterns: extra keys ignored, **rest, nested, value keys, custom mappings."""

import collections


class Keys:
    NAME = "name"


def mapping(v):
    match v:
        case {"type": "point", "x": x, "y": y}:
            return f"point {x},{y}"
        case {"type": "circle", "r": r, **rest}:
            return f"circle r={r} rest={rest}"
        case {Keys.NAME: name, 1: one}:
            return f"dotted key name={name} one={one}"
        case {"nested": {"inner": [a, *b]}}:
            return f"nested a={a} b={b}"
        case {"opt": None}:
            return "opt is None"
        case {**everything} if not everything:
            return "empty mapping"
        case {"k": _}:
            return "has k"
        case _:
            return "no match " + type(v).__name__


samples = [
    {"type": "point", "x": 1, "y": 2, "extra": "ignored"},
    {"type": "circle", "r": 5, "color": "red", "fill": True},
    {"type": "circle", "r": 1},
    {"name": "bob", 1: "uno", 2: "dos"},
    {"nested": {"inner": [1, 2, 3], "other": 0}},
    {"nested": {"inner": "str"}},
    {"opt": None},
    {"opt": 0},
    {},
    {"k": "anything"},
    [("type", "point")],
    "not a mapping",
]
for s in samples:
    print("mapping", s, "->", mapping(s))


# {} pattern matches any mapping, including non-empty ones
match {"a": 1}:
    case {}:
        print("empty pattern matches non-empty dict")

# **rest is a new dict, not the original
src = {"a": 1, "b": 2, "c": 3}
match src:
    case {"a": 1, **rest}:
        rest["z"] = 26
        print("rest", sorted(rest.items()), "src unchanged", sorted(src.items()))

# mapping patterns use .get-style lookup, not iteration: defaultdict missing keys not created
dd = collections.defaultdict(list, {"present": [1]})
match dd:
    case {"absent": value}:
        print("absent matched", value)
    case {"present": [x]}:
        print("present matched", x, "keys", sorted(dd))

# OrderedDict, Counter, ChainMap are mappings
for m in (collections.OrderedDict(a=1), collections.Counter("aab"),
          collections.ChainMap({"a": 9}, {"b": 2})):
    match m:
        case {"a": a_value}:
            print("mapping type", type(m).__name__, a_value)


# custom Mapping
class Lookup(collections.abc.Mapping):
    def __init__(self, data):
        self.data = data
        self.gets = []

    def __getitem__(self, key):
        return self.data[key]

    def __iter__(self):
        return iter(self.data)

    def __len__(self):
        return len(self.data)

    def get(self, key, default=None):
        self.gets.append(key)
        return self.data.get(key, default)


lk = Lookup({"x": 1, "y": 2})
match lk:
    case {"x": x, "y": y, **rest}:
        print("custom mapping", x, y, sorted(rest.items()))

# duplicate key in a pattern is a SyntaxError detected at compile time
try:
    compile("match v:\n case {'a': 1, 'a': 2}: pass\n", "<test>", "exec")
except SyntaxError as e:
    print("duplicate key", type(e).__name__)

# keys of different types: int, float equality, None, True
match {1: "one", None: "none", True: "true-overwrites-1"}:
    case {1.0: one, None: n}:
        print("int/float/True key equality", one, n)


# mapping inside sequence inside mapping
def config(v):
    match v:
        case {"servers": [{"host": str(host), "port": int(port)}, *_]}:
            return f"first server {host}:{port}"
        case {"servers": []}:
            return "no servers"
        case _:
            return "bad config"


print("config", config({"servers": [{"host": "a", "port": 80}, {"host": "b"}]}))
print("config empty", config({"servers": []}))
print("config bad", config({"servers": [{"host": "a", "port": "80"}]}))
