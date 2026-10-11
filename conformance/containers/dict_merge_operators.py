"""dict | and |= merging, {**d} displays, and precedence of later keys."""

a = {"x": 1, "y": 2}
b = {"y": 20, "z": 30}
print("or", a | b)
print("or reversed", b | a)
print("or does not mutate", a, b)
c = a
c |= b
print("ior mutates in place", a, c is a)
a |= [("w", 0)]
print("ior with pairs", a)
try:
    {"a": 1} | [("b", 2)]
except TypeError as e:
    print("or with list", type(e).__name__)

print("star display", {**{"a": 1}, **{"b": 2}})
print("star override", {"a": 0, **{"a": 1}, "b": 0, **{"b": 2}})
print("star then literal", {**{"a": 1}, "a": 99})
print("star empty", {**{}})
print("star dup keys allowed", {**{"k": 1}, **{"k": 2}})
print("dup literal keys", {"k": 1, "k": 2})
print("key order first insertion", {"b": 1, "a": 2, "b": 3})

try:
    {**[1, 2]}
except TypeError as e:
    print("star non mapping", type(e).__name__)


class Keys:
    def keys(self):
        return ["p", "q"]

    def __getitem__(self, k):
        return ord(k)


print("star keys protocol", {**Keys()})


def kw(**kwargs):
    return kwargs


print("call star", kw(**{"a": 1}, b=2, **{"c": 3}))
try:
    kw(**{"a": 1}, **{"a": 2})
except TypeError as e:
    print("call dup kw", type(e).__name__)

nested = {"outer": {"inner": 1}}
merged = nested | {}
merged["outer"]["inner"] = 2
print("merge shallow", nested)
