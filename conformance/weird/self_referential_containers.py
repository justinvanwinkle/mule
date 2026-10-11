"""Self-referential lists, dicts and objects: chained assignment order, recursive reprs, and equality."""

# Chained assignment assigns left to right: x is bound first, then x[0] = that same list.
x = x[0] = [None]
print("simple", x, x[0] is x)

y = y_alias = [None, None]
y = y[1] = [7, 8]
print("rebinding before subscript", y, y[1] is y, y_alias)

a = []
a.append(a)
a.append([a, 1])
print("nested list", a)

d = {}
d["self"] = d
d["list"] = [d, {"inner": d}]
print("dict self", d)

t = ([],)
t[0].append(t)
print("tuple via list", t)

s = [1, 2]
s.append({"k": s, "t": (s,)})
print("mixed", s)


class Node:
    def __init__(self, name):
        self.name = name
        self.children = []

    def __repr__(self):
        return f"Node({self.name}, {self.children})"


n = Node("root")
n.children.append(n)
try:
    print("custom repr cut by inner list", repr(n))
except RecursionError as e:
    print("custom repr recursion", type(e).__name__)

# reprlib.recursive_repr handles it
import reprlib


class Safe:
    def __init__(self):
        self.items = []

    @reprlib.recursive_repr("<loop>")
    def __repr__(self):
        return f"Safe({self.items})"


s2 = Safe()
s2.items.append(s2)
s2.items.append([s2])
print("recursive_repr", s2)

# Equality on self-referential structures: identity shortcut makes this True.
p = []
p.append(p)
print("self eq identity", p == p)
q = []
q.append(q)
try:
    print(p == q)
except RecursionError as e:
    print("distinct self eq", type(e).__name__)

# str() of nested dict with list referencing outer
outer = {"a": []}
outer["a"].append(outer)
outer["a"].append(outer["a"])
print("str outer", str(outer))

# Deep but finite nesting repr
deep = []
cur = deep
for i in range(30):
    nxt = [i]
    cur.append(nxt)
    cur = nxt
print("deep", deep)

# OrderedDict and defaultdict self references
from collections import OrderedDict, defaultdict, deque

od = OrderedDict()
od["me"] = od
print("ordereddict", od)
dd = defaultdict(list)
dd["me"].append(dd)
print("defaultdict", dd)
dq = deque([1])
dq.append(dq)
print("deque", dq)

# Set can't hold a list, but a frozenset inside a list in a dict can reference back
holder = {"fs": frozenset({1}), "back": None}
holder["back"] = [holder]
print("holder", holder)
print("len of self-containing", len(x), len(d), len(a))
