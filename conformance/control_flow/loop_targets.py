"""for-loop targets that are attributes, subscripts, slices, and starred/nested tuples."""


class Box:
    def __repr__(self):
        return f"Box({vars(self)})"


class Tracing:
    def __init__(self):
        self.log = []

    def __setattr__(self, name, value):
        if name != "log":
            self.log.append((name, value))
        object.__setattr__(self, name, value)


box = Box()
for box.x in range(3):
    print("attr target", box.x)
print("after loop", box)

d = {}
for d["k"] in "abc":
    print("subscript target", d)
print("dict after", d)

lst = [0, 0, 0]
for i, lst[i] in enumerate("xyz"):
    pass
print("enumerate into list slot", lst)

# subscript index evaluated each iteration
idx = [0]
out = [None] * 3
for out[idx[0]] in "pqr":
    idx[0] += 1
print("moving subscript", out)

t = Tracing()
for t.value in (10, 20):
    pass
print("tracing setattr", t.log)

# nested tuple and starred targets
for a, (b, *c) in [(1, (2, 3, 4)), (5, (6,)), (7, "89x")]:
    print("nested star", a, b, c)

for *head, tail in ["abc", [1, 2], (9,)]:
    print("star head", head, tail)

for first, *middle, last in [range(5), "xy"]:
    print("star middle", first, middle, last)

for [p, q] in [(1, 2), [3, 4], "ab"]:
    print("list target", p, q)

for (x, y), z in zip([(1, 2), (3, 4)], "ab"):
    print("zip nested", x, y, z)

# mixing attribute and subscript inside a tuple target
holder = Box()
store = {}
for holder.name, store["v"] in [("a", 1), ("b", 2)]:
    print("mixed", holder.name, store)

# slice target
buf = [0, 0, 0, 0]
for buf[1:3] in (["a", "b"], ["c"]):
    print("slice target", buf)

# unpacking errors
for bad in ([(1, 2, 3)], [(1,)], [5]):
    try:
        for m, n in bad:
            print("never", m, n)
    except (ValueError, TypeError) as e:
        print("unpack error", type(e).__name__)

# target is reused variable name from outer scope
value = "outer"
for value in []:
    pass
print("empty loop leaves name", value)

# loop target is an existing loop variable of enclosing loop
for i in range(2):
    for i in range(10, 12):
        pass
    print("inner rebinding i", i)

# dict iteration with item unpacking and starred
for k, (v1, *vrest) in {"a": [1, 2, 3], "b": [4]}.items():
    print("dict items unpack", k, v1, vrest)

# assigning to a property through for loop
class Prop:
    def __init__(self):
        self.seen = []

    @property
    def p(self):
        return self.seen[-1]

    @p.setter
    def p(self, v):
        self.seen.append(v * 10)


pr = Prop()
for pr.p in range(3):
    print("property current", pr.p)
print("property seen", pr.seen)
