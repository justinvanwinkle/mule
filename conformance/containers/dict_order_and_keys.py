"""dict insertion order, key equality across 1/True/1.0, overwrites, and mutation during iteration."""

d = {}
d["z"] = 1
d["a"] = 2
d["m"] = 3
print("insertion order", list(d))
d["z"] = 100
print("overwrite keeps position", list(d.items()))
del d["z"]
d["z"] = 5
print("reinsert moves to end", list(d.items()))

k = {1: "int"}
k[True] = "bool"
k[1.0] = "float"
print("1/True/1.0 same key", k, len(k))
print("first key object kept", type(next(iter(k))).__name__)
k2 = {True: "a", 1: "b", 1.0: "c"}
print("display same", k2)
k3 = {0: "z", False: "f", 0.0: "g", -0.0: "h"}
print("zero keys", k3)
print("complex key", {1 + 0j: "c", 1: "i"})
print("lookup cross type", {2: "two"}[2.0], {True: "t"}[1])
print("tuple key equality", {(1, 2): "a", (1.0, 2.0): "b"})

nan = float("nan")
nd = {nan: 1}
nd[nan] = 2
print("same nan identity", len(nd))
nd[float("nan")] = 3
print("distinct nans", len(nd))

it = {"a": 1, "b": 2}
try:
    for key in it:
        it["c"] = 3
except RuntimeError as e:
    print("add during iter", type(e).__name__)

it2 = {"a": 1, "b": 2}
try:
    for key in it2:
        del it2[key]
except RuntimeError as e:
    print("delete during iter", type(e).__name__)

it3 = {"a": 1, "b": 2}
for key in it3:
    it3[key] = it3[key] * 10
print("value change ok", it3)

it4 = {"a": 1, "b": 2, "c": 3}
for key in list(it4):
    if key != "b":
        del it4[key]
print("iterate over copy", it4)

it5 = {"a": 1}
try:
    for v in it5.values():
        it5.pop("a")
        it5["b"] = 2
except RuntimeError as e:
    print("values view mutation", type(e).__name__)

order = []


class K:
    def __init__(self, name):
        self.name = name

    def __hash__(self):
        order.append("hash " + self.name)
        return 7

    def __eq__(self, other):
        order.append(f"eq {self.name} {other.name}")
        return self.name.lower() == other.name.lower()

    def __repr__(self):
        return f"K({self.name})"


cd = {K("a"): 1}
cd[K("A")] = 2
print("custom eq collapse", cd)
cd[K("b")] = 3
print("collision distinct", cd)
print("custom calls", order)

display_order = []


def note(x):
    display_order.append(x)
    return x


{note("k1"): note("v1"), note("k2"): note("v2")}
print("display eval order", display_order)
