"""Augmented assignment into tuple items: TypeError raised after the in-place mutation already happened."""

t = ([],)
try:
    t[0] += [1]
except TypeError as e:
    print("list in tuple raised", type(e).__name__)
print("list mutated anyway", t)

t2 = ([1], [2])
try:
    t2[1] += (3, 4)
except TypeError as e:
    print("tuple rhs raised", type(e).__name__)
print("extended with tuple", t2)


class Acc:
    def __init__(self, items):
        self.items = list(items)
        self.log = []

    def __iadd__(self, other):
        self.log.append(("iadd", other))
        self.items.append(other)
        return self

    def __repr__(self):
        return f"Acc({self.items})"


a = Acc([0])
t3 = (a, "x")
try:
    t3[0] += 9
except TypeError as e:
    print("custom iadd raised", type(e).__name__)
print("custom mutated", t3[0], a.log)


class ReturnsOther:
    def __init__(self, n):
        self.n = n

    def __iadd__(self, other):
        return ReturnsOther(self.n + other)

    def __repr__(self):
        return f"RO({self.n})"


r = ReturnsOther(1)
t4 = (r,)
try:
    t4[0] += 5
except TypeError as e:
    print("new object raised", type(e).__name__)
print("original unchanged", t4, r)

# Strings in a tuple: no mutation possible, the item stays the same.
t5 = ("ab",)
try:
    t5[0] += "c"
except TypeError as e:
    print("str in tuple raised", type(e).__name__)
print("str unchanged", t5)

# Nested: a list inside a tuple inside a list -- the outer list item can be reassigned.
outer = [([1],)]
outer[0][0].extend([2])
try:
    outer[0][0] += [3]
except TypeError as e:
    print("nested raised", type(e).__name__)
print("nested state", outer)
outer[0] += ([4],)
print("outer list item rebound", outer)

# frozen attribute via __setattr__ refusing after __iadd__ ran
class Frozen:
    def __init__(self):
        object.__setattr__(self, "data", [])

    def __setattr__(self, name, value):
        raise AttributeError(f"frozen: {name}")


f = Frozen()
try:
    f.data += [7, 8]
except AttributeError as e:
    print("frozen attr raised", e)
print("frozen data mutated", f.data)

# dict whose __setitem__ refuses
class NoSet(dict):
    def __setitem__(self, k, v):
        raise KeyError(f"refused {k}")


d = NoSet()
dict.__setitem__(d, "k", [1])
try:
    d["k"] += [2]
except KeyError as e:
    print("dict setitem refused", e.args[0])
print("dict value mutated", dict(d))
