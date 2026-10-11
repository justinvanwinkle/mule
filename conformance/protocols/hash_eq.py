"""__hash__/__eq__ interplay: eq without hash is unhashable, custom keys in dicts and sets, hash collisions."""


class NoHash:
    def __init__(self, v):
        self.v = v

    def __eq__(self, o):
        return isinstance(o, NoHash) and self.v == o.v


print("hash attr is None", NoHash.__hash__ is None)
for label, fn in [("hash()", lambda: hash(NoHash(1))),
                  ("set add", lambda: {NoHash(1)}),
                  ("dict key", lambda: {NoHash(1): 1}),
                  ("frozenset", lambda: frozenset([NoHash(1)]))]:
    try:
        fn()
    except TypeError as e:
        print("unhashable", label, type(e).__name__)

# Unhashable objects still work in lists and with `in` on lists (uses __eq__)
print("list membership", NoHash(2) in [NoHash(1), NoHash(2)], [NoHash(3)].index(NoHash(3)))


# Default object: hashable by identity, eq by identity
class Plain:
    pass


p = Plain()
print("default hashable", hash(p) == hash(p), p == p, Plain() == Plain(), {p: 1}[p])


class Key:
    hashes = 0
    eqs = 0

    def __init__(self, name, group):
        self.name = name
        self.group = group

    def __eq__(self, o):
        Key.eqs += 1
        return isinstance(o, Key) and self.name.lower() == o.name.lower()

    def __hash__(self):
        Key.hashes += 1
        return hash(self.name.lower())

    def __repr__(self):
        return f"Key({self.name})"


d = {Key("Alpha", 1): "first"}
d[Key("ALPHA", 2)] = "second"  # same key: value replaced, original key object kept
print("dict key kept", list(d.items()), len(d))
print("lookup case-insensitive", d[Key("alpha", 0)])
s = {Key("x", 1), Key("X", 2), Key("y", 3)}
print("set dedup", sorted(k.name for k in s))
print("hash called", Key.hashes > 0, "eq called", Key.eqs > 0)


# Constant hash: everything collides, eq decides
class Collide:
    def __init__(self, v):
        self.v = v

    def __hash__(self):
        return 42

    def __eq__(self, o):
        return isinstance(o, Collide) and self.v == o.v

    def __repr__(self):
        return f"C{self.v}"


cs = {Collide(i % 5) for i in range(20)}
print("collisions", sorted(c.v for c in cs), Collide(3) in cs, Collide(9) in cs)
cd = {Collide(i): i * i for i in range(5)}
del cd[Collide(2)]
print("collide dict", [(k.v, v) for k, v in cd.items()])


# Identity is checked before __eq__ in containers
class NeverEqual:
    def __eq__(self, o):
        return False

    def __hash__(self):
        return 1


ne = NeverEqual()
print("identity before eq", ne in [ne], ne in {ne}, ne in {ne: 1}, ne == ne, [ne] == [ne])


# Inheriting: overriding __eq__ in a subclass sets __hash__ to None again
class HashBase:
    def __hash__(self):
        return 7


class EqChild(HashBase):
    def __eq__(self, o):
        return True


class EqChildKeepHash(HashBase):
    def __eq__(self, o):
        return True

    __hash__ = HashBase.__hash__


print("subclass eq resets hash", EqChild.__hash__ is None, hash(EqChildKeepHash()))


# Numeric equality across types implies equal hashes; dict keys unify
print("numeric hash", hash(1) == hash(1.0) == hash(True), hash(0.5) == hash(0.5 + 0j))
mixed = {1: "int", 1.0: "float", True: "bool"}
print("dict unify numeric keys", mixed, type(list(mixed)[0]).__name__)
print("set unify", len({0, 0.0, False, -0.0}))
print("hash(-1)", hash(-1), hash(-2))


# Hash must be int; returning other types errors
class BadHash:
    def __hash__(self):
        return "nope"


try:
    hash(BadHash())
except TypeError as e:
    print("bad hash type", type(e).__name__)


class BigHash:
    def __hash__(self):
        return 2 ** 100


print("big hash reduced", hash(BigHash()) == hash(2 ** 100))


class BoolHash:
    def __hash__(self):
        return True


print("bool hash ok", hash(BoolHash()))

# Mutating a key after insertion makes it unfindable
class Mutable:
    def __init__(self, v):
        self.v = v

    def __hash__(self):
        return hash(self.v)

    def __eq__(self, o):
        return self.v == o.v


m = Mutable(1)
md = {m: "val"}
m.v = 2
print("mutated key lookup", Mutable(2) in md, Mutable(1) in md, m in md, len(md))

# Tuples hash by content; tuples containing unhashables are unhashable
print("tuple hash", hash((1, "a")) == hash((1, "a")))
try:
    hash((1, [2]))
except TypeError as e:
    print("tuple with list", type(e).__name__)
print("frozenset hash order-indep", hash(frozenset([1, 2, 3])) == hash(frozenset([3, 2, 1])))
