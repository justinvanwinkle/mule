"""__slots__: restricted attributes, inheritance with and without slots, __dict__ and __weakref__ absence."""


class Pt:
    __slots__ = ("x", "y")

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __repr__(self):
        return f"Pt({self.x}, {self.y})"


p = Pt(1, 2)
print("slots values", p, p.x, p.y)
p.x = 10
print("slot reassigned", p.x)

try:
    p.z = 3
except AttributeError as e:
    print("unknown attr", type(e).__name__)

print("has __dict__", hasattr(p, "__dict__"))
print("has __weakref__", hasattr(p, "__weakref__"))
print("slots attr", Pt.__slots__)
print("slot descriptor type", type(Pt.__dict__["x"]).__name__)

del p.y
try:
    p.y
except AttributeError as e:
    print("deleted slot read", type(e).__name__)
try:
    del p.y
except AttributeError as e:
    print("deleted slot del", type(e).__name__)
p.y = 5
print("refilled", p)

# Unset slot
q = Pt.__new__(Pt)
try:
    q.x
except AttributeError as e:
    print("unset slot", type(e).__name__)


# Subclass without __slots__ gets a __dict__ again.
class Loose(Pt):
    pass


lo = Loose(1, 2)
lo.extra = "ok"
print("subclass without slots", lo.extra, hasattr(lo, "__dict__"), sorted(vars(lo)))


# Subclass with slots extends.
class Pt3(Pt):
    __slots__ = ("z",)

    def __init__(self, x, y, z):
        super().__init__(x, y)
        self.z = z


p3 = Pt3(1, 2, 3)
print("subclass slots", p3.x, p3.y, p3.z, hasattr(p3, "__dict__"))
try:
    p3.w = 1
except AttributeError as e:
    print("subclass unknown", type(e).__name__)


# Slots with __dict__ explicitly included.
class Hybrid:
    __slots__ = ("a", "__dict__")


h = Hybrid()
h.a = 1
h.b = 2
print("hybrid", h.a, h.b, vars(h))

# Single string slot
class One:
    __slots__ = "only"


o = One()
o.only = "v"
print("string slot", o.only, One.__slots__)

# Slot conflicting with class variable is an error.
try:
    class Conflict:
        __slots__ = ("v",)
        v = 1
except ValueError as e:
    print("slot/classvar conflict", type(e).__name__)

# Empty slots: no attributes at all.
class Empty:
    __slots__ = ()


try:
    Empty().anything = 1
except AttributeError as e:
    print("empty slots", type(e).__name__)


# Slots with class-level default via property-like default on base
class WithDefault:
    __slots__ = ("val",)
    kind = "shared"  # class attribute not in slots is fine (read-only on instances)


wd = WithDefault()
print("class attr readable", wd.kind)
try:
    wd.kind = "x"
except AttributeError as e:
    print("class attr not settable", type(e).__name__)


# Slots and multiple inheritance layout conflict
class SA:
    __slots__ = ("a",)


class SB:
    __slots__ = ("b",)


try:
    class SAB(SA, SB):
        pass
except TypeError as e:
    print("layout conflict", type(e).__name__)


class EA:
    __slots__ = ()


class EB:
    __slots__ = ()


class EAB(EA, EB):
    __slots__ = ("c",)


print("empty slots MI ok", EAB.__mro__[1].__name__)

# __slots__ given as dict (3.x allows mapping; values become docstrings)
class Documented:
    __slots__ = {"a": "first attribute", "b": "second"}


print("dict slots doc", Documented.a.__doc__, Documented.b.__doc__)

# Slotted objects in a list with getattr
items = [Pt(i, i * i) for i in range(3)]
print("getattr loop", [getattr(it, "y") for it in items])
