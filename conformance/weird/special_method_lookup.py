"""Implicit special-method lookup goes to the type, bypassing instance attributes and __getattr__; explicit calls do not."""


class Dynamic:
    """Synthesizes methods for any attribute name via __getattr__."""

    def __getattr__(self, name):
        if name.startswith("__") and name.endswith("__") and name not in ("__enter__", "__exit__", "__len__", "__iter__", "__str__", "__add__", "__bool__"):
            raise AttributeError(name)

        def synthesized(*args):
            return f"synth {name}{args}"
        return synthesized


d = Dynamic()
print("explicit attribute", d.anything(1, 2))
print("explicit dunder via __getattr__", d.__len__())
try:
    with d:
        pass
except TypeError as e:
    print("with via __getattr__", type(e).__name__)
for label, fn in [
    ("len", lambda: len(d)),
    ("iter", lambda: iter(d)),
    ("add", lambda: d + 1),
]:
    try:
        print(label, fn())
    except TypeError as e:
        print(label, "raised", type(e).__name__)
print("str falls back to object.__str__ not __getattr__", str(d).startswith("<"))
print("bool falls back to default truth", bool(d))


class Plain:
    def __repr__(self):
        return "Plain()"


p = Plain()
p.__enter__ = lambda: "instance enter"
p.__exit__ = lambda *a: True
p.__len__ = lambda: 99
p.__iter__ = lambda: iter([1, 2])
p.__str__ = lambda: "instance str"
p.__add__ = lambda other: "instance add"
p.__call__ = lambda: "instance call"
p.__getitem__ = lambda k: "instance getitem"
p.__eq__ = lambda other: True
p.__hash__ = lambda: 1

try:
    with p:
        pass
except TypeError as e:
    print("with instance __enter__", type(e).__name__)
for label, fn in [
    ("len", lambda: len(p)),
    ("iter", lambda: list(p)),
    ("add", lambda: p + 1),
    ("call", lambda: p()),
    ("getitem", lambda: p[0]),
]:
    try:
        print(label, fn())
    except TypeError as e:
        print(label, "raised", type(e).__name__)
print("str ignores instance", str(p))
print("eq ignores instance", p == Plain(), p == p)
print("explicit calls use instance", p.__len__(), p.__str__(), p.__add__(0), p.__call__())

# Assigning to the class does take effect immediately, for existing instances too
Plain.__len__ = lambda self: 7
Plain.__str__ = lambda self: "class str"
Plain.__enter__ = lambda self: "class enter"
Plain.__exit__ = lambda self, *a: False
print("class-level len", len(p))
print("class-level str", str(p))
with p as entered:
    print("class-level with", entered)
del Plain.__len__
try:
    len(p)
except TypeError as e:
    print("deleted class len", type(e).__name__)


# __getattribute__ is also bypassed for implicit lookups
class Loud:
    def __getattribute__(self, name):
        print("  getattribute", name)
        return object.__getattribute__(self, name)

    def __len__(self):
        return 3

    def size(self):
        return 4


lo = Loud()
print("len bypasses getattribute", len(lo))
print("explicit goes through it", lo.size())


# Metaclass provides special methods for the class object itself
class Meta(type):
    def __len__(cls):
        return 42

    def __getitem__(cls, k):
        return f"{cls.__name__}[{k}]"

    def __iter__(cls):
        return iter("ab")


class WithMeta(metaclass=Meta):
    def __len__(self):
        return 1


print("len(class) uses metaclass", len(WithMeta), "len(instance) uses class", len(WithMeta()))
print("class getitem via metaclass", WithMeta[int], list(WithMeta))


# __class_getitem__ vs metaclass __getitem__
class Generic:
    def __class_getitem__(cls, item):
        return f"cgi {item}"


print("__class_getitem__", Generic[str], Generic[1, 2])


# Special methods are bound through the descriptor protocol: a staticmethod __len__ takes no self
class StaticLen:
    @staticmethod
    def __len__():
        return 5


try:
    print("staticmethod len", len(StaticLen()))
except TypeError as e:
    print("staticmethod len", type(e).__name__)


class CallableLen:
    class _L:
        def __call__(self, *a):
            return 6
    __len__ = _L()


print("non-function callable as __len__ gets no self binding", len(CallableLen()))


# __hash__ = None on class makes instances unhashable even if instance sets __hash__
class NoHash:
    __hash__ = None


nh = NoHash()
nh.__hash__ = lambda: 1
try:
    hash(nh)
except TypeError as e:
    print("unhashable despite instance __hash__", type(e).__name__)
