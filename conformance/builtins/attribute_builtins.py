"""callable, getattr/setattr/hasattr/delattr with defaults and errors, vars(), and filtered dir()."""


class Thing:
    kind = "class attr"

    def __init__(self):
        self.a = 1
        self.b = [2]

    def method(self):
        return "method result"

    @property
    def prop(self):
        return "prop value"


t = Thing()
print("getattr", t.a, t.kind, t.prop, t.method())
print("getattr default", getattr(t, "missing", "dflt"), getattr(t, "missing", None))
try:
    t.missing
except AttributeError as e:
    print("getattr missing", type(e).__name__)
try:
    getattr(t, 5)
except TypeError as e:
    print("getattr non str", type(e).__name__)
print("hasattr", hasattr(t, "a"), hasattr(t, "zzz"), hasattr(t, "method"), hasattr(Thing, "a"))

t.c = 3
setattr(t, "with space", "odd but legal")
print("setattr", t.c, getattr(t, "with space"))
Thing.shared = "on class"
print("setattr class", t.shared, Thing().shared)
delattr(t, "c")
print("delattr", hasattr(t, "c"))
try:
    delattr(t, "c")
except AttributeError as e:
    print("delattr missing", type(e).__name__)
try:
    delattr(t, "kind")
except AttributeError as e:
    print("delattr class attr via instance", type(e).__name__)
try:
    t.prop = 1
except AttributeError as e:
    print("setattr readonly property", type(e).__name__)
try:
    (5).x = 1
except AttributeError as e:
    print("setattr on int", type(e).__name__)


class Raises:
    @property
    def bad(self):
        raise ValueError("inner")

    @property
    def attr_bad(self):
        raise AttributeError("hidden")


r = Raises()
try:
    hasattr(r, "bad")
except ValueError as e:
    print("hasattr propagates non attribute error", e)
print("hasattr swallows attribute error", hasattr(r, "attr_bad"), getattr(r, "attr_bad", "fallback"))


class Dynamic:
    def __getattr__(self, name):
        if name.startswith("dyn_"):
            return name[4:]
        raise AttributeError(name)


dy = Dynamic()
print("__getattr__", dy.dyn_hello, dy.dyn_x, hasattr(dy, "other"), getattr(dy, "other", "default"))

print("callable", callable(len), callable(Thing), callable(t), callable(t.method), callable(lambda: 0), callable(5))


class CallMe:
    def __call__(self):
        return "called"


print("callable instance", callable(CallMe()), CallMe()())
print("callable str", callable("print"), callable(print))

print("vars", vars(t))
vars(t)["z"] = 26
print("vars is __dict__", t.z, vars(t) is t.__dict__)
print("vars class has kind", "kind" in vars(Thing), "a" in vars(Thing))
try:
    vars(5)
except TypeError as e:
    print("vars int", type(e).__name__)


class Slotted:
    __slots__ = ("x",)


try:
    vars(Slotted())
except TypeError as e:
    print("vars slots", type(e).__name__)

print("dir public", sorted(n for n in dir(t) if not n.startswith("__")))
print("dir class public", sorted(n for n in dir(Thing) if not n.startswith("_")))
print("dir has dunder init", "__init__" in dir(t))


class WithDir:
    def __dir__(self):
        return ["zeta", "alpha", "alpha"]


print("custom dir sorted", dir(WithDir()))
