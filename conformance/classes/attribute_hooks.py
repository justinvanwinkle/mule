"""__getattr__ vs __getattribute__ vs __setattr__ vs __delattr__, and avoiding recursion with object.__setattr__."""


class Fallback:
    existing = "class attr"

    def __init__(self):
        self.inst = "instance attr"

    def __getattr__(self, name):
        # Only called when normal lookup fails.
        if name.startswith("dyn_"):
            return name[4:]
        raise AttributeError(name)


f = Fallback()
print("normal", f.existing, f.inst)
print("fallback", f.dyn_hello)
try:
    f.missing
except AttributeError as e:
    print("fallback raises", type(e).__name__)
print("getattr default with __getattr__", getattr(f, "missing", "dflt"), hasattr(f, "dyn_x"))


class Intercept:
    def __init__(self):
        object.__setattr__(self, "log", [])
        self.value = 1

    def __getattribute__(self, name):
        log = object.__getattribute__(self, "log")
        log.append(f"get {name}")
        return object.__getattribute__(self, name)

    def __getattr__(self, name):
        return f"missing:{name}"

    def __setattr__(self, name, value):
        object.__getattribute__(self, "log").append(f"set {name}")
        object.__setattr__(self, name, value * 10 if isinstance(value, int) else value)

    def __delattr__(self, name):
        object.__getattribute__(self, "log").append(f"del {name}")
        object.__delattr__(self, name)


ic = Intercept()
print("intercepted value", ic.value)
print("falls to __getattr__", ic.nothing)
ic.other = 2
del ic.other
print("after delete", ic.other)
print("log", object.__getattribute__(ic, "log"))

# Special method lookup bypasses __getattribute__ on the instance
class Special:
    def __getattribute__(self, name):
        if name == "__len__":
            return lambda: 99
        return object.__getattribute__(self, name)

    def __len__(self):
        return 3


sp = Special()
print("implicit len bypasses", len(sp), "explicit", sp.__len__())


# Instance __getattr__ cannot supply dunder methods for operators
class Proxy:
    def __init__(self, target):
        object.__setattr__(self, "_target", target)

    def __getattr__(self, name):
        return getattr(self._target, name)

    def __setattr__(self, name, value):
        setattr(self._target, name, value)


class Target:
    def __init__(self):
        self.a = 1

    def hello(self):
        return "target hello"


tg = Target()
px = Proxy(tg)
print("proxy read", px.a, px.hello())
px.b = 2
print("proxy write forwards", tg.b, "b" in vars(px))
try:
    len(Proxy([1, 2]))
except TypeError as e:
    print("proxy implicit len", type(e).__name__)
print("proxy explicit __len__", Proxy([1, 2]).__len__())


# Read-only attributes via __setattr__
class Frozen:
    __slots__ = ("x", "y")

    def __init__(self, x, y):
        object.__setattr__(self, "x", x)
        object.__setattr__(self, "y", y)

    def __setattr__(self, name, value):
        raise AttributeError(f"cannot set {name}")

    def __delattr__(self, name):
        raise AttributeError(f"cannot delete {name}")


fr = Frozen(1, 2)
for action in ("set", "del"):
    try:
        if action == "set":
            fr.x = 5
        else:
            del fr.x
    except AttributeError as e:
        print("frozen", action, e)
print("frozen values", fr.x, fr.y)


# __getattribute__ raising AttributeError triggers __getattr__
class Chain:
    def __getattribute__(self, name):
        if name == "hidden":
            raise AttributeError(name)
        return object.__getattribute__(self, name)

    def __getattr__(self, name):
        return f"getattr({name})"

    hidden = "should be hidden"


print("getattribute -> getattr", Chain().hidden)


# __setattr__ with super() and attribute counting
class Recorder:
    def __init__(self):
        super().__setattr__("history", [])

    def __setattr__(self, name, value):
        self.history.append((name, value))
        super().__setattr__(name, value)


rc = Recorder()
rc.a = 1
rc.a += 1
rc.b = "x"
print("history", rc.history, rc.a)


# Class-level __getattr__ on module-like namespace via metaclass
class MetaLookup(type):
    def __getattr__(cls, name):
        return f"class-level {name}"


class Namespace(metaclass=MetaLookup):
    real = "real"


print("metaclass __getattr__", Namespace.real, Namespace.virtual)
try:
    Namespace().virtual
except AttributeError as e:
    print("instance doesn't see meta __getattr__", type(e).__name__)

# __dir__ customization
class Dirs:
    def __dir__(self):
        return ["zeta", "alpha", "alpha"]


print("dir custom", dir(Dirs()))
