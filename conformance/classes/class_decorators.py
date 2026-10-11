"""Class decorators: mutation, replacement, stacking order, parameterized decorators, and wrapping."""

trace = []


def add_greet(cls):
    trace.append(f"add_greet {cls.__name__}")
    cls.greet = lambda self: f"hello from {type(self).__name__}"
    return cls


def tag(label):
    trace.append(f"tag factory {label}")

    def deco(cls):
        trace.append(f"tag apply {label} to {cls.__name__}")
        cls.tags = getattr(cls, "tags", ()) + (label,)
        return cls

    return deco


@tag("outer")
@add_greet
@tag("inner")
class Widget:
    pass


print("trace", trace)
print("tags order", Widget.tags)
print("greet", Widget().greet())


# Decorator replacing the class with a function-produced wrapper class
def counted(cls):
    class Wrapper(cls):
        instances = 0

        def __init__(self, *a, **k):
            type(self).instances += 1
            super().__init__(*a, **k)

    Wrapper.__name__ = cls.__name__
    Wrapper.__qualname__ = cls.__qualname__
    return Wrapper


@counted
class Item:
    def __init__(self, v):
        self.v = v


Item(1); Item(2)
print("wrapped", Item.__name__, Item.instances, [c.__name__ for c in Item.__mro__])


# Decorator returning a non-class
def to_instance(cls):
    return cls()


@to_instance
class settings:
    debug = False

    def __repr__(self):
        return "settings-instance"


print("decorator returns instance", settings, settings.debug)


# Decorator that adds dunder methods (like a mini-dataclass)
def auto_repr_eq(cls):
    fields = list(cls.__init__.__code__.co_varnames[1:cls.__init__.__code__.co_argcount])

    def __repr__(self):
        args = ", ".join(f"{f}={getattr(self, f)!r}" for f in fields)
        return f"{cls.__name__}({args})"

    def __eq__(self, other):
        if type(other) is not type(self):
            return NotImplemented
        return all(getattr(self, f) == getattr(other, f) for f in fields)

    cls.__repr__ = __repr__
    cls.__eq__ = __eq__
    cls.__hash__ = None
    return cls


@auto_repr_eq
class Pt:
    def __init__(self, x, y):
        self.x = x
        self.y = y


print("auto repr", Pt(1, "a"), Pt(1, 2) == Pt(1, 2), Pt(1, 2) == Pt(2, 1), Pt(1, 2) == (1, 2))
try:
    hash(Pt(0, 0))
except TypeError as e:
    print("unhashable", type(e).__name__)


# Registry decorator capturing classes in definition order
REG = []


def register(cls):
    REG.append(cls.__name__)
    return cls


@register
class A:
    pass


@register
class B(A):
    pass


print("registry", REG)


# Decorator expression can be arbitrary (PEP 614)
decorators = {"g": add_greet}


@decorators["g"]
class Fancy:
    pass


@(lambda c: tag("lam")(c))
class Lam:
    pass


print("pep614", Fancy().greet(), Lam.tags)


# Decorator applied to a nested class within another class body
class Outer:
    @tag("nested")
    class Inner:
        pass


print("nested decorated", Outer.Inner.tags, Outer.Inner.__qualname__)

# Decorator order relative to metaclass: metaclass runs first
class M(type):
    def __new__(mcls, name, bases, ns):
        trace.append(f"meta {name}")
        return super().__new__(mcls, name, bases, ns)


trace.clear()


@tag("after-meta")
class WithMeta(metaclass=M):
    pass


print("meta then decorator", trace)
