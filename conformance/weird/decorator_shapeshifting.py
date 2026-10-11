"""Decorators that turn functions into classes and classes into functions, stacked with property/classmethod/descriptors."""

import functools


def classify(fn):
    class Wrapped:
        calls = 0

        def __init__(self, *args):
            type(self).calls += 1
            self.value = fn(*args)

        def __repr__(self):
            return f"<{fn.__name__} -> {self.value!r}>"

    Wrapped.__name__ = fn.__name__.title()
    return Wrapped


@classify
def double(x):
    return x * 2


print("function became class", double.__name__, isinstance(double, type))
print("instances", double(3), double("ab"), double.calls)


def functionize(cls):
    def make(*args, **kw):
        obj = cls(*args, **kw)
        return obj.describe()
    make.original = cls
    return make


@functionize
class Point:
    def __init__(self, x, y=0):
        self.x, self.y = x, y

    def describe(self):
        return f"P({self.x},{self.y})"


print("class became function", type(Point).__name__, Point(1, y=2), Point.original.__name__)


def counting(fn):
    @functools.wraps(fn)
    def wrapper(*a, **k):
        wrapper.count += 1
        return fn(*a, **k)
    wrapper.count = 0
    return wrapper


class Logged:
    """A custom data descriptor that logs access and wraps the value."""

    def __init__(self, default):
        self.default = default

    def __set_name__(self, owner, name):
        self.name = "_" + name
        print("set_name", owner.__name__, name)

    def __get__(self, obj, owner=None):
        if obj is None:
            return self
        return ("logged", getattr(obj, self.name, self.default))

    def __set__(self, obj, value):
        setattr(obj, self.name, value * 2)


class Thing:
    level = Logged(1)

    def __init__(self):
        self._size = 5

    @property
    @counting
    def size(self):
        return self._size

    @size.setter
    def size(self, v):
        self._size = -v

    @classmethod
    @counting
    def make(cls, n):
        t = cls()
        t.size = n
        return t

    @staticmethod
    @functools.lru_cache(maxsize=None)
    def square(n):
        print("computing", n)
        return n * n

    @functools.cached_property
    def heavy(self):
        print("heavy computed")
        return [self._size] * 2


t = Thing.make(4)
print("property", t.size, t.size)
print("counts", Thing.make.count, Thing.size.fget.count)
print("descriptor default", t.level)
t.level = 21
print("descriptor set", t.level, t._level)
print("lru staticmethod", Thing.square(3), t.square(3), Thing.square(4))
print("cached_property", t.heavy, t.heavy is t.heavy)
del t.heavy
print("recomputed", t.heavy)
print("wraps name", Thing.size.fget.__name__, Thing.make.__func__.__name__)


def decorator_factory(*tags):
    print("factory called", tags)

    def deco(obj):
        print("decorating", obj.__name__, tags)
        obj.tags = getattr(obj, "tags", ()) + tags
        return obj
    return deco


@decorator_factory("outer")
@decorator_factory("inner")
def tagged():
    pass


print("stack order", tagged.tags)

# Decorator expressions are arbitrary (PEP 614)
decos = {"id": lambda f: f, "twice": lambda f: lambda *a: f(f(*a))}


@decos["twice"]
@(lambda f: (lambda x: f(x) + 1))
def inc(x):
    return x * 3


print("arbitrary decorator expressions", inc(1))


@type
class Weird:
    pass


print("class decorated with type", Weird.__name__)


@str
@len
@list
@range
@abs
@(-10).__add__
@int
@lambda f: f.__name__.count("a") + 2
def banana():
    pass


print("decorator chain over builtins", repr(banana))
