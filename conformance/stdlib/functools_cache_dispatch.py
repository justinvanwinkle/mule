"""functools: lru_cache with cache_info, cache, cached_property, singledispatch with annotation registration."""
from functools import lru_cache, cache, cached_property, singledispatch, singledispatchmethod

calls = []


@lru_cache(maxsize=2)
def slow_square(n):
    calls.append(n)
    return n * n


for n in [1, 2, 1, 3, 1, 2]:
    slow_square(n)
print("lru calls", calls)
info = slow_square.cache_info()
print("lru info", info.hits, info.misses, info.maxsize, info.currsize)
print("lru info tuple", tuple(info))
slow_square.cache_clear()
print("lru after clear", slow_square.cache_info())


@lru_cache(maxsize=None)
def fib(n):
    return n if n < 2 else fib(n - 1) + fib(n - 2)


print("fib 80", fib(80))
print("fib info", fib.cache_info())


@lru_cache
def typed_default(x):
    return type(x).__name__


print("lru bare decorator", typed_default(1), typed_default(1.0), typed_default.cache_info().currsize)


@lru_cache(typed=True)
def typed_on(x):
    return type(x).__name__


print("lru typed", typed_on(1), typed_on(1.0), typed_on.cache_info().currsize)

kw_calls = []


@cache
def kw_fn(a, b=0):
    kw_calls.append((a, b))
    return a + b


kw_fn(1, b=2)
kw_fn(1, b=2)
kw_fn(1, 2)
print("cache kw calls", kw_calls)
try:
    kw_fn([1])
except TypeError as e:
    print("cache unhashable", type(e).__name__)


class Circle:
    def __init__(self, r):
        self.r = r
        self.computed = 0

    @cached_property
    def area(self):
        self.computed += 1
        return 3 * self.r * self.r


c = Circle(2)
print("cached_property first", c.area, c.computed)
print("cached_property second", c.area, c.computed)
print("cached_property in dict", "area" in vars(c))
del c.area
print("cached_property recompute", c.area, c.computed)
c.area = 99
print("cached_property set", c.area, c.computed)


@singledispatch
def describe(obj):
    return f"object {obj!r}"


@describe.register
def _(obj: int):
    return f"int {obj}"


@describe.register
def _(obj: list):
    return "list of " + ", ".join(describe(x) for x in obj)


@describe.register(str)
def _(obj):
    return f"str {obj!r}"


@describe.register
def _(obj: float | complex):
    return f"inexact {obj}"


print("dispatch int", describe(5))
print("dispatch bool (int subclass)", describe(True))
print("dispatch str", describe("hi"))
print("dispatch list", describe([1, "a", 2.5]))
print("dispatch union", describe(1j))
print("dispatch fallback", describe((1, 2)))
print("dispatch lookup", describe.dispatch(bool) is describe.dispatch(int))
print("dispatch registry has", int in describe.registry, dict in describe.registry)


class Shape:
    @singledispatchmethod
    def scale(self, factor):
        return "unsupported"

    @scale.register
    def _(self, factor: int):
        return f"int scale {factor}"

    @scale.register
    def _(self, factor: tuple):
        return f"tuple scale {factor}"


s = Shape()
print("dispatchmethod", s.scale(3), s.scale((1, 2)), s.scale("x"))
