"""functools.wraps metadata, decorators on methods/classmethods/staticmethods/properties, and class decorators."""

import functools


def plain(fn):
    def wrapper(*a, **k):
        return fn(*a, **k)

    return wrapper


def wrapped(fn):
    @functools.wraps(fn)
    def wrapper(*a, **k):
        return fn(*a, **k)

    return wrapper


@plain
def without_wraps():
    """Original doc."""


@wrapped
def with_wraps(x: int) -> int:
    """Doubles x."""
    return x * 2


print("no wraps name", without_wraps.__name__, without_wraps.__doc__)
print("wraps name", with_wraps.__name__, with_wraps.__doc__)
print("wraps qualname", with_wraps.__qualname__)
print("wraps __wrapped__", with_wraps.__wrapped__.__name__, with_wraps.__wrapped__(4))
print("wraps annotations", with_wraps.__annotations__)
print("wraps result", with_wraps(21))
print("has __wrapped__ without wraps", hasattr(without_wraps, "__wrapped__"))


@wrapped
@wrapped
def twice():
    """Twice wrapped."""
    return "inner"


print("double wraps", twice.__name__, twice.__wrapped__.__wrapped__())
import inspect

print("inspect.unwrap", inspect.unwrap(twice)())

calls = []


def log_method(fn):
    @functools.wraps(fn)
    def wrapper(self, *a):
        calls.append((type(self).__name__, fn.__name__, a))
        return fn(self, *a)

    return wrapper


def log_any(fn):
    @functools.wraps(fn)
    def wrapper(*a):
        calls.append(("any", fn.__name__, len(a)))
        return fn(*a)

    return wrapper


class Account:
    rate = 2

    def __init__(self, balance):
        self._balance = balance

    @log_method
    def deposit(self, amount):
        self._balance += amount
        return self._balance

    @classmethod
    @log_any
    def from_zero(cls):
        return cls(0)

    @staticmethod
    @log_any
    def fee(x):
        return x // 10

    @property
    @log_any
    def balance(self):
        return self._balance

    @log_any
    def __repr__(self):
        return "Account(" + str(self._balance) + ")"


acct = Account.from_zero()
acct.deposit(50)
print("fee", Account.fee(55), acct.fee(99))
print("property", acct.balance)
print("repr", repr(acct))
print("method calls", calls)
print("method name preserved", Account.deposit.__name__, Account.deposit.__qualname__)


def add_methods(cls):
    def describe(self):
        return cls.__name__ + " with " + ", ".join(sorted(vars(self)))

    cls.describe = describe
    cls.decorated = True
    return cls


def numbered(cls):
    original_init = cls.__init__

    def __init__(self, *a, **k):
        original_init(self, *a, **k)
        numbered.count += 1
        self.serial = numbered.count

    cls.__init__ = __init__
    return cls


numbered.count = 0


@add_methods
@numbered
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y


p1 = Point(1, 2)
p2 = Point(3, 4)
print("class decorator added method", p1.describe())
print("class decorator attrs", Point.decorated, p1.serial, p2.serial)


def singleton(cls):
    instances = {}

    @functools.wraps(cls, updated=())
    def get(*a):
        if cls not in instances:
            instances[cls] = cls(*a)
        return instances[cls]

    return get


@singleton
class Config:
    def __init__(self, name):
        self.name = name


print("singleton", Config("first").name, Config("second").name, Config("x") is Config("y"))
print("singleton name", Config.__name__)


@functools.cache
def cached_square(n):
    calls.append(("square", n))
    return n * n


calls.clear()
print("functools.cache", [cached_square(n) for n in (2, 3, 2, 3, 4)], calls)
print("cache info", cached_square.cache_info().hits, cached_square.cache_info().misses)

partial_pow = functools.partial(pow, 2)
print("partial", partial_pow(10), partial_pow.args)
