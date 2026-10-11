"""__call__: callable instances, argument forwarding, callable(), stateful callables, and class-level lookup."""


class Adder:
    def __init__(self, n):
        self.n = n
        self.calls = 0

    def __call__(self, x, *rest, scale=1, **extra):
        self.calls += 1
        return (self.n + x + sum(rest)) * scale, sorted(extra)


add5 = Adder(5)
print("call", add5(1), add5(1, 2, 3), add5(1, scale=10), add5(0, flag=True, z=1))
print("call count", add5.calls)
print("callable", callable(add5), callable(Adder), callable(5), callable(print), callable(lambda: 0))
print("map with callable", list(map(Adder(1), [1, 2])))
print("sorted key callable", sorted([3, 1, 2], key=Adder(0)))
args = (2, 3)
kwargs = {"scale": 2}
print("star args", add5(*args, **kwargs))

try:
    add5()
except TypeError as e:
    print("missing arg", type(e).__name__)


# Instance attribute __call__ does not make an instance callable
class NotCallable:
    pass


nc = NotCallable()
nc.__call__ = lambda: "instance call"
print("instance __call__ ignored", callable(nc))
try:
    nc()
except TypeError as e:
    print("calling it", type(e).__name__)


# Callable chains: __call__ returning callables
class Curry:
    def __init__(self, fn, n, args=()):
        self.fn, self.n, self.args = fn, n, args

    def __call__(self, *more):
        args = self.args + more
        if len(args) >= self.n:
            return self.fn(*args)
        return Curry(self.fn, self.n, args)


c3 = Curry(lambda a, b, c: a * 100 + b * 10 + c, 3)
print("curry", c3(1)(2)(3), c3(1, 2)(3), c3(1)(2, 3), c3(4, 5, 6))


# Callable used as decorator with state
class Memo:
    def __init__(self, fn):
        self.fn = fn
        self.cache = {}
        self.misses = 0

    def __call__(self, n):
        if n not in self.cache:
            self.misses += 1
            self.cache[n] = self.fn(n)
        return self.cache[n]


@Memo
def fib(n):
    return n if n < 2 else fib(n - 1) + fib(n - 2)


print("memo fib", fib(30), fib.misses, type(fib).__name__)


# Callable class instance as a method: not bound (no __get__)
class Helper:
    def __call__(self, *args):
        return f"helper got {len(args)} args"


class Uses:
    h = Helper()

    def regular(self, *args):
        return f"method got {len(args) + 1} args"


u = Uses()
print("callable attr not bound", u.h(1), u.regular(1))


# Making a callable bindable with __get__
import types


class Bindable:
    def __call__(self, obj, x):
        return f"{type(obj).__name__}:{x}"

    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        return types.MethodType(self, obj)


class Host:
    m = Bindable()


print("bindable callable", Host().m(5), Host.m(Host(), 6))

# __call__ on a metaclass controls class calls
class Meta(type):
    def __call__(cls, *a):
        return f"{cls.__name__} called with {a}"


class K(metaclass=Meta):
    pass


print("metaclass __call__", K(1, 2))

# Class-level __call__ replaced after creation
Adder.__call__ = lambda self, x: f"patched {x}"
print("patched class __call__", add5(9))

# Builtin types as callables
print("types as callables", list(map(int, ["1", "2"])), list(map(str.upper, ["a"])), list(map(len, ["ab", ""])))

# Callable returning itself for chaining
class Chain:
    def __init__(self):
        self.parts = []

    def __call__(self, part):
        self.parts.append(part)
        return self


print("chained calls", Chain()("a")("b")("c").parts)

# Keyword-only and positional-only in __call__
class Strict:
    def __call__(self, a, /, b, *, c):
        return (a, b, c)


st = Strict()
print("param kinds", st(1, 2, c=3), st(1, b=2, c=3))
try:
    st(a=1, b=2, c=3)
except TypeError as e:
    print("positional-only violation", type(e).__name__)
