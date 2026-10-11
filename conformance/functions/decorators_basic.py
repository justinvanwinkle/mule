"""Decorators: simple, stacked (application and call order), with arguments, and returning non-functions."""

log = []


def trace(fn):
    log.append("decorate " + fn.__name__)

    def wrapper(*args, **kwargs):
        log.append("enter " + fn.__name__)
        result = fn(*args, **kwargs)
        log.append("exit " + fn.__name__)
        return result

    return wrapper


@trace
def add(a, b):
    return a + b


print("simple", add(2, 3), log)
print("wrapped name", add.__name__)

log.clear()


def tag(label):
    log.append("factory " + label)

    def deco(fn):
        log.append("apply " + label)

        def wrapper(*a):
            log.append("call " + label)
            return "<" + label + ">" + fn(*a) + "</" + label + ">"

        return wrapper

    return deco


@tag("outer")
@tag("middle")
@tag("inner")
def text(s):
    return s


print("stacking order at definition", log)
log.clear()
print("stacked result", text("hi"))
print("stacking order at call", log)


def repeat(times):
    def deco(fn):
        def wrapper(*a):
            return [fn(*a) for _ in range(times)]

        return wrapper

    return deco


@repeat(3)
def hello(name):
    return "hi " + name


print("decorator with args", hello("bob"))


def to_value(fn):
    return fn()


@to_value
def computed():
    return 6 * 7


print("decorator returns non-function", computed, type(computed).__name__)


def register_into(registry):
    def deco(fn):
        registry[fn.__name__] = fn
        return fn

    return deco


handlers = {}


@register_into(handlers)
def on_a():
    return "a handled"


@register_into(handlers)
def on_b():
    return "b handled"


print("registry", sorted(handlers), handlers["on_b"]())
print("registered returns original", on_a is handlers["on_a"])


def replace_with_none(fn):
    return None


@replace_with_none
def gone():
    return 1


print("decorator returning None", gone)


def counting(fn):
    def wrapper(*a, **k):
        wrapper.calls += 1
        return fn(*a, **k)

    wrapper.calls = 0
    return wrapper


@counting
def noop():
    pass


for _ in range(4):
    noop()
print("call counter attribute", noop.calls)


def memoize(fn):
    cache = {}

    def wrapper(n):
        if n not in cache:
            cache[n] = fn(n)
        return cache[n]

    wrapper.cache = cache
    return wrapper


@memoize
def slow_fib(n):
    return n if n < 2 else slow_fib(n - 1) + slow_fib(n - 2)


print("memoized fib", slow_fib(80), len(slow_fib.cache))

# Decorator evaluated before the function is created: side effects order
order = []


def deco_expr(name):
    order.append("eval " + name)
    return lambda fn: (order.append("apply " + name), fn)[1]


@deco_expr("first")
@deco_expr("second")
def ordered():
    pass


print("decorator expression order", order)

# Decorator returning a class instance replacing the function
class Box:
    def __init__(self, fn):
        self.value = fn.__name__.upper()

    def __repr__(self):
        return "Box(" + self.value + ")"


@Box
def boxed():
    pass


print("decorator returning instance", boxed)
