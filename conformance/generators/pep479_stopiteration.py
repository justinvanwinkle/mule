"""PEP 479: StopIteration raised inside a generator becomes RuntimeError, with __cause__ preserved."""


def leaks_next():
    it = iter([1])
    yield next(it)
    yield next(it)  # raises StopIteration inside the generator


g = leaks_next()
print("first", next(g))
try:
    next(g)
except RuntimeError as e:
    print("converted", type(e).__name__, type(e.__cause__).__name__)
except StopIteration:
    print("leaked StopIteration (wrong)")


def explicit_raise():
    yield 1
    raise StopIteration("explicit")


try:
    list(explicit_raise())
except RuntimeError as e:
    print("explicit raise converted", type(e.__cause__).__name__, e.__cause__.value if hasattr(e.__cause__, "value") else None)


def subclass_raise():
    class MyStop(StopIteration):
        pass

    yield 1
    raise MyStop


try:
    list(subclass_raise())
except RuntimeError as e:
    print("subclass converted", type(e.__cause__).__name__)


def handled_inside():
    it = iter([])
    try:
        next(it)
    except StopIteration:
        yield "caught inside, fine"


print("caught inside", list(handled_inside()))


def return_is_fine():
    yield 1
    return "value"


print("return is fine", list(return_is_fine()))


def next_with_default():
    it = iter([])
    yield next(it, "default used")


print("next default avoids error", list(next_with_default()))

# Iterator classes are not affected: __next__ raising StopIteration ends iteration
class Countdown:
    def __init__(self, n):
        self.n = n

    def __iter__(self):
        return self

    def __next__(self):
        if self.n <= 0:
            raise StopIteration
        self.n -= 1
        return self.n


print("iterator class unaffected", list(Countdown(3)))

# Generator expression also converts
def bad_iter():
    raise StopIteration
    yield


try:
    list(next(iter(())) for _ in range(1))
except RuntimeError as e:
    print("genexp converted", type(e).__name__)
except StopIteration:
    print("genexp leaked StopIteration")

# list comprehension (not a generator) lets StopIteration propagate
try:
    [next(iter(())) for _ in range(1)]
except StopIteration:
    print("list comprehension propagates StopIteration")
except RuntimeError:
    print("list comprehension converted (wrong)")


# In yield from, delegate's StopIteration is its return, not an error
def delegator():
    v = yield from return_is_fine()
    yield v


print("yield from return", list(delegator()))


# Converted error leaves generator closed
g = leaks_next()
next(g)
try:
    next(g)
except RuntimeError:
    pass
print("closed after conversion", next(g, "closed"))


# Throwing StopIteration into a generator also converts
def plain():
    yield 1
    yield 2


g = plain()
next(g)
try:
    g.throw(StopIteration)
except RuntimeError as e:
    print("thrown StopIteration converted", type(e).__name__)


# StopAsyncIteration in a sync generator is not converted
def async_stop():
    yield 1
    raise StopAsyncIteration


try:
    list(async_stop())
except StopAsyncIteration:
    print("StopAsyncIteration propagates unchanged in sync generator")
except RuntimeError:
    print("StopAsyncIteration converted")
