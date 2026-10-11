"""Coroutines driven by hand with send(None): return values, await chains, coroutine objects and states."""

import inspect


def run(coro):
    """Drive a coroutine to completion, counting suspension points."""
    steps = 0
    try:
        while True:
            coro.send(None)
            steps += 1
    except StopIteration as e:
        return e.value, steps


async def constant():
    return 42


async def add(a, b):
    return a + b


async def chain(n):
    if n == 0:
        return 0
    return n + await chain(n - 1)


async def outer():
    x = await constant()
    y = await add(x, 1)
    z = await chain(10)
    return (x, y, z)


print("constant", run(constant()))
print("chain", run(chain(5)))
print("outer", run(outer()))

c = constant()
print("type", type(c).__name__)
print("created state", inspect.getcoroutinestate(c))
print("iscoroutine", inspect.iscoroutine(c), inspect.iscoroutinefunction(constant))
print("result", run(c))
print("closed state", inspect.getcoroutinestate(c))

try:
    c.send(None)
except RuntimeError as e:
    print("reuse finished coroutine", type(e).__name__)

c = constant()
try:
    c.send("not none")
except TypeError as e:
    print("send non-None at start", type(e).__name__)
c.close()

c = constant()
try:
    iter(c)
except TypeError as e:
    print("coroutine not iterable", type(e).__name__)
c.close()

print("coroutine name", constant.__name__, constant.__qualname__)


async def raises():
    raise ValueError("inside coroutine")


async def catches():
    try:
        await raises()
    except ValueError as e:
        return "caught: " + str(e)


print("exception through await", run(catches()))

try:
    run(raises())
except ValueError as e:
    print("exception escapes driver", e)


async def awaits_same_twice():
    inner = constant()
    first = await inner
    try:
        await inner
    except RuntimeError as e:
        return first, type(e).__name__


print("await same coroutine twice", run(awaits_same_twice()))


async def with_locals(n):
    acc = []
    for i in range(n):
        acc.append(await add(i, i))
    return acc


print("await in loop", run(with_locals(4)))


async def nested_def():
    async def helper(v):
        return v * 3

    return [await helper(i) for i in range(3)]


print("nested async def", run(nested_def()))


class Service:
    def __init__(self, base):
        self.base = base

    async def compute(self, x):
        return self.base + await add(x, x)

    @staticmethod
    async def static(x):
        return -x

    @classmethod
    async def make(cls, base):
        return cls(base)


async def use_service():
    svc = await Service.make(100)
    return await svc.compute(5), await Service.static(3)


print("async methods", run(use_service()))


def returns_coroutine():
    return add(2, 3)


async def await_non_async_returning_coroutine():
    return await returns_coroutine()


print("await returned coroutine", run(await_non_async_returning_coroutine()))


async def recursive_fib(n):
    return n if n < 2 else await recursive_fib(n - 1) + await recursive_fib(n - 2)


print("recursive fib", run(recursive_fib(15)))


class Suspend:
    def __await__(self):
        yield


async def suspends(n):
    for _ in range(n):
        await Suspend()
    return "suspended " + str(n)


async def suspends_nested():
    return [await suspends(2), await suspends(3)]


print("suspension count", run(suspends(4)))
print("nested suspension count", run(suspends_nested()))

c = add(1, 2)
c.close()
print("close unstarted", inspect.getcoroutinestate(c))
