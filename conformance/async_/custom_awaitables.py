"""Custom awaitables with __await__: yielding to the driver, values sent back in, and awaitable errors."""

import types

log = []


def run(coro, replies=None):
    """Drive coro; each yielded request is logged and answered from replies."""
    replies = list(replies or [])
    value = None
    try:
        while True:
            request = coro.send(value)
            log.append(("yielded", request))
            value = replies.pop(0) if replies else None
    except StopIteration as e:
        return e.value


class Ready:
    def __init__(self, value):
        self.value = value

    def __await__(self):
        return self.value
        yield  # generator, but never suspends


class Ask:
    """Suspends once and returns whatever the driver sends back."""

    def __init__(self, question):
        self.question = question

    def __await__(self):
        answer = yield self.question
        return answer


class Countdown:
    def __init__(self, n):
        self.n = n

    def __await__(self):
        for i in range(self.n, 0, -1):
            yield ("tick", i)
        return "liftoff"


async def uses_ready():
    return await Ready(7) + await Ready(8)


log.clear()
print("ready awaitable", run(uses_ready()), log)


async def asks():
    name = await Ask("name?")
    age = await Ask("age?")
    return name + " is " + str(age)


log.clear()
print("ask awaitable", run(asks(), ["ada", 36]), log)


async def counts():
    return await Countdown(3)


log.clear()
print("countdown", run(counts()), log)


class IterAwait:
    """__await__ may return any iterator, not only a generator."""

    def __await__(self):
        return iter(["x", "y"])


async def uses_iter_await():
    return await IterAwait()


log.clear()
print("iterator __await__", run(uses_iter_await()), log)


class BadAwait:
    def __await__(self):
        return 5


async def awaits_bad():
    try:
        await BadAwait()
    except TypeError as e:
        return type(e).__name__


print("__await__ returns non-iterator", run(awaits_bad()))


async def awaits_non_awaitable():
    try:
        await 3
    except TypeError as e:
        return type(e).__name__


print("await int", run(awaits_non_awaitable()))


@types.coroutine
def legacy_sleep(tag):
    got = yield ("legacy", tag)
    return got


async def uses_legacy():
    a = await legacy_sleep("a")
    b = await legacy_sleep("b")
    return a, b


log.clear()
print("types.coroutine", run(uses_legacy(), ["A", "B"]), log)


class Wrapper:
    def __init__(self, inner):
        self.inner = inner

    def __await__(self):
        result = yield from self.inner.__await__()
        return ("wrapped", result)


async def wraps_coroutine():
    return await Wrapper(asks())


log.clear()
print("awaitable wrapping coroutine", run(wraps_coroutine(), ["bo", 9]), log)


class RaisesInAwait:
    def __await__(self):
        yield "about to fail"
        raise LookupError("from __await__")


async def handles_await_error():
    try:
        await RaisesInAwait()
    except LookupError as e:
        return "handled " + str(e)


log.clear()
print("error inside __await__", run(handles_await_error()), log)


class Throwable:
    def __await__(self):
        try:
            yield "waiting"
        except KeyError:
            return "awaitable caught thrown"


async def awaits_throwable():
    return await Throwable()


c = awaits_throwable()
print("first suspend", c.send(None))
try:
    c.throw(KeyError)
except StopIteration as e:
    print("throw into awaitable", e.value)


class AwaitableReused:
    def __init__(self):
        self.calls = 0

    def __await__(self):
        self.calls += 1
        return (yield self.calls)


async def reuse_awaitable():
    a = AwaitableReused()
    return [await a, await a, await a], a.calls


log.clear()
print("awaitable object reused", run(reuse_awaitable(), [10, 20, 30]), log)
