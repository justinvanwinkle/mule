"""async for with __aiter__/__anext__, StopAsyncIteration, else clauses, break/continue, and protocol errors."""

log = []


def run(coro):
    try:
        while True:
            coro.send(None)
    except StopIteration as e:
        return e.value


class Suspend:
    def __await__(self):
        yield


class Ticker:
    def __init__(self, n):
        self.n = n
        self.i = 0

    def __aiter__(self):
        log.append("aiter")
        return self

    async def __anext__(self):
        await Suspend()
        if self.i >= self.n:
            log.append("stop")
            raise StopAsyncIteration
        self.i += 1
        return self.i


async def collect():
    out = []
    async for v in Ticker(3):
        out.append(v)
    return out


log.clear()
print("basic", run(collect()), log)


async def with_else(n, stop_at):
    out = []
    async for v in Ticker(n):
        if v == stop_at:
            out.append("break")
            break
        out.append(v)
    else:
        out.append("else")
    return out


print("else runs", run(with_else(2, 99)))
print("else skipped on break", run(with_else(5, 2)))


async def with_continue():
    out = []
    async for v in Ticker(6):
        if v % 2:
            continue
        out.append(v)
    return out


print("continue", run(with_continue()))


async def nested():
    out = []
    async for a in Ticker(2):
        async for b in Ticker(2):
            out.append((a, b))
    return out


print("nested", run(nested()))


async def unpacking():
    class Pairs:
        def __init__(self):
            self.items = [(1, "a"), (2, "b")]

        def __aiter__(self):
            return self

        async def __anext__(self):
            if not self.items:
                raise StopAsyncIteration
            return self.items.pop(0)

    out = []
    async for num, letter in Pairs():
        out.append(letter * num)
    return out


print("unpacking target", run(unpacking()))


class SyncIterable:
    def __iter__(self):
        return iter([1])


async def wrong_protocol():
    try:
        async for _ in SyncIterable():
            pass
    except TypeError as e:
        return type(e).__name__


print("sync iterable in async for", run(wrong_protocol()))


class AnextNotAwaitable:
    def __aiter__(self):
        return self

    def __anext__(self):
        return 5


async def bad_anext():
    try:
        async for _ in AnextNotAwaitable():
            pass
    except TypeError as e:
        return type(e).__name__


print("non-awaitable __anext__", run(bad_anext()))


class RaisesMidway:
    def __init__(self):
        self.i = 0

    def __aiter__(self):
        return self

    async def __anext__(self):
        self.i += 1
        if self.i == 3:
            raise ValueError("midway")
        return self.i


async def error_midway():
    seen = []
    try:
        async for v in RaisesMidway():
            seen.append(v)
    except ValueError as e:
        seen.append(str(e))
    return seen


print("error from __anext__", run(error_midway()))


class AwaitableAnext:
    """__anext__ may return any awaitable, not just a coroutine."""

    def __init__(self):
        self.values = ["p", "q"]

    def __aiter__(self):
        return self

    def __anext__(self):
        values = self.values

        class Step:
            def __await__(self):
                yield
                if not values:
                    raise StopAsyncIteration
                return values.pop(0)

        return Step()


async def awaitable_anext():
    return [v async for v in AwaitableAnext()]


print("custom awaitable __anext__", run(awaitable_anext()))


async def return_inside_loop():
    async for v in Ticker(10):
        if v == 4:
            return "returned at " + str(v)


print("return inside async for", run(return_inside_loop()))


async def target_is_attribute():
    class Box:
        pass

    box = Box()
    async for box.value in Ticker(3):
        pass
    return box.value


print("attribute loop target", run(target_is_attribute()))


async def manual_iteration():
    it = Ticker(2).__aiter__()
    first = await it.__anext__()
    second = await anext(it)
    third = await anext(it, "default")
    return first, second, third


print("manual anext", run(manual_iteration()))
print("aiter builtin", type(aiter(Ticker(1))).__name__)
