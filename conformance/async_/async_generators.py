"""Async generators: async for consumption, asend, athrow, aclose, finally cleanup, and misuse errors."""

import inspect

log = []


def run(aw):
    try:
        while True:
            aw.send(None)
    except StopIteration as e:
        return e.value


class Suspend:
    def __await__(self):
        yield


async def agen(n):
    try:
        for i in range(n):
            await Suspend()
            yield i
    finally:
        log.append("agen finally")


async def consume():
    return [v async for v in agen(3)]


log.clear()
print("consume", run(consume()), log)
print("types", type(agen(1)).__name__, inspect.isasyncgen(agen(1)), inspect.isasyncgenfunction(agen))


async def echo():
    received = []
    try:
        while True:
            v = yield len(received)
            received.append(v)
    finally:
        log.append("echo received " + repr(received))


async def drive_asend():
    g = echo()
    first = await g.asend(None)
    a = await g.asend("x")
    b = await g.asend("y")
    await g.aclose()
    return first, a, b


log.clear()
print("asend", run(drive_asend()), log)


async def handler():
    while True:
        try:
            yield "waiting"
        except ValueError as e:
            yield "handled " + str(e)


async def drive_athrow():
    g = handler()
    first = await anext(g)
    handled = await g.athrow(ValueError("oops"))
    after = await anext(g)
    try:
        await g.athrow(KeyError("k"))
    except KeyError as e:
        dead = "propagated " + type(e).__name__
    exhausted = await anext(g, "exhausted")
    return first, handled, after, dead, exhausted


print("athrow", run(drive_athrow()))


async def cleanup_with_await():
    try:
        yield 1
        yield 2
    finally:
        await Suspend()
        log.append("awaited in finally")


async def drive_aclose():
    g = cleanup_with_await()
    v = await anext(g)
    await g.aclose()
    await g.aclose()
    return v, await anext(g, "closed")


log.clear()
print("aclose", run(drive_aclose()), log)


async def ignores_close():
    try:
        yield 1
    except GeneratorExit:
        yield "ignored"


async def drive_bad_close():
    g = ignores_close()
    await anext(g)
    try:
        await g.aclose()
    except RuntimeError as e:
        return type(e).__name__


print("yield in GeneratorExit", run(drive_bad_close()))


async def returns_early():
    yield "a"
    return
    yield "never"


async def drive_early():
    return [v async for v in returns_early()]


print("bare return", run(drive_early()))


async def break_out():
    g = agen(10)
    out = []
    async for v in g:
        if v == 2:
            break
        out.append(v)
    log.append("after break")
    await g.aclose()
    return out


log.clear()
print("break then aclose", run(break_out()), log)


async def stop_iteration_inside():
    yield 1
    raise StopAsyncIteration


async def drive_stop():
    try:
        return [v async for v in stop_iteration_inside()]
    except RuntimeError as e:
        return type(e).__name__


print("StopAsyncIteration inside agen", run(drive_stop()))


async def nested_agens():
    async def inner(prefix):
        for c in "ab":
            yield prefix + c

    async for p in agen(2):
        async for s in inner(str(p)):
            yield s


async def drive_nested():
    return [v async for v in nested_agens()]


log.clear()
print("nested async generators", run(drive_nested()), log)


async def concurrent_anext():
    g = agen(5)
    first = g.__anext__()
    first.send(None)
    second = g.__anext__()
    try:
        second.send(None)
    except RuntimeError as e:
        result = type(e).__name__
    first.close()
    await g.aclose()
    return result


print("concurrent __anext__", run(concurrent_anext()))


async def agen_state():
    g = agen(2)
    states = [g.ag_running]
    await anext(g)
    states.append(g.ag_frame is not None)
    await g.aclose()
    states.append(g.ag_frame is None)
    return states


print("ag_running/ag_frame", run(agen_state()))


async def agen_send_before_start():
    g = echo()
    try:
        await g.asend("x")
    except TypeError as e:
        return type(e).__name__


print("asend non-None first", run(agen_send_before_start()))
