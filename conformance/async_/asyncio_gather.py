"""asyncio.run with gather, sleep(0) interleaving, create_task ordering, and exception propagation."""

import asyncio

trace = []


async def worker(name, steps):
    for i in range(steps):
        trace.append(name + str(i))
        await asyncio.sleep(0)
    return name.upper() * steps


async def gather_basic():
    results = await asyncio.gather(worker("a", 3), worker("b", 2), worker("c", 1))
    return results


print("gather results", asyncio.run(gather_basic()))
print("gather interleaving", trace)

trace.clear()


async def tasks_ordering():
    t1 = asyncio.create_task(worker("x", 2))
    t2 = asyncio.create_task(worker("y", 2))
    trace.append("main before await")
    await asyncio.sleep(0)
    trace.append("main after one sleep")
    return await t1, await t2


print("create_task", asyncio.run(tasks_ordering()))
print("create_task trace", trace)


async def failing(n):
    await asyncio.sleep(0)
    raise ValueError("fail " + str(n))


async def gather_exceptions():
    results = await asyncio.gather(worker("ok", 1), failing(1), return_exceptions=True)
    return [type(r).__name__ if isinstance(r, BaseException) else r for r in results]


print("return_exceptions", asyncio.run(gather_exceptions()))


async def gather_raises():
    try:
        await asyncio.gather(failing(2), worker("z", 1))
    except ValueError as e:
        return "propagated " + str(e)


print("gather raises", asyncio.run(gather_raises()))


async def taskgroup():
    async with asyncio.TaskGroup() as tg:
        t1 = tg.create_task(worker("g", 2))
        t2 = tg.create_task(worker("h", 1))
    return t1.result(), t2.result()


trace.clear()
print("TaskGroup", asyncio.run(taskgroup()), trace)


async def taskgroup_error():
    messages = []
    try:
        async with asyncio.TaskGroup() as tg:
            tg.create_task(failing(3))
            tg.create_task(failing(4))
    except* ValueError as eg:
        messages.extend(str(e) for e in eg.exceptions)
    return sorted(messages)


print("TaskGroup errors", asyncio.run(taskgroup_error()))


async def queue_pipeline():
    q = asyncio.Queue()

    async def producer():
        for i in range(4):
            await q.put(i)
            trace.append("put" + str(i))
        await q.put(None)

    async def consumer():
        got = []
        while (item := await q.get()) is not None:
            got.append(item)
            trace.append("got" + str(item))
        return got

    _, got = await asyncio.gather(producer(), consumer())
    return got


trace.clear()
print("queue", asyncio.run(queue_pipeline()), trace)


async def cancellation():
    async def forever():
        try:
            while True:
                await asyncio.sleep(0)
        except asyncio.CancelledError:
            trace.append("cancelled cleanup")
            raise

    t = asyncio.create_task(forever())
    await asyncio.sleep(0)
    t.cancel()
    try:
        await t
    except asyncio.CancelledError:
        return "task cancelled", t.cancelled()


trace.clear()
print("cancel", asyncio.run(cancellation()), trace)


async def async_gen_with_asyncio():
    async def ticks(n):
        for i in range(n):
            await asyncio.sleep(0)
            yield i

    return [i async for i in ticks(4)]


print("async gen under asyncio", asyncio.run(async_gen_with_asyncio()))
