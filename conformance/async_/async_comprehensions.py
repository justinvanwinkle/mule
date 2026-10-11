"""Async comprehensions: list/set/dict comprehensions and genexps with async for, await in element and filter."""


def run(coro):
    try:
        while True:
            coro.send(None)
    except StopIteration as e:
        return e.value


class Suspend:
    def __await__(self):
        yield


async def arange(n):
    for i in range(n):
        await Suspend()
        yield i


async def double(x):
    await Suspend()
    return x * 2


async def is_even(x):
    return x % 2 == 0


async def main():
    results = {}
    results["list async for"] = [x async for x in arange(5)]
    results["list await element"] = [await double(x) for x in range(4)]
    results["list both"] = [await double(x) async for x in arange(4)]
    results["await in filter"] = [x for x in range(8) if await is_even(x)]
    results["async for with await filter"] = [x async for x in arange(8) if await is_even(x)]
    results["set"] = sorted({x % 3 async for x in arange(10)})
    results["dict"] = {x: await double(x) async for x in arange(3)}
    results["nested async fors"] = [(a, b) async for a in arange(2) async for b in arange(2)]
    results["mixed sync then async"] = [(a, b) for a in "xy" async for b in arange(2)]
    results["mixed async then sync"] = [(a, b) async for a in arange(2) for b in "xy"]
    results["nested comprehension"] = [[await double(y) for y in range(x)] async for x in arange(3)]
    results["empty"] = [x async for x in arange(0)]

    gen = (await double(x) async for x in arange(3))
    results["async genexp type"] = type(gen).__name__
    results["async genexp values"] = [v async for v in gen]

    gen2 = (await double(x) for x in range(3))
    results["sync-for genexp with await is async"] = type(gen2).__name__
    results["consumed"] = [v async for v in gen2]

    total = 0
    async for v in (x * x async for x in arange(4)):
        total += v
    results["iterate async genexp"] = total

    results["walrus"] = [y async for x in arange(4) if (y := await double(x)) > 2]

    for k, v in results.items():
        print(k, v)


run(main())


async def outer_scope_var():
    factor = 10
    return [x * factor async for x in arange(3)]


print("closure in async comprehension", run(outer_scope_var()))


async def exception_in_comprehension():
    async def boom(x):
        if x == 2:
            raise ValueError("at 2")
        return x

    try:
        return [await boom(x) async for x in arange(4)]
    except ValueError as e:
        return "caught " + str(e)


print("exception propagates", run(exception_in_comprehension()))


async def comprehension_in_conditional(flag):
    return [x async for x in arange(3)] if flag else {x async for x in arange(0)}


print("conditional comprehension", run(comprehension_in_conditional(True)), run(comprehension_in_conditional(False)))
