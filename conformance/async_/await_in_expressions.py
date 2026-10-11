"""await in expressions: f-strings, conditional expressions, boolean short-circuit, call args, subscripts, operators."""

trace = []


def run(coro):
    try:
        while True:
            coro.send(None)
    except StopIteration as e:
        return e.value


class Suspend:
    def __await__(self):
        yield


async def val(label, v):
    trace.append(label)
    await Suspend()
    return v


async def main():
    print("f-string", f"{await val('a', 1)} + {await val('b', 2)} = {await val('c', 3)}")
    print("f-string format spec", f"{await val('f', 3.14159):.2f}|{await val('g', 'x'):>3}")
    print("f-string nested", f"{f'{await val('n', 'inner')}'!r}")

    trace.clear()
    r = await val("t", "yes") if await val("cond", True) else await val("f", "no")
    print("conditional expr", r, trace)

    trace.clear()
    r = (await val("left", 0)) or (await val("right", "fallback"))
    print("or short-circuit", r, trace)
    trace.clear()
    r = (await val("left", 0)) and (await val("right", "never"))
    print("and short-circuit", r, trace)

    trace.clear()
    r = max(await val("x", 3), await val("y", 9), key=lambda v: -v)
    print("call args order", r, trace)

    trace.clear()
    d = {"k": [10, 20, 30]}
    r = (await val("obj", d))[await val("key", "k")][await val("idx", 1)]
    print("subscripts", r, trace)

    trace.clear()
    r = await val("l", 2) ** await val("r", 10)
    print("power", r, trace)
    r = -await val("neg", 5)
    print("unary", r)
    r = not await val("not", [])
    print("not", r)

    trace.clear()
    r = [await val("e0", 0), await val("e1", 1)] + [await val("e2", 2)]
    print("list display order", r, trace)

    trace.clear()
    r = {await val("k1", "a"): await val("v1", 1), await val("k2", "b"): await val("v2", 2)}
    print("dict display order", r, trace)

    trace.clear()
    a, b = await val("tuple", (1, 2))
    print("unpack awaited", a, b)

    trace.clear()
    r = 0 < await val("mid", 5) < await val("hi", 10)
    print("chained comparison", r, trace)

    trace.clear()
    r = await val("in", 3) in await val("container", [1, 2, 3])
    print("membership", r, trace)

    x = 1
    x += await val("aug", 41)
    print("augmented assignment", x)

    items = [0, 0]
    items[await val("i", 1)] = await val("v", "set")
    print("subscript assignment", items)

    class Obj:
        pass

    o = Obj()
    o.attr = await val("attr", "attr value")
    print("attribute assignment", o.attr, (await val("o", o)).attr)

    r = (lambda v: v * 2)(await val("lam", 21))
    print("lambda arg", r)

    match await val("match", (1, "two")):
        case (1, label):
            print("match subject", label)

    while (n := await val("walrus", len(trace))) < 0:
        pass
    print("walrus await in while", n > 0)

    print("await in t-string interpolation", (t"{await val('ts', 7)}").values)

    async def returns_coro():
        return val("deferred", "double await")

    print("double await", await (await returns_coro()))


run(main())
