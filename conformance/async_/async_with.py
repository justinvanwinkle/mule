"""async with: __aenter__/__aexit__ ordering, exception info, suppression, multiple managers, and errors."""

import contextlib

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


class Manager:
    def __init__(self, name, suppress=False):
        self.name = name
        self.suppress = suppress

    async def __aenter__(self):
        log.append("aenter " + self.name)
        await Suspend()
        return self.name.upper()

    async def __aexit__(self, exc_type, exc, tb):
        await Suspend()
        log.append("aexit " + self.name + " " + (exc_type.__name__ if exc_type else "None"))
        return self.suppress


async def basic():
    async with Manager("a") as value:
        log.append("body " + value)
    return "done"


log.clear()
print("basic", run(basic()), log)


async def multiple():
    async with Manager("x") as x, Manager("y") as y:
        log.append("body " + x + y)


log.clear()
run(multiple())
print("multiple managers", log)


async def parenthesized():
    async with (
        Manager("p"),
        Manager("q") as q,
    ):
        log.append("paren body " + q)


log.clear()
run(parenthesized())
print("parenthesized managers", log)


async def suppressed():
    async with Manager("s", suppress=True):
        raise ValueError("hidden")
    return "after suppressed"


log.clear()
print("suppressed", run(suppressed()), log)


async def propagated():
    try:
        async with Manager("e"):
            raise KeyError("visible")
    except KeyError as e:
        return "caught " + type(e).__name__


log.clear()
print("propagated", run(propagated()), log)


async def inner_suppresses_outer_sees_none():
    async with Manager("outer"):
        async with Manager("inner", suppress=True):
            raise ZeroDivisionError


log.clear()
run(inner_suppresses_outer_sees_none())
print("nested suppression", log)


async def return_inside():
    async with Manager("r"):
        return "returned from body"


log.clear()
print("return inside", run(return_inside()), log)


async def loop_control():
    out = []
    for i in range(3):
        async with Manager(str(i)):
            if i == 1:
                continue
            out.append(i)
    return out


log.clear()
print("continue inside", run(loop_control()), log)


class SyncOnly:
    def __enter__(self):
        return self

    def __exit__(self, *a):
        return False


async def wrong_protocol():
    try:
        async with SyncOnly():
            pass
    except TypeError as e:
        return type(e).__name__


print("sync manager in async with", run(wrong_protocol()))


class NotAwaitableEnter:
    def __aenter__(self):
        return "not awaitable"

    async def __aexit__(self, *a):
        return False


async def bad_aenter():
    try:
        async with NotAwaitableEnter():
            pass
    except TypeError as e:
        return type(e).__name__


print("non-awaitable __aenter__", run(bad_aenter()))


@contextlib.asynccontextmanager
async def acm(name):
    log.append("acm enter " + name)
    try:
        yield name * 2
    except RuntimeError:
        log.append("acm caught")
    finally:
        log.append("acm exit " + name)


async def uses_acm():
    async with acm("z") as v:
        log.append("acm body " + v)
    async with acm("w"):
        raise RuntimeError


log.clear()
run(uses_acm())
print("asynccontextmanager", log)


class ExitRaises:
    async def __aenter__(self):
        return self

    async def __aexit__(self, *a):
        raise LookupError("from aexit")


async def exit_raises():
    try:
        async with ExitRaises():
            raise ValueError("original")
    except LookupError as e:
        return type(e).__name__, type(e.__context__).__name__


print("__aexit__ raises", run(exit_raises()))
