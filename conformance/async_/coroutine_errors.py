"""Coroutine exceptions: throw/close on suspended coroutines, finally on close, exception chaining across awaits."""

import inspect

log = []


class Suspend:
    def __await__(self):
        yield "suspended"


async def guarded():
    try:
        await Suspend()
        return "normal"
    except ValueError as e:
        return "caught " + str(e)
    finally:
        log.append("guarded finally")


c = guarded()
print("suspend", c.send(None), inspect.getcoroutinestate(c))
try:
    c.throw(ValueError("thrown"))
except StopIteration as e:
    print("throw handled", e.value, log)

log.clear()
c = guarded()
c.send(None)
c.close()
print("close runs finally", log, inspect.getcoroutinestate(c))

log.clear()
c = guarded()
c.send(None)
try:
    c.throw(KeyError("unhandled"))
except KeyError as e:
    print("throw unhandled", type(e).__name__, log)


async def inner():
    await Suspend()
    raise ValueError("inner failure")


async def middle():
    try:
        await inner()
    except ValueError as e:
        raise RuntimeError("middle failure") from e


async def top():
    try:
        await middle()
    except RuntimeError as e:
        return str(e), str(e.__cause__), type(e.__cause__).__name__


def run(coro):
    try:
        while True:
            coro.send(None)
    except StopIteration as e:
        return e.value


print("explicit chaining", run(top()))


async def implicit_context():
    try:
        try:
            await inner()
        except ValueError:
            raise LookupError("during handling")
    except LookupError as e:
        return type(e.__context__).__name__, e.__suppress_context__


print("implicit context", run(implicit_context()))


async def awaits_suspend_throw_into_chain():
    try:
        await nested_await()
    except ZeroDivisionError:
        return "outer caught thrown exception"


async def nested_await():
    await Suspend()


c = awaits_suspend_throw_into_chain()
c.send(None)
try:
    c.throw(ZeroDivisionError)
except StopIteration as e:
    print("throw reaches through await chain", e.value)


async def finally_with_await():
    try:
        await Suspend()
    finally:
        log.append("finally before await")
        await Suspend()
        log.append("finally after await")


log.clear()
c = finally_with_await()
c.send(None)
try:
    c.close()
except RuntimeError as e:
    print("await in finally during close", type(e).__name__, log)


async def exception_groups():
    try:
        raise ExceptionGroup("group", [ValueError(1), TypeError(2), ValueError(3)])
    except* ValueError as eg:
        log.append(("values", len(eg.exceptions)))
    except* TypeError as eg:
        log.append(("types", len(eg.exceptions)))
    return "groups handled"


log.clear()
print("except* in coroutine", run(exception_groups()), log)


async def except_unparenthesized():
    try:
        await inner()
    except KeyError, ValueError:
        return "PEP 758 except list"


print("unparenthesized except", run(except_unparenthesized()))


async def base_exception_passes():
    try:
        await Suspend()
    except Exception:
        return "wrongly caught"


c = base_exception_passes()
c.send(None)
try:
    c.throw(KeyboardInterrupt)
except KeyboardInterrupt:
    print("BaseException not caught by except Exception")


async def stop_iteration_in_coroutine():
    raise StopIteration("bad")


try:
    run(stop_iteration_in_coroutine())
except RuntimeError as e:
    print("StopIteration in coroutine", type(e).__name__, type(e.__cause__).__name__)


async def return_in_finally_overrides():
    try:
        raise ValueError
    finally:
        return "finally wins"


print("return in finally", run(return_in_finally_overrides()))
