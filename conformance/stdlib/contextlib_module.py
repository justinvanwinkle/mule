"""contextlib: contextmanager (incl. exception handling in the generator), suppress, ExitStack LIFO callbacks, closing, nullcontext, redirect_stdout, ContextDecorator, AbstractContextManager."""
import io
from contextlib import (
    AbstractContextManager,
    ContextDecorator,
    ExitStack,
    closing,
    contextmanager,
    nullcontext,
    redirect_stdout,
    suppress,
)

log = []


@contextmanager
def tag(name):
    log.append(f"<{name}>")
    try:
        yield name.upper()
    except ValueError as e:
        log.append(f"handled {e}")
    finally:
        log.append(f"</{name}>")


with tag("a") as t:
    log.append(f"body {t}")
print("basic", log)
log.clear()
with tag("b"):
    raise ValueError("oops")
print("swallowed", log)
log.clear()
try:
    with tag("c"):
        raise KeyError("k")
except KeyError:
    log.append("outer caught KeyError")
print("propagated", log)


@contextmanager
def reraiser():
    try:
        yield
    except ZeroDivisionError as e:
        raise RuntimeError("wrapped") from e


try:
    with reraiser():
        1 / 0
except RuntimeError as e:
    print("translated", e, type(e.__cause__).__name__)


@contextmanager
def no_yield():
    if False:
        yield


try:
    with no_yield():
        pass
except RuntimeError as e:
    print("no yield", type(e).__name__)


@contextmanager
def double_yield():
    yield 1
    yield 2


try:
    with double_yield():
        pass
except RuntimeError as e:
    print("double yield", type(e).__name__)

with suppress(KeyError, IndexError):
    {}["x"]
    print("not reached")
print("suppress continues")
try:
    with suppress(KeyError):
        [][0]
except IndexError:
    print("suppress other type propagates")
with suppress(LookupError):
    [][1]
print("suppress base class")
with suppress():
    pass
print("suppress no args ok")

order = []


@contextmanager
def ordered(name):
    order.append(f"enter {name}")
    yield name
    order.append(f"exit {name}")


with ExitStack() as stack:
    stack.callback(order.append, "cb0")
    got = stack.enter_context(ordered("ctx1"))
    stack.callback(order.append, "cb1")
    stack.enter_context(ordered("ctx2"))
    order.append(f"body got {got}")
print("ExitStack LIFO", order)
order.clear()
with ExitStack() as stack:
    stack.callback(order.append, "kept")
    moved = stack.pop_all()
    order.append("exited first stack")
print("pop_all deferred", order)
moved.close()
print("after close", order)


def suppressor(exc_type, exc, tb):
    order.append(f"saw {exc_type.__name__}")
    return True


order.clear()
with ExitStack() as stack:
    stack.push(suppressor)
    stack.callback(order.append, "cleanup")
    raise TypeError("boom")
print("push exit suppresses", order)


class Resource:
    def __init__(self, name):
        self.name = name
        self.closed = False

    def close(self):
        self.closed = True
        print("closing", self.name)


with closing(Resource("r1")) as r:
    print("inside closing", r.name, r.closed)
print("closed", r.closed)
with nullcontext(42) as v:
    print("nullcontext value", v)
with nullcontext() as v:
    print("nullcontext none", v)


def maybe_ctx(use):
    return tag("real") if use else nullcontext("fake")


for use in (True, False):
    with maybe_ctx(use) as v:
        print("optional ctx", v)

buf = io.StringIO()
with redirect_stdout(buf):
    print("captured line 1")
    print("captured", 2, sep="-")
print("redirect captured", repr(buf.getvalue()))


class timed(ContextDecorator):
    def __enter__(self):
        print("enter decorator")
        return self

    def __exit__(self, *exc):
        print("exit decorator", exc[0].__name__ if exc[0] else None)
        return False


@timed()
def work(x):
    print("working", x)
    return x * 2


print("ContextDecorator result", work(5))


class Simple(AbstractContextManager):
    def __exit__(self, *exc):
        print("Simple exit")


with Simple() as s:
    print("default __enter__ returns self", type(s).__name__)

with tag("x") as x, tag("y") as y:
    pass
print("multi-with order", log[-4:])
