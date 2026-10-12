"""Exceptions inside generators and context managers: __exit__ arguments, suppression, chained replacements."""
from contextlib import contextmanager, suppress


class Tracker:
    def __init__(self, name, suppress_types=(), replace=None):
        self.name = name
        self.suppress_types = suppress_types
        self.replace = replace

    def __enter__(self):
        print(" enter", self.name)
        return self

    def __exit__(self, et, ev, tb):
        print(" exit", self.name, et.__name__ if et else None,
              repr(ev) if ev is not None else None, tb is not None)
        if self.replace is not None:
            raise self.replace
        return et is not None and issubclass(et, self.suppress_types)


print("normal exit:")
with Tracker("a") as t:
    print(" body uses", t.name)

print("propagating:")
try:
    with Tracker("b"):
        raise ValueError("boom")
except ValueError as e:
    print(" outside got", e)

print("suppressed:")
with Tracker("c", suppress_types=(KeyError,)):
    raise KeyError("swallowed")
print(" continued after suppression")

print("not suppressed type:")
try:
    with Tracker("d", suppress_types=(KeyError,)):
        raise IndexError("passes")
except IndexError as e:
    print(" outside got", type(e).__name__)

print("replaced in __exit__:")
try:
    with Tracker("e", replace=RuntimeError("replacement")):
        raise ValueError("original")
except RuntimeError as e:
    print(" replacement context", repr(e.__context__))

print("nested managers, inner suppresses:")
with Tracker("outer"), Tracker("inner", suppress_types=(ValueError,)):
    raise ValueError("inner only")

print("nested managers, outer suppresses:")
with Tracker("outer2", suppress_types=(ValueError,)), Tracker("inner2"):
    raise ValueError("to outer")


class TruthyExit:
    def __enter__(self):
        return None

    def __exit__(self, *a):
        return "non-empty string is truthy"


with TruthyExit():
    raise OSError("suppressed by truthy return")
print("truthy return suppresses")


@contextmanager
def managed(label, swallow=False):
    print(" cm setup", label)
    try:
        yield label.upper()
    except ValueError as e:
        print(" cm saw", e)
        if not swallow:
            raise
    finally:
        print(" cm teardown", label)


print("contextmanager swallow:")
with managed("x", swallow=True) as v:
    print(" got", v)
    raise ValueError("cm error")
print(" after swallow")

print("contextmanager reraise:")
try:
    with managed("y"):
        raise ValueError("cm error 2")
except ValueError as e:
    print(" outside", e)

with suppress(ZeroDivisionError, KeyError):
    1 / 0
print("contextlib.suppress worked")


# Generators: exceptions thrown in, raised out, and cleaned up.
def gen():
    try:
        yield 1
        yield 2
    except KeyError as e:
        print(" gen caught thrown", e)
        yield "recovered"
    finally:
        print(" gen finally")


g = gen()
print("gen next:", next(g))
print("gen throw:", g.throw(KeyError("thrown")))
try:
    next(g)
except StopIteration:
    print("gen exhausted")

g = gen()
next(g)
try:
    g.throw(ValueError("uncaught in gen"))
except ValueError as e:
    print("gen propagated throw:", e)


def gen_raises():
    yield 1
    raise LookupError("from generator body")


try:
    for x in gen_raises():
        print("gen value", x)
except LookupError as e:
    print("loop got", e)


def stop_iter_inside():
    yield 1
    raise StopIteration("bad")


try:
    list(stop_iter_inside())
except RuntimeError as e:
    print("StopIteration in generator becomes", type(e).__name__,
          "cause", type(e.__cause__).__name__)


def closer():
    try:
        yield 1
    except GeneratorExit:
        print(" saw GeneratorExit")
        raise


c = closer()
next(c)
c.close()


def bad_closer():
    try:
        yield 1
    except GeneratorExit:
        yield 2


b = bad_closer()
next(b)
try:
    b.close()
except RuntimeError as e:
    print("yield in GeneratorExit handler ->", type(e).__name__)
