"""Context manager error paths: exceptions raised in __exit__, chaining, re-raising, generator CM exceptions."""

from contextlib import contextmanager

log = []


class RaisingExit:
    def __init__(self, name, exc=None):
        self.name = name
        self.exc = exc

    def __enter__(self):
        log.append(f"enter {self.name}")
        return self

    def __exit__(self, et, ev, tb):
        log.append(f"exit {self.name} saw {et.__name__ if et else None}")
        if self.exc is not None:
            raise self.exc
        return False


# __exit__ raises during normal completion
log.clear()
try:
    with RaisingExit("a", ValueError("from exit")):
        log.append("body ok")
except ValueError as e:
    print("exit raises", e, e.__context__, log)

# __exit__ raises while handling a body exception: new exception, context chained
log.clear()
try:
    with RaisingExit("b", KeyError("exit-key")):
        raise TypeError("body-type")
except KeyError as e:
    print("exit replaces", type(e).__name__, type(e.__context__).__name__, e.__context__.args, log)

# Outer manager sees the exception raised by inner __exit__
log.clear()
try:
    with RaisingExit("outer"), RaisingExit("inner", RuntimeError("inner-exit")):
        pass
except RuntimeError as e:
    print("outer sees inner-exit error", e, log)


# __exit__ re-raising the same exception explicitly
class Reraise:
    def __enter__(self):
        return self

    def __exit__(self, et, ev, tb):
        if ev is not None:
            raise ev
        return False


try:
    with Reraise():
        raise IndexError("same")
except IndexError as e:
    print("explicit reraise", e.args, e.__context__ is None or type(e.__context__).__name__)


# __exit__ translating exception type with `from`
class Translate:
    def __enter__(self):
        return self

    def __exit__(self, et, ev, tb):
        if et is ZeroDivisionError:
            raise ArithmeticError("translated") from ev
        return False


try:
    with Translate():
        1 // 0
except ArithmeticError as e:
    print("translated", e, type(e.__cause__).__name__, e.__suppress_context__)


# __exit__ that suppresses only certain exceptions and records them
class Selective:
    def __init__(self):
        self.seen = []

    def __enter__(self):
        return self

    def __exit__(self, et, ev, tb):
        if et is None:
            return False
        self.seen.append(et.__name__)
        return issubclass(et, LookupError)


sel = Selective()
for exc in (KeyError, IndexError, ValueError):
    try:
        with sel:
            raise exc("x")
        print("selective suppressed", exc.__name__)
    except ValueError:
        print("selective propagated", exc.__name__)
print("selective seen", sel.seen)


# Generator-based CM: exception thrown into generator
@contextmanager
def guarded(name, swallow=False):
    log.append(f"setup {name}")
    try:
        yield name
    except Exception as e:
        log.append(f"gen caught {type(e).__name__}")
        if not swallow:
            raise
    finally:
        log.append(f"teardown {name}")


log.clear()
with guarded("g1", swallow=True):
    raise ValueError("swallowed")
print("generator swallow", log)

log.clear()
try:
    with guarded("g2"):
        raise ValueError("reraised")
except ValueError as e:
    print("generator reraise", e, log)


@contextmanager
def double_yield():
    yield 1
    yield 2


try:
    with double_yield():
        pass
except RuntimeError as e:
    print("generator didn't stop", type(e).__name__)


@contextmanager
def no_yield():
    if False:
        yield


try:
    with no_yield():
        pass
except RuntimeError as e:
    print("generator didn't yield", type(e).__name__)


# StopIteration raised inside body of generator CM is not confused with generator exhaustion
@contextmanager
def plain():
    yield


try:
    with plain():
        raise StopIteration("inner")
except StopIteration as e:
    print("StopIteration passes through", e.args)


# Exception in __enter__ of a generator CM (before yield)
@contextmanager
def fails_setup():
    raise LookupError("setup failed")
    yield


try:
    with fails_setup():
        print("never")
except LookupError as e:
    print("setup failure", e)

# The with statement's exception info inside __exit__ uses the actual exception instance
captured = {}


class Capture:
    def __enter__(self):
        return self

    def __exit__(self, et, ev, tb):
        captured["same"] = ev is err
        captured["type"] = et is type(ev)
        return True


err = OSError(2, "nope")
with Capture():
    raise err
print("exit gets same instance", captured)

# Exception group raised from body goes to __exit__ intact
with Capture():
    err = ExceptionGroup("eg", [ValueError(1), TypeError(2)])
    raise err
print("exception group to exit", captured)
