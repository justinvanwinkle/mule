"""Exception groups split by except* nested inside context managers that suppress some branches."""


class Suppress:
    """Suppresses exception groups' members of the given types, re-raising the rest."""

    def __init__(self, *types):
        self.types = types
        self.seen = []

    def __enter__(self):
        return self

    def __exit__(self, et, ev, tb):
        if ev is None:
            self.seen.append("clean")
            return False
        if isinstance(ev, BaseExceptionGroup):
            match, rest = ev.split(self.types)
            self.seen.append(("matched", describe(match), "rest", describe(rest)))
            if rest is None:
                return True
            raise rest
        self.seen.append(("plain", type(ev).__name__))
        return isinstance(ev, self.types)


def describe(eg):
    if eg is None:
        return None
    if isinstance(eg, BaseExceptionGroup):
        return [describe(e) for e in eg.exceptions]
    return f"{type(eg).__name__}({eg})"


def raise_group():
    raise ExceptionGroup("outer", [
        ValueError("v1"),
        ExceptionGroup("inner", [TypeError("t1"), ValueError("v2")]),
        KeyError("k1"),
    ])


cm = Suppress(KeyError)
try:
    with cm:
        raise_group()
except* ValueError as eg:
    print("star ValueError", describe(eg))
except* TypeError as eg:
    print("star TypeError", describe(eg))
print("context manager saw", cm.seen)

# except* inside a with; the with suppresses whatever the handlers re-raise
cm2 = Suppress(OSError)
with cm2:
    try:
        raise ExceptionGroup("g", [ValueError("a"), OSError("b"), FileNotFoundError("c")])
    except* ValueError as eg:
        print("handled inside with", describe(eg))
print("outer with suppressed leftovers", cm2.seen)

# Handler that raises a new exception: combined with unhandled remainder
try:
    with Suppress(ZeroDivisionError) as cm3:
        try:
            raise ExceptionGroup("g", [ValueError("x"), TypeError("y"), ZeroDivisionError("z")])
        except* ValueError:
            raise RuntimeError("from handler")
except* RuntimeError as eg:
    print("raised in handler", describe(eg))
except* TypeError as eg:
    print("unhandled remainder", describe(eg))
print("cm3 saw", cm3.seen)

# A naked exception is wrapped into a group by except*
try:
    raise IndexError("naked")
except* IndexError as eg:
    print("naked wrapped", type(eg).__name__, describe(eg))

# except* with a tuple of types and nested structure preserved
try:
    raise ExceptionGroup("top", [
        ExceptionGroup("mid", [KeyError("a"), IndexError("b")]),
        ArithmeticError("c"),
    ])
except* (KeyError, ArithmeticError) as eg:
    print("tuple match preserves nesting", describe(eg), eg.message)
except* LookupError as eg:
    print("LookupError gets rest", describe(eg))

# Exiting handler with no exception: everything handled, flow continues
handled = []
for i in range(2):
    try:
        raise ExceptionGroup("loop", [ValueError(i)])
    except* ValueError:
        handled.append(i)
print("handled in loop", handled)

# contextlib.suppress with exception groups (3.12+ splits groups)
import contextlib

with contextlib.suppress(ValueError):
    raise ExceptionGroup("all values", [ValueError(1), ValueError(2)])
print("suppress swallowed full group")
try:
    with contextlib.suppress(ValueError):
        raise ExceptionGroup("mixed", [ValueError(1), TypeError(2)])
except ExceptionGroup as eg:
    print("suppress left remainder", describe(eg))

# ExitStack with multiple callbacks and a group
with contextlib.ExitStack() as stack:
    stack.callback(print, "callback 1")
    stack.callback(print, "callback 2")
    try:
        raise ExceptionGroup("es", [OSError("io")])
    except* OSError as eg:
        print("inside stack", describe(eg))

# Subclassed ExceptionGroup derive() keeps the subclass through split
class MyGroup(ExceptionGroup):
    def derive(self, excs):
        return MyGroup(self.message + "+", excs)


try:
    raise MyGroup("mine", [ValueError(1), TypeError(2)])
except* ValueError as eg:
    print("derived type", type(eg).__name__, eg.message)
except* TypeError as eg:
    print("derived type", type(eg).__name__, eg.message)

# Notes survive splitting
try:
    g = ExceptionGroup("noted", [ValueError("n")])
    g.add_note("a note")
    raise g
except* ValueError as eg:
    print("notes", eg.__notes__)
