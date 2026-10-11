"""Exception chaining: implicit __context__, explicit `from` (__cause__, __suppress_context__), `from None`, walking chains."""


def describe(e):
    return (f"{type(e).__name__}({e}) cause={e.__cause__!r} "
            f"context={type(e.__context__).__name__ if e.__context__ else None} "
            f"suppress={e.__suppress_context__}")


def implicit():
    try:
        raise ValueError("a")
    except ValueError:
        raise TypeError("b")


def explicit():
    try:
        raise ValueError("a")
    except ValueError as e:
        raise TypeError("b") from e


def from_none():
    try:
        raise ValueError("a")
    except ValueError:
        raise TypeError("b") from None


def from_unrelated():
    raise TypeError("b") from KeyError("k")


def from_class():
    raise TypeError("b") from KeyError


for fn in (implicit, explicit, from_none, from_unrelated, from_class):
    try:
        fn()
    except TypeError as e:
        print(fn.__name__, "|", describe(e))


# from None still records the context, it is only suppressed for display.
try:
    from_none()
except TypeError as e:
    print("from None keeps context:", repr(e.__context__))

# Fresh exceptions start unchained.
fresh = ValueError("fresh")
print("fresh", fresh.__cause__, fresh.__context__, fresh.__suppress_context__)

# Assigning __cause__ directly also sets __suppress_context__ to True.
e = RuntimeError("manual")
e.__cause__ = KeyError("k")
print("manual cause suppress", e.__suppress_context__, repr(e.__cause__))
try:
    e.__cause__ = 5
except TypeError as err:
    print("non-exception cause ->", type(err).__name__)


def three_deep():
    try:
        try:
            try:
                raise KeyError("level1")
            except KeyError as e1:
                raise ValueError("level2") from e1
        except ValueError:
            raise RuntimeError("level3")
    except RuntimeError as e3:
        raise LookupError("level4") from e3


def walk(e):
    parts = []
    while e is not None:
        if e.__cause__ is not None:
            link = "cause"
            nxt = e.__cause__
        elif e.__context__ is not None and not e.__suppress_context__:
            link = "context"
            nxt = e.__context__
        else:
            link = "end"
            nxt = None
        parts.append(f"{type(e).__name__}:{e.args[0]}[{link}]")
        e = nxt
    return " <- ".join(parts)


try:
    three_deep()
except LookupError as e:
    print("chain:", walk(e))


# Context is the exception being handled when the new one is raised, even from a callee.
def callee():
    raise OSError("in callee")


try:
    try:
        1 / 0
    except ZeroDivisionError:
        callee()
except OSError as e:
    print("callee context:", type(e.__context__).__name__)


# Re-raising an exception that already has a context does not overwrite it with itself,
# and raising inside a finally after a handled exception has no context.
try:
    try:
        raise ValueError("x")
    except ValueError:
        pass
    finally:
        raise KeyError("in finally")
except KeyError as e:
    print("finally after handled context:", e.__context__)

# Context cycle avoidance: raising the context exception again.
try:
    try:
        raise ValueError("A")
    except ValueError as a:
        try:
            raise TypeError("B")
        except TypeError:
            raise a
except ValueError as e:
    ctx = e.__context__
    print("cycle broken:", type(ctx).__name__, ctx.__context__)

# Chain created inside a generator.
def gen():
    try:
        yield 1
        raise ValueError("gen inner")
    except ValueError as e:
        raise RuntimeError("gen outer") from e


try:
    list(gen())
except RuntimeError as e:
    print("generator chain:", walk(e))
