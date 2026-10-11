"""Context managers: __enter__ return values, __exit__ arguments and suppression, multiple and parenthesized managers."""

log = []


class CM:
    def __init__(self, name, suppress=False, enter_value=None):
        self.name = name
        self.suppress = suppress
        self.enter_value = enter_value

    def __enter__(self):
        log.append(f"enter {self.name}")
        return self if self.enter_value is None else self.enter_value

    def __exit__(self, exc_type, exc, tb):
        kind = exc_type.__name__ if exc_type else None
        log.append(f"exit {self.name} {kind} {exc.args if exc else None} tb={tb is not None}")
        return self.suppress

    def __repr__(self):
        return f"CM({self.name})"


with CM("a") as a:
    log.append(f"body sees {a!r}")
print("basic", log)

log.clear()
with CM("b", enter_value=42) as v:
    log.append(f"as value {v}")
print("enter value", log)

log.clear()
with CM("c"):
    log.append("no as target")
print("no target", log)

# Exception propagates when __exit__ returns falsy
log.clear()
try:
    with CM("d"):
        raise ValueError("boom", 1)
except ValueError as e:
    log.append(f"caught {e.args}")
print("propagates", log)

# Suppression when __exit__ returns truthy
log.clear()
with CM("e", suppress=True):
    raise KeyError("hidden")
    log.append("unreachable")
log.append("after suppressed with")
print("suppressed", log)


# Truthy non-bool return value suppresses too
class Truthy:
    def __enter__(self):
        return None

    def __exit__(self, *a):
        return "yes"


with Truthy():
    1 / 0
print("truthy string suppresses")

# Multiple managers: enter left to right, exit right to left
log.clear()
with CM("x") as x, CM("y") as y, CM("z", enter_value="Z") as z:
    log.append(f"body {x!r} {y!r} {z}")
print("multiple", log)

# Parenthesized multi-line with (3.10+)
log.clear()
with (
    CM("p1") as p1,
    CM("p2"),
    CM("p3", enter_value=[1, 2]) as (first, second),
):
    log.append(f"paren body {p1!r} {first} {second}")
print("parenthesized", log)

# Inner suppressing manager hides exception from outer
log.clear()
with CM("outer"), CM("inner", suppress=True):
    raise RuntimeError("inner-only")
print("inner suppress", log)

# Exception in second __enter__: first's __exit__ still runs
class BadEnter:
    def __enter__(self):
        log.append("bad enter")
        raise OSError("enter failed")

    def __exit__(self, *a):
        log.append("bad exit (should not run)")


log.clear()
try:
    with CM("first"), BadEnter():
        log.append("body never")
except OSError as e:
    log.append(f"caught {type(e).__name__}")
print("enter failure", log)

# Return / break / continue inside with still run __exit__
def ret():
    with CM("ret"):
        return "returned"


log.clear()
print("return in with", ret(), log)
log.clear()
for i in range(3):
    with CM(f"loop{i}"):
        if i == 0:
            continue
        if i == 1:
            break
print("break/continue", log)

# `as` target can be any assignment target
log.clear()
store = {}


class Obj:
    pass


o = Obj()
with CM("t1", enter_value="dict-target") as store["k"], CM("t2", enter_value="attr") as o.attr:
    pass
print("complex targets", store, o.attr)

# __enter__/__exit__ looked up on type; missing __exit__ is TypeError before __enter__
class OnlyEnter:
    def __enter__(self):
        log.append("only enter")


log.clear()
try:
    with OnlyEnter():
        pass
except TypeError as e:
    print("missing __exit__", type(e).__name__, log)

# contextlib-based generator managers
from contextlib import contextmanager, suppress, nullcontext, ExitStack


@contextmanager
def tag(name):
    log.append(f"<{name}>")
    try:
        yield name.upper()
    finally:
        log.append(f"</{name}>")


log.clear()
with tag("a") as A, tag("b") as B:
    log.append(A + B)
print("contextmanager", log)

with suppress(ZeroDivisionError, KeyError):
    {}["x"]
print("suppress ok")
with nullcontext(5) as n:
    print("nullcontext", n)

log.clear()
with ExitStack() as stack:
    for nm in ("s1", "s2", "s3"):
        stack.enter_context(CM(nm))
    stack.callback(lambda: log.append("callback"))
print("ExitStack", log)
