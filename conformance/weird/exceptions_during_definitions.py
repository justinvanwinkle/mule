"""Exceptions raised while building things: f-string fields, def-time defaults, decorators, class bodies, __set_name__, __init_subclass__."""


def boom(msg):
    raise ValueError(msg)


log = []


def note(x):
    log.append(x)
    return x


# f-string: earlier fields are evaluated, later ones are not
try:
    s = f"{note('a')}-{boom('in fstring')}-{note('c')}"
except ValueError as e:
    print("fstring raised", e, "evaluated", log)
log.clear()

# nested f-string with format spec computed by a raising expression
try:
    s = f"{3.14159:{note('.')}{boom('in spec')}f}"
except ValueError as e:
    print("format spec raised", e, log)
log.clear()

# Format spec itself invalid -> ValueError from format()
try:
    f"{42:q}"
except ValueError as e:
    print("bad format spec", type(e).__name__)

# Default arguments evaluated left to right at def time; failure means name never bound
try:
    def f(a=note(1), b=boom("in default"), c=note(3)):
        return a
except ValueError as e:
    print("default raised", e, log)
print("f bound?", "f" in globals())
log.clear()

# keyword-only defaults are evaluated after positional defaults
def order(a=note("pos-a"), *, k=note("kw-k"), b=note("kw-b")):
    pass


print("default order", log)
log.clear()

# Decorator raising: function name never bound, decorators below already ran
def tracer(name):
    note(("factory", name))

    def deco(fn):
        note(("apply", name))
        if name == "bad":
            raise RuntimeError("deco failed")
        return fn
    return deco


try:
    @tracer("top")
    @tracer("bad")
    @tracer("bottom")
    def decorated():
        pass
except RuntimeError as e:
    print("decorator raised", e, log)
print("decorated bound?", "decorated" in globals())
log.clear()

# Class body raising midway: class not bound, earlier statements ran
try:
    class Half:
        note("body start")
        x = 1
        boom("in class body")
        note("never")
except ValueError as e:
    print("class body raised", e, log)
print("Half bound?", "Half" in globals())
log.clear()


class Picky:
    def __set_name__(self, owner, name):
        note(("set_name", name))
        if name.startswith("bad"):
            raise AttributeError(f"refuse {name}")


try:
    class Holder:
        good = Picky()
        bad_one = Picky()
        other = Picky()
except Exception as e:
    print("set_name raised", type(e).__name__, "cause", type(e.__cause__).__name__ if e.__cause__ else None)
    print("set_name calls before failure", log)
log.clear()


class Base:
    def __init_subclass__(cls, /, limit=None, **kw):
        super().__init_subclass__(**kw)
        note(("init_subclass", cls.__name__, limit))
        if limit is not None and limit < 0:
            raise TypeError("negative limit")


class Ok(Base, limit=3):
    pass


try:
    class NotOk(Base, limit=-1):
        pass
except TypeError as e:
    print("init_subclass raised", e)
print("subclass log", log, "NotOk" in globals())
log.clear()

# Exception in a lambda default inside a comprehension at def time
try:
    fs = [lambda v=10 // k: v for k in (2, 1, 0)]
except ZeroDivisionError as e:
    print("lambda default in comprehension", type(e).__name__)

# Annotations are lazy in 3.14: a bad annotation does not fail the def
def lazy(x: undefined_name) -> also_undefined:
    return x


print("lazy annotations def ok", lazy(5))
try:
    lazy.__annotations__
except NameError as e:
    print("annotations evaluated on access", type(e).__name__)
