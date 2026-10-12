"""Argument binding errors: missing, duplicate, unexpected, too many, and positional-only by keyword."""


def f(a, b, c=3):
    return (a, b, c)


def g(a, /, b, *, c):
    return (a, b, c)


def h(*args, **kwargs):
    return (args, sorted(kwargs.items()))


def try_call(label, thunk):
    try:
        print(label, "ok", thunk())
    except Exception as e:
        print(label, "raised", type(e).__name__)


try_call("missing one", lambda: f(1))
try_call("missing all", lambda: f())
try_call("too many", lambda: f(1, 2, 3, 4))
try_call("duplicate", lambda: f(1, 2, a=5))
try_call("unexpected kw", lambda: f(1, 2, d=4))
try_call("pos-only by kw", lambda: g(a=1, b=2, c=3))
try_call("kw-only positionally", lambda: g(1, 2, 3))
try_call("missing kw-only", lambda: g(1, 2))
try_call("h accepts anything", lambda: h(1, 2, x=3))
try_call("dup via unpack", lambda: f(1, a=2, b=3))
try_call("dup between two unpacks", lambda: h(**{"x": 1}, **{"x": 2}))
try_call("non-str key unpack", lambda: h(**{1: 2}))
try_call("non-mapping unpack", lambda: h(**[1, 2]))
try_call("non-iterable star", lambda: h(*5))
five = 5
nothing = None
try_call("call non-callable", lambda: five())
try_call("call None", lambda: nothing())


def zero():
    return "zero"


try_call("zero ok", lambda: zero())
try_call("zero with arg", lambda: zero(1))
try_call("zero with kw", lambda: zero(x=1))


class Meth:
    def m(self, x):
        return x

    @staticmethod
    def s(x):
        return x

    @classmethod
    def c(cls, x):
        return (cls.__name__, x)


obj = Meth()
try_call("method ok", lambda: obj.m(1))
try_call("method missing", lambda: obj.m())
try_call("method too many", lambda: obj.m(1, 2))
try_call("unbound missing self", lambda: Meth.m(1))
try_call("unbound with self", lambda: Meth.m(obj, 7))
try_call("static too many", lambda: Meth.s(1, 2))
try_call("classmethod ok", lambda: obj.c(2))
try_call("classmethod self dup", lambda: Meth.c(1, cls=2))


# The error is raised at call time, before the body runs.
events = []


def side_effect(a):
    events.append("body ran")
    return a


try:
    side_effect(1, 2)
except TypeError:
    events.append("caught")
print("no body run", events)


# Arguments are evaluated before binding errors are detected.
order = []


def note(x):
    order.append(x)
    return x


try:
    f(note(1), note(2), note(3), note(4))
except TypeError:
    order.append("TypeError")
print("arg eval before bind error", order)

# TypeError is catchable as Exception subclass and via tuple
try:
    f()
except (ValueError, TypeError) as e:
    print("tuple catch", type(e).__name__, isinstance(e, Exception))
