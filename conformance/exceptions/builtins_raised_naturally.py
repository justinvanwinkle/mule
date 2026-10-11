"""Builtin exceptions raised naturally by operations, caught and identified by type."""


def attempt(label, fn):
    try:
        result = fn()
    except Exception as e:
        print(label, "->", type(e).__name__)
    else:
        print(label, "ok", result)


class Thing:
    __slots__ = ("x",)

    def __init__(self):
        self.x = 1


attempt("int div zero", lambda: 1 // 0)
attempt("int mod zero", lambda: 5 % 0)
attempt("float div zero", lambda: 1.0 / 0.0)
attempt("divmod zero", lambda: divmod(3, 0))
attempt("dict missing key", lambda: {"a": 1}["b"])
attempt("list index", lambda: [1, 2, 3][3])
attempt("list negative index", lambda: [1, 2, 3][-4])
attempt("tuple index", lambda: ()[0])
attempt("str index", lambda: "abc"[10])
attempt("list pop empty", lambda: [].pop())
attempt("dict popitem empty", lambda: {}.popitem())
attempt("attribute missing", lambda: Thing().y)
attempt("attribute on int", lambda: (5).nope)
attempt("slots assign", lambda: setattr(Thing(), "z", 2))
attempt("add str int", lambda: "a" + 1)
attempt("call non-callable", lambda: 5())
attempt("len of int", lambda: len(7))
attempt("unhashable key", lambda: {[1]: 2})
attempt("int of junk", lambda: int("12x"))
attempt("float of junk", lambda: float("nanx"))
attempt("list remove absent", lambda: [1, 2].remove(3))
attempt("str index absent", lambda: "abc".index("z"))
attempt("unpack too many", lambda: (lambda: [a for a, b in [(1, 2, 3)]])())
attempt("chr out of range", lambda: chr(-1))
attempt("name error", lambda: undefined_name_here)  # noqa: F821
attempt("float pow overflow", lambda: 10.0 ** 400)
attempt("float pow overflow int base", lambda: 2.5 ** 10000)
attempt("float mul overflow gives inf", lambda: 1e308 * 10)
attempt("int to float overflow", lambda: float(10 ** 400))
attempt("next exhausted", lambda: next(iter([])))
attempt("next exhausted gen", lambda: next(x for x in ()))
attempt("next default", lambda: next(iter([]), "dflt"))
attempt("negative shift", lambda: 1 << -1)
attempt("bytes out of range", lambda: bytes([256]))
attempt("set remove", lambda: set().remove(1))


def unbound():
    if False:
        local = 1
    return local  # noqa: F821


attempt("unbound local", unbound)


def unbound_after_del():
    v = 1
    del v
    return v


attempt("unbound after del", unbound_after_del)
try:
    unbound()
except NameError as e:
    print("UnboundLocalError caught as NameError:", type(e).__name__)


def recurse(n):
    return recurse(n + 1) + 1


try:
    recurse(0)
except RecursionError as e:
    print("recursion caught", type(e).__name__, isinstance(e, RuntimeError))
# Still usable after recovering from recursion.
print("after recursion", sum(range(10)))


def mutual_a(n):
    return mutual_b(n + 1)


def mutual_b(n):
    return mutual_a(n + 1)


try:
    mutual_a(0)
except RuntimeError as e:
    print("mutual recursion caught as RuntimeError:", type(e).__name__)

it = iter([1])
print("first next", next(it))
try:
    next(it)
except StopIteration as e:
    print("StopIteration value", e.value, e.args)


def gen_returning():
    yield 1
    return "done"


g = gen_returning()
next(g)
try:
    next(g)
except StopIteration as e:
    print("generator return value", e.value, e.args)
