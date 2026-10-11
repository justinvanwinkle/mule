"""NotImplemented fallbacks across both operands, resulting TypeError, and NotImplemented in comparisons."""

log = []


class A:
    def __init__(self, handles):
        self.handles = handles

    def __repr__(self):
        return "A"

    def __add__(self, o):
        log.append("A.__add__")
        return "A wins" if "add" in self.handles else NotImplemented

    def __radd__(self, o):
        log.append("A.__radd__")
        return "A rwins" if "radd" in self.handles else NotImplemented

    def __eq__(self, o):
        log.append("A.__eq__")
        return NotImplemented

    def __lt__(self, o):
        log.append("A.__lt__")
        return NotImplemented


class B:
    def __init__(self, handles):
        self.handles = handles

    def __repr__(self):
        return "B"

    def __add__(self, o):
        log.append("B.__add__")
        return "B wins" if "add" in self.handles else NotImplemented

    def __radd__(self, o):
        log.append("B.__radd__")
        return "B rwins" if "radd" in self.handles else NotImplemented

    def __eq__(self, o):
        log.append("B.__eq__")
        return NotImplemented

    def __gt__(self, o):
        log.append("B.__gt__")
        return "B.__gt__ result" if "gt" in self.handles else NotImplemented


def attempt(label, fn):
    log.clear()
    try:
        r = fn()
    except TypeError as e:
        r = type(e).__name__
    print(label, r, log)


attempt("left handles", lambda: A({"add"}) + B(set()))
attempt("right reflects", lambda: A(set()) + B({"radd"}))
attempt("neither", lambda: A(set()) + B(set()))
attempt("right wins ignored left radd", lambda: A({"radd"}) + B({"radd"}))

# Equality: both NotImplemented -> identity comparison fallback, never TypeError
attempt("eq both NotImplemented", lambda: A(set()) == B(set()))
x = A(set())
attempt("eq identity fallback", lambda: x == x)
attempt("ne both NotImplemented", lambda: A(set()) != B(set()))

# Ordering: A.__lt__ NotImplemented -> B.__gt__
attempt("lt reflects to gt", lambda: A(set()) < B({"gt"}))
attempt("lt neither", lambda: A(set()) < B(set()))


# Returning NotImplemented from a method called directly just returns the singleton
print("direct call", A(set()).__add__(1) is NotImplemented)
print("NotImplemented repr", repr(NotImplemented), type(NotImplemented).__name__)

# Using NotImplemented in boolean context is a TypeError (3.14)
try:
    bool(NotImplemented)
except TypeError as e:
    print("bool(NotImplemented)", type(e).__name__)


# Mixed with builtins: int.__add__ returns NotImplemented for unknown types
print("int.__add__ str", (1).__add__("x") is NotImplemented, (1).__add__(2))
print("float.__radd__ int", (1.5).__radd__(1))
try:
    [].__add__(())
except TypeError as e:
    print("list.__add__ raises directly", type(e).__name__)


# Raising TypeError inside __add__ does NOT trigger reflection
class Raiser:
    def __add__(self, o):
        log.append("Raiser.__add__")
        raise TypeError("explicit")


class Catcher:
    def __radd__(self, o):
        log.append("Catcher.__radd__")
        return "caught"


attempt("raise does not reflect", lambda: Raiser() + Catcher())


# A class implementing only the reflected method for every op
class Sink:
    def __repr__(self):
        return "Sink"


for name in ["radd", "rsub", "rmul", "rtruediv", "rfloordiv", "rmod", "rpow", "rlshift",
             "rrshift", "rand", "ror", "rxor", "rmatmul"]:
    setattr(Sink, f"__{name}__", (lambda nm: lambda self, o: f"{nm}({o})")(name))

s = Sink()
print("sink", 1 + s, 1 - s, 1 * s, 1 / s, 1 // s, 1 % s, 1 ** s)
print("sink2", 1 << s, 1 >> s, 1 & s, 1 | s, 1 ^ s, 1 @ s)

# Unary operators have no fallback
try:
    -Sink()
except TypeError as e:
    print("unary no fallback", type(e).__name__)


# Comparison between unrelated builtins
for label, fn in [("int<str", lambda: 1 < "a"), ("list<tuple", lambda: [1] < (1,)), ("None<None", lambda: None < None)]:
    try:
        fn()
    except TypeError as e:
        print("builtin compare", label, type(e).__name__)
print("int==str", 1 == "1", [1] == (1,), None == 0)
