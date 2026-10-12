"""__repr__, __str__, __format__: fallbacks, containers using repr, format specs through f-strings and format()."""


class Both:
    def __repr__(self):
        return "Both(repr)"

    def __str__(self):
        return "Both(str)"


class ReprOnly:
    def __repr__(self):
        return "ReprOnly()"


class StrOnly:
    def __str__(self):
        return "StrOnly-str"

    def __repr__(self):
        # define repr too to keep output deterministic
        return "StrOnly-repr"


b, r, s = Both(), ReprOnly(), StrOnly()
print("print uses str", b, r, s)
print("str()", str(b), str(r), str(s))
print("repr()", repr(b), repr(r), repr(s))
print("containers use repr", [b, r, s], (b,), {"k": b})
print("f-string conversions", f"{b}", f"{b!r}", f"{b!s}", f"{r}", f"{b!a}")
print("percent", "%s %r" % (b, b))
print("str.format", f"{b} {b!r} {b!s}")
print("ascii", ascii(r), ascii("café"))


class Money:
    def __init__(self, cents):
        self.cents = cents

    def __format__(self, spec):
        if spec == "":
            return f"${self.cents / 100:.2f}"
        if spec == "c":
            return f"{self.cents}c"
        if spec.startswith("w"):
            width = int(spec[1:])
            return f"${self.cents / 100:.2f}".rjust(width, "*")
        return format(self.cents / 100, spec)

    def __str__(self):
        return "Money-str"

    def __repr__(self):
        return f"Money({self.cents})"


m = Money(12345)
print("format default", f"{m}", format(m), f"{m}")
print("format specs", f"{m:c}", f"{m:w12}", f"{m:,.1f}", format(m, ">10.0f"))
print("str vs format", str(m), f"{m!s}", f"{m!r}")
width = 12
print("nested spec", f"{m:w{width}}", f"{m:{'c'}}")
print("format method spec", f"{m:c} {m:w9}", f"{m:c}")


# __format__ inherited from object: empty spec -> str(), non-empty spec -> TypeError
class Plain:
    def __str__(self):
        return "plain-str"


print("object format empty", format(Plain()), f"{Plain()}")
try:
    format(Plain(), "10")
except TypeError as e:
    print("object format nonempty", type(e).__name__)


# __format__ must return str
class BadFormat:
    def __format__(self, spec):
        return 5


try:
    f"{BadFormat()}"
except TypeError as e:
    print("bad __format__", type(e).__name__)


# __str__ must return str
class BadStr:
    def __str__(self):
        return 1

    def __repr__(self):
        return "BadStr"


try:
    str(BadStr())
except TypeError as e:
    print("bad __str__", type(e).__name__)


# str subclass returned by __str__ is allowed
class S(str):
    pass


class ReturnsSub:
    def __str__(self):
        return S("subclass-str")


print("str subclass result", str(ReturnsSub()), type(str(ReturnsSub())).__name__)


# Recursive container reprs
lst = [1]
lst.append(lst)
d = {}
d["self"] = d
print("recursive repr", lst, d)


# __repr__ with nested objects calls repr recursively
class Node:
    def __init__(self, v, kids=()):
        self.v, self.kids = v, list(kids)

    def __repr__(self):
        return f"Node({self.v!r}, {self.kids!r})" if self.kids else f"Node({self.v!r})"


print("nested repr", Node("a", [Node(1), Node("b", [Node(None)])]))

# Builtin format specs pinned
n = 1234567.891
print("builtin specs", f"{n:,.2f}", f"{n:_.0f}", f"{42:08b}", f"{255:#x}", f"{-3:+d}", f"{0.25:.1%}", f"{'ab':^6}|", f"{'x':-<4}|")
print("int with str spec", format(10, ""), format(10, "d"), f"{10!r:>5}")
print("debug f-string", f"{m=}", f"{1+1=}", f"{m = !s}")

# __format__ called for int subclass spec
class Hex(int):
    def __format__(self, spec):
        if spec == "h":
            return hex(self)
        return super().__format__(spec)


print("int subclass format", f"{Hex(255):h}", f"{Hex(255):05d}", f"{Hex(255)}")

# PEP 750 template strings keep interpolations unformatted
t = t"value {m:c} and {b!r}"
print("t-string parts", list(t.strings), [(i.expression, i.conversion, i.format_spec) for i in t.interpolations])
print("t-string values", [type(i.value).__name__ for i in t.interpolations])
