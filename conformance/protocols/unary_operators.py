"""Unary operators __neg__, __pos__, __invert__, __abs__, plus `not` and stacked unary operators."""


class Sym:
    def __init__(self, s):
        self.s = s

    def __repr__(self):
        return self.s

    def __neg__(self):
        return Sym(f"(-{self.s})")

    def __pos__(self):
        return Sym(f"(+{self.s})")

    def __invert__(self):
        return Sym(f"(~{self.s})")

    def __abs__(self):
        return Sym(f"|{self.s}|")


x = Sym("x")
print("neg", -x)
print("pos", +x)
print("invert", ~x)
print("abs", abs(x))
print("stacked", -+~x, - -x, ~~x, -abs(-x))
print("not uses truthiness", not x)


class Vec:
    def __init__(self, *c):
        self.c = c

    def __repr__(self):
        return f"Vec{self.c}"

    def __neg__(self):
        return Vec(*(-v for v in self.c))

    def __pos__(self):
        # Commonly used to return a copy
        return Vec(*self.c)

    def __abs__(self):
        return sum(v * v for v in self.c) ** 0.5


v = Vec(3, -4)
print("vec neg", -v, "pos copy", +v, (+v) is v, "abs", abs(v))

try:
    ~v
except TypeError as e:
    print("missing __invert__", type(e).__name__)


class Empty:
    pass


for label, fn in [("neg", lambda: -Empty()), ("pos", lambda: +Empty()), ("abs", lambda: abs(Empty()))]:
    try:
        fn()
    except TypeError as e:
        print("unsupported unary", label, type(e).__name__)

# Builtin unary semantics worth pinning
print("int unary", -5, +5, ~5, ~-1, abs(-7), -(-0))
print("bool unary", -True, +False, abs(True), ~int(True))
print("float unary", -0.0, abs(-0.0), +(-2.5))
print("complex abs", abs(3 + 4j))

# __pos__ on Decimal-like class used for rounding semantics
class Rounded:
    def __init__(self, v):
        self.v = v

    def __pos__(self):
        return Rounded(round(self.v, 2))

    def __repr__(self):
        return f"Rounded({self.v})"


print("pos rounding", +Rounded(1.23456))

# Unary on int subclass returns int, not subclass
class MyInt(int):
    pass


print("subclass unary types", type(-MyInt(3)).__name__, type(abs(MyInt(-3))).__name__, type(~MyInt(1)).__name__)

# Unary operators are looked up on the type
class Inst:
    def __neg__(self):
        return "class neg"


obj = Inst()
obj.__neg__ = lambda: "instance neg"
print("type lookup", -obj)

# Precedence: -x**2 is -(x**2); ~ binds tighter than binary ops
print("precedence", -2 ** 2, (-2) ** 2, ~1 + 1, not 1 == 2, -3 % 5)


class Power:
    def __init__(self, v):
        self.v = v

    def __pow__(self, o):
        return Power(f"({self.v}**{o})")

    def __neg__(self):
        return Power(f"-{self.v}")

    def __repr__(self):
        return str(self.v)


print("custom precedence", -Power("a") ** 2)
