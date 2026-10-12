"""Numeric conversion dunders: __int__, __float__, __complex__, __round__, __trunc__, __floor__, __ceil__, __abs__."""

import math


class Num:
    def __init__(self, v):
        self.v = v

    def __repr__(self):
        return f"Num({self.v})"

    def __int__(self):
        return int(self.v) + 1000

    def __float__(self):
        return float(self.v) + 0.5

    def __complex__(self):
        return complex(self.v, 1)

    def __round__(self, ndigits=None):
        if ndigits is None:
            return f"round({self.v})"
        return f"round({self.v}, {ndigits})"

    def __trunc__(self):
        return f"trunc({self.v})"

    def __floor__(self):
        return f"floor({self.v})"

    def __ceil__(self):
        return f"ceil({self.v})"

    def __abs__(self):
        return Num(abs(self.v))


n = Num(-2.7)
print("int()", int(n))
print("float()", float(n))
print("complex()", complex(n))
print("round", round(n), round(n, 2), round(n, None))
print("math.trunc/floor/ceil", math.trunc(n), math.floor(n), math.ceil(n))
print("abs", abs(n))


# Without conversion dunders
class Bare:
    pass


for label, fn in [("int", int), ("float", float), ("complex", complex), ("round", round),
                  ("trunc", math.trunc), ("floor", math.floor), ("ceil", math.ceil), ("abs", abs)]:
    try:
        fn(Bare())
    except TypeError as e:
        print("bare", label, type(e).__name__)


# Fallback chains
class OnlyIndex:
    def __index__(self):
        return 7


class OnlyFloat:
    def __float__(self):
        return 2.5


class OnlyTrunc:
    def __trunc__(self):
        return 9


print("int via __index__", int(OnlyIndex()), "float via __index__", float(OnlyIndex()), "complex via __index__", complex(OnlyIndex()))
print("complex via __float__", complex(OnlyFloat()))
try:
    int(OnlyFloat())
except TypeError as e:
    print("int ignores __float__", type(e).__name__)
try:
    print("int via __trunc__", int(OnlyTrunc()))
except TypeError as e:
    print("int via __trunc__ removed", type(e).__name__)
print("math.floor via __float__", math.floor(OnlyFloat()), math.ceil(OnlyFloat()))


# Return-type validation
class BadInt:
    def __int__(self):
        return "3"


class BadFloat:
    def __float__(self):
        return 3


class BadComplex:
    def __complex__(self):
        return 1.0


for label, fn in [("int", lambda: int(BadInt())), ("float", lambda: float(BadFloat())), ("complex", lambda: complex(BadComplex()))]:
    try:
        fn()
    except TypeError as e:
        print("bad return", label, type(e).__name__)


# Builtin round semantics (banker's rounding, ndigits)
print("round builtins", round(0.5), round(1.5), round(2.5), round(-0.5), round(2.675, 2), round(1234, -2), round(1250, -2), round(1350, -2))
print("round types", type(round(2.5)).__name__, type(round(2.5, 0)).__name__, int.__name__)
print("math funcs on floats", math.trunc(-2.7), math.floor(-2.7), math.ceil(-2.7), math.floor(3), math.ceil(-0.0))
print("int() truncation", int(-2.7), int(2.999), int("  -12 "), int("ff", 16), int("0b101", 0), int(True))
print("float parse", float(" 1e3 "), float("-inf"), float("nan") != float("nan"), float(10 ** 20))
print("complex parse", complex("1+2j"), complex(1, -0.0), complex("3"), complex(2.0))
try:
    float(10 ** 400)
except OverflowError as e:
    print("int too big for float", type(e).__name__)
try:
    int(float("inf"))
except OverflowError as e:
    print("int(inf)", type(e).__name__)
try:
    int(float("nan"))
except ValueError as e:
    print("int(nan)", type(e).__name__)


# __round__ used by round() on a Fraction-like class returning instances
class Frac:
    def __init__(self, num, den):
        self.num, self.den = num, den

    def __round__(self, ndigits=None):
        q = self.num / self.den
        return round(q, ndigits) if ndigits is not None else round(q)

    def __float__(self):
        return self.num / self.den

    def __int__(self):
        return self.num // self.den


f = Frac(22, 7)
print("Frac", round(f), round(f, 3), float(f), int(f), math.floor(f), f"{float(f):.4f}")

# Conversions on int/float subclasses return exact base types
class MyFloat(float):
    pass


class MyInt(int):
    pass


print("subclass conversions", type(float(MyFloat(1.5))).__name__, type(int(MyInt(3))).__name__, type(int(MyFloat(2.5))).__name__)
print("subclass math", type(math.floor(MyFloat(2.5))).__name__, type(round(MyInt(7))).__name__, type(abs(MyInt(-7))).__name__)
print("int.__round__ ndigits", round(MyInt(1234), -1), round(15, -1), round(25, -1))

# divmod and pow on builtin mixed types
print("divmod mixed", divmod(7, 2), divmod(-7, 2), divmod(7.5, 2), divmod(-7.5, 2))
print("pow types", pow(2, -1), pow(-8, 1 / 3).real.__class__.__name__, pow(4, 0.5), (-2) ** 2, 2 ** 0.5 == math.sqrt(2))
print("math.fsum / isclose", math.fsum([0.1] * 10), math.isclose(0.1 + 0.2, 0.3))
