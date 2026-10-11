"""fractions.Fraction (arithmetic, normalization, limit_denominator, from float/str) and decimal.Decimal (local context precision, quantize rounding modes)."""
from fractions import Fraction
from decimal import Decimal, localcontext, getcontext, ROUND_HALF_EVEN, ROUND_HALF_UP, ROUND_DOWN, ROUND_CEILING, InvalidOperation, DivisionByZero
import decimal

a = Fraction(1, 3)
b = Fraction(2, 4)
print("normalized", b, b.numerator, b.denominator)
print("arith", a + b, a - b, a * b, a / b, -a, abs(-a))
print("pow", a ** 2, a ** -2, Fraction(4, 9) ** Fraction(1, 2))
print("floor div/mod", Fraction(7, 2) // 1, Fraction(7, 2) % 1, divmod(Fraction(7, 2), Fraction(1, 3)))
print("mixed int", a + 1, 2 * a, 1 / a)
print("mixed float", type(a + 0.5).__name__, a + 0.5)
print("compare", a < b, Fraction(1, 2) == 0.5, Fraction(2, 1) == 2, Fraction(1, 3) == 1 / 3)
print("negative denom", Fraction(3, -6), Fraction(-3, -6))
print("from str", Fraction("3/4"), Fraction("-1.25"), Fraction(" 7 "), Fraction("1e-3"))
print("from float exact", Fraction(0.1))
print("from_float", Fraction.from_float(0.25), Fraction.from_decimal(Decimal("1.1")))
print("limit_denominator", Fraction(0.1).limit_denominator(), Fraction(3.141592653589793).limit_denominator(100))
print("limit pi 1000", Fraction(3.141592653589793).limit_denominator(1000))
print("as_integer_ratio", Fraction(6, 4).as_integer_ratio(), (0.75).as_integer_ratio())
print("round", round(Fraction(5, 2)), round(Fraction(7, 2)), round(Fraction(1, 3), 2))
print("int/float", int(Fraction(-7, 2)), float(Fraction(1, 8)))
print("is_integer", Fraction(4, 2).is_integer(), Fraction(1, 2).is_integer())
print("sum fractions", sum(Fraction(1, n) for n in range(1, 7)))
print("repr", repr(Fraction(1, 3)), str(Fraction(5)))
print("hash eq int", hash(Fraction(5)) == hash(5), hash(Fraction(1, 2)) == hash(0.5))
print("format", format(Fraction(1, 3), ".5f"), f"{Fraction(22, 7):.3g}", f"{Fraction(1, 8):.1%}")
try:
    Fraction(1, 0)
except ZeroDivisionError as e:
    print("zero denom", type(e).__name__)
try:
    Fraction("abc")
except ValueError as e:
    print("bad str", type(e).__name__)

print("decimal exact", Decimal("0.1") + Decimal("0.2"), Decimal("0.1") + Decimal("0.2") == Decimal("0.3"))
print("decimal from float", Decimal(0.1))
print("default prec", getcontext().prec)
print("division", Decimal(1) / Decimal(7))
with localcontext() as ctx:
    ctx.prec = 5
    print("prec 5", Decimal(1) / Decimal(7), Decimal("1.23456789") + 0, Decimal("1.23456789"))
    with localcontext(prec=3):
        print("nested prec 3", Decimal(2).sqrt())
    print("back to 5", Decimal(2).sqrt())
print("restored", Decimal(1) / Decimal(3))
for s in ["0.125", "0.135", "2.5", "3.5", "-2.5", "1.005"]:
    d = Decimal(s)
    print("quantize", s,
          d.quantize(Decimal("0.01"), rounding=ROUND_HALF_EVEN),
          d.quantize(Decimal("0.01"), rounding=ROUND_HALF_UP),
          d.quantize(Decimal("1"), rounding=ROUND_HALF_EVEN),
          d.quantize(Decimal("1"), rounding=ROUND_HALF_UP))
print("other rounding", Decimal("-1.7").quantize(Decimal("1"), rounding=ROUND_DOWN), Decimal("1.1").quantize(Decimal("1"), rounding=ROUND_CEILING))
print("round()", round(Decimal("2.675"), 2), round(Decimal("2.5")))
print("trailing zeros", Decimal("1.50"), Decimal("1.50") == Decimal("1.5"), Decimal("1.50").normalize())
print("tuple", Decimal("-3.14").as_tuple())
print("int ops", Decimal(10) // 3, Decimal(-7) // 2, Decimal(-7) % 2, -7 % 2)
print("compare int", Decimal(3) == 3, Decimal("3.0") == 3, Decimal("0.5") < 1)
print("special", Decimal("Infinity"), Decimal("-0"), Decimal("NaN").is_nan())
print("sum", sum([Decimal("1.10"), Decimal("2.20")]))
print("exp ln", Decimal(1).exp(), Decimal(10).ln())
print("to_integral", Decimal("2.5").to_integral_value(), Decimal("3.5").to_integral_value())
print("str/float", str(Decimal("1E+3")), float(Decimal("0.5")), int(Decimal("-9.9")))
try:
    Decimal(1) / Decimal(0)
except DivisionByZero as e:
    print("div zero", type(e).__name__, isinstance(e, ZeroDivisionError))
try:
    Decimal("abc")
except InvalidOperation as e:
    print("bad decimal", type(e).__name__)
try:
    Decimal("1.1") + 1.1
except TypeError as e:
    print("decimal + float", type(e).__name__)
with localcontext() as ctx:
    ctx.traps[decimal.DivisionByZero] = False
    print("untrapped", Decimal(1) / Decimal(0), bool(ctx.flags[decimal.DivisionByZero]))
