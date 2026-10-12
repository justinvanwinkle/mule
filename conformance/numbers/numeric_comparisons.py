"""Numeric tower comparisons and chained comparison semantics."""

print("tower eq", 1 == 1.0 == True, 0 == 0.0 == False == -0.0, 1 == 1.0 == (1 + 0j))
print("chained", 1 < 2 < 3, 1 < 3 < 2, 3 > 2 > 1 > 0, 1 <= 1 < 2, 1 == 1 != 2)
print("chain mixed types", 1 < 1.5 < 2 < 2**64 < float("inf"))

big = 2**53 + 1
print("bigint vs float exact", big == float(big), big > float(big), float(big) < big, float(2**53) == 2**53)
print("bigint vs float order", 2**1000 < float("inf"), -(2**1000) > float("-inf"), 2**1024 > 1.7976931348623157e308)
print("bigint vs nan", 2**100 == float("nan"), 2**100 < float("nan"), 2**100 > float("nan"))
print("int vs float near", 9007199254740993 == 9007199254740992.0, 9007199254740992 == 9007199254740992.0)
print("float vs int fraction", 1.5 > 1, 1.5 < 2, -1.5 < -1, -0.5 > -1)
print("compare 0.1", 0.1 + 0.2 == 0.3, 0.1 + 0.2 > 0.3, 0.5 + 0.25 == 0.75)
print("bool vs float", True < 1.5, False > -0.1, True == 1.0)
print("max/min mixed", max(1, 1.0), max(1.0, 1), min(0, -0.0), min(-0.0, 0), max(True, 1), max(2**70, 1e21))


def noisy(label, value):
    print("  eval", label)
    return value


print("short-circuit chain:")
result = noisy("a", 1) < noisy("b", 0) < noisy("c", 5)
print("result", result)
print("middle evaluated once:")
result = noisy("a", 1) < noisy("b", 2) < noisy("c", 3)
print("result", result)

try:
    print(1 < "1")
except TypeError as e:
    print("int < str", type(e).__name__)
try:
    print(1 < None)
except TypeError as e:
    print("int < None", type(e).__name__)
print("int == str", 1 == "1", 1 != "1", None == 0)
print("sort mixed numeric", sorted([3, 1.5, True, -2, 2**64, 0.0, -0.5]))
print("tuple compare", (1, 2.0) == (1.0, 2), (1, 2) < (1, 2.5), (1, float("nan")) == (1, float("nan")))
print("list compare", [1, 2] < [1, 2, 0], [2] > [1, 100], [1.0] == [True])

import decimal
import fractions

print("fraction compare", fractions.Fraction(1, 2) == 0.5, fractions.Fraction(1, 3) < 0.34, fractions.Fraction(3, 1) == 3)
print("decimal compare", decimal.Decimal("0.5") == 0.5, decimal.Decimal(1) == 1, decimal.Decimal("0.1") == 0.1)
