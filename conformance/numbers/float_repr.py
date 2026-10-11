"""Float repr/str shortest round-trip formatting and exponent thresholds."""

samples = [
    0.1, 0.2, 0.1 + 0.2, 1.0, 1.5, -2.25, 100.0, 1e15, 1e16, 1e17, 123456789012345678.0,
    1e-4, 1e-5, 0.0001, 0.00001, 1.0e22, 1e23, 9.999999999999999e22, 3.141592653589793,
    2.0**53, 2.0**53 + 1, 1 / 3, 2 / 3, 5e-324, 1.7976931348623157e308, 0.30000000000000004,
    123.456, -0.000123, 1e100, 1.23e-100,
]
for x in samples:
    print("repr", repr(x), "str", str(x), "roundtrip", float(repr(x)) == x)

print("float from ints", float(1), float(-7), float(2**53), float(2**53 + 1), float(10**20))
print("float big int", float(2**1023), float(-(2**1000)))
try:
    float(2**1024)
except OverflowError as e:
    print("float(2**1024)", type(e).__name__)
print("int to float rounding", float(2**64 - 1), float(9007199254740993))
print("in containers", [0.1, 1e20, 1e-7], (2.5,), {"k": 1e16})
print("f-string default", f"{0.1 + 0.2}", f"{1e16}", f"{1e-5}")
print("format empty spec", format(1e16, ""), format(0.1, ""))
print("str format concat", "x=" + str(1.0) + ";" + repr(-0.5))

total = 0.0
for _ in range(10):
    total += 0.1
print("accumulated", total, total == 1.0)
print("sum vs fsum", sum([0.1] * 10))
import math
print("fsum", math.fsum([0.1] * 10))
