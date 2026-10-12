"""float methods: hex, fromhex, as_integer_ratio, is_integer, and related builtins."""

import math

for x in [1.0, 0.5, 0.1, -2.75, 0.0, -0.0, 1e300, 5e-324, 3.0, 255.5]:
    print("hex", x, x.hex())

print("fromhex", float.fromhex("0x1.8p1"), float.fromhex("-0x1p-2"), float.fromhex("0xA"), float.fromhex("  0x1.fffffffffffffp+1023  "))
print("fromhex specials", float.fromhex("inf"), float.fromhex("-infinity"), float.fromhex("nan"))
print("fromhex short forms", float.fromhex("1p4"), float.fromhex("0x.8"), float.fromhex("0X1P-1"))
try:
    float.fromhex("0x1p99999")
except OverflowError as e:
    print("fromhex overflow", type(e).__name__)
try:
    float.fromhex("xyz")
except ValueError as e:
    print("fromhex bad", type(e).__name__)
print("hex roundtrip", all(float.fromhex(v.hex()) == v for v in [0.1, 1 / 3, 1e-310, 123.456]))

for x in [0.5, 0.1, -1.25, 3.0, 1e20, 0.0]:
    print("as_integer_ratio", x, x.as_integer_ratio())
for x in [float("inf"), float("nan")]:
    try:
        x.as_integer_ratio()
    except (OverflowError, ValueError) as e:
        print("as_integer_ratio special", x, type(e).__name__)

for x in [1.0, 1.5, -2.0, 0.0, 1e300, float("inf"), float("nan")]:
    print("is_integer", x, x.is_integer())

print("real/imag/conjugate", (2.5).real, (2.5).imag, (-1.5).conjugate())
print("float() idempotent", 2.5, (-0.0))
print("float dunders", (2.5).__trunc__(), (2.5).__floor__(), (2.5).__ceil__(), (-2.5).__round__(), (2.675).__round__(2))
print("float.__int__", (3.99).__int__(), (-3.99).__int__())
print("math.modf", math.modf(3.75), math.modf(-3.75))
print("math.frexp/ldexp", math.frexp(8.0), math.frexp(0.1), math.ldexp(0.5, 4))
print("math.isclose", math.isclose(0.1 + 0.2, 0.3), math.isclose(1.0, 1.1, rel_tol=0.2))
print("math.nextafter", math.nextafter(1.0, 2.0), math.nextafter(0.0, 1.0))
print("math.ulp", math.ulp(1.0), math.ulp(0.0))
print("math.copysign", math.copysign(3, -0.0), math.copysign(-3, 1))
print("math.fabs", math.fabs(-3), math.fabs(-0.0))
print("divmod method", (7.5).__divmod__(2), (7.5).__rdivmod__(20))


class MyFloat(float):
    def __repr__(self):
        return f"MyFloat({float(self)!r})"


f = MyFloat(1.5)
print("subclass", f, f * 2, type(f * 2).__name__, f.is_integer(), f.hex())
print("fromhex on subclass", type(MyFloat.fromhex("0x1p1")).__name__, MyFloat.fromhex("0x1p1"))
