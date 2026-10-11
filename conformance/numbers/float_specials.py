"""Float special values: inf, nan, negative zero, comparisons, and nan in containers."""

import math

inf = float("inf")
ninf = float("-inf")
nan = float("nan")

print("reprs", inf, ninf, nan, -nan, float("Infinity"), float("-INF"), float("NaN"), float("+nan"))
print("inf arithmetic", inf + 1, inf - inf, inf * 0, inf * -1, inf / inf, 1 / inf, -1 / inf)
print("nan arithmetic", nan + 1, nan * 0, nan - nan)
print("comparisons", inf > 10**300, ninf < -(10**300), inf == inf, inf == ninf)
print("nan comparisons", nan == nan, nan != nan, nan < 1, nan > 1, nan <= nan, nan >= nan)
print("isnan/isinf", math.isnan(nan), math.isinf(ninf), math.isfinite(1e308), math.isfinite(inf))

nz = -0.0
print("neg zero", nz, 0.0 == -0.0, math.copysign(1, nz), math.copysign(1, 0.0))
print("neg zero ops", nz + 0.0, nz - 0.0, nz * 1, -0.0 * -1, 0.0 * -1, abs(nz))
print("neg zero str", str(-0.0), repr(-0.0), -0.0 < 0.0)
print("int of neg zero", int(-0.0))
print("neg zero in list", [0.0].count(-0.0), -0.0 in [0.0], {0.0: "pos"}[-0.0])

print("nan in list via identity", nan in [nan], [nan].count(nan), [nan] == [nan])
print("fresh nan not identical", float("nan") in [float("nan")])
print("nan index", [1, nan, 2].index(nan))
t = (nan,)
print("tuple eq identity", t == t, (nan,) == (nan,), t == (float("nan"),))
d = {nan: "x"}
print("dict nan key identity", d[nan], float("nan") in d)
print("max/min with nan first", max(nan, 1), min(nan, 1))
print("max/min with nan later", max(1, nan), min(1, nan))
print("sorted with inf", sorted([3.0, inf, -1.0, ninf, 0.0]))

print("overflow to inf", 1e308 * 10, -1e308 * 10, float("1e309"), float("-1e400"))
print("underflow", 1e-320, 5e-324, 5e-324 / 2, 2.2250738585072014e-308)
print("max float", 1.7976931348618157e308, 1.7976931348623157e308)
try:
    print(int(inf))
except OverflowError as e:
    print("int(inf)", type(e).__name__)
try:
    print(int(nan))
except ValueError as e:
    print("int(nan)", type(e).__name__)
try:
    print(math.sqrt(-1))
except ValueError as e:
    print("sqrt(-1)", type(e).__name__)
try:
    print(1.0 / 0.0)
except ZeroDivisionError as e:
    print("1.0/0.0", type(e).__name__)

print("bool of specials", bool(nan), bool(inf), bool(-0.0))
print("round inf", end=" ")
try:
    round(inf)
except OverflowError as e:
    print(type(e).__name__)
print("round nan", end=" ")
try:
    round(nan)
except ValueError as e:
    print(type(e).__name__)
print("round inf ndigits", round(inf, 2), round(nan, 2))
print("math.inf", math.inf == inf, math.isnan(math.nan))
