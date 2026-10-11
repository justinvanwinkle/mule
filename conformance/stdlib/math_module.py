"""math: floor/ceil/trunc, gcd/lcm multi-arg, isqrt, comb/perm, factorial, fsum vs sum, isclose, copysign, inf/nan, prod, dist, hypot."""
import math

for x in [2.5, -2.5, 3.0, -0.5, 7]:
    print("floor/ceil/trunc", x, math.floor(x), math.ceil(x), math.trunc(x), type(math.floor(x)).__name__)
print("round half even", round(2.5), round(3.5), round(-0.5))
print("big floor", math.floor(1e20), math.ceil(-1e20))
try:
    math.floor(math.inf)
except OverflowError as e:
    print("floor inf", type(e).__name__)
try:
    math.ceil(math.nan)
except ValueError as e:
    print("ceil nan", type(e).__name__)

print("gcd", math.gcd(12, 18), math.gcd(12, 18, 8), math.gcd(), math.gcd(0, 0), math.gcd(-4, 6))
print("lcm", math.lcm(4, 6), math.lcm(4, 6, 10), math.lcm(), math.lcm(0, 5), math.lcm(-4, 6))
print("isqrt", math.isqrt(0), math.isqrt(15), math.isqrt(16), math.isqrt(10**40 + 1))
try:
    math.isqrt(-1)
except ValueError as e:
    print("isqrt neg", type(e).__name__)
print("comb", math.comb(10, 3), math.comb(5, 0), math.comb(3, 5), math.comb(60, 30))
print("perm", math.perm(5, 2), math.perm(5), math.perm(3, 4))
print("factorial", math.factorial(0), math.factorial(10), math.factorial(25))
try:
    math.factorial(-1)
except ValueError as e:
    print("factorial neg", type(e).__name__)
try:
    math.factorial(2.0)
except TypeError as e:
    print("factorial float", type(e).__name__)

vals = [0.1] * 10
print("sum vs fsum", sum(vals), math.fsum(vals))
running = 0.0
for v in vals:
    running += v
print("naive loop", running, running == 1.0)
print("fsum cancellation", math.fsum([1e100, 1.0, -1e100]), sum([1e100, 1.0, -1e100]))
print("isclose", math.isclose(0.1 + 0.2, 0.3), 0.1 + 0.2 == 0.3)
print("isclose tol", math.isclose(100, 101, rel_tol=0.01), math.isclose(100, 102, rel_tol=0.01))
print("isclose zero", math.isclose(0.0, 1e-12), math.isclose(0.0, 1e-12, abs_tol=1e-9))
print("isclose inf/nan", math.isclose(math.inf, math.inf), math.isclose(math.nan, math.nan))

print("copysign", math.copysign(3, -0.0), math.copysign(-2.5, 1), math.copysign(1, math.copysign(0.0, -1)))
print("inf", math.inf, -math.inf, math.inf > 1e308, math.inf - math.inf)
nan = math.nan
print("nan", nan == nan, nan != nan, math.isnan(nan), math.isinf(-math.inf), math.isfinite(1e308))
print("nan in list identity", nan in [nan], float("nan") in [float("nan")])
print("inf arithmetic", math.inf * 0, 1 / math.inf, -1 / math.inf, math.inf + 1 == math.inf)
print("float inf parse", float("-inf"), float("Infinity"), float("nan"))
print("max with nan", max(nan, 1), max(1, nan))
print("sorted with inf", sorted([3, -math.inf, 0, math.inf]))

print("prod", math.prod([1, 2, 3, 4]), math.prod([]), math.prod([2, 3], start=10), math.prod([0.5, 4]))
print("dist", math.dist((0, 0), (3, 4)), math.dist([1, 1, 1], [2, 2, 2]))
try:
    math.dist((0,), (1, 2))
except ValueError as e:
    print("dist mismatch", type(e).__name__)
print("hypot", math.hypot(3, 4), math.hypot(), math.hypot(1, 2, 2), math.hypot(-5))
print("sqrt/exp/log", math.sqrt(2), math.exp(0), math.log(math.e), math.log(8, 2), math.log10(1000), math.log2(1024))
try:
    math.sqrt(-1)
except ValueError as e:
    print("sqrt neg", type(e).__name__)
try:
    math.log(0)
except ValueError as e:
    print("log zero", type(e).__name__)
print("pow/fmod", math.pow(2, 10), math.fmod(-7, 3), -7 % 3)
print("modf/frexp/ldexp", math.modf(-3.25), math.frexp(8.0), math.ldexp(0.5, 4))
print("trig", math.sin(0), math.cos(0), round(math.degrees(math.pi), 9), math.radians(180) == math.pi)
print("constants", math.pi, math.e, math.tau)
print("remainder", math.remainder(7, 2), math.remainder(5, 2))
print("big int log", math.log10(10**400))
print("sumprod", math.sumprod([1, 2, 3], [4, 5, 6]))
print("nextafter/ulp", math.nextafter(1.0, 2.0) - 1.0 == math.ulp(1.0))
print("fma", math.fma(2.0, 3.0, 1.0))
