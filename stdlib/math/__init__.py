"""Compiled real math operations and exact integer algorithms."""

__mule_builtin_implementation__ = True

from _mule_native import math_acos
from _mule_native import math_cos
from _mule_native import math_exp
from _mule_native import math_float_bits
from _mule_native import inf
from _mule_native import nan
from _mule_native import math_lgamma
from _mule_native import math_log
from _mule_native import math_log2
from _mule_native import math_number_kind
from _mule_native import math_special_method
from _mule_native import math_sin
from _mule_native import math_sqrt
from _mule_native import math_to_double

from . import integer
from .integer import math_integer_bit_length, _index, comb, factorial, gcd, isqrt, lcm, perm

__all__ = (
    "acos",
    "asin",
    "atan",
    "atan2",
    "ceil",
    "comb",
    "copysign",
    "cos",
    "cosh",
    "degrees",
    "dist",
    "e",
    "erf",
    "erfc",
    "exp",
    "fabs",
    "factorial",
    "floor",
    "fma",
    "fmod",
    "frexp",
    "fsum",
    "gcd",
    "hypot",
    "inf",
    "integer",
    "isclose",
    "isfinite",
    "isinf",
    "isnan",
    "isqrt",
    "lcm",
    "ldexp",
    "lgamma",
    "log",
    "log10",
    "log2",
    "modf",
    "nan",
    "nextafter",
    "perm",
    "pi",
    "pow",
    "prod",
    "radians",
    "remainder",
    "sin",
    "sinh",
    "sqrt",
    "sumprod",
    "tan",
    "tanh",
    "tau",
    "trunc",
    "ulp",
)

pi = 3.141592653589793
e = 2.718281828459045
tau = 6.283185307179586


def _as_float(value):
    if math_number_kind(value) != 0:
        return math_to_double(value)
    method = math_special_method(value, "__float__")
    if method is not None:
        result = method()
        if math_number_kind(result) != 2:
            raise TypeError("__float__ returned a non-float")
        return result
    return math_to_double(_index(value))


def sqrt(x, /):
    value = _as_float(x)
    if value < 0.0:
        raise ValueError("math domain error")
    return math_sqrt(value)


def _logarithm(x, primitive, binary):
    if math_number_kind(x) == 1:
        x = _index(x)
        if x <= 0:
            raise ValueError("math domain error")
        bits = math_integer_bit_length(x)
        if bits > 1023:
            shift = bits - 53
            leading = math_to_double(x >> shift)
            correction = float(shift) if binary else float(shift) * math_log(2.0)
            return primitive(leading) + correction
    value = _as_float(x)
    if value <= 0.0:
        raise ValueError("math domain error")
    return primitive(value)


def log(x, base=None, /):
    result = _logarithm(x, math_log, False)
    if base is None:
        return result
    return result / _logarithm(base, math_log, False)


def log2(x, /):
    return _logarithm(x, math_log2, True)


def log10(x, /):
    if math_number_kind(x) == 1:
        n = _index(x)
        if n <= 0:
            raise ValueError("math domain error")
        exp10 = 0
        tmp = n
        while tmp >= 10 and tmp % 10 == 0:
            tmp //= 10
            exp10 += 1
        if tmp == 1:
            return float(exp10)
    else:
        v = _as_float(x)
        if v <= 0.0:
            raise ValueError("math domain error")
        if v == inf:
            return inf
        if v != v:
            return v
        if v >= 1.0 and v == floor(v):
            n = int(v)
            exp10 = 0
            tmp = n
            while tmp >= 10 and tmp % 10 == 0:
                tmp //= 10
                exp10 += 1
            if tmp == 1:
                return float(exp10)
    return _logarithm(x, math_log, False) / math_log(10.0)


def exp(x, /):
    value = _as_float(x)
    result = math_exp(value)
    if result == inf and value != inf:
        raise OverflowError("math range error")
    return result


def sin(x, /):
    value = _as_float(x)
    if value == inf or value == -inf:
        raise ValueError("math domain error")
    return math_sin(value)


def cos(x, /):
    value = _as_float(x)
    if value == inf or value == -inf:
        raise ValueError("math domain error")
    return math_cos(value)


def acos(x, /):
    value = _as_float(x)
    if value < -1.0 or value > 1.0:
        raise ValueError("math domain error")
    return math_acos(value)


def lgamma(x, /):
    value = _as_float(x)
    if value == inf or value == -inf:
        return inf
    if value <= 0.0 and value == floor(value):
        raise ValueError("math domain error")
    result = math_lgamma(value)
    if result == inf:
        raise OverflowError("math range error")
    return result


def fabs(x, /):
    return abs(_as_float(x))


def floor(x, /):
    method = math_special_method(x, "__floor__")
    if method is not None:
        return method()
    return _as_float(x).__floor__()


def ceil(x, /):
    method = math_special_method(x, "__ceil__")
    if method is not None:
        return method()
    return _as_float(x).__ceil__()


def isfinite(x, /):
    value = _as_float(x)
    return value == value and value != inf and value != -inf


def isinf(x, /):
    value = _as_float(x)
    return value == inf or value == -inf


def isnan(x, /):
    value = _as_float(x)
    return value != value


def copysign(x, y, /):
    magnitude = abs(_as_float(x))
    sign_bit = math_float_bits(_as_float(y)) >> 63
    return -magnitude if sign_bit else magnitude


def trunc(x, /):
    method = math_special_method(x, "__trunc__")
    if method is not None:
        return method()
    raise TypeError(f"type {type(x).__name__} doesn't define __trunc__ method")


def modf(x, /):
    v = _as_float(x)
    if v != v:
        return (v, v)
    if v == inf or v == -inf:
        return (copysign(0.0, v), v)
    whole = float(int(v))
    if whole == 0.0:
        whole = copysign(0.0, v)
    return (copysign(v - whole, v), whole)


def frexp(x, /):
    v = _as_float(x)
    if v == 0.0 or v != v or v == inf or v == -inf:
        return (v, 0)
    bits = math_float_bits(v)
    negative = bits >> 63
    exponent = (bits >> 52) & 0x7FF
    fraction = bits & ((1 << 52) - 1)
    if exponent == 0:
        exp_out = -1021
        while fraction < (1 << 52):
            fraction <<= 1
            exp_out -= 1
        mantissa = fraction / float(1 << 53)
    else:
        mantissa = ((1 << 52) | fraction) / float(1 << 53)
        exp_out = exponent - 1022
    return (-mantissa if negative else mantissa, exp_out)


def ldexp(x, i, /):
    v = _as_float(x)
    if isinstance(i, float):
        raise TypeError("Expected an int as second argument to ldexp.")
    exp_val = _index(i)
    if v == 0.0 or v != v or v == inf or v == -inf or exp_val == 0:
        return v
    if exp_val > 2098:
        raise OverflowError("math range error")
    if exp_val < -2098:
        return copysign(0.0, v)
    e = exp_val
    res = v
    while e > 1023:
        res *= 2.0 ** 1023
        e -= 1023
        if res == inf or res == -inf:
            raise OverflowError("math range error")
    while e < -1022:
        res *= 2.0 ** -1022
        e += 1022
    res *= 2.0 ** e
    if res == inf or res == -inf:
        raise OverflowError("math range error")
    return res


def isclose(a, b, *, rel_tol=1e-09, abs_tol=0.0):
    a = _as_float(a)
    b = _as_float(b)
    rel_tol = _as_float(rel_tol)
    abs_tol = _as_float(abs_tol)
    if rel_tol < 0.0 or abs_tol < 0.0:
        raise ValueError("tolerances must be non-negative")
    if a == b:
        return True
    if a == inf or a == -inf or b == inf or b == -inf:
        return False
    diff = abs(b - a)
    return diff <= abs(rel_tol * b) or diff <= abs(rel_tol * a) or diff <= abs_tol


def ulp(x, /):
    v = _as_float(x)
    if v != v:
        return v
    v = abs(v)
    if v == inf:
        return inf
    bits = math_float_bits(v)
    exponent = (bits >> 52) & 0x7FF
    if exponent <= 52:
        return 5e-324
    return ldexp(1.0, exponent - 1023 - 52)


def nextafter(x, y, /):
    x = _as_float(x)
    y = _as_float(y)
    if x != x or y != y:
        return nan
    if x == y:
        return y
    if x == 0.0:
        return 5e-324 if y > 0.0 else -5e-324
    if x < 0.0:
        return -nextafter(-x, -y)
    if y > x:
        return x + ulp(x)
    bits = math_float_bits(x)
    exponent = (bits >> 52) & 0x7FF
    fraction = bits & ((1 << 52) - 1)
    if fraction == 0 and exponent > 1:
        return x - ulp(x) / 2.0
    res = x - ulp(x)
    return 0.0 if res == 0.0 else res


def atan2(y, x, /):
    y = _as_float(y)
    x = _as_float(x)
    if y != y or x != x:
        return nan
    if y == 0.0:
        y_neg = (math_float_bits(y) >> 63) != 0
        if (math_float_bits(x) >> 63) != 0:
            return -pi if y_neg else pi
        return -0.0 if y_neg else 0.0
    if x == 0.0:
        return -pi / 2.0 if y < 0.0 else pi / 2.0
    if (x == inf or x == -inf) and (y == inf or y == -inf):
        angle = pi / 4.0 if x > 0.0 else 3.0 * pi / 4.0
        return -angle if y < 0.0 else angle
    if x == inf or x == -inf:
        if x > 0.0:
            return -0.0 if y < 0.0 else 0.0
        return -pi if y < 0.0 else pi
    if y == inf or y == -inf:
        return -pi / 2.0 if y < 0.0 else pi / 2.0
    scale = abs(x) if abs(x) >= abs(y) else abs(y)
    nx = x / scale
    ny = y / scale
    ratio = nx / math_sqrt(nx * nx + ny * ny)
    if ratio > 1.0:
        ratio = 1.0
    elif ratio < -1.0:
        ratio = -1.0
    angle = math_acos(ratio)
    return -angle if y < 0.0 else angle


def _round_int_ldexp(mantissa, exp2, negative):
    if mantissa == 0:
        return -0.0 if negative else 0.0
    bits = math_integer_bit_length(mantissa)
    top_exp = exp2 + bits - 1
    if top_exp > 1023:
        raise OverflowError("math range error")
    if top_exp < -1075:
        return -0.0 if negative else 0.0
    low_exp = -1074 if top_exp - 52 < -1074 else top_exp - 52
    shift = exp2 - low_exp
    if shift >= 0:
        q = mantissa << shift
    else:
        rshift = -shift
        q = mantissa >> rshift
        rem = mantissa & ((1 << rshift) - 1)
        half = 1 << (rshift - 1)
        if rem > half or (rem == half and (q & 1)):
            q += 1
        if low_exp > -1074 and q >= (1 << 53):
            q >>= 1
            low_exp += 1
        if low_exp + 52 > 1023 and q >= (1 << 52):
            raise OverflowError("math range error")
    val = ldexp(float(q), low_exp)
    return -val if negative else val


def fsum(iterable, /):
    total_units = 0
    special = None
    has_pos_inf = False
    has_neg_inf = False
    for item in iterable:
        v = _as_float(item)
        if v != v:
            special = v
        elif v == inf:
            has_pos_inf = True
        elif v == -inf:
            has_neg_inf = True
        elif v != 0.0:
            bits = math_float_bits(v)
            exponent = (bits >> 52) & 0x7FF
            fraction = bits & ((1 << 52) - 1)
            if exponent:
                unit = ((1 << 52) | fraction) << (exponent - 1)
            else:
                unit = fraction
            if bits >> 63:
                total_units -= unit
            else:
                total_units += unit
    if special is not None:
        return special
    if has_pos_inf and has_neg_inf:
        raise ValueError("-inf + inf in fsum")
    if has_pos_inf:
        return inf
    if has_neg_inf:
        return -inf
    if total_units < 0:
        return _round_int_ldexp(-total_units, -1074, True)
    return _round_int_ldexp(total_units, -1074, False)


def prod(iterable, /, *, start=1):
    result = start
    for item in iterable:
        result = result * item
    return result


def sumprod(p, q, /):
    it_p = iter(p)
    it_q = iter(q)
    total = 0
    while True:
        try:
            x = next(it_p)
        except StopIteration:
            try:
                next(it_q)
            except StopIteration:
                return total
            raise ValueError("Inputs are not the same length")
        try:
            y = next(it_q)
        except StopIteration:
            raise ValueError("Inputs are not the same length")
        total = total + x * y


def hypot(*coordinates):
    vals = []
    has_inf = False
    has_nan = False
    max_val = 0.0
    for c in coordinates:
        v = abs(_as_float(c))
        if v == inf:
            has_inf = True
        elif v != v:
            has_nan = True
        else:
            if v > max_val:
                max_val = v
            vals.append(v)
    if has_inf:
        return inf
    if has_nan:
        return nan
    if max_val == 0.0:
        return 0.0
    s = 0.0
    for v in vals:
        r = v / max_val
        s += r * r
    return max_val * math_sqrt(s)


def dist(p, q, /):
    p_vals = [_as_float(x) for x in p]
    q_vals = [_as_float(x) for x in q]
    if len(p_vals) != len(q_vals):
        raise ValueError("both points must have the same number of dimensions")
    return hypot(*(a - b for a, b in zip(p_vals, q_vals)))


def pow(x, y, /):
    vx = _as_float(x)
    vy = _as_float(y)
    if vx == 1.0 or vy == 0.0:
        return 1.0
    if vx != vx or vy != vy:
        return nan
    if vx == 0.0 and vy < 0.0:
        raise ValueError("math domain error")
    if vx < 0.0 and isfinite(vx) and isfinite(vy) and vy != floor(vy):
        raise ValueError("math domain error")
    res = vx ** vy
    if isinstance(res, complex):
        raise ValueError("math domain error")
    return float(res)


def fmod(x, y, /):
    vx = _as_float(x)
    vy = _as_float(y)
    if vx != vx or vy != vy:
        return nan
    if vy == 0.0 or isinf(vx):
        raise ValueError("math domain error")
    if isinf(vy):
        return vx
    if vx == 0.0:
        return vx
    ax = abs(vx)
    ay = abs(vy)
    rem = ax % ay
    return copysign(rem, vx)


def remainder(x, y, /):
    vx = _as_float(x)
    vy = _as_float(y)
    if vx != vx or vy != vy:
        return nan
    if vy == 0.0 or isinf(vx):
        raise ValueError("math domain error")
    if isinf(vy):
        return vx
    if vx == 0.0:
        return vx
    ax = abs(vx)
    ay = abs(vy)
    rem = ax % ay
    half_y = 0.5 * ay
    if rem > half_y:
        rem -= ay
    elif rem == half_y:
        q = round((ax - rem) / ay)
        if q & 1:
            rem -= ay
    if rem == 0.0:
        rem = 0.0
    return -rem if vx < 0.0 else rem


def degrees(x, /):
    return _as_float(x) * (180.0 / pi)


def radians(x, /):
    return _as_float(x) * (pi / 180.0)


def fma(x, y, z, /):
    vx = _as_float(x)
    vy = _as_float(y)
    vz = _as_float(z)
    if vx != vx or vy != vy or vz != vz:
        return nan
    if (isinf(vx) and vy == 0.0) or (vx == 0.0 and isinf(vy)):
        raise ValueError("invalid operation in fma")
    return vx * vy + vz


def tan(x, /):
    v = _as_float(x)
    if v == inf or v == -inf:
        raise ValueError("math domain error")
    return math_sin(v) / math_cos(v)


def asin(x, /):
    v = _as_float(x)
    if v < -1.0 or v > 1.0:
        raise ValueError("math domain error")
    return atan2(v, math_sqrt(1.0 - v * v))


def atan(x, /):
    v = _as_float(x)
    return atan2(v, 1.0)


def sinh(x, /):
    v = _as_float(x)
    if v == 0.0 or v != v or isinf(v):
        return v
    return 0.5 * (exp(v) - exp(-v))


def cosh(x, /):
    v = _as_float(x)
    if v != v:
        return v
    if isinf(v):
        return inf
    return 0.5 * (exp(v) + exp(-v))


def tanh(x, /):
    v = _as_float(x)
    if v == 0.0 or v != v:
        return v
    if v > 20.0:
        return 1.0
    if v < -20.0:
        return -1.0
    ep = math_exp(v)
    em = math_exp(-v)
    return (ep - em) / (ep + em)


def erfc(x, /):
    v = _as_float(x)
    if v != v:
        return v
    if v == inf:
        return 0.0
    if v == -inf:
        return 2.0
    z = abs(v)
    t = 1.0 / (1.0 + 0.5 * z)
    ans = t * math_exp(
        -z * z
        - 1.26551223
        + t
        * (
            1.00002368
            + t
            * (
                0.37409196
                + t
                * (
                    0.09678418
                    + t
                    * (
                        -0.18628806
                        + t
                        * (
                            0.27886807
                            + t
                            * (
                                -1.13520398
                                + t
                                * (
                                    1.48851587
                                    + t * (-0.82215223 + t * 0.17087277)
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    return ans if v >= 0.0 else 2.0 - ans


def erf(x, /):
    v = _as_float(x)
    if v == 0.0 or v != v:
        return v
    return 1.0 - erfc(v) if v >= 0.0 else erfc(-v) - 1.0
