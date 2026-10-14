"""Compiled real math operations and exact integer algorithms."""

__mule_builtin_implementation__ = True

from _mule_native import math_acos as _acos
from _mule_native import math_cos as _cos
from _mule_native import math_exp as _exp
from _mule_native import math_inf as inf
from _mule_native import math_nan as nan
from _mule_native import math_lgamma as _lgamma
from _mule_native import math_log as _log
from _mule_native import math_log2 as _log2
from _mule_native import math_number_kind as _number_kind
from _mule_native import math_special_method as _special_method
from _mule_native import math_sin as _sin
from _mule_native import math_sqrt as _sqrt
from _mule_native import math_to_double as _to_double

from . import integer
from .integer import _bit_length, _index, comb, factorial, gcd, isqrt, lcm, perm

__all__ = (
    "acos",
    "ceil",
    "comb",
    "cos",
    "e",
    "exp",
    "fabs",
    "factorial",
    "floor",
    "gcd",
    "inf",
    "integer",
    "isfinite",
    "isinf",
    "isnan",
    "isqrt",
    "lcm",
    "lgamma",
    "log",
    "log2",
    "nan",
    "perm",
    "pi",
    "sin",
    "sqrt",
    "tau",
)

pi = 3.141592653589793
e = 2.718281828459045
tau = 6.283185307179586


def _as_float(value):
    if _number_kind(value) != 0:
        return _to_double(value)
    method = _special_method(value, "__float__")
    if method is not None:
        result = method()
        if _number_kind(result) != 2:
            raise TypeError("__float__ returned a non-float")
        return result
    return _to_double(_index(value))


def sqrt(x, /):
    value = _as_float(x)
    if value < 0.0:
        raise ValueError("math domain error")
    return _sqrt(value)


def _logarithm(x, primitive, binary):
    if _number_kind(x) == 1:
        x = _index(x)
        if x <= 0:
            raise ValueError("math domain error")
        bits = _bit_length(x)
        if bits > 1023:
            shift = bits - 53
            leading = _to_double(x >> shift)
            correction = float(shift) if binary else float(shift) * _log(2.0)
            return primitive(leading) + correction
    value = _as_float(x)
    if value <= 0.0:
        raise ValueError("math domain error")
    return primitive(value)


def log(x, base=None, /):
    result = _logarithm(x, _log, False)
    if base is None:
        return result
    return result / _logarithm(base, _log, False)


def log2(x, /):
    return _logarithm(x, _log2, True)


def exp(x, /):
    value = _as_float(x)
    result = _exp(value)
    if result == inf and value != inf:
        raise OverflowError("math range error")
    return result


def sin(x, /):
    value = _as_float(x)
    if value == inf or value == -inf:
        raise ValueError("math domain error")
    return _sin(value)


def cos(x, /):
    value = _as_float(x)
    if value == inf or value == -inf:
        raise ValueError("math domain error")
    return _cos(value)


def acos(x, /):
    value = _as_float(x)
    if value < -1.0 or value > 1.0:
        raise ValueError("math domain error")
    return _acos(value)


def lgamma(x, /):
    value = _as_float(x)
    if value == inf or value == -inf:
        return inf
    if value <= 0.0 and value == floor(value):
        raise ValueError("math domain error")
    result = _lgamma(value)
    if result == inf:
        raise OverflowError("math range error")
    return result


def fabs(x, /):
    return abs(_as_float(x))


def floor(x, /):
    method = _special_method(x, "__floor__")
    if method is not None:
        return method()
    return _as_float(x).__floor__()


def ceil(x, /):
    method = _special_method(x, "__ceil__")
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
