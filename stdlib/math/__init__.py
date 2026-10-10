"""Initial compiled math support: square root and exact integer operations."""

from _mule_native import math_inf as inf
from _mule_native import math_nan as nan
from _mule_native import math_number_kind as _number_kind
from _mule_native import math_special_method as _special_method
from _mule_native import math_sqrt as _sqrt
from _mule_native import math_to_double as _to_double

from . import integer
from .integer import _index, comb, factorial, gcd, isqrt, lcm, perm

__all__ = (
    "comb",
    "e",
    "factorial",
    "gcd",
    "inf",
    "integer",
    "isqrt",
    "lcm",
    "nan",
    "perm",
    "pi",
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
