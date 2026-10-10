"""Compiled exact integer operations for the initial math package."""

from _mule_native import math_integer_bit_length as _bit_length
from _mule_native import math_integer_value as _integer_value
from _mule_native import math_number_kind as _number_kind
from _mule_native import math_special_method as _special_method

__all__ = ("comb", "factorial", "gcd", "isqrt", "lcm", "perm")


def _index(value):
    if _number_kind(value) == 1:
        return _integer_value(value)
    method = _special_method(value, "__index__")
    if method is None:
        raise TypeError("an integer is required")
    result = method()
    if _number_kind(result) != 1:
        raise TypeError("__index__ returned a non-integer")
    return _integer_value(result)


def factorial(n, /):
    n = _index(n)
    if n < 0:
        raise ValueError("factorial requires a nonnegative integer")
    result = 1
    factor = 2
    while factor <= n:
        result *= factor
        factor += 1
    return result


def comb(n, k, /):
    n = _index(n)
    k = _index(k)
    if n < 0 or k < 0:
        raise ValueError("comb requires nonnegative integers")
    if k > n:
        return 0
    if k > n // 2:
        k = n - k
    result = 1
    factor = 1
    while factor <= k:
        result = result * (n - k + factor) // factor
        factor += 1
    return result


def perm(n, k=None, /):
    n = _index(n)
    if k is None:
        k = n
    else:
        k = _index(k)
    if n < 0 or k < 0:
        raise ValueError("perm requires nonnegative integers")
    if k > n:
        return 0
    result = 1
    factor = n - k + 1
    while factor <= n:
        result *= factor
        factor += 1
    return result


def _gcd(left, right):
    if left < 0:
        left = -left
    if right < 0:
        right = -right
    while right:
        left, right = right, left % right
    return left


def gcd(*integers):
    result = 0
    for value in integers:
        result = _gcd(result, _index(value))
    return result


def lcm(*integers):
    result = 1
    for value in integers:
        value = _index(value)
        if result == 0 or value == 0:
            result = 0
        else:
            result = result // _gcd(result, value) * value
            if result < 0:
                result = -result
    return result


# SBCL's unsigned-byte-8 seed: min(round(sqrt(i * 256 + 128)), 255), i=64..255.
_ISQRT_TABLE = (
    b"\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8d\x8e\x8f"
    b"\x90\x90\x91\x92\x93\x94\x95\x96\x97\x97\x98\x99\x9a\x9b\x9c\x9c"
    b"\x9d\x9e\x9f\xa0\xa0\xa1\xa2\xa3\xa4\xa4\xa5\xa6\xa7\xa7\xa8\xa9"
    b"\xaa\xaa\xab\xac\xad\xad\xae\xaf\xb0\xb0\xb1\xb2\xb3\xb3\xb4\xb5"
    b"\xb5\xb6\xb7\xb7\xb8\xb9\xba\xba\xbb\xbc\xbc\xbd\xbe\xbe\xbf\xc0"
    b"\xc0\xc1\xc2\xc2\xc3\xc4\xc4\xc5\xc6\xc6\xc7\xc8\xc8\xc9\xc9\xca"
    b"\xcb\xcb\xcc\xcd\xcd\xce\xce\xcf\xd0\xd0\xd1\xd2\xd2\xd3\xd3\xd4"
    b"\xd5\xd5\xd6\xd6\xd7\xd8\xd8\xd9\xd9\xda\xdb\xdb\xdc\xdc\xdd\xdd"
    b"\xde\xdf\xdf\xe0\xe0\xe1\xe1\xe2\xe3\xe3\xe4\xe4\xe5\xe5\xe6\xe6"
    b"\xe7\xe8\xe8\xe9\xe9\xea\xea\xeb\xeb\xec\xed\xed\xee\xee\xef\xef"
    b"\xf0\xf0\xf1\xf1\xf2\xf2\xf3\xf3\xf4\xf4\xf5\xf6\xf6\xf7\xf7\xf8"
    b"\xf8\xf9\xf9\xfa\xfa\xfb\xfb\xfc\xfc\xfd\xfd\xfe\xfe\xff\xff\xff"
)


def _approximate_isqrt(n):
    estimate = _ISQRT_TABLE[(n >> 56) - 64]
    estimate = (estimate << 7) + (n >> 41) // estimate
    return (estimate << 15) + (n >> 17) // estimate


def isqrt(n, /):
    n = _index(n)
    if n < 0:
        raise ValueError("isqrt requires a nonnegative integer")
    if n == 0:
        return 0
    bits = _bit_length(n)
    half_bits = (bits - 1) // 2
    if bits <= 64:
        shift = 31 - half_bits
        root = _approximate_isqrt(n << (2 * shift)) >> shift
    else:
        half_bit_length = _bit_length(half_bits)
        prefix = half_bits >> (half_bit_length - 5)
        leading_word = n >> (2 * half_bits - 62)
        root = _approximate_isqrt(leading_word) >> (31 - prefix)
        shift = half_bit_length - 6
        while shift >= 0:
            previous = prefix
            prefix = half_bits >> shift
            numerator = n >> (2 * half_bits + 1 - prefix - previous)
            root = (root << (prefix - previous - 1)) + numerator // root
            shift -= 1
    if root * root > n:
        root -= 1
    return root
