"""Unary operators dispatch through compiled methods and retain scalar proofs."""

import pytest

from test.mule.compiler.test_exception_groups import compare


def test_unary_special_methods_descriptors_and_subclass_results(tmp_path):
    compare(
        tmp_path,
        """
def positive(value):
    return +value
def negative(value):
    return -value
def invert(value):
    return ~value
class Custom:
    def __pos__(self):
        return 'positive'
    def __neg__(self):
        return 'negative'
    def __invert__(self):
        return NotImplemented
    def __abs__(self):
        return 'absolute'
value = Custom()
value.__neg__ = lambda: 'instance override'
print(positive(value), negative(value), invert(value) is NotImplemented)
print(abs(value))
class Static:
    @staticmethod
    def __neg__():
        return 'static'
class Class:
    @classmethod
    def __pos__(cls):
        return cls.__name__
print(negative(Static()), positive(Class()))
class Integer(int):
    pass
class Float(float):
    pass
for value in (Integer(42), Float(2.5), True, 2**100):
    print(type(positive(value)).__name__, positive(value))
    print(type(negative(value)).__name__, negative(value))
    print(type(abs(negative(value))).__name__, abs(negative(value)))
print(type(invert(Integer(42))).__name__, invert(Integer(42)))
print(str(negative(Float(0.0))), str(negative(Float(-0.0))))
class Overridden(int):
    def __neg__(self):
        return 'override'
print(negative(Overridden(42)), -Overridden(42))
for operation in (positive, negative, invert, abs):
    try:
        operation(object())
    except TypeError as error:
        print(str(error))
class Broken:
    def __neg__(self):
        raise ValueError('method error')
try:
    negative(Broken())
except ValueError as error:
    print(str(error))
""",
    )


@pytest.mark.integration
def test_timedelta_unary_operations_match_python(tmp_path):
    compare(
        tmp_path,
        """
from datetime import timedelta
value = timedelta(hours=24, microseconds=-1)
print(str(value), str(-value), str(+value), str(abs(-value)))
print(-value + value == timedelta(0))
""",
    )
