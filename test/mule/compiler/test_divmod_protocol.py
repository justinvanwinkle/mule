"""Quotient/remainder negotiation uses Python's binary method protocol."""

from test.mule.compiler.test_exception_groups import compare


def test_divmod_numeric_values_reflection_subclass_priority_and_descriptors(tmp_path):
    compare(
        tmp_path,
        """
for left, right in ((7, 3), (-7, 3), (7, -3), (7, 3.0), (7.0, 3),
                    (True, 2), (1 << 100, 7), (-0.0, 3.0)):
    print(divmod(left, right))
for left, right in (('x', 2), (object(), 2), (2, object()), (2, 0), (2.0, 0.0)):
    try:
        divmod(left, right)
    except (TypeError, ZeroDivisionError) as error:
        print(type(error).__name__, str(error))
class Left:
    def __divmod__(self, other):
        print('forward')
        return NotImplemented
class Right:
    def __rdivmod__(self, other): return 'reflected'
print(divmod(Left(), Right()))
class Base:
    def __divmod__(self, other): return 'base forward'
    def __rdivmod__(self, other): return 'base reverse'
class Child(Base):
    def __rdivmod__(self, other): return 'child reverse'
class Inherited(Base): pass
class Declining(Base):
    def __rdivmod__(self, other): return NotImplemented
print(divmod(Base(), Child()), divmod(Base(), Inherited()), divmod(Base(), Declining()))
class Static:
    __divmod__ = staticmethod(lambda other: 'static')
class Class:
    @classmethod
    def __divmod__(cls, other): return cls.__name__
print(divmod(Static(), 2), divmod(Class(), 2))
class Reverse(float):
    def __rdivmod__(self, other): return 'numeric reflected'
print(divmod(2, Reverse(3)))
class Integer(int):
    def __floordiv__(self, other): raise AssertionError('floor override')
    def __mod__(self, other): raise AssertionError('modulo override')
class Float(float):
    def __floordiv__(self, other): raise AssertionError('floor override')
    def __mod__(self, other): raise AssertionError('modulo override')
print(divmod(Integer(7), Integer(3)), divmod(Float(7), Float(3)))
for cls in (int, float):
    for method in (cls.__divmod__, cls.__rdivmod__):
        try:
            method('7', 3)
        except TypeError as error:
            print(str(error))
""",
    )
