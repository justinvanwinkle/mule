"""Staticmethod objects forward calls both directly and through descriptors."""

from test.mule.compiler.test_exception_groups import compare


def test_staticmethod_forwards_keyword_calls_and_nonfunction_targets(tmp_path):
    compare(
        tmp_path,
        """
def function(first, *, second=7):
    return first + second
method = staticmethod(function)
print(callable(method), method(5), method(5, second=9))
print(method.__func__ is function, method.__wrapped__ is function)
print(method.__name__, method.__qualname__, method.__annotations__)
class Container:
    member = method
print(Container.member is function, Container().member is function)
print(Container.member(5, second=9), Container().member(5))
class Callable:
    def __call__(self, value, *, add):
        return value + add
print(staticmethod(Callable())(5, add=8))
invalid = staticmethod(42)
print(callable(invalid))
try:
    invalid()
except TypeError:
    print('invalid target')
""",
    )
