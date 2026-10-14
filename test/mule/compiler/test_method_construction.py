"""Explicit method construction uses the same binding as attribute access."""

from test.mule.compiler.test_exception_groups import compare


def test_method_type_binds_functions_and_callable_objects(tmp_path):
    compare(
        tmp_path,
        """
from types import MethodType
class Receiver:
    value = 7
def function(self, increment=0):
    return self.value + increment
class Callable:
    def __call__(self, receiver, increment=0):
        return receiver.value * 2 + increment
receiver = Receiver()
for target in (function, Callable()):
    method = MethodType(target, receiver)
    print(method(), method(increment=3))
    print(method.__self__ is receiver, method.__func__ is target)
for target, instance in ((42, receiver), (function, None)):
    try:
        MethodType(target, instance)
    except TypeError as error:
        print(type(error).__name__, str(error))
""",
    )
