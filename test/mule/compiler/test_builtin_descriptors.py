"""Builtin descriptors retain their type, name and owner independently of properties."""

import pytest

from test.mule.compiler.test_exception_groups import compare


def test_builtin_descriptor_metadata_and_receiver_validation(tmp_path):
    compare(
        tmp_path,
        """
from types import FunctionType, GetSetDescriptorType, MemberDescriptorType
def function():
    return 42
for name, kind in (('__code__', GetSetDescriptorType), ('__globals__', MemberDescriptorType), ('__closure__', MemberDescriptorType)):
    descriptor = vars(FunctionType)[name]
    print(type(descriptor).__name__, type(descriptor) is kind)
    print(descriptor.__name__, descriptor.__qualname__, descriptor.__objclass__ is FunctionType)
    print(descriptor.__get__(None, FunctionType) is descriptor)
    print(descriptor.__get__(function) is getattr(function, name))
    try:
        descriptor.__get__(object())
    except TypeError as error:
        print(str(error))
print(GetSetDescriptorType is MemberDescriptorType, GetSetDescriptorType is property)
print(type.__mro__ == (type, object), FunctionType.__mro__ == (FunctionType, object))
""",
    )


@pytest.mark.integration
def test_static_inspection_recognizes_builtin_dictionary_descriptors(tmp_path):
    compare(
        tmp_path,
        """
import inspect
class Example:
    def method(self, x=1):
        return x
    @property
    def property(self):
        raise AssertionError('static inspection must not evaluate a property')
value = Example()
value.field = 42
print(inspect.getattr_static(value, 'field'))
print(inspect.getattr_static(value, 'property') is vars(Example)['property'])
print(inspect.getattr_static(Example, 'method') is vars(Example)['method'])
print(inspect.signature(Example.method), inspect.signature(value.method))
""",
    )
