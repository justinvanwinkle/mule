"""Private class cleanup used by Python 3.14 dataclass slot replacement."""

import pytest

from test.mule.compiler.test_exception_groups import compare


def test_heap_class_own_descriptors_are_removed_without_touching_slots(tmp_path):
    compare(
        tmp_path,
        """
import sys
class Base:
    @property
    def __dict__(self):
        return {'base': True}
class Child(Base):
    __dict__ = 'custom'
    __weakref__ = 'custom'
    other = 42
class Slots:
    __slots__ = ('field',)
for cls in (Child, Slots):
    print(sys._clear_type_descriptors(cls))
    print('__dict__' in vars(cls), '__weakref__' in vars(cls))
print(Child.other, Child().__dict__, '__dict__' in vars(Base))
print(Slots.__slots__)
obj = Slots()
obj.field = 7
print(obj.field)
class DocSlot:
    __slots__ = ('__doc__',)
obj = DocSlot()
obj.__doc__ = 'instance documentation'
print(obj.__doc__)
for cls in (int, type, list, 42):
    try:
        sys._clear_type_descriptors(cls)
    except TypeError as error:
        print(str(error))
""",
    )


def test_dynamic_slot_storage_descriptors_mangling_and_rejection(tmp_path):
    compare(
        tmp_path,
        """
import types
Base = type('Base', (), {'__slots__': ('base',)})
Value = type('Value', (Base,), {'__slots__': ('name', '__private')})
value = Value()
value.base = 7
value.name = 'hello'
value._Value__private = 9
print(value.base, value.name, value._Value__private)
descriptor = Value.__dict__['name']
print(type(descriptor) is types.MemberDescriptorType, descriptor.__name__, descriptor.__objclass__ is Value)
print(descriptor.__get__(None, Value) is descriptor, descriptor.__get__(value))
descriptor.__set__(value, 'changed')
print(value.name)
descriptor.__delete__(value)
for operation in (lambda: value.name, lambda: descriptor.__delete__(value)):
    try:
        operation()
    except AttributeError:
        print('unset')
try:
    value.other = 1
except AttributeError:
    print('no dictionary')
try:
    descriptor.__get__(object())
except TypeError:
    print('wrong receiver')
for namespace in ({'__slots__': ('bad name',)}, {'__slots__': (7,)}, {'__slots__': ('name',), 'name': 1}):
    try:
        type('Invalid', (), namespace)
    except (TypeError, ValueError) as error:
        print(type(error).__name__, str(error))
""",
    )


@pytest.mark.integration
def test_slotted_frozen_dataclass_replacement_retains_field_behavior(tmp_path):
    compare(
        tmp_path,
        """
from dataclasses import dataclass, fields, FrozenInstanceError
@dataclass(frozen=True, slots=True, kw_only=True)
class Value:
    name: str
    count: int = 7
value = Value(name='hello')
print(value.name, value.count, value == Value(name='hello'))
print(Value.__slots__, [field.name for field in fields(Value)])
try:
    value.count = 9
except FrozenInstanceError:
    print('frozen')
try:
    Value('hello')
except TypeError:
    print('keyword only')
""",
    )
