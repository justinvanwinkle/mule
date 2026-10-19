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


@pytest.mark.integration
def test_dataclass_initializer_retains_definition_time_field_metadata(tmp_path):
    compare(
        tmp_path,
        """
from dataclasses import dataclass, field
events = []
def original_factory():
    events.append('original')
    return ['original']
def hidden_factory():
    events.append('hidden')
    return ['hidden']
def replacement_factory():
    events.append('replacement')
    return ['replacement']
@dataclass
class Value:
    count: int = 7
    items: list = field(default_factory=original_factory)
    hidden: list = field(init=False, default_factory=hidden_factory)
    keyword: int = field(kw_only=True, default=8)
metadata = Value.__dataclass_fields__
metadata['count'].name = 'renamed'
metadata['count'].default = 99
metadata['count'].init = False
metadata['items'].default_factory = replacement_factory
metadata['hidden'].default_factory = replacement_factory
metadata['keyword'].name = 'renamed_keyword'
metadata['keyword'].default = 88
metadata['keyword'].kw_only = False
first = Value()
print(getattr(first, 'count', 'missing'), hasattr(first, 'renamed'))
print(getattr(first, 'keyword', 'missing'), first.items, first.hidden, events)
shared = []
second = Value(4, shared, keyword=9)
print(second.count, second.items is shared, second.keyword, second.hidden)
try:
    Value(4, shared, 9)
except TypeError:
    print('still keyword-only')
""",
    )


@pytest.mark.integration
def test_frozen_slot_dataclass_initializer_retains_definition_time_field_name(tmp_path):
    compare(
        tmp_path,
        """
from dataclasses import dataclass
@dataclass(frozen=True, slots=True)
class Value:
    count: int = 7
metadata = Value.__dataclass_fields__['count']
metadata.name = 'renamed'
metadata.default = 99
print(Value().count, Value(4).count)
""",
    )


@pytest.mark.integration
def test_dataclass_initializer_orders_effects_and_binds_before_initialization(tmp_path):
    compare(
        tmp_path,
        """
from dataclasses import dataclass, field
events = []
class Required:
    def __get__(self, instance, owner):
        if instance is None:
            raise AttributeError('required')
        return instance._first
    def __set__(self, instance, value):
        events.append('first:' + str(value))
        instance._first = value
class Default:
    def __get__(self, instance, owner):
        return 5 if instance is None else instance._last
    def __set__(self, instance, value):
        events.append('last:' + str(value))
        instance._last = value
def hidden_factory():
    events.append('hidden')
    return []
def later_factory():
    events.append('later')
    return []
@dataclass
class Value:
    first: int = Required()
    second: int
    hidden: list = field(init=False, default_factory=hidden_factory)
    last: int = Default()
    later: list = field(default_factory=later_factory)
events.clear()
value = Value(1, 2)
print(value.first, value.second, value.last, events)
for invoke in (lambda: Value(1), lambda: Value(1, 2, first=3),
               lambda: Value(1, 2, unknown=3)):
    events.clear()
    try:
        invoke()
    except TypeError:
        print('rejected', events)
""",
    )


@pytest.mark.integration
def test_dataclass_subclass_snapshots_shared_metadata_and_preserves_default_identity(
    tmp_path,
):
    compare(
        tmp_path,
        """
from dataclasses import dataclass, field
@dataclass
class Base:
    value: int = 7
metadata = Base.__dataclass_fields__['value']
metadata.default = 11
@dataclass
class Child(Base):
    pass
metadata.default = 99
metadata.name = 'renamed'
print(Base().value, Child().value)
print(hasattr(Base(), 'renamed'), hasattr(Child(), 'renamed'))
class Marker:
    pass
marker = Marker()
counter = [0]
def factory():
    counter[0] += 1
    return counter[0]
@dataclass
class Identity:
    value: object = marker
    generated: int = field(default_factory=factory)
Identity.__dataclass_fields__['value'].default = Marker()
first, second = Identity(), Identity()
print(first.value is marker, second.value is marker)
print(first.generated, second.generated)
""",
    )


@pytest.mark.integration
def test_dataclass_subclass_captures_generated_default_binding_after_metadata_mutation(
    tmp_path,
):
    compare(
        tmp_path,
        """
import dataclasses
from dataclasses import dataclass
events = []
def factory():
    events.append('factory')
    return 13
@dataclass
class Base:
    value: int = 7
Base.__dataclass_fields__['value'].default_factory = factory
@dataclass
class Child(Base):
    pass
print(Child().value is factory, Child(value=7).value, events)
print(Child(dataclasses._HAS_DEFAULT_FACTORY).value, events)
""",
    )


@pytest.mark.integration
def test_dataclass_initializer_retains_initvars_and_factory_sentinel_behavior(tmp_path):
    compare(
        tmp_path,
        """
import dataclasses
from dataclasses import dataclass, field, InitVar
events = []
def factory():
    events.append('factory')
    return len(events)
@dataclass
class Value:
    generated: int = field(default_factory=factory)
    seed: InitVar[int] = 3
    keyword: InitVar[str] = field(kw_only=True, default='default')
    def __post_init__(self, seed, keyword):
        self.seen = seed, keyword
Value.__dataclass_fields__['seed']._field_type = dataclasses._FIELD
Value.__dataclass_fields__['keyword'].default = 'changed'
first = Value()
second = Value(dataclasses._HAS_DEFAULT_FACTORY, 4, keyword='supplied')
print(first.generated, first.seen, second.generated, second.seen, events)
""",
    )
