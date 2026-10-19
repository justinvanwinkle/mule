"""Resolved method calls keep explicit and implicit receivers distinct."""

import pytest

from mule.compiler import emit_llvm
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_modules import execute


@pytest.mark.parametrize(
    ("parameters", "call"),
    [
        ("self, value=7", "Base.method(instance, 4)"),
        ("", "Base.method()"),
        ("self, *, value=7", "Base.method(instance, value=4)"),
    ],
)
def test_class_qualified_method_selects_guarded_direct_body(parameters, call):
    llvm = emit_llvm(
        f"class Base:\n def method({parameters}):\n  return 37\n"
        f"instance = Base()\nprint({call})\n"
    )

    marker = 'define internal ptr @"mule.method.mule_fn_Base.method.'
    assert marker in llvm
    helper = llvm.split(marker, 1)[1].split("\n}", 1)[0]
    assert "call ptr @mule_method_entry(" in helper
    assert 'call ptr @"mule_fn_Base.method.body"(' in helper
    fallback = helper.split("fallback:\n", 1)[1]
    if "value=" in call:
        assert "call ptr @mule.call.kw." in fallback
        assert "@mule_call_keywords(" in llvm
    else:
        assert "call ptr @mule.call.resolved." in fallback
        assert "@mule_call_resolved_owned(" in llvm


def test_bound_and_unbound_method_calls_keep_defaults_keywords_and_arity(tmp_path):
    compare(
        tmp_path,
        """
class Base:
    def method(self, value=7, *, scale=2):
        return self.offset + value * scale
    def zero():
        return 'zero'
class Child(Base):
    def method(self, value=100, *, scale=3):
        return self.offset - value * scale
instance = Child()
instance.offset = 5
print(Base.method(instance), Base.method(instance, scale=4, value=3))
print(Base.method(self=instance, scale=3))
print(instance.method(), instance.method(scale=2, value=4))
print(Base.zero())
for invoke in (lambda: instance.zero(), lambda: Base.method(),
               lambda: Base.method(instance, 1, value=2),
               lambda: Base.method(instance, unknown=2)):
    try:
        invoke()
    except TypeError:
        print('arity')
""",
    )


def test_unbound_methods_use_actual_closure_defaults_and_replacement(tmp_path):
    compare(
        tmp_path,
        """
def make(seed):
    class Value:
        def method(self, value=seed):
            return seed, value
    return Value
First = make(['first'])
Second = make(['second'])
first, second = First(), Second()
print(First.method(first), Second.method(second))
print(First.method(first, ['explicit']))
saved = First.method
def replacement(self, value=['replacement']):
    return 'new', value
First.method = replacement
print(First.method(first), saved(first))
events = []
class Descriptor:
    def __get__(self, instance, owner):
        events.append('lookup')
        return replacement
First.method = Descriptor()
print(First.method(first), events)
""",
    )


def test_unbound_descriptor_calls_and_exception_arguments_preserve_effects(tmp_path):
    compare(
        tmp_path,
        """
events = []
class Value:
    def __init__(self):
        self._value = None
    def set_value(self, value):
        events.append(value)
        self._value = value
    value = property(lambda self: self._value, set_value)
value = Value()
property.__set__(Value.value, value, ['kept'])
print(value.value, events)
Slot = type('Slot', (), {'__slots__': ('value',)})
slot = Slot()
object.__setattr__(slot, 'value', ['slot'])
print(slot.value)
try:
    Slot.value.__set__(object(), ['wrong'])
except TypeError:
    print('slot receiver')
class Failure:
    def method(self, value, *, fail=False):
        if fail:
            raise ValueError(value[0])
        return value
failure = Failure()
for index in range(3):
    try:
        Failure.method(failure, ['failed'], fail=True)
    except ValueError as error:
        print(str(error))
print(Failure.method(failure, ['alive']))
""",
    )


def test_unbound_native_method_retains_receiver_layout_guard(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
import mule
@mule.optimize(speed=3, safety=3)
class Native:
    value: mule.int64
    def __init__(self, value: mule.int64):
        self.value = value
    def method(self, amount: mule.int64=1) -> mule.int64:
        return self.value + amount
instance = Native(5)
print(Native.method(instance, 3))
try:
    Native.method(object(), 3)
except TypeError:
    print('receiver')
try:
    Native.method(instance, 3, 4)
except TypeError:
    print('arity')
print(Native.method(instance))
""",
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout == "8\nreceiver\narity\n6\n"
