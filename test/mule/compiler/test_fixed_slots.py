"""Fixed instance layouts specialize normal attribute operations."""

import re

import pytest

from mule.compiler import emit_llvm
from test.mule.compiler.rewrite_support import build_case
from test.mule.compiler.test_native import run_native

SOURCE = """
class Record:
    __slots__ = ("first", "second")
    def __init__(self, value):
        self.first = None
        self.second = value
    def read(self):
        return self.second
record = Record(42)
print(record.read())
"""


def test_fixed_slot_access_uses_constant_index_after_layout_guard():
    text = emit_llvm(SOURCE)
    assert "attr.fixed" in text


@pytest.mark.parametrize("declaration", ["__slots__", "__mule_slots__"])
def test_declared_only_slot_enters_layout_metadata(declaration):
    text = emit_llvm(
        f'class Record:\n {declaration} = ("first", "second")\n'
        " def read(self):\n  return self.second\n"
        "record = Record()\nrecord.second = 42\nprint(record.read())\n"
    )
    method = re.search(
        r'define internal ptr @"mule_fn_Record.read.spec.0"[^\n]*\{\n(.*?)^}',
        text,
        re.MULTILINE | re.DOTALL,
    )
    assert method is not None
    assert "attr.fixed" in method[1]


def test_fixed_slot_access_preserves_unset_and_replaced_descriptor(tmp_path):
    source = (
        SOURCE
        + """
del record.second
try:
    print(record.read())
except AttributeError:
    print("unset")
Record.second = property(lambda self: 99)
print(record.read())
"""
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "42\nunset\n99\n"


BORROW_SOURCE = """
class Packet:
    __slots__ = ("value",)
    def __init__(self):
        self.value = None
    def empty(self):
        result = self.value is None
        return result and self is not None
packet = Packet()
print(packet.empty(), packet.empty())
Packet.value = property(lambda self: [])
print(packet.empty(), packet.empty())
"""


def test_fixed_slot_read_borrows_with_owned_descriptor_fallback(tmp_path):
    text = emit_llvm(BORROW_SOURCE)
    assert "attr.borrowed" in text
    result = run_native(tmp_path, BORROW_SOURCE)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True True\nFalse False\n"


def test_fixed_slot_read_owns_value_across_callback_capable_local_overwrite():
    source = """
class Marker:
    def __del__(self):
        print("marker")
class Trigger:
    def __init__(self, packet):
        self.packet = packet
    def __del__(self):
        self.packet.value = None
        print("trigger")
class Packet:
    __slots__ = ("value",)
    def __init__(self):
        self.value = Marker()
    def inspect(self):
        trigger = Trigger(self)
        result = self.value is (trigger := None)
        return result and self is not None
packet = Packet()
print(packet.inspect())
"""
    text = emit_llvm(source)
    body = re.search(
        r'define internal ptr @"mule_fn_Packet.inspect.spec.0"[^\n]*\{\n(.*?)^}',
        text,
        re.MULTILINE | re.DOTALL,
    )
    assert body is not None
    # Overwriting trigger can destroy a user object. The attribute temporary
    # must acquire its own reference instead of depending on the mutable slot.
    loaded = re.search(r"attr\.loaded[^:]*:([^:]*?)attr\.slow[^:]*:", body[1])
    assert loaded is not None, body[1]
    assert "@mule.fast.retain" in loaded[1]


def test_fixed_slot_layouts_preserve_inheritance_and_dynamic_fallback(tmp_path):
    source = """
class Base:
    __slots__ = ("first",)
    def __init__(self, value):
        self.first = value
class Child(Base):
    __slots__ = ("second",)
    def __init__(self, value):
        self.first = value
        self.second = value + 1
    def read(self):
        return self.first, self.second
class Other:
    __slots__ = ("second", "first")
    def __init__(self, value):
        self.second = value + 10
        self.first = value + 20
    def read(self):
        return self.first, self.second
class Inferred:
    def __init__(self, value):
        self.field = value
    def read(self):
        return self.field
class Missing:
    __slots__ = ("field",)
    def read(self):
        return self.field
    def __getattr__(self, name):
        return [name]
value = len(input())
child = Child(value)
other = Other(value)
inferred = Inferred(value)
missing = Missing()
for index in range(2):
    print(child.read(), other.read(), inferred.read(), missing.read())
del child.second
try:
    child.read()
except AttributeError:
    print("unset")
Child.second = property(lambda self: 91)
print(child.read())
Inferred.field = property(lambda self: 92)
print(inferred.read())
"""
    case = build_case(tmp_path, source, optimize=1)
    case.compare("abcd\n")


@pytest.mark.integration
def test_fixed_slot_borrow_optimized_path_loads_without_retaining(tmp_path):
    case = build_case(tmp_path, BORROW_SOURCE)
    body = case.function_ir(r"Packet\.empty\.spec\.0$")
    # Check the guarded hit, not unrelated retains in callable/error helpers.
    direct = re.search(
        r"^attr\.fixed[^:]*:[^\n]*\n((?:[ \t]+[^\n]*\n)+)",
        body,
        re.MULTILINE,
    )
    assert direct is not None, body
    assert "load ptr" in direct[1]
    assert "getelementptr" in direct[1]
    assert "retain" not in direct[1]
    assert "atomicrmw" not in direct[1]
    slow = re.search(r"(%[^ ]+) = call ptr @mule_get_attr_cached", body)
    assert slow is not None
    assert f"atomicrmw sub ptr {slow[1]}" in body
    assert f"@mule_destroy(ptr nonnull {slow[1]})" in body
    case.compare()


def test_fixed_slot_borrow_preserves_descriptor_results_on_error_edges(tmp_path):
    source = """
class Ratio:
    __slots__ = ("value",)
    def __init__(self):
        self.value = 3
    def divide(self, denominator):
        result = self.value // denominator
        return result if self is not None else 0
ratio = Ratio()
for step in range(2):
    for denominator in range(2):
        try:
            print(ratio.divide(denominator))
        except ZeroDivisionError:
            print("zero")
setattr(Ratio, "value", property(lambda self: int("7")))
for step in range(2):
    for denominator in range(2):
        try:
            print(ratio.divide(denominator))
        except ZeroDivisionError:
            print("zero")
"""
    assert "attr.borrowed" in emit_llvm(source)
    case = build_case(tmp_path, source, optimize=1)
    case.compare()


def test_layout_metadata_does_not_prove_custom_constructor_result(tmp_path):
    source = """
class Actual:
    def read(self):
        return 91
class Redirect:
    __slots__ = ("field",)
    def __new__(cls):
        return Actual()
    def read(self):
        return self.field
result = Redirect()
print(type(result).__name__, result.read())
"""
    case = build_case(tmp_path, source, optimize=1)
    case.compare()


def test_custom_allocation_does_not_hide_initial_field_values(tmp_path):
    source = """
class Cache:
    __slots__ = ("value",)
    def __new__(cls):
        self = object.__new__(cls)
        self.value = None
        return self
    def read(self):
        if self.value is None:
            self.value = 3
        return self.value
cache = Cache()
print(cache.read(), cache.read())
"""
    case = build_case(tmp_path, source, optimize=1)
    case.compare()


@pytest.mark.integration
def test_slot_read_owns_value_across_function_weak_callback(tmp_path):
    source = """
from weakref import ref
events = []
class Payload:
    pass
class Packet:
    __slots__ = ("value",)
    def __init__(self):
        self.value = Payload()
    def check(self):
        def trigger():
            pass
        def callback(ignored):
            self.value = None
            events.append("trigger")
        # Keep an owning binding across the weak-reference constructor, so
        # destruction occurs at the overwrite under test, not at that call.
        keep = trigger
        watch = ref(trigger, callback)
        result = self.value is (trigger := (keep := None))
        return result and self is not None, watch() is None
packet = Packet()
watch = ref(packet.value, lambda ignored: events.append("payload"))
print(packet.check())
print(events, watch() is None)
"""
    case = build_case(tmp_path, source)
    case.compare()
