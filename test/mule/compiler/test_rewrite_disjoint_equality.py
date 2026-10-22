"""Disjoint equality must remove dispatch without removing Python effects."""

import re

import pytest

from test.mule.compiler.rewrite_support import build_case

pytestmark = pytest.mark.integration


def test_runtime_record_disjoint_comparisons_remove_dispatch(tmp_path):
    case = build_case(
        tmp_path,
        """
def validate_record(raw):
    number = int(raw)
    label = str(raw)
    payload = label.encode()
    pair = (number, label)
    items = [number, label]
    return number, (
        number == label, number != label,
        label == payload, label != payload,
        label == None, label != None,
        pair == items, pair != items,
        (number, label) == (number, label, payload),
        (number, label) != (number, label, payload),
    )

print(validate_record(input()))
""",
    )
    for raw in ("0\n", "-17\n", "123456789012345678901234567890\n"):
        case.compare(raw)

    body = case.function_ir(r"^mule_fn_validate_record\.(?:body|spec\.)")
    # The parse still runs on runtime data. The comparisons themselves require
    # no runtime helper, including the alwaysinline fast-compare wrappers.
    assert "call " in body
    assert "ret ptr" in body
    assert not re.search(
        r"@(?:mule_compare\b|mule_compare_bool\b|mule\.fast\.compare)", body
    )
    # Check before LLVM inlining too: moving dispatch into a helper cannot make
    # the optimized caller-only absence assertion pass.
    original = case.function_ir(
        r"^mule_fn_validate_record\.(?:body|spec\.)", optimized=False
    )
    assert not re.search(
        r"@(?:mule_compare\b|mule_compare_bool\b|mule\.fast\.compare)", original
    )


def test_overlapping_equal_arity_unknown_and_custom_equality_keep_semantics(tmp_path):
    case = build_case(
        tmp_path,
        """
class Expected:
    def __init__(self, value):
        self.value = value

    def __eq__(self, other):
        print("eq", self.value, other)
        return self.value == other

    def __ne__(self, other):
        print("ne", self.value, other)
        return self.value != other

def compare_fields(actual, expected):
    return actual == expected, actual != expected

def numeric_record(raw):
    number = int(raw)
    floating = float(raw)
    return number == floating, number != floating, number == True, number != True

def tuple_record(raw):
    number = int(raw)
    return (number, raw) == (1, "1"), (number, raw) != (1, "1")

raw = input()
print(numeric_record(raw))
print(tuple_record(raw))
print(compare_fields(raw, "1"))
print(compare_fields(int(raw), 1))
print(compare_fields(Expected(raw), raw))
print(compare_fields(raw, Expected(raw)))
""",
    )
    for raw in ("1\n", "2\n", "9007199254740993\n"):
        case.compare(raw)
    # An external/general entry accepts custom equality and must not fold its
    # unknown operands merely because observed built-in calls are disjoint.
    unknown = case.function_ir(r"^mule_fn_compare_fields\.body$")
    assert re.search(r"@(?:mule_compare\b|mule_compare_bool\b)", unknown)


def test_unequal_tuple_arities_preserve_element_evaluation_and_errors(tmp_path):
    case = build_case(
        tmp_path,
        """
events = []

def field(raw, label):
    events.append(label)
    return int(raw)

def validate_direct(raw, bad):
    return (field(raw, "left"),) == (field(raw, "right"), field(bad, "tail"))

def validate_stored(raw, bad):
    left = (field(raw, "left"),)
    right = (field(raw, "right"), field(bad, "tail"))
    return left != right

class Field:
    def __init__(self, raw):
        self.raw = raw

    def __eq__(self, other):
        events.append("element-eq")
        if self.raw == "raise":
            raise ValueError("element comparison")
        return True

def validate_custom(raw):
    left = (Field(raw),)
    right = (Field(raw), 0)
    return left == right, left != right

raw = input()
print(validate_direct(raw, raw), events)
events.clear()
print(validate_stored(raw, raw), events)
events.clear()
try:
    validate_direct(raw, "invalid")
except ValueError:
    print("conversion error", events)
events.clear()
try:
    validate_stored(raw, "invalid")
except ValueError:
    print("stored conversion error", events)
events.clear()
print(validate_custom(raw), events)
events.clear()
try:
    validate_custom("raise")
except ValueError:
    print("comparison error", events)
""",
    )
    for raw in ("1\n", "-29\n"):
        case.compare(raw)
