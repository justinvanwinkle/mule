"""Singleton membership must retain Python's identity and equality protocols."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize("display", ["(element,)", "[element]"])
@pytest.mark.parametrize(
    "operator,comparison", [("in", "equal"), ("not in", "not_equal")]
)
@pytest.mark.parametrize("primitive", ["int", "bool", "str", "bytes"])
def test_singleton_primitive_filter_uses_equality(
    display, operator, comparison, primitive
):
    compilation = compile_source(
        "def selected(value, element):\n"
        f" value = {primitive}(value)\n"
        f" element = {primitive}(element)\n"
        f" return value {operator} {display}\n"
    )
    function = compilation.lambdas[1]
    assert list(known_calls(function, f"%compare_{comparison}"))
    for operation in ("%compare_in", "%compare_not_in", "%build_tuple", "%build_list"):
        assert not list(known_calls(function, operation))
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("display", ["(element,)", "[element]"])
@pytest.mark.parametrize("operator", ["in", "not in"])
@pytest.mark.parametrize("primitive", [None, "float"])
def test_singleton_unknown_or_float_filter_keeps_identity_protocol(
    display, operator, primitive
):
    conversions = (
        " value = float(value)\n element = float(element)\n" if primitive else ""
    )
    compilation = compile_source(
        "def selected(value, element):\n"
        f"{conversions}"
        f" return value {operator} {display}\n"
    )
    function = compilation.lambdas[1]
    operation = "%compare_in" if operator == "in" else "%compare_not_in"
    assert list(known_calls(function, operation))
    assert not list(known_calls(function, "%compare_equal"))
    assert not list(known_calls(function, "%compare_not_equal"))
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
@pytest.mark.parametrize("display", ["(target,)", "[target]"])
@pytest.mark.parametrize("operator", ["in", "not in"])
def test_singleton_runtime_code_filter_has_native_comparison(
    tmp_path, display, operator
):
    case = build_case(
        tmp_path,
        f"""
def selected_code(value: int, target: int):
    value = value & 255
    target = target & 255
    return value {operator} {display}

target = int(input())
for record in range(6):
    value = int(input())
    if selected_code(value, target):
        print(record, value)
""",
    )
    case.compare(stdin="257\n1\n2\n257\n-255\n255\n0\n")
    body = case.function_ir(r"^mule_fn_selected_code\.body$")
    # Both runtime values reach the comparison. Masking bounds the actual data,
    # rather than asking the compiler for an unchecked native annotation.
    assert re.search(r"icmp (?:eq|ne) (?:i64|ptr) %[^,]+, %", body), (
        "The runtime filter body lacks a native comparison between its operands"
    )
    generic = re.search(
        r"call[^\n]*@[^\s(]*(?:contains|mule_compare|fast\.compare|"
        r"mule_sequence_list|mule_sequence_tuple|build_tuple|build_list)",
        body,
    )
    assert generic is None, (
        f"The runtime filter retains membership overhead: {generic.group(0)}"
    )
    # The comparison is emitted inline, so no separately compiled membership
    # helper can conceal the removed singleton allocation or protocol dispatch.


@pytest.mark.integration
def test_singleton_runtime_primitive_filters_match_oracle(tmp_path):
    case = build_case(
        tmp_path,
        """
def text_selected(value: str, element: str):
    return value in (element,), value not in [element]

def bytes_selected(value: bytes, element: bytes):
    return value in [element], value not in (element,)

def bool_selected(value: bool, element: bool):
    return value in (element,), value not in [element]

text = input()
flag = int(input()) != 0
print(text_selected(text, "accepted"))
payload = b"accepted" if flag else b"rejected"
print(bytes_selected(payload, b"accepted"))
print(bool_selected(flag, True))
print(None in (None,), None not in [None])
""",
    )
    case.compare(stdin="accepted\n1\n")
    case.compare(stdin="rejected\n0\n")


@pytest.mark.integration
def test_singleton_nan_and_custom_identity_precede_equality(tmp_path):
    case = build_case(
        tmp_path,
        """
class NeverEqual:
    def __eq__(self, other):
        print("unexpected identity equality")
        return False

def tuple_selected(value, element):
    return value in (element,)

def list_selected(value, element):
    return value in [element]

nan = float(input())
other_nan = float(input())
item = NeverEqual()
for selected in (tuple_selected, list_selected):
    print(selected(nan, nan), selected(nan, other_nan))
    print(selected(item, item))
""",
    )
    case.compare(stdin="nan\nnan\n")


@pytest.mark.integration
@pytest.mark.parametrize("display", ["(7,)", "[7]", "(None,)", "[None]"])
def test_singleton_primitive_element_uses_boolean_eq_not_custom_ne(tmp_path, display):
    case = build_case(
        tmp_path,
        f"""
class Query:
    def __init__(self, verdict):
        self.verdict = verdict
    def __eq__(self, other):
        print("eq", other)
        return self.verdict
    def __ne__(self, other):
        print("ne must not run", other)
        return True

def selected(value):
    return value in {display}

def rejected(value):
    return value not in {display}

query = Query(int(input()))
for check in (selected, rejected):
    result = check(query)
    print(type(result) is bool, result)
""",
    )
    case.compare(stdin="9\n")
    case.compare(stdin="0\n")


@pytest.mark.integration
def test_singleton_custom_equality_order_effects_and_errors(tmp_path):
    case = build_case(
        tmp_path,
        """
class Verdict:
    def __init__(self, flag):
        self.flag = flag
    def __bool__(self):
        print("truth", self.flag)
        if self.flag < 0:
            raise ValueError("comparison truth")
        return self.flag != 0

class Item:
    def __init__(self, label, flag):
        self.label = label
        self.flag = flag
    def __eq__(self, other):
        print("eq", self.label, other.label)
        return Verdict(self.flag)

def operand(label, flag):
    print("evaluate", label)
    return Item(label, flag)

def filter_record(flag):
    return operand("query", 0) in [operand("element", flag)]

flag = int(input())
try:
    print("selected", filter_record(flag))
except ValueError:
    print("comparison error")
""",
    )
    for flag in ("1\n", "0\n", "-1\n"):
        case.compare(stdin=flag)


@pytest.mark.integration
def test_singleton_container_alias_retains_mutation_and_operand_failure(tmp_path):
    case = build_case(
        tmp_path,
        """
def selected(value, element):
    allowed = [element]
    alias = allowed
    alias.append(value)
    return value in allowed, len(alias)

def fail():
    print("element evaluated")
    raise ValueError("element")

value = int(input())
print(selected(value, value + 1))
try:
    print(value in [fail()])
except ValueError:
    print("element error")
""",
    )
    case.compare(stdin="41\n")
