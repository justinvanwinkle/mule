"""Direct decimal parsing must preserve Python's numeric-record semantics."""

import re

import pytest

from test.mule.compiler.rewrite_support import build_case

pytestmark = pytest.mark.integration


RECORD_SOURCE = """\
def parse_record(raw):
    text = str(raw)
    return int(text)

records = [input(), input(), input()]
saved = records[:]
total = 0
for record in records:
    try:
        value = parse_record(record)
        total += value
        print(value)
    except ValueError:
        print("ValueError")
    print(total)
print(records == saved)
print(records[0])
"""


@pytest.fixture(scope="module")
def decimal_records(tmp_path_factory):
    return build_case(tmp_path_factory.mktemp("decimal-records"), RECORD_SOURCE)


@pytest.mark.parametrize(
    "records",
    [
        ("\t +00042 \v", " -7 ", "1_234_567"),
        ("0", "-0", "+0"),
        ("4611686018427387903", "4611686018427387904", "-4611686018427387905"),
        ("9223372036854775807", "9223372036854775808", "-9223372036854775808"),
        ("9" * 80, "-" + "8" * 80, "1_000_000_000_000_000_000_000_000"),
    ],
    ids=[
        "ascii-sign-space-underscore",
        "zero",
        "tag-boundary",
        "i64-boundary",
        "bigint",
    ],
)
def test_int_text_records_preserve_values_and_borrowed_text(decimal_records, records):
    decimal_records.compare(stdin="\n".join(records) + "\n")


@pytest.mark.parametrize(
    "records",
    [
        ("١٢٣", "-１２３", "+१_२३४"),
        ("\u00a0+42\u00a0", "\u2003-7\u2003", "\u30001_234\u3000"),
        ("1٢３", "٠٠٧", "\u202f-१२_३\u202f"),
    ],
    ids=["unicode-decimal-digits", "unicode-whitespace", "mixed-digit-scripts"],
)
def test_int_text_records_accept_python_unicode_decimal_syntax(
    decimal_records, records
):
    decimal_records.compare(stdin="\n".join(records) + "\n")


@pytest.mark.parametrize(
    "bad",
    [
        "",
        "   ",
        "+",
        "-",
        "_1",
        "1_",
        "1__2",
        "+ 1",
        "1 2",
        "12x",
        "0x10",
        "1.0",
        "²",
        "1\x002",
    ],
)
def test_int_text_invalid_record_preserves_accumulated_state(decimal_records, bad):
    # A failed parse must not change the running total or its borrowed input.
    decimal_records.compare(stdin=f"17\n{bad}\n-2\n")


def test_int_text_optimized_record_path_parses_digits_without_constructor_dispatch(
    decimal_records,
):
    caller = decimal_records.function_ir(r"^mule_fn_parse_record\.body$")
    assert re.search(r"call\s+ptr\s+@mule_parse_int_text\(", caller)
    assert "cell:__main__:int" not in caller

    # This is the linked Rust implementation, not an LLVM declaration or a
    # synthetic helper. Follow a non-inlined digit parser if LLVM retained it.
    parser = decimal_records.function_assembly(
        r"^(mule_parse_int_text)$|builtins.*parse_int_bytes_base10"
    )
    assert re.search(r"\b(movzbl|movzbq)\b", parser), parser
    # A byte-register add of 0xd0 implements the same wrapping subtraction
    # of ASCII '0' as subtracting 0x30 in a wider register.
    assert re.search(
        r"\b(?:sub[a-z]*\s+\$0x30|"
        r"add[a-z]*\s+\$(?:0xd0|0xffffffd0|0xffffffffffffffd0)),",
        parser,
    ), parser
    assert re.search(r"\$(0x9|0xa)\b", parser), parser
    assert re.search(r"\b(lea|imul|shl)[a-z]*\b", parser), parser
    assert re.search(r"\bj(a|ae|b|be|e|ne|g|ge|l|le)\b", parser), parser
    assert not re.search(r"<(mule_call|mule_getattr|mule_resolve_method)([+>])", parser)


def test_int_text_explicit_base_keyword_and_nontext_keep_python_fallbacks(tmp_path):
    source = """\
def parse_decimal(text):
    return int(text)

def parse_base(text, base):
    return int(text, base)

def parse_keyword(text, base):
    return int(text, base=base)

text = input()
print(parse_base(text, 16))
print(parse_keyword(text, 16))
print(parse_base("0b_101", 0))
print(parse_keyword("101", 2))
for base in (1, 37):
    try:
        print(parse_base(text, base))
    except ValueError:
        print("ValueError")
for value in (True, 17, 3.75, None, [], b"123"):
    try:
        print(parse_decimal(value))
    except TypeError:
        print("TypeError")
try:
    print(int(x=text))
except TypeError:
    print("TypeError")
"""
    case = build_case(tmp_path, source)
    case.compare(stdin="ff\n")
    for function in ("parse_base", "parse_keyword"):
        body = case.function_ir(rf"^mule_fn_{function}\.(body|spec\.)")
        assert "@mule_parse_int_text(" not in body


def test_int_text_rebound_constructor_preserves_argument_effects(tmp_path):
    source = """\
events = []

def argument(value):
    events.append("argument")
    return str(value)

def replacement(value):
    events.append("replacement")
    return len(value)

def parse_record(raw):
    return int(argument(raw))

int = replacement
print(parse_record(input()))
print(events)
"""
    case = build_case(tmp_path, source)
    case.compare(stdin="  123  \n")
    body = case.function_ir(r"^mule_fn_parse_record\.(body|spec\.)")
    assert "@mule_parse_int_text(" not in body


def test_int_text_safety_three_retains_checked_constructor_path(tmp_path):
    case = build_case(tmp_path, "__safety__ = 3\n" + RECORD_SOURCE)
    case.compare(stdin=" +42 \n1_000\n-7\n")
    body = case.function_ir(r"^mule_fn_parse_record\.(body|spec\.)")
    assert "@mule_parse_int_text(" not in body
    assert re.search(r"call\b[^\n]*@[^\n]*call", body), body
