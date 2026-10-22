"""Integer fields parsed from runtime records without materializing text slices."""

import re

import pytest

from test.mule.compiler.rewrite_native_trace import trace_native
from test.mule.compiler.rewrite_support import build_case

pytestmark = pytest.mark.integration


def test_int_span_record_scanner_parses_without_slice_allocation(tmp_path):
    case = build_case(
        tmp_path,
        """def scan_record(text):
    total = 0
    start = 0
    for stop in range(len(text)):
        if text[stop] == ":":
            total += int(text[start:stop])
            start = stop + 1
    return total + int(text[start:])

print(scan_record(input()))
""",
    )
    for record in (
        "12:-34: +5 :1_000",
        "99999999999999999999999999999999999999:1:-2",
    ):
        case.compare(record + "\n")
    scanner = case.function_ir(r"mule_fn_scan_record\.spec\.\d+$")
    assert not re.search(r"@mule(?:\.fast\.)?getslice\(", scanner), (
        "The optimized record scanner still materializes a slice before parsing"
    )
    assert "@mule_getslice(" not in scanner, (
        "The optimized record scanner still uses generic allocating slicing"
    )
    # These assertions cover removal of intermediate slices in the actual caller.
    # Runtime parser efficiency remains unverified until fusion selects it here.
    assert "@mule_parse_int_slice(" in scanner


def test_normalized_int_record_span_parser_avoids_temporary_text_allocation(tmp_path):
    case = build_case(
        tmp_path,
        """def parse_record(record, prefix, suffix):
    text = f"{record}"
    start = len(f"{prefix}")
    stop = len(text) - len(f"{suffix}")
    return int(text[start:stop])

def retained_span(text, start, stop):
    fragment = text[start:stop]
    value = int(fragment)
    if len(fragment) != stop - start:
        raise ValueError("truncated numeric field")
    return value

mode = input()
record = input()
prefix = input()
suffix = input()
if mode == "fused":
    print(parse_record(record, prefix, suffix))
else:
    text = f"{record}"
    start = len(f"{prefix}")
    stop = len(text) - len(f"{suffix}")
    print(retained_span(text, start, stop))
""",
    )
    normalized = case.function_ir(r"^mule_fn_parse_record\.body$")
    assert "@mule_parse_int_slice(" in normalized
    assert not re.search(r"@mule(?:\.fast\.)?getslice\(", normalized)
    assert "@mule_getslice(" not in normalized
    retained = case.function_ir(r"^mule_fn_retained_span\.spec\.\d+$")
    assert re.search(r"@mule(?:\.fast\.|_fast_)?getslice\(", retained)

    parser = case.function_assembly(r"^mule_parse_int_slice$")
    # Rust inlines the short-decimal parser into this entry. Inspect byte reads,
    # digit validation and accumulator multiplication, not an outlined symbol.
    assert re.search(r"\bmovzbl\s+\([^)]*\),%\w+", parser)
    assert re.search(r"\bcmp\s+\$0x9,%\w+", parser)
    multiply_by_ten = re.search(r"\bimul\w*\s+\$0xa,", parser)
    scaled_accumulation = re.search(
        r"\blea\s+\((%\w+),\1,4\),\1", parser
    ) and re.search(r"\blea\s+\(%\w+,%\w+,2\),%\w+", parser)
    assert multiply_by_ten or scaled_accumulation

    watched = {
        "substring": r"object16text_slice_range",
        "object": r"object9new_value",
        "buffer": r"Text(?:5slice|7to_wtf8)",
        "allocator": r"^(?:__libc_)?(?:malloc|calloc|realloc|aligned_alloc|posix_memalign)$",
    }
    # Short text can use pooled objects with inline bytes. Observe that copy
    # through its physical substring entry, not an assumed libc allocation.
    # The long field must allocate a backing buffer even when objects are pooled.
    # Leading zeros keep its numeric result small, excluding legitimate bigint
    # allocation from the zero-allocation parser assertion.
    for prefix, digits, suffix in (
        ("id=", "12345", ";active"),
        ("request-identifier-0123456789:", "-1729", ";ok"),
        ("id=", "0" * 80 + "12345", ";active"),
    ):
        record = prefix + digits + suffix
        runtime_input = f"{record}\n{prefix}\n{suffix}\n"
        expected = f"{int(digits)}\n"
        for mode in ("fused", "retained"):
            assert case.compare(mode + "\n" + runtime_input).stdout == expected

        fused = trace_native(
            case,
            roots={"parser": r"^mule_parse_int_slice$"},
            watched=watched,
            stdin="fused\n" + runtime_input,
        )["parser"]
        assert fused["entries"] == fused["exits"] == 1
        assert all(hits == 0 for hits in fused["watched"].values()), fused

        control = trace_native(
            case,
            roots={"retained": r"^mule_fn_retained_span\.spec\.\d+$"},
            watched=watched,
            stdin="retained\n" + runtime_input,
        )["retained"]
        assert control["entries"] == control["exits"] == 1
        assert control["watched"]["substring"] > 0, control
        if len(digits) > 32:
            assert control["watched"]["allocator"] > 0, control


def test_int_span_bounds_errors_and_operand_effects_match_python(tmp_path):
    case = build_case(
        tmp_path,
        """def span(text, start, stop):
    return int(text[start:stop])

def left(text, stop):
    return int(text[:stop])

def right(text, start):
    return int(text[start:])

def whole(text):
    return int(text[:])

def observed(label, value):
    print(label)
    return value

text = input()
print(span(text, 2, -2))
print(left(text, -2))
print(right(text, 2))
print(whole(text))
print(span(text, -(2 ** 100), 2 ** 100))
print(int(observed("text", text)[observed("start", 2):observed("stop", -2)]))
for start, stop in ((4, 2), (2, 2)):
    try:
        print(span(text, start, stop))
    except ValueError:
        print("ValueError")
for invalid in ("", " ", "12__3", "_123", "123_", "+ 1", "12x3"):
    try:
        print(span("xx" + invalid + "yy", 2, -2))
    except ValueError:
        print("ValueError")
""",
    )
    for text in ("  -12345  ", "  99999999999999999999999999999999999999  "):
        case.compare(text + "\n")


def test_int_span_step_type_keyword_and_base_exclusions_match_python(tmp_path):
    case = build_case(
        tmp_path,
        """def stepped(text):
    return int(text[::2])

def reversed_text(text):
    return int(text[::-1])

def based(text):
    return int(text[1:-1], 16)

def keyword_base(text):
    return int(text[1:-1], base=16)

def keyword_value(text):
    return int(x=text[1:-1])

def bytes_span(text):
    return int(text[1:-1])

class Source:
    def __getitem__(self, key):
        print("getitem", key.start, key.stop, key.step)
        return "37"

class Bound:
    def __index__(self):
        print("index")
        return 1

def custom_span(text, start):
    return int(text[start:-1])

text = input()
print(stepped(text))
print(reversed_text(text))
print(based("x" + text + "y"))
print(keyword_base("x" + text + "y"))
print(bytes_span(("x" + text + "y").encode()))
print(custom_span(Source(), Bound()))
print(custom_span("x" + text + "y", Bound()))
try:
    keyword_value(text)
except TypeError:
    print("TypeError")
""",
    )
    case.compare("12345\n")
    for function in (
        "stepped",
        "reversed_text",
        "based",
        "keyword_base",
        "keyword_value",
        "bytes_span",
        "custom_span",
    ):
        body = case.function_ir(r"mule_fn_" + function + r"\.(?:spec\.\d+|body)$")
        assert "@mule_parse_int_slice(" not in body


def test_int_span_rebound_constructor_receives_slice_once(tmp_path):
    case = build_case(
        tmp_path,
        """def replacement(value):
    print("replacement", value)
    return len(value)

int = replacement

def parse_record(text):
    return int(text[1:-1])

print(parse_record(input()))
""",
    )
    case.compare("x12345y\n")
    body = case.function_ir(r"mule_fn_parse_record\.(?:spec\.\d+|body)$")
    assert "@mule_parse_int_slice(" not in body
    assert "@mule_parse_int_text(" not in body
