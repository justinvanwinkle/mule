"""Proven low-bit bounds remove masks without changing packing semantics."""

import re
from textwrap import dedent

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize("expression", ["value & 0x1fffff", "0x1fffff & value"])
def test_codepoint_packing_inferred_interval_removes_mask(expression):
    compilation = compile_source(
        f"def pack(character):\n value = ord(character)\n return ({expression}) << 8\n"
    )
    function = next(fn for fn in compilation.lambdas if fn.source_name == "pack")
    check_ir1_consistency(compilation.components)
    assert not list(known_calls(function, "%bit_and"))
    assert list(known_calls(function, "%left_shift"))


@pytest.mark.parametrize("expression", ["value & 255", "255 & value"])
@pytest.mark.parametrize(
    "body",
    [
        """
        def pack(value: mule.int64):
            if value >= 0:
                if value <= 255:
                    return (EXPRESSION) << 8
            return -1
        """,
        """
        def pack(scale: mule.int64):
            total = 0
            for value in range(256):
                total += (EXPRESSION) * scale
            return total
        """,
    ],
    ids=["branch-inclusive-endpoint", "counted-loop"],
)
def test_byte_packing_flow_intervals_remove_mask(expression, body):
    compilation = compile_source(
        "import mule\n" + dedent(body).replace("EXPRESSION", expression)
    )
    function = next(fn for fn in compilation.lambdas if fn.source_name == "pack")
    check_ir1_consistency(compilation.components)
    assert not list(known_calls(function, "%bit_and"))


@pytest.mark.parametrize(
    ("lower", "upper", "mask"),
    [
        (-1, 255, 255),
        (0, 256, 255),
        (0, 255, 254),
        (0, 255, 0xF0),
        (0, 255, -2),
    ],
    ids=[
        "negative-value",
        "one-past-mask",
        "noncontiguous",
        "high-nibble",
        "negative-mask",
    ],
)
def test_packing_near_miss_intervals_keep_mask(lower, upper, mask):
    compilation = compile_source(
        "import mule\n"
        "def pack(value: mule.int64):\n"
        f" if value >= {lower}:\n"
        f"  if value <= {upper}:\n"
        f"   return value & {mask}\n"
        " return 0\n"
    )
    function = next(fn for fn in compilation.lambdas if fn.source_name == "pack")
    check_ir1_consistency(compilation.components)
    assert list(known_calls(function, "%bit_and"))


def test_unknown_packing_value_keeps_operator_dispatch():
    compilation = compile_source("def pack(value):\n return value & 255\n")
    function = next(fn for fn in compilation.lambdas if fn.source_name == "pack")
    check_ir1_consistency(compilation.components)
    assert list(known_calls(function, "%bit_and"))


_PACKING_PROGRAM = dedent("""
    import mule

    def pack_codepoint_right(character):
        value = ord(character)
        return (value & 0x1fffff) << 8

    def pack_codepoint_left(character):
        value = ord(character)
        return (0x1fffff & value) << 8

    def pack_byte_right(value: mule.int64):
        if value >= 0:
            if value <= 255:
                return (value & 255) << 8
        return -1

    def pack_byte_left(value: mule.int64):
        if value >= 0:
            if value <= 255:
                return (255 & value) << 8
        return -1

    def palette_checksum(scale: mule.int64):
        total = 0
        for value in range(256):
            total += (value & 255) * scale
        return total

    def retain_required_bits(value: mule.int64):
        return value & 255, value & 240, value & 254

    def unknown_right(value):
        return value & 255

    def unknown_left(value):
        return 255 & value

    class Field:
        def __and__(self, mask):
            print('and', mask)
            return 37

        def __rand__(self, mask):
            print('rand', mask)
            return 41

    def evaluated():
        print('evaluate')
        return Field()

    character = input()
    value = int(input())
    print(pack_codepoint_right(character), pack_codepoint_left(character))
    print(pack_byte_right(value), pack_byte_left(value))
    print(palette_checksum(value))
    print(retain_required_bits(value))
    print(unknown_right(evaluated()), unknown_left(evaluated()))
    try:
        unknown_right(character)
    except TypeError:
        print('noninteger-mask-error')
    try:
        pack_codepoint_right(character + character)
    except TypeError:
        print('codepoint-length-error')
""")


@pytest.mark.integration
def test_runtime_packing_eliminates_actual_masks_and_matches_python(tmp_path):
    case = build_case(tmp_path, _PACKING_PROGRAM, optimize=3, lto=False)
    for character, value in [("\0", -1), ("é", 0), ("𐐷", 255), ("\U0010ffff", 256)]:
        case.compare(stdin=f"{character}\n{value}\n")

    # Inspect every retained entry, not just a selected specialization. These
    # packing bodies may retain tag checks (AND 1), but not the source masks.
    for name, mask in [
        ("pack_codepoint_right", 0x1FFFFF),
        ("pack_codepoint_left", 0x1FFFFF),
        ("pack_byte_right", 255),
        ("pack_byte_left", 255),
    ]:
        body = case.function_ir(rf"^mule_fn_{name}\.(?:body|spec\.)")
        assert re.search(r"\bshl\s+(?:(?:nuw|nsw)\s+)*i64\b", body), body
        # Tagged values use (n << 1) | 1. LLVM may combine masking with tagging.
        for possible_mask in (mask, mask << 1, (mask << 1) | 1):
            assert not re.search(rf"\band i64 [^\n]*, {possible_mask}\b", body), body
        assert not re.search(r'@"?mule\.(?:fast|fixnum)\.binary\.10\b', body), body
        assert "@mule_binary" not in body, body
