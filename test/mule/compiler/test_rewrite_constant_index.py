"""Constant marker tables fold before runtime-dependent record classification."""

import re

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_literal
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize(
    ("literal", "index", "expected"),
    [
        ("'aé🚀Z'", "0", "a"),
        ("'aé🚀Z'", "1", "é"),
        ("'aé🚀Z'", "2", "🚀"),
        ("'aé🚀Z'", "-1", "Z"),
        ("'aé🚀Z'", "-4", "a"),
        ("'aé🚀Z'", "False", "a"),
        ("'aé🚀Z'", "True", "é"),
        ("'e\\u0301'", "1", "\u0301"),
        ("b'\\x00\\x80\\xffA'", "0", 0),
        ("b'\\x00\\x80\\xffA'", "1", 128),
        ("b'\\x00\\x80\\xffA'", "2", 255),
        ("b'\\x00\\x80\\xffA'", "-1", 65),
        ("b'\\x00\\x80\\xffA'", "-4", 0),
        ("b'\\x00\\x80\\xffA'", "False", 0),
        ("b'\\x00\\x80\\xffA'", "True", 128),
    ],
)
def test_constant_index_supplies_literal_to_runtime_comparison(
    literal, index, expected
):
    compilation = compile_source(
        f"def matches(value):\n return value == {literal}[{index}]\n"
    )
    function = next(fn for fn in compilation.lambdas if fn.source_name == "matches")
    assert not list(known_calls(function, "%getitem"))
    (comparison,) = known_calls(function, "%compare_equal")
    folded = constant_literal(comparison.args[1])
    if isinstance(expected, str):
        assert isinstance(folded, ast.TextLiteral)
    else:
        # Bytes indexing returns an integer, including when its index is Boolean.
        assert isinstance(folded, ast.IntegerLiteral)
    assert folded.value == expected
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "expression",
    [
        "'aé🚀Z'[4]",
        "'aé🚀Z'[-5]",
        "b'\\x00\\x80\\xffA'[4]",
        "b'\\x00\\x80\\xffA'[-5]",
        "''[False]",
        "b''[False]",
        "'x'[True]",
        "b'x'[True]",
        "'aé🚀Z'[1.0]",
        "b'\\x00\\x80\\xffA'[1.0]",
        "'aé🚀Z'['1']",
        "b'\\x00\\x80\\xffA'['1']",
        "'aé🚀Z'[None]",
        "b'\\x00\\x80\\xffA'[None]",
    ],
)
def test_invalid_constant_index_keeps_runtime_error_path(expression):
    compilation = compile_source(f"def lookup():\n return {expression}\n")
    function = next(fn for fn in compilation.lambdas if fn.source_name == "lookup")
    assert list(known_calls(function, "%getitem"))
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("literal", ["'aé🚀Z'", "b'\\x00\\x80\\xffA'"])
def test_runtime_index_is_not_mistaken_for_a_constant(literal):
    compilation = compile_source(f"def lookup(index):\n return {literal}[index]\n")
    function = next(fn for fn in compilation.lambdas if fn.source_name == "lookup")
    assert list(known_calls(function, "%getitem"))
    check_ir1_consistency(compilation.components)


MARKER_CLASSIFIER = r"""
def marker_matches(value):
    return (
        value == 'aé🚀Z'[0],
        value == 'aé🚀Z'[1],
        value == 'aé🚀Z'[2],
        value == 'aé🚀Z'[-1],
        value == 'aé🚀Z'[-4],
        value == 'aé🚀Z'[False],
        value == 'aé🚀Z'[True],
        value == 'e\u0301'[1],
    )

def octet_matches(value):
    return (
        value == b'\x00\x80\xffA'[0],
        value == b'\x00\x80\xffA'[1],
        value == b'\x00\x80\xffA'[2],
        value == b'\x00\x80\xffA'[-1],
        value == b'\x00\x80\xffA'[-4],
        value == b'\x00\x80\xffA'[False],
        value == b'\x00\x80\xffA'[True],
    )

print(marker_matches(input()))
print(octet_matches(int(input())))
"""


@pytest.mark.integration
def test_constant_marker_indexing_disappears_from_optimized_classifier(tmp_path):
    case = build_case(tmp_path, MARKER_CLASSIFIER, optimize=3, lto=False)
    # Every value being classified comes from stdin. Only the table lookups fold.
    for marker, octet in (
        ("a", 0),
        ("é", 128),
        ("🚀", 255),
        ("Z", 65),
        ("\u0301", -1),
        ("none", 256),
    ):
        case.compare(stdin=f"{marker}\n{octet}\n")

    for function in ("marker_matches", "octet_matches"):
        body = case.function_ir(rf"mule_fn_{function}(?:\.|$)")
        # The work disappears entirely, rather than moving to a lookup helper.
        assert not re.search(r"@[^\s(]*getitem[^\s(]*\s*\(", body), body
        assert re.search(r"\bicmp (?:eq|ne)\b|@[^\s(]*(?:compare|text_eq)", body), body


INDEX_ERRORS = r"""
def constant_lookup(kind):
    print('before', kind)
    if kind == 'text-high':
        return 'aé🚀Z'[4]
    if kind == 'text-low':
        return 'aé🚀Z'[-5]
    if kind == 'bytes-high':
        return b'\x00\x80\xffA'[4]
    if kind == 'bytes-low':
        return b'\x00\x80\xffA'[-5]
    if kind == 'text-empty':
        return ''[False]
    if kind == 'bytes-empty':
        return b''[False]
    if kind == 'text-bool-high':
        return 'x'[True]
    if kind == 'bytes-bool-high':
        return b'x'[True]
    if kind == 'text-float':
        return 'aé🚀Z'[1.0]
    if kind == 'bytes-float':
        return b'\x00\x80\xffA'[1.0]
    if kind == 'text-string':
        return 'aé🚀Z'['1']
    if kind == 'bytes-string':
        return b'\x00\x80\xffA'['1']
    if kind == 'text-none':
        return 'aé🚀Z'[None]
    return b'\x00\x80\xffA'[None]

try:
    print(constant_lookup(input()))
except IndexError:
    print('IndexError')
except TypeError:
    print('TypeError')
"""


@pytest.mark.integration
def test_invalid_literal_index_reports_error_after_prior_effect(tmp_path):
    case = build_case(tmp_path, INDEX_ERRORS, optimize=3, lto=False)
    for kind in (
        "text-high",
        "text-low",
        "bytes-high",
        "bytes-low",
        "text-empty",
        "bytes-empty",
        "text-bool-high",
        "bytes-bool-high",
        "text-float",
        "bytes-float",
        "text-string",
        "bytes-string",
        "text-none",
        "bytes-none",
    ):
        case.compare(stdin=f"{kind}\n")


DYNAMIC_INDEX = r"""
def read_marker(index):
    return 'aé🚀Z'[index]

def read_octet(index):
    return b'\x00\x80\xffA'[index]

index = int(input())
for read in (read_marker, read_octet):
    try:
        print(read(index))
    except IndexError:
        print('IndexError')
"""


@pytest.mark.integration
def test_runtime_index_preserves_selection_and_bounds_errors(tmp_path):
    case = build_case(tmp_path, DYNAMIC_INDEX, optimize=3, lto=False)
    for index in (-5, -4, -1, 0, 1, 2, 3, 4):
        case.compare(stdin=f"{index}\n")
