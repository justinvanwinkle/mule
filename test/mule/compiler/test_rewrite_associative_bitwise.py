"""Associative bitwise rewrites on runtime permission/feature words."""

import re

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_literal
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize(
    ("operator", "name", "first", "second", "combined"),
    [
        ("&", "%bit_and", 0xFF0F, 0xF0FF, 0xF00F),
        ("|", "%bit_or", 0x30, 0x81, 0xB1),
        ("^", "%bit_xor", 0x55, 0xAA, 0xFF),
    ],
)
@pytest.mark.integration
def test_associative_flags_combine_constants_in_optimized_work(
    tmp_path, operator, name, first, second, combined
):
    source = (
        "import mule\n"
        "def flags(word: mule.int64) -> mule.int64:\n"
        f" return (word {operator} {first}) {operator} {second}\n"
        "word = int(input())\n"
        "normalized = flags(word)\n"
        "print(normalized, bool(normalized & 4), bool(normalized & 128), word)\n"
    )
    compilation = compile_source(source)
    fn = next(fn for fn in compilation.lambdas if fn.source_name == "flags")
    operations = list(known_calls(fn, name))
    assert len(operations) == 1
    literal = constant_literal(operations[0].args[1])
    assert isinstance(literal, ast.IntegerLiteral) and literal.value == combined
    check_ir1_consistency(compilation.components)

    case = build_case(tmp_path, source)
    for word in (0, 1, 255, -1, -(1 << 63), (1 << 63) - 1):
        case.compare(f"{word}\n")

    # Follow the native body selected by the real annotated entry. Clang can
    # inline it into the wrapper, so inspect both if the native symbol survives.
    entry = case.function_ir(r"^mule_fn_flags\.body$", optimized=False)
    native = re.findall(
        r"@(?:\"(mule\.native\.[^\"]+)\"|(mule\.native\.[\w.]+))", entry
    )
    assert native, entry
    symbols = [re.escape(a or b) for a, b in native]
    body = case.function_ir(r"^(?:mule_fn_flags\.body|" + "|".join(symbols) + r")$")
    opcode = {"&": "and", "|": "or", "^": "xor"}[operator]
    if not re.search(rf"\b{opcode} i64 [^\n]*, {combined}\b", body):
        # Clang can distribute the result's tag shift through AND. Require
        # the same input shift, combined payload mask, and tagged result.
        assert operator == "&", body
        shifted_mask = re.search(
            rf"(%[\w.]+) = and i64 (%[\w.]+), {combined << 1}\b", body
        )
        assert shifted_mask, body
        output, shifted_input = shifted_mask.groups()
        assert re.search(
            rf"{re.escape(shifted_input)} = shl (?:\w+ )*i64 %[^,\n]+, 1\b", body
        ), body
        assert re.search(rf"= or (?:\w+ )*i64 {re.escape(output)}, 1\b", body), body
    for original in (first, second):
        for representation in (original, original << 1):
            assert not re.search(rf"\b{opcode} i64 [^\n]*, {representation}\b", body), (
                body
            )
    # These entries operate on raw int64 words. No bitwise work may move into
    # boxed dispatch or a separately compiled arithmetic helper.
    assert not re.search(r"call[^\n]*@[^\n]*(?:binary|bit_and|bit_or|bit_xor)", body), (
        body
    )


@pytest.mark.integration
def test_associative_flag_toggle_cancellation_preserves_runtime_word(tmp_path):
    source = (
        "import mule\n"
        "def restore(word: mule.int64) -> mule.int64:\n"
        " return (word ^ 21930) ^ 21930\n"
        "word = int(input())\n"
        "print(restore(word), word)\n"
    )
    case = build_case(tmp_path, source)
    for word in (0, 21930, -1, -(1 << 63), (1 << 63) - 1):
        case.compare(f"{word}\n")
    # Cancellation needs no raw arithmetic entry. The annotated wrapper can
    # delegate to the boxed identity body instead. Inspect that body as well.
    body = case.function_ir(r"^mule_fn_restore\.body(?:\.boxed)?$")
    assert not re.search(r"\bxor i64\b", body), body
    assert not re.search(r"call[^\n]*@[^\n]*(?:binary|bit_xor)", body), body
    callees = re.findall(r'call[^\n]*?@"?([\w.]+)', body)
    delegates = [callee for callee in callees if callee.startswith("mule_fn_")]
    assert set(delegates) <= {"mule_fn_restore.body.boxed"}, body
    # Error cleanup merges null with the unchanged input. Follow those phi
    # edges rather than requiring LLVM to emit a particular return spelling.
    phis = {
        result: re.findall(r"\[\s*([^,\]]+),", operands)
        for result, operands in re.findall(r"(%[\w.]+) = phi ptr ([^\n]+)", body)
    }
    pending = re.findall(r"ret ptr (%[\w.]+)", body)
    assert pending, body
    seen = set()
    while pending:
        value = pending.pop()
        if value in seen:
            continue
        seen.add(value)
        if value in ("null", "%parameter.0"):
            continue
        assert value in phis, body
        pending.extend(phis[value])
    assert "%parameter.0" in seen, body


@pytest.mark.parametrize(
    "operator,name", [("&", "%bit_and"), ("|", "%bit_or"), ("^", "%bit_xor")]
)
def test_associative_shared_flag_word_keeps_intermediate_value(operator, name):
    source = (
        "import mule\n"
        "def audit(word: mule.int64):\n"
        f" intermediate = word {operator} 85\n"
        f" final = intermediate {operator} 170\n"
        " return intermediate, final\n"
    )
    compilation = compile_source(source)
    fn = next(fn for fn in compilation.lambdas if fn.source_name == "audit")
    assert len(list(known_calls(fn, name))) == 2
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "operator,name", [("&", "%bit_and"), ("|", "%bit_or"), ("^", "%bit_xor")]
)
@pytest.mark.parametrize("bits", [128, 129])
def test_associative_constant_combination_respects_fold_size(operator, name, bits):
    # Literal spelling avoids testing an unrelated constant-shift transform.
    # The result is exactly at, or one bit above, the 128-bit fold budget.
    first = (1 << (bits - 1)) | 5
    second = first if operator == "&" else 2
    source = (
        "import mule\n"
        "def flags(word: mule.int64):\n"
        f" return (word {operator} {first}) {operator} {second}\n"
    )
    compilation = compile_source(source)
    fn = next(fn for fn in compilation.lambdas if fn.source_name == "flags")
    assert len(list(known_calls(fn, name))) == (1 if bits == 128 else 2)
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
@pytest.mark.parametrize(
    "observe_finalizers", [False, True], ids=["effects", "finalizers"]
)
def test_associative_flag_fallbacks_preserve_types_aliases_and_effect_order(
    tmp_path, observe_finalizers
):
    source = """
import mule
def audit(word: mule.int64):
    intermediate = word ^ 85
    final = intermediate ^ 170
    return intermediate, final

def mask():
    print("mask")
    return 170

def nonadjacent(word):
    return (word ^ 85) ^ mask()

def typed_nonadjacent(word: mule.int64):
    return (word ^ 85) ^ mask()

def boolean_word(raw):
    word = bool(raw)
    result = (word ^ 85) ^ 85
    print(result, result is True, result is False, word)
    print((word & True) & True, (word | False) | False)
    print((word & 1) & 1, (word | 0) | 0)

def wide_flags(word: mule.int64):
    at = (word | 170141183460469231731687303715884105733) | 2
    above = (word | 340282366920938463463374607431768211461) | 2
    return at, above

class Flags:
    def __init__(self, word):
        self.word = word
    def __xor__(self, mask):
        print("xor", self.word, mask)
        if mask == 170:
            raise ValueError("invalid flags")
        return Flags(self.word ^ mask)
FINALIZER

word = int(input())
print(audit(word), word)
print(wide_flags(word))
print(nonadjacent(word))
print(typed_nonadjacent(word))
boolean_word(input())
original = Flags(word)
alias = original
try:
    nonadjacent(original)
except ValueError:
    print("error", original is alias, original.word)
del original
print("alias", alias.word)
del alias
try:
    nonadjacent("not flags")
except TypeError:
    print("bad word")
"""
    finalizer = (
        '    def __del__(self):\n        print("released", self.word)'
        if observe_finalizers
        else ""
    )
    source = source.replace("FINALIZER", finalizer)
    compilation = compile_source(source)
    fn = next(fn for fn in compilation.lambdas if fn.source_name == "typed_nonadjacent")
    assert len(list(known_calls(fn, "%bit_xor"))) == 2
    check_ir1_consistency(compilation.components)
    case = build_case(tmp_path, source)
    for stdin in ("0\n\n", "255\nyes\n", "-1\nfalse\n"):
        case.compare(stdin)
