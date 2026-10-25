"""Proven floats use native arithmetic without changing Python object semantics."""

import pytest

from mule.compiler import emit_llvm
from mule.compiler.ctype import FLOAT, REAL, UNKNOWN, FloatType
from mule.compiler.ltn import primitive_type_of
from mule.compiler.vop import BOXED_FLOAT, T
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_native import run_native


def test_only_bound_concrete_float_facts_select_float_templates():
    assert primitive_type_of(FLOAT) is BOXED_FLOAT
    for fact in (REAL, UNKNOWN, FloatType(may_be_unbound=True)):
        assert primitive_type_of(fact) is T


def test_float_unary_positive_preserves_identity_and_escaping_aliases(tmp_path):
    compare(
        tmp_path,
        """
def check(text):
    original = float(text)
    positive = +original
    print(positive is original)
    saved = [+positive]
    del original, positive
    print(saved[0])
check('1.25')
check('-0.0')
check('inf')
check('nan')
""",
    )


@pytest.mark.parametrize(
    "operator, opcode", [("+", "fadd"), ("-", "fsub"), ("*", "fmul"), ("/", "fdiv")]
)
def test_inferred_floats_emit_native_arithmetic_in_object_entry(operator, opcode):
    llvm = emit_llvm(
        f"def run(n):\n value = 0.5\n for index in range(n):\n"
        f"  value = value {operator} 1.5\n return value\nprint(run(10))\n"
    )
    body = llvm.split('define internal ptr @"mule_fn_run.body"(')[1].split("\n}")[0]
    assert f"{opcode} double" in body
    assert "call double @mule.box.f64.load(" in body
    assert "call ptr @mule.box.f64(" in body
    assert "alloca double" not in body


def test_augmented_float_assignment_selects_guarded_box_reuse():
    llvm = emit_llvm(
        "def run(n):\n value = 0.5\n for i in range(n):\n  value += 1.5\n return value\n"
    )
    assert "call ptr @mule_box_f64_move" in llvm


def test_arithmetic_intermediate_stays_double_until_the_local_store():
    llvm = emit_llvm(
        "def run(n):\n total = 0.0\n value = 0.5\n for i in range(n):\n  total += value * 0.125\n return total\n"
    )
    body = llvm.split('define internal ptr @"mule_fn_run.body"(')[1].split("\n}")[0]
    for multiply in body.split("fmul double")[1:]:
        before_add = multiply.split("fadd double", 1)[0]
        assert "@mule.box.f64(" not in before_add
        assert (
            "@mule.box.f64.load(" in before_add
        )  # The other operand remains an object.


def test_arithmetic_literal_is_loaded_without_a_box_or_reference_updates():
    llvm = emit_llvm(
        "def run(n):\n value = 0.5\n for i in range(n):\n  value += 1.5\n return value\n"
    )
    assert "fadd double" in llvm
    assert "0x3FF8000000000000" in llvm
    assert "call ptr @mule_literal_float(double 0x3FF8000000000000)" not in llvm


def test_inferred_float_arithmetic_preserves_aliases_and_special_values(tmp_path):
    compare(
        tmp_path,
        """
def run(n):
    value = 0.5
    original = value
    for index in range(n):
        value += 0.5
    print(value, original, value is original)
    print((1e308 * 2.0).hex(), (-0.0 * 1.5).hex())
    print((1e308 - -1e308).hex())
    for zero in (0.0, -0.0):
        try:
            value /= zero
        except ZeroDivisionError:
            print('zero division', value)
run(0)
run(3)
class Custom:
    def __add__(self, other):
        return 'custom add'
def add(value):
    return value + 1.5
print(add(Custom()), add(2.0), add(2))
def doubled(n):
    value = 1.5
    for index in range(n):
        previous = value
        value += value
        print(previous, value, previous is value)
doubled(3)
def escaped(n):
    value = 0.5
    for index in range(n):
        middle = value * 0.125
        alias = middle
        print(middle is alias, middle.hex())
        value = middle + 0.5
escaped(3)
""",
    )


def test_unknown_arguments_keep_protocol_dispatch():
    llvm = emit_llvm("def add(value):\n return value + 1.5\n")
    body = llvm.split('define internal ptr @"mule_fn_add.body"(')[1].split("\n}")[0]
    assert "call i32 @mule_unbox_f64" not in body


def test_float_literal_pool_preserves_bits_and_releases_unit_roots(tmp_path):
    source = """
def values():
    return (0.0, -0.0, 0.5, 0.5)
for value in values():
    print(value.hex())
print(values() == values())
"""
    llvm = emit_llvm(source)
    assert "@mule.literal.float.slot.0 = internal global ptr null" in llvm
    assert "call void @mule_release(ptr %float.root.0)" in llvm
    compare(tmp_path, source)


def test_float_pool_keys_distinguish_both_signs_of_zero():
    from mule import ast
    from mule.compiler import compile_program
    from mule.compiler.codegen import ModuleGenerator

    module = ModuleGenerator(compile_program(""))
    positive = module.constant_float(ast.FloatLiteral(value=0.0))
    negative = module.constant_float(ast.FloatLiteral(value=-0.0))
    assert positive != negative
    assert module.constant_float(ast.FloatLiteral(value=0.0)) == positive
    assert module.constant_float(ast.FloatLiteral(value=-0.0)) == negative


def test_object_float_division_keeps_python_error_at_safety_zero(tmp_path):
    result = run_native(
        tmp_path,
        """
from mule_types import optimize
@optimize(speed=3, safety=0)
def run():
    value = 1.5
    try:
        value /= -0.0
    except ZeroDivisionError:
        print('zero division', value)
run()
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "zero division 1.5\n"


@pytest.mark.parametrize(
    "source",
    [
        """
def run(n, label):
    total = 0.5
    for index in range(n):
        if index % 2:
            delta = 1.5
        else:
            delta = 2.5
        total += delta
    print(label)
    return total > 5.0
""",
        """
def run(n, label):
    value = 0.5
    index = 0
    while index < n:
        previous = value
        value = previous * 1.5
        index += 1
    print(label)
    return value > 5.0
""",
    ],
    ids=["branch-and-counted-loop", "copy-and-while-loop"],
)
def test_numeric_only_float_locals_stay_unboxed_across_blocks(source, tmp_path):
    program = source + '\nprint(run(0, "zero"))\nprint(run(8, "eight"))\n'
    llvm = emit_llvm(program)
    body = llvm.split('define internal ptr @"mule_fn_run.body"(')[1].split("\n}")[0]
    assert "fcmp ogt double" in body
    assert "call ptr @mule_box_f64" not in body
    assert "call i32 @mule_unbox_f64" not in body
    assert "@mule.box.f64(" not in body
    assert "@mule.box.f64.load(" not in body
    compare(tmp_path, program)


def test_numeric_only_float_locals_preserve_value_on_failed_assignment(tmp_path):
    compare(
        tmp_path,
        """
def run(n):
    value = -0.0
    for index in range(n):
        value += 1.5
    try:
        value /= -0.0
    except ZeroDivisionError:
        print(value == 0.0, value > 0.0)
    return value > 1.0
print(run(0))
print(run(3))
""",
    )


def test_float_local_storage_preserves_boundness_and_captured_identity(tmp_path):
    compare(
        tmp_path,
        """
def maybe(flag):
    if flag:
        value = 1.5
    try:
        print(value > 1.0)
    except UnboundLocalError:
        print('unbound')
maybe(False)
maybe(True)
def deleted():
    value = 1.5
    del value
    try:
        print(value > 1.0)
    except UnboundLocalError:
        print('deleted')
deleted()
def captured():
    value = 1.5
    def read():
        return value
    saved = value
    print(read() is saved)
    value += 1.5
    print(read() is saved, value > 1.0)
captured()
""",
    )


def test_list_float_indexing_setitem_and_math_sqrt_lower_to_unboxed_doubles(tmp_path):
    source = """
import math

def step(body_a: list[float], body_b: list[float], dt: float) -> float:
    dx = body_a[0] - body_b[0]
    dy = body_a[1] - body_b[1]
    dist = math.sqrt(dx * dx + dy * dy)
    mag = dt / (dist * dist * dist)
    body_a[3] -= dx * body_b[6] * mag
    body_b[3] += dx * body_a[6] * mag
    return body_a[3] + body_b[3]

a = [1.0, 2.0, 0.0, 0.5, 0.0, 0.0, 2.0]
b = [4.0, 6.0, 0.0, -0.25, 0.0, 0.0, 3.0]
print(round(step(a, b, 0.01), 12))
print(round(a[3], 12), round(b[3], 12))
try:
    math.sqrt(-1.0)
except ValueError as exc:
    print("sqrt error:", type(exc).__name__)
"""
    llvm = emit_llvm(source)
    assert "call double @llvm.sqrt.f64(double" in llvm
    assert "@mule_list_getitem_borrow(" in llvm
    assert "call double @mule.box.f64.load(" in llvm
    compare(tmp_path, source)


def test_min2_and_max2_specialize_inline_and_preserve_first_operand_on_ties(tmp_path):
    source = """
def clamp_int(x: int, lo: int, hi: int) -> int:
    return min(max(x, lo), hi)

def clamp_float(x: float, lo: float, hi: float) -> float:
    return min(max(x, lo), hi)

print(clamp_int(-5, 0, 10), clamp_int(5, 0, 10), clamp_int(15, 0, 10))
print(clamp_float(-5.5, 0.0, 10.0), clamp_float(5.5, 0.0, 10.0), clamp_float(15.5, 0.0, 10.0))
a = [1, 2]
b = [1, 2]
print(min(a, b) is a, max(a, b) is a)
"""
    llvm = emit_llvm(source)
    assert "select i1" in llvm
    compare(tmp_path, source)


def test_bytes_and_bytearray_fast_indexing_length_and_append(tmp_path):
    source = """
def transform(packet: bytes) -> int:
    buf = bytearray()
    n = len(packet)
    i = 0
    while i < n:
        buf.append((packet[i] ^ (i & 255)) & 255)
        i += 1
    if len(buf) > 1:
        buf[0] = (buf[0] + buf[-1]) & 255
    total = 0
    for b in buf:
        total = (total + b) & 0xFFFFFFFF
    return total

print(transform(b"hello world"))
print(transform(b"\\x00\\xff\\x7f\\x80"))
"""
    llvm = emit_llvm(source)
    assert "@mule_bytes_getitem(" in llvm
    assert "@mule_bytearray_append_fast(" in llvm
    assert "@mule_bytearray_getitem_fast(" in llvm
    assert "@mule_bytearray_setitem_fast(" in llvm
    compare(tmp_path, source)


def test_exact_list_getitem_tuple_unpack_borrows_container_and_elements(tmp_path):
    source = """
def binary_search_intervals(merged: list[tuple[int, int, int]], n: int) -> int:
    checksum = 0
    for q in range(n):
        lo = 0
        hi = len(merged) - 1
        matched_weight = 0
        while lo <= hi:
            mid = (lo + hi) >> 1
            m_start, m_end, m_weight = merged[mid]
            if m_end < q:
                lo = mid + 1
            elif q < m_start:
                hi = mid - 1
            else:
                matched_weight = m_weight
                break
        checksum += matched_weight
    return checksum

print(binary_search_intervals([(0, 2, 1), (4, 6, 2), (8, 10, 3), (12, 14, 4)], 16))
"""
    llvm = emit_llvm(source)
    body = llvm.split('define internal ptr @"mule_fn_binary_search_intervals.spec.1"')[
        1
    ].split("\n}")[0]
    assert "call ptr @mule_list_getitem_borrow(" in body
    assert body.count("call ptr @mule.fast.tuple.borrow(") == 2
    compare(tmp_path, source)
