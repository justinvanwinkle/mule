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
from mule import optimize
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
