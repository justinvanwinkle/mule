"""A value delivered by several producers retains their common representation."""

import pytest

from mule.compiler.codegen import ModuleGenerator
from mule.compiler.driver import compile_program, emit_llvm
from mule.compiler.ltn import ltn_analyze
from mule.compiler.vop import BOXED_FLOAT, FIXNUM, T
from test.mule.compiler.test_llvm_boxing import function_body


@pytest.mark.parametrize(
    ("left", "right", "expected"),
    [("1.25", "2.5", BOXED_FLOAT), ("1", "2", FIXNUM), ("1.25", "None", T)],
    ids=["float", "integer", "heterogeneous"],
)
def test_return_value_join_retains_common_primitive_type(left, right, expected):
    program = compile_program(
        f"def choose(flag):\n if flag:\n  return {left}\n return {right}\n"
    )
    module = ModuleGenerator(program)
    function = program.lambdas[1]
    context, _ = module.contexts(function)
    result = function.return_.result
    assert len(result.uses) > 1
    ltn_analyze(function, context)
    assert result.info.primitive_type is expected


def test_none_narrowed_arithmetic_selects_native_operation_with_joined_return():
    llvm = emit_llvm(
        "def optional(flag):\n"
        " if flag:\n"
        "  value = 1.25\n"
        " else:\n"
        "  value = None\n"
        " if value is None:\n"
        "  return 0.0\n"
        " return value + 2.0\n"
    )
    body = function_body(llvm, '"mule_fn_optional.body"')
    assert "fadd double" in body
    assert "@mule_binary_reuse" not in body
