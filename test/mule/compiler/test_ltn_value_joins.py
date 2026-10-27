"""A value delivered by several producers retains their common representation."""

import pytest

from mule.compiler import vops
from mule.compiler.codegen import ModuleGenerator, compile_component
from mule.compiler.ctype import UNKNOWN
from mule.compiler.driver import compile_program, emit_llvm
from mule.compiler.ir2tran import ir2_convert
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


def test_conversion_uses_current_types_after_a_standalone_ltn_pass():
    program = compile_program("def choose(flag):\n if flag:\n  return 1\n return 2\n")
    function = program.lambdas[1]
    context, _ = ModuleGenerator(program).contexts(function)
    assert function.return_ is not None
    result = function.return_.result
    ltn_analyze(function, context)
    context.types.update({producer: UNKNOWN for producer in result.uses})

    component = compile_component(function, context)
    returned = [
        vop.args[0].tn.type
        for block in component.blocks
        for vop in block.vops
        if vop.info is vops.RETURN
    ]
    assert returned == [UNKNOWN]
    assert result.info is None


def test_successive_conversions_do_not_reuse_previous_types():
    program = compile_program("def choose(flag):\n if flag:\n  return 1\n return 2\n")
    function = program.lambdas[1]
    context, _ = ModuleGenerator(program).contexts(function)
    assert function.return_ is not None
    result = function.return_.result

    first = compile_component(function, context)
    context.types.update({producer: UNKNOWN for producer in result.uses})
    second = compile_component(function, context)
    first_type = next(
        vop.args[0].tn.type
        for block in first.blocks
        for vop in block.vops
        if vop.info is vops.RETURN
    )
    second_type = next(
        vop.args[0].tn.type
        for block in second.blocks
        for vop in block.vops
        if vop.info is vops.RETURN
    )
    assert first_type.is_fixnum
    assert second_type is UNKNOWN


@pytest.mark.parametrize("stale_token", [False, True], ids=["absent", "stale"])
def test_ir2_without_current_ltn_provenance_joins_current_types(stale_token):
    program = compile_program("def choose(flag):\n if flag:\n  return 1\n return 2\n")
    function = program.lambdas[1]
    context, _ = ModuleGenerator(program).contexts(function)
    assert function.return_ is not None
    result = function.return_.result
    ltn_analyze(function, context)
    context.types.update({producer: UNKNOWN for producer in result.uses})

    component = ir2_convert(
        function, context, ltn_token=object() if stale_token else None
    )
    returned = [
        vop.args[0].tn.type
        for block in component.blocks
        for vop in block.vops
        if vop.info is vops.RETURN
    ]
    assert returned == [UNKNOWN]
