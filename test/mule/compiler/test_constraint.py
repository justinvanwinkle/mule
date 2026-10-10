"""Flow propagation and local-call analysis establish proven facts and
predictions from source, through the IR1 phases."""

import pytest

from mule import ast
from mule.compiler import ir, lower
from mule.compiler.ctype import (
    BOTTOM,
    FLOAT,
    INTEGER,
    UNBOUND,
    UNKNOWN,
    BooleanType,
    BytesType,
    ImmortalType,
    IntegerType,
    NoneType,
    TextType,
)


def function(module, name):
    return next(f for f in module.functions if f.name == name)


def instructions(function, kind):
    return [
        instruction
        for block in function.blocks
        for instruction in block.instructions
        if isinstance(instruction, kind)
    ]


def fact(function, instruction, context=None):
    facts = context.types if context is not None else function.facts
    return facts[instruction.result]


def last_fact(function, kind, context=None):
    return fact(function, instructions(function, kind)[-1], context)


def observed(function):
    return next(context for context in function.variants if context.id == "observed")


def fixnum_context(function):
    return next(context for context in function.variants if context.id == "fixnum")


@pytest.mark.parametrize(
    ("literal", "expected", "fixnum", "immortal"),
    [
        ("0", IntegerType(0, 0), True, False),
        (str(-(2**62)), IntegerType(-(2**62), -(2**62)), True, False),
        (str(2**62 - 1), IntegerType(2**62 - 1, 2**62 - 1), True, False),
        (str(-(2**62) - 1), IntegerType(-(2**62) - 1, -(2**62) - 1), False, False),
        (str(2**62), IntegerType(2**62, 2**62), False, False),
        ("True", BooleanType(1, 1), False, True),
        ("False", BooleanType(0, 0), False, True),
        ("None", NoneType(), False, True),
        ("1.0", FLOAT, False, False),
        ("'text'", TextType(), False, False),
        ("b'bytes'", BytesType(), False, False),
    ],
)
def test_constant_representation_is_proven(literal, expected, fixnum, immortal):
    module = lower(f"x = {literal}\n")
    entry = module.functions[0]
    # A negative literal is a unary operation on a constant; test the stored value.
    facts = entry.facts[instructions(entry, ir.Store)[0].value]
    assert facts == expected
    assert facts.is_fixnum is fixnum
    assert facts.is_immortal is immortal


@pytest.mark.parametrize(
    ("operator", "left", "right", "minimum", "maximum", "fixnum"),
    [
        ("+", 3, 4, 7, 7, True),
        ("+", 2**62 - 1, 1, 2**62, 2**62, False),
        ("-", -(2**62), 1, -(2**62) - 1, -(2**62) - 1, False),
        ("*", 2**40, 2**40, 2**80, 2**80, False),
        ("&", -(2**100), 255, 0, 0, True),
        (">>", -5, 1, -3, -3, True),
        ("<<", 1, 62, 2**62, 2**62, False),
    ],
)
def test_integer_ranges_detect_promotion(
    operator, left, right, minimum, maximum, fixnum
):
    # Operands flow through locals so the operation is derived, not folded.
    module = lower(f"a = {left}\nb = {right}\nx = a {operator} b\n")
    result = last_fact(module.functions[0], ir.Binary)
    assert (result.minimum, result.maximum) == (minimum, maximum)
    assert result.is_fixnum is fixnum


def test_literal_arithmetic_is_folded_to_a_constant():
    module = lower("x = 3 + 4\ny = 1 < 2\n")
    entry = module.functions[0]
    assert instructions(entry, ir.Binary) == []
    assert instructions(entry, ir.Compare) == []
    values = [constant.value for constant in instructions(entry, ir.Constant)]
    assert isinstance(values[0], ast.IntegerLiteral) and values[0].value == 7
    assert isinstance(values[1], ast.BooleanLiteral) and values[1].value is True


def test_bool_arithmetic_does_not_decode_heap_bool_as_integer_tag():
    module = lower("a = True\nb = False\ns = a + b\n")
    entry = module.functions[0]
    loads = instructions(entry, ir.Load)
    assert not fact(entry, loads[0]).is_fixnum
    assert fact(entry, loads[0]).is_immortal
    assert last_fact(entry, ir.Binary).is_fixnum


def test_unknown_parameter_operator_can_return_arbitrary_object():
    module = lower("def f(x):\n return x + 1\n")
    body = module.functions[1]
    assert last_fact(body, ir.Load) == UNKNOWN
    assert last_fact(body, ir.Binary) == UNKNOWN


@pytest.mark.parametrize("operator", ["-", "~"])
def test_unary_range_follows_integer_bounds(operator):
    module = lower(f"a = {-(2**62)}\nr = {operator}a\n")
    result = last_fact(module.functions[0], ir.Unary)
    assert result.is_fixnum is (operator == "~")


def joined(left, right=None):
    else_body = "" if right is None else f"\n else:\n  x = {right}"
    return lower(f"def f(c):\n if c:\n  x = {left}{else_body}\n return x\n").functions[
        1
    ]


def test_branch_join_unions_integer_bounds():
    result = last_fact(joined(-3, 8), ir.Load)
    assert result.is_fixnum
    assert (result.minimum, result.maximum) == (-3, 8)


def test_branch_join_preserves_null_possibility():
    result = last_fact(joined(3), ir.Load)
    assert result.may_be_unbound
    assert not result.is_fixnum
    assert not result.is_immortal


def test_bool_and_integer_join_is_not_proven_tagged():
    assert not last_fact(joined("True", 1), ir.Load).is_fixnum


def test_bool_and_none_join_preserves_immortality():
    assert last_fact(joined("True", "None"), ir.Load).is_immortal


def test_missing_immortal_assignment_keeps_unbound_load_check():
    result = last_fact(joined("None"), ir.Load)
    assert result.may_be_unbound
    assert not result.is_immortal


def loop(*, bounded):
    test = "x < 10" if bounded else "x"
    return lower(
        f"def f():\n x = 0\n while {test}:\n  x = x + 1\n return x\n"
    ).functions[1]


def test_unbounded_loop_widens_and_converges():
    result = last_fact(loop(bounded=False), ir.Binary)
    assert result.fits(INTEGER)
    assert not result.is_fixnum
    assert result.maximum is None


def test_integer_branch_constraint_keeps_bounded_loop_body_tagged():
    result = last_fact(loop(bounded=True), ir.Binary)
    assert result.is_fixnum
    assert result.maximum == 10


def test_bounded_bit_mask_recovers_fixnum_after_loop_widening():
    module = lower("def f(c):\n x = 0\n while c:\n  x = x + 1\n return x & 255\n")
    body = module.functions[1]
    mask = next(
        instruction
        for instruction in instructions(body, ir.Binary)
        if instruction.operator == ast.BinaryOperator.BIT_AND
    )
    final = body.facts[mask.left]
    masked = fact(body, mask)
    assert not final.is_fixnum
    assert masked.is_fixnum
    assert (masked.minimum, masked.maximum) == (0, 255)


def test_rebound_global_is_only_a_guarded_target_candidate():
    module = lower("def f():\n return 7\nf = 3\nresult = f()\n")
    entry = module.functions[0]
    call = instructions(entry, ir.Call)[0]
    assert entry.targets[call.callee] == "mule_fn_1"
    assert entry.facts[call.callee].fits(INTEGER)
    assert fact(entry, call) == UNKNOWN


def test_local_alias_propagates_guarded_candidate():
    module = lower("def f():\n return 7\ndef g():\n alias = f\n return alias()\n")
    body = module.functions[2]
    call = instructions(body, ir.Call)[0]
    assert body.targets[call.callee] == "mule_fn_1"
    assert fact(body, call) == UNKNOWN


def test_proven_function_identity_propagates_its_return_summary():
    module = lower("def target():\n return 7\nresult = target()\n")
    entry = module.functions[0]
    result = last_fact(entry, ir.Call)
    assert result.is_fixnum
    assert (result.minimum, result.maximum) == (7, 7)


@pytest.mark.parametrize(
    ("initial", "step", "expected"),
    [(0, -1, UNKNOWN), (-1, -1, FLOAT), (0, 1, INTEGER)],
)
def test_widened_integer_exponent_preserves_negative_power_type(
    initial, step, expected
):
    module = lower(
        f"def f(c):\n exponent = {initial}\n power = None\n while c:\n"
        f"  power = 2 ** exponent\n  exponent = exponent + {step}\n return power\n"
    )
    body = module.functions[1]
    power = next(
        instruction
        for instruction in instructions(body, ir.Binary)
        if instruction.operator == ast.BinaryOperator.POWER
    )
    result = fact(body, power)
    assert result == expected
    assert not result.is_fixnum


@pytest.mark.parametrize("right_target", ["target", "other", None])
def test_local_candidate_join_requires_same_target_on_every_path(right_target):
    else_body = "" if right_target is None else f"\n else:\n  alias = {right_target}"
    module = lower(
        "def target():\n return None\ndef other():\n return None\n"
        f"def f(c):\n if c:\n  alias = target{else_body}\n return alias()\n"
    )
    body = module.functions[3]
    call = instructions(body, ir.Call)[0]
    assert body.targets.get(call.callee) == (
        "mule_fn_1" if right_target == "target" else None
    )
    assert not fact(body, call).is_fixnum


def test_unknown_rich_comparison_may_return_non_boolean():
    module = lower("def f(x, y):\n return x < y\n")
    assert not last_fact(module.functions[1], ir.Compare).is_immortal


def test_not_of_unknown_object_always_returns_immortal_boolean():
    module = lower("def f(x):\n return not x\n")
    assert last_fact(module.functions[1], ir.Unary).is_immortal


def integer_calls(arguments):
    calls = "".join(f"increment({argument})\n" for argument in arguments)
    return lower(f"def increment(x):\n return x + 1\n{calls}")


def test_callsite_ranges_join_in_optional_context_not_public_parameters():
    module = integer_calls(("2", "8"))
    increment = module.functions[1]
    context = observed(increment)
    assert (context.parameters[0].minimum, context.parameters[0].maximum) == (2, 8)
    assert (context.return_type.minimum, context.return_type.maximum) == (3, 9)
    parameter = instructions(increment, ir.Load)[0]
    assert fact(increment, parameter, context).is_fixnum
    assert fact(increment, parameter) == UNKNOWN
    assert increment.return_type == UNKNOWN


def test_proven_caller_uses_matching_context_return_range():
    module = integer_calls(("2", "8"))
    entry = module.functions[0]
    first, second = instructions(entry, ir.Call)
    assert fact(entry, first).is_fixnum
    assert (fact(entry, first).minimum, fact(entry, first).maximum) == (3, 9)
    # The first call may run arbitrary code, so the second binding is unproven.
    assert fact(entry, second) == UNKNOWN


def test_fixnum_context_cannot_assume_fixnum_arithmetic_result_on_overflow():
    module = integer_calls((str(2**62 - 1),))
    increment = module.functions[1]
    context = observed(increment)
    assert context.parameters[0].is_fixnum
    assert not context.return_type.is_fixnum
    assert context.return_type.maximum == 2**62
    full = fixnum_context(increment)
    assert fact(increment, instructions(increment, ir.Load)[0], full).is_fixnum
    assert not last_fact(increment, ir.Binary, full).is_fixnum


def test_mixed_calls_do_not_assign_integer_facts_to_bool_float_public_entry():
    module = integer_calls(("3", "True", "2.0"))
    entry, increment = module.functions[0], module.functions[1]
    context = observed(increment)
    assert (context.parameters[0].minimum, context.parameters[0].maximum) == (3, 3)
    calls = instructions(entry, ir.Call)
    assert fact(entry, calls[0]).is_fixnum
    assert fact(entry, calls[1]) == UNKNOWN
    assert fact(entry, calls[2]) == UNKNOWN
    assert fact(increment, instructions(increment, ir.Load)[0]) == UNKNOWN


def test_dynamic_call_has_guarded_fixnum_context_without_observed_integer_seed():
    module = lower("def increment(x):\n return x + 1\nincrement(argument)\n")
    entry, increment = module.functions[0], module.functions[1]
    assert [context.id for context in increment.variants] == ["fixnum"]
    assert last_fact(entry, ir.Call) == UNKNOWN


def test_global_store_proves_function_until_an_arbitrary_call():
    module = lower("def target():\n return 7\nfirst = target()\nsecond = target()\n")
    entry = module.functions[0]
    first, second = instructions(entry, ir.Call)
    assert fact(entry, first).is_fixnum
    assert entry.facts[second.callee] == UNKNOWN
    assert fact(entry, second) == UNKNOWN


def test_loaded_callee_keeps_identity_when_argument_call_rebinds_globals():
    module = lower("def target(ignored):\n return 7\nresult = target(effect())\n")
    entry = module.functions[0]
    assert last_fact(entry, ir.Call).is_fixnum


def test_module_definition_is_not_a_fact_at_another_function_entry():
    module = lower("def target():\n return 7\ndef wrapper():\n return target()\n")
    wrapper = module.functions[2]
    call = instructions(wrapper, ir.Call)[0]
    assert wrapper.targets[call.callee] == "mule_fn_1"
    assert wrapper.return_type == UNKNOWN


@pytest.mark.parametrize("same_target", [False, True])
def test_joined_global_functions_require_unanimous_proven_identity(same_target):
    second = "first" if same_target else "second"
    module = lower(
        "def first():\n return 7\ndef second():\n return 'text'\n"
        f"c = 1\nif c < 2:\n f = first\nelse:\n f = {second}\nresult = f()\n"
    )
    entry = module.functions[0]
    result = last_fact(entry, ir.Call)
    assert result.is_fixnum is same_target
    if not same_target:
        assert result == UNKNOWN


def test_unbound_argument_does_not_seed_or_prove_specialized_call():
    module = lower(
        "def increment(x):\n return x + 1\ndef f(c):\n if c:\n  x = 3\n return increment(x)\n"
    )
    increment, body = module.functions[1], module.functions[2]
    call = instructions(body, ir.Call)[0]
    assert body.facts[call.args[0]].may_be_unbound
    assert fact(body, call) == UNKNOWN
    assert [context.id for context in increment.variants] == ["fixnum"]


def recursive_calls():
    return lower(
        "def recursive(n):\n if n < 100:\n  return recursive(n + 1)\n return n\nrecursive(3)\n"
    )


def test_recursive_argument_propagation_widens_before_publication():
    recursive = recursive_calls().functions[1]
    context = observed(recursive)
    assert context.parameters[0].minimum == 3
    assert context.parameters[0].maximum == 2**62 - 1
    assert fact(recursive, instructions(recursive, ir.Load)[0]) == UNKNOWN


def test_recursive_global_candidate_return_remains_unknown():
    recursive = recursive_calls().functions[1]
    context = observed(recursive)
    assert context.parameters[0].is_fixnum
    assert last_fact(recursive, ir.Binary, context).is_fixnum
    assert last_fact(recursive, ir.Call, context) == UNKNOWN
    assert context.return_type == UNKNOWN


def test_context_ids_and_cardinality_are_stable():
    first = integer_calls(("2", "8")).functions[1].variants
    second = integer_calls(("2", "8")).functions[1].variants
    assert [(c.id, c.parameters, c.return_type) for c in first] == [
        (c.id, c.parameters, c.return_type) for c in second
    ]
    assert len(first) <= 2


def test_argument_types_propagate_through_multiple_local_calls():
    module = lower(
        "def increment(x):\n return x + 1\ndef wrapper(x):\n return increment(x)\nwrapper(3)\n"
    )
    increment, wrapper = module.functions[1], module.functions[2]
    for body in (increment, wrapper):
        context = observed(body)
        assert (context.parameters[0].minimum, context.parameters[0].maximum) == (3, 3)
    returned = observed(increment).return_type
    assert (returned.minimum, returned.maximum) == (4, 4)
    # The call inside wrapper is only predicted, so wrapper publishes no range.
    assert observed(wrapper).return_type == UNKNOWN
    assert last_fact(module.functions[0], ir.Call) == UNKNOWN


def test_source_defined_global_call_gets_only_flow_proven_return_facts():
    module = lower(
        "def increment(x):\n    return x + 1\ny = increment(3)\nz = increment(4)\n"
    )
    entry = module.functions[0]
    calls = instructions(entry, ir.Call)
    assert fact(entry, calls[0]).is_fixnum
    assert fact(entry, calls[1]) == UNKNOWN


@pytest.mark.parametrize("effect", ["x + 1", "not x", "x == 1", "if x:\n    pass"])
def test_user_operator_or_truth_hook_invalidates_stored_global_function(effect):
    module = lower(f"def target():\n    return 7\n{effect}\nresult = target()\n")
    entry = module.functions[0]
    assert last_fact(entry, ir.Call) == UNKNOWN


def test_constant_branch_removes_the_dead_arm():
    module = lower("if True:\n w = 1\nelse:\n w = 0\n")
    entry = module.functions[0]
    assert instructions(entry, ir.Branch) == []
    assert [constant.value.value for constant in instructions(entry, ir.Constant)][
        :1
    ] == [1]


@pytest.mark.parametrize(
    ("actual", "expected", "semantic_match", "dispatch_match"),
    [
        (UNKNOWN, UNKNOWN, True, True),
        (IntegerType(2, 4), IntegerType(1, 5), True, True),
        (IntegerType(2, 4), UNKNOWN, True, True),
        (UNKNOWN, IntegerType(1, 5), False, False),
        (IntegerType(), IntegerType(1, 5), False, False),
        (IntegerType(1, 5), IntegerType(), True, False),
        (BooleanType(1, 1), IntegerType(0, 1), False, False),
        (BooleanType(1, 1), BooleanType(), True, True),
        (NoneType(), ImmortalType(), True, True),
        (BooleanType(), ImmortalType(), True, True),
        (ImmortalType(), ImmortalType(), True, True),
        (ImmortalType(), NoneType(), False, False),
        (IntegerType(1, 1), ImmortalType(), False, False),
        (NoneType(may_be_unbound=True), NoneType(), False, False),
    ],
)
def test_semantic_containment_and_dispatch_proofs(
    actual, expected, semantic_match, dispatch_match
):
    assert actual.fits(expected) is semantic_match
    assert actual.dispatch_fits(expected) is dispatch_match


@pytest.mark.parametrize("actual", [UNKNOWN, IntegerType(3, 3), BOTTOM, UNBOUND])
@pytest.mark.parametrize("expected", [BOTTOM, UNBOUND])
def test_nonvalue_expectations_cannot_prove_a_call_argument(actual, expected):
    assert not actual.fits(expected)
    assert not actual.dispatch_fits(expected)
