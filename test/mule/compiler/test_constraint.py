"""Flow propagation and local-call analysis establish proven facts and
predictions from source, through the IR1 phases."""

import pytest

from mule import ast
from mule.compiler import fndb
from mule.compiler.ctype import (
    BOTTOM,
    FLOAT,
    INTEGER,
    PYTHON_INTEGER,
    REAL,
    UNBOUND,
    UNKNOWN,
    BooleanType,
    BytesType,
    ImmortalType,
    IntegerType,
    NoneType,
    PythonIntegerType,
    TextType,
    TupleType,
    UnionType,
)
from mule.compiler.driver import emit_llvm
from mule.compiler.main import Compilation, compile_source
from mule.compiler.node import (
    CallKind,
    Combination,
    Constant,
    GlobalVar,
    If,
    Lambda,
    LambdaVar,
    Ref,
    Set,
)

BINARY_KNOWNS = frozenset(fndb.BINARY.values())
UNARY_KNOWNS = frozenset(fndb.UNARY.values())
COMPARE_KNOWNS = frozenset(fndb.COMPARE.values())


def lower(source):
    return compile_source(source)


def function(compilation, index):
    return compilation.lambdas[index]


def nodes(lambda_):
    return [node for block in lambda_.component.blocks() for node in block.nodes()]


def knowns(lambda_, infos):
    return [
        node
        for node in nodes(lambda_)
        if isinstance(node, Combination)
        and node.kind is CallKind.KNOWN
        and node.fun_info in infos
    ]


def binaries(lambda_):
    return knowns(lambda_, BINARY_KNOWNS)


def unaries(lambda_):
    return knowns(lambda_, UNARY_KNOWNS)


def compares(lambda_):
    return knowns(lambda_, COMPARE_KNOWNS)


def calls(lambda_):
    return [
        node
        for node in nodes(lambda_)
        if isinstance(node, Combination)
        and node.kind in (CallKind.FULL, CallKind.GUARDED)
    ]


def loads(lambda_):
    """References to variables, local or global, in control order."""
    return [
        node
        for node in nodes(lambda_)
        if isinstance(node, Ref) and isinstance(node.leaf, (LambdaVar, GlobalVar))
    ]


def stores(lambda_):
    return [node for node in nodes(lambda_) if isinstance(node, Set)]


def stored_value(lambda_, name):
    return next(node.value for node in stores(lambda_) if node.var.name == name)


def constants(lambda_):
    return [
        node.leaf.value
        for node in nodes(lambda_)
        if isinstance(node, Ref) and isinstance(node.leaf, Constant)
    ]


def branches(lambda_):
    return [node for node in nodes(lambda_) if isinstance(node, If)]


def fact(compilation, lambda_, node, context=None):
    facts = context.types if context is not None else compilation.facts[lambda_].types
    return facts.get(node, UNKNOWN)


def operand_fact(compilation, lambda_, lvar, context=None):
    return fact(compilation, lambda_, lvar.use(), context)


def last_fact(compilation, lambda_, selector, context=None):
    return fact(compilation, lambda_, selector(lambda_)[-1], context)


def target(compilation, lambda_, call):
    predicted = compilation.facts[lambda_].targets.get(call.fun.use())
    return predicted.name if predicted is not None else None


def variants(compilation, lambda_):
    return compilation.facts[lambda_].variants


def observed(compilation, lambda_):
    return next(
        context
        for context in variants(compilation, lambda_)
        if context.id == "observed"
    )


def fixnum_context(compilation, lambda_):
    return next(
        context for context in variants(compilation, lambda_) if context.id == "fixnum"
    )


def return_type(compilation, lambda_):
    return compilation.facts[lambda_].return_type


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
    compilation = lower(f"x = {literal}\n")
    entry = function(compilation, 0)
    # A negative literal is a unary operation on a constant; test the stored value.
    facts = operand_fact(compilation, entry, stored_value(entry, "x"))
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
    # Operands flow through a non-singleton branch so the operation is derived, not folded.
    compilation = lower(
        f"def f(c):\n a = {left}\n b = {right}\n"
        f" x = (a if c else {left}) {operator} b\n return x\n"
        f"a = {left}\nb = {right}\nx = a {operator} b\n"
    )
    entry = function(compilation, 0)
    result = operand_fact(compilation, entry, stored_value(entry, "x"))
    assert (result.minimum, result.maximum) == (minimum, maximum)
    assert result.is_fixnum is fixnum


def test_literal_arithmetic_is_folded_to_a_constant():
    compilation = lower("x = 3 + 4\ny = 1 < 2\n")
    entry = function(compilation, 0)
    assert binaries(entry) == []
    assert compares(entry) == []
    values = [stored_value(entry, name).use().leaf.value for name in ("x", "y")]
    assert isinstance(values[0], ast.IntegerLiteral) and values[0].value == 7
    assert isinstance(values[1], ast.BooleanLiteral) and values[1].value is True


def test_bool_arithmetic_does_not_decode_heap_bool_as_integer_tag():
    compilation = lower(
        "def f(c):\n a = True if c else False\n b = False\n return a + b\n"
    )
    body = function(compilation, 1)
    first = loads(body)[1]
    assert not fact(compilation, body, first).is_fixnum
    assert fact(compilation, body, first).is_immortal
    assert last_fact(compilation, body, binaries).is_fixnum


def test_unknown_parameter_operator_can_return_arbitrary_object():
    compilation = lower("def f(x):\n return x + 1\n")
    body = function(compilation, 1)
    assert last_fact(compilation, body, loads) == UNKNOWN
    assert last_fact(compilation, body, binaries) == UNKNOWN


@pytest.mark.parametrize("operator", ["-", "~"])
def test_unary_range_follows_integer_bounds(operator):
    compilation = lower(
        f"def f(c):\n a = {-(2**62)} if c else {-(2**62) + 1}\n return {operator}a\n"
    )
    result = last_fact(compilation, function(compilation, 1), unaries)
    assert result.is_fixnum is (operator == "~")


def joined(left, right=None) -> tuple[Compilation, Lambda]:
    else_body = "" if right is None else f"\n else:\n  x = {right}"
    compilation = lower(f"def f(c):\n if c:\n  x = {left}{else_body}\n return x\n")
    return compilation, function(compilation, 1)


def test_branch_join_unions_integer_bounds():
    result = last_fact(*joined(-3, 8), loads)
    assert result.is_fixnum
    assert (result.minimum, result.maximum) == (-3, 8)


def test_branch_join_preserves_null_possibility():
    result = last_fact(*joined(3), loads)
    assert result.may_be_unbound
    assert not result.is_fixnum
    assert not result.is_immortal


def test_bool_and_integer_join_is_not_proven_tagged():
    assert not last_fact(*joined("True", 1), loads).is_fixnum


def test_bool_and_none_join_preserves_immortality():
    assert last_fact(*joined("True", "None"), loads).is_immortal


def test_missing_immortal_assignment_keeps_unbound_load_check():
    result = last_fact(*joined("None"), loads)
    assert result.may_be_unbound
    assert not result.is_immortal


def loop(*, bounded) -> tuple[Compilation, Lambda]:
    test = "x < 10" if bounded else "x"
    compilation = lower(f"def f():\n x = 0\n while {test}:\n  x = x + 1\n return x\n")
    return compilation, function(compilation, 1)


def test_unbounded_loop_widens_and_converges():
    result = last_fact(*loop(bounded=False), binaries)
    assert result.fits(INTEGER)
    assert not result.is_fixnum
    assert result.maximum is None


def test_integer_branch_constraint_keeps_bounded_loop_body_tagged():
    result = last_fact(*loop(bounded=True), binaries)
    assert result.is_fixnum
    assert result.maximum == 10


def test_bounded_bit_mask_recovers_fixnum_after_loop_widening():
    compilation = lower("def f(c):\n x = 0\n while c:\n  x = x + 1\n return x & 255\n")
    body = function(compilation, 1)
    mask = next(
        node
        for node in binaries(body)
        if node.fun_info is fndb.BINARY[ast.BinaryOperator.BIT_AND]
    )
    final = operand_fact(compilation, body, mask.args[0])
    masked = fact(compilation, body, mask)
    assert not final.is_fixnum
    assert masked.is_fixnum
    assert (masked.minimum, masked.maximum) == (0, 255)


def test_rebound_global_is_only_a_guarded_target_candidate():
    compilation = lower("def f():\n return 7\nf = 3\nresult = f()\n")
    entry = function(compilation, 0)
    call = calls(entry)[0]
    assert target(compilation, entry, call) == "mule_fn_f"
    assert operand_fact(compilation, entry, call.fun).fits(INTEGER)
    assert fact(compilation, entry, call) == UNKNOWN


def test_local_alias_propagates_guarded_candidate():
    compilation = lower("def f():\n return 7\ndef g():\n alias = f\n return alias()\n")
    body = function(compilation, 2)
    call = calls(body)[0]
    assert target(compilation, body, call) == "mule_fn_f"
    assert fact(compilation, body, call) == UNKNOWN


def test_proven_function_identity_propagates_its_return_summary():
    compilation = lower("def target():\n return 7\nresult = target()\n")
    result = last_fact(compilation, function(compilation, 0), calls)
    assert result.is_fixnum
    assert (result.minimum, result.maximum) == (7, 7)


@pytest.mark.parametrize(
    ("initial", "step", "expected"),
    [(0, -1, UNKNOWN), (-1, -1, FLOAT), (0, 1, INTEGER)],
)
def test_widened_integer_exponent_preserves_negative_power_type(
    initial, step, expected
):
    compilation = lower(
        f"def f(c):\n exponent = {initial}\n power = None\n while c:\n"
        f"  power = 2 ** exponent\n  exponent = exponent + {step}\n return power\n"
    )
    body = function(compilation, 1)
    power = next(
        node
        for node in binaries(body)
        if node.fun_info is fndb.BINARY[ast.BinaryOperator.POWER]
    )
    result = fact(compilation, body, power)
    assert result == expected
    assert not result.is_fixnum


@pytest.mark.parametrize("right_target", ["target", "other", None])
def test_local_candidate_join_requires_same_target_on_every_path(right_target):
    else_body = "" if right_target is None else f"\n else:\n  alias = {right_target}"
    compilation = lower(
        "def target():\n return None\ndef other():\n return None\n"
        f"def f(c):\n if c:\n  alias = target{else_body}\n return alias()\n"
    )
    body = function(compilation, 3)
    call = calls(body)[0]
    assert target(compilation, body, call) == (
        "mule_fn_target" if right_target == "target" else None
    )
    assert not fact(compilation, body, call).is_fixnum


def test_unknown_rich_comparison_may_return_non_boolean():
    compilation = lower("def f(x, y):\n return x < y\n")
    assert not last_fact(compilation, function(compilation, 1), compares).is_immortal


def test_not_of_unknown_object_always_returns_immortal_boolean():
    compilation = lower("def f(x):\n return not x\n")
    assert last_fact(compilation, function(compilation, 1), unaries).is_immortal


def integer_calls(arguments):
    call_lines = "".join(f"increment({argument})\n" for argument in arguments)
    return lower(f"def increment(x):\n return x + 1\n{call_lines}")


def test_callsite_ranges_join_in_optional_context_not_public_parameters():
    compilation = integer_calls(("2", "8"))
    increment = function(compilation, 1)
    context = observed(compilation, increment)
    assert (context.parameters[0].minimum, context.parameters[0].maximum) == (2, 8)
    assert (context.return_type.minimum, context.return_type.maximum) == (3, 9)
    parameter = loads(increment)[0]
    assert fact(compilation, increment, parameter, context).is_fixnum
    assert fact(compilation, increment, parameter) == UNKNOWN
    assert return_type(compilation, increment) == UNKNOWN


def test_proven_caller_uses_matching_context_return_range():
    compilation = integer_calls(("2", "8"))
    entry = function(compilation, 0)
    first, second = calls(entry)
    first_fact = fact(compilation, entry, first)
    assert first_fact.is_fixnum
    assert (first_fact.minimum, first_fact.maximum) == (3, 9)
    # The first call may run arbitrary code, so the second binding is unproven.
    assert fact(compilation, entry, second) == UNKNOWN


def test_fixnum_context_cannot_assume_fixnum_arithmetic_result_on_overflow():
    compilation = integer_calls((str(2**62 - 1),))
    increment = function(compilation, 1)
    context = observed(compilation, increment)
    assert context.parameters[0].is_fixnum
    assert not context.return_type.is_fixnum
    assert context.return_type.maximum == 2**62
    full = fixnum_context(compilation, increment)
    assert fact(compilation, increment, loads(increment)[0], full).is_fixnum
    assert not last_fact(compilation, increment, binaries, full).is_fixnum


def test_mixed_calls_do_not_assign_integer_facts_to_bool_float_public_entry():
    compilation = integer_calls(("3", "True", "2.0"))
    entry, increment = function(compilation, 0), function(compilation, 1)
    context = observed(compilation, increment)
    assert (context.parameters[0].minimum, context.parameters[0].maximum) == (3, 3)
    entry_calls = calls(entry)
    assert fact(compilation, entry, entry_calls[0]).is_fixnum
    assert fact(compilation, entry, entry_calls[1]) == UNKNOWN
    assert fact(compilation, entry, entry_calls[2]) == UNKNOWN
    assert fact(compilation, increment, loads(increment)[0]) == UNKNOWN


def test_dynamic_call_has_guarded_fixnum_context_without_observed_integer_seed():
    compilation = lower("def increment(x):\n return x + 1\nincrement(argument)\n")
    entry, increment = function(compilation, 0), function(compilation, 1)
    assert [context.id for context in variants(compilation, increment)] == ["fixnum"]
    assert last_fact(compilation, entry, calls) == UNKNOWN


def test_global_store_proves_function_until_an_arbitrary_call():
    compilation = lower(
        "def target():\n return 7\nfirst = target()\nsecond = target()\n"
    )
    entry = function(compilation, 0)
    first, second = calls(entry)
    assert fact(compilation, entry, first).is_fixnum
    assert operand_fact(compilation, entry, second.fun) == UNKNOWN
    assert fact(compilation, entry, second) == UNKNOWN


def test_loaded_callee_keeps_identity_when_argument_call_rebinds_globals():
    compilation = lower("def target(ignored):\n return 7\nresult = target(effect())\n")
    assert last_fact(compilation, function(compilation, 0), calls).is_fixnum


def test_module_definition_is_not_a_fact_at_another_function_entry():
    compilation = lower("def target():\n return 7\ndef wrapper():\n return target()\n")
    wrapper = function(compilation, 2)
    call = calls(wrapper)[0]
    assert target(compilation, wrapper, call) == "mule_fn_target"
    assert return_type(compilation, wrapper) == UNKNOWN


@pytest.mark.parametrize("same_target", [False, True])
def test_joined_global_functions_require_unanimous_proven_identity(same_target):
    second = "first" if same_target else "second"
    compilation = lower(
        "def first():\n return 7\ndef second():\n return 'text'\n"
        f"c = len([1])\nif c < 2:\n f = first\nelse:\n f = {second}\nresult = f()\n"
    )
    result = last_fact(compilation, function(compilation, 0), calls)
    assert result.is_fixnum is same_target
    if not same_target:
        assert result == UNKNOWN


def test_unbound_argument_does_not_seed_or_prove_specialized_call():
    compilation = lower(
        "def increment(x):\n return x + 1\ndef f(c):\n if c:\n  x = 3\n return increment(x)\n"
    )
    increment, body = function(compilation, 1), function(compilation, 2)
    call = calls(body)[0]
    assert operand_fact(compilation, body, call.args[0]).may_be_unbound
    assert fact(compilation, body, call) == UNKNOWN
    assert [context.id for context in variants(compilation, increment)] == ["fixnum"]


def recursive_calls():
    compilation = lower(
        "def recursive(n):\n if n < 100:\n  return recursive(n + 1)\n return n\nrecursive(3)\n"
    )
    return compilation, function(compilation, 1)


def test_recursive_argument_propagation_widens_before_publication():
    compilation, recursive = recursive_calls()
    context = observed(compilation, recursive)
    assert context.parameters[0].minimum == 3
    assert context.parameters[0].maximum == 2**62 - 1
    assert fact(compilation, recursive, loads(recursive)[0]) == UNKNOWN


def test_recursive_global_candidate_return_remains_unknown():
    compilation, recursive = recursive_calls()
    context = observed(compilation, recursive)
    assert context.parameters[0].is_fixnum
    assert last_fact(compilation, recursive, binaries, context).is_fixnum
    assert last_fact(compilation, recursive, calls, context) == UNKNOWN
    assert context.return_type == UNKNOWN


def test_context_ids_and_cardinality_are_stable():
    first_compilation = integer_calls(("2", "8"))
    second_compilation = integer_calls(("2", "8"))
    first = variants(first_compilation, function(first_compilation, 1))
    second = variants(second_compilation, function(second_compilation, 1))
    assert [(c.id, c.parameters, c.return_type) for c in first] == [
        (c.id, c.parameters, c.return_type) for c in second
    ]
    assert len(first) <= 2


def test_argument_types_propagate_through_multiple_local_calls():
    compilation = lower(
        "def increment(x):\n return x + 1\ndef wrapper(x):\n return increment(x)\nwrapper(3)\n"
    )
    increment, wrapper = function(compilation, 1), function(compilation, 2)
    for body in (increment, wrapper):
        context = observed(compilation, body)
        assert (context.parameters[0].minimum, context.parameters[0].maximum) == (3, 3)
    returned = observed(compilation, increment).return_type
    assert (returned.minimum, returned.maximum) == (4, 4)
    # The call inside wrapper is only predicted, so wrapper publishes no range.
    assert observed(compilation, wrapper).return_type == UNKNOWN
    assert last_fact(compilation, function(compilation, 0), calls) == UNKNOWN


def test_source_defined_global_call_gets_only_flow_proven_return_facts():
    compilation = lower(
        "def increment(x):\n    return x + 1\ny = increment(3)\nz = increment(4)\n"
    )
    entry = function(compilation, 0)
    entry_calls = calls(entry)
    assert fact(compilation, entry, entry_calls[0]).is_fixnum
    assert fact(compilation, entry, entry_calls[1]) == UNKNOWN


@pytest.mark.parametrize("effect", ["x + 1", "not x", "x == 1", "if x:\n    pass"])
def test_user_operator_or_truth_hook_invalidates_stored_global_function(effect):
    compilation = lower(f"def target():\n    return 7\n{effect}\nresult = target()\n")
    assert last_fact(compilation, function(compilation, 0), calls) == UNKNOWN


def test_constant_branch_removes_the_dead_arm():
    compilation = lower("if True:\n w = 1\nelse:\n w = 0\n")
    entry = function(compilation, 0)
    assert branches(entry) == []
    assert stored_value(entry, "w").use().leaf.value.value == 1


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


def test_post_widening_narrowing_recovers_bounded_loop_interval():
    compilation = lower(
        "def sum_loop():\n"
        " i = 0\n"
        " total = 0\n"
        " while i < 10:\n"
        "  total = total + i\n"
        "  i = i + 1\n"
        " return total\n"
    )
    fn = function(compilation, 1)
    add_i = [
        node
        for node in binaries(fn)
        if any(
            isinstance(use, Ref)
            and isinstance(use.leaf, LambdaVar)
            and use.leaf.name == "i"
            for arg in node.args
            for use in arg.uses
        )
    ]
    assert add_i
    for node in add_i:
        for arg in node.args:
            use = arg.use()
            if (
                isinstance(use, Ref)
                and isinstance(use.leaf, LambdaVar)
                and use.leaf.name == "i"
            ):
                assert fact(compilation, fn, use) == IntegerType(0, 9)


def test_dynamic_range_arguments_propagate_bounds_into_loop_variable():
    compilation = lower(
        "def count(flag):\n"
        " n = 8 if flag else 12\n"
        " acc = 0\n"
        " for i in range(2, n):\n"
        "  acc = acc + i\n"
        " return acc\n"
    )
    fn = function(compilation, 1)
    advance = [
        node
        for node in nodes(fn)
        if isinstance(node, Combination)
        and node.fun_info is not None
        and node.fun_info.name == "%range_advance"
    ]
    assert len(advance) == 1
    assert fact(compilation, fn, advance[0]) == IntegerType(2, 11)


def test_local_set_records_previous_binding_type_for_release_elision():
    compilation = lower("def rebind():\n x = 1\n x = 'hello'\n x = 2\n return x\n")
    fn = function(compilation, 1)
    assignments = [
        node
        for node in nodes(fn)
        if isinstance(node, Set)
        and isinstance(node.var, LambdaVar)
        and node.var.name == "x"
    ]
    assert [fact(compilation, fn, node) for node in assignments] == [
        UNBOUND,
        IntegerType(1, 1),
        TextType(),
    ]
    assert not UNBOUND.needs_release
    assert not IntegerType(1, 1).needs_release
    assert TextType().needs_release


def test_nonnumeric_binary_operations_preserve_sequence_and_text_types():
    compilation = lower(
        "def build(a, b):\n"
        " s = 'x' + 'y'\n"
        " r = s * 3\n"
        " raw = b'a' + b'b'\n"
        " t = (a, b) + (s,)\n"
        " return r, raw, t\n"
    )
    fn = function(compilation, 1)
    assert fact(compilation, fn, stored_value(fn, "s").use()) == TextType()
    assert fact(compilation, fn, stored_value(fn, "r").use()) == TextType()
    assert fact(compilation, fn, stored_value(fn, "raw").use()) == BytesType()
    assert fact(compilation, fn, stored_value(fn, "t").use()) == TupleType(3)


@pytest.mark.parametrize(
    ("actual", "expected", "possible"),
    [
        (UNKNOWN, IntegerType(0, 10), True),
        (IntegerType(5, 15), IntegerType(0, 10), True),
        (IntegerType(11, 20), IntegerType(0, 10), False),
        (TextType(), IntegerType(0, 10), False),
        (BytesType(), IntegerType(0, 10), False),
        (TupleType(2), IntegerType(0, 10), False),
        (BooleanType(), IntegerType(0, 10), False),
        (TupleType(2), TupleType(2), True),
        (TupleType(3), TupleType(2), False),
    ],
)
def test_may_dispatch_fit_filters_impossible_specializations(
    actual, expected, possible
):
    assert actual.may_dispatch_fit(expected) is possible


def test_nonnumeric_codegen_elides_impossible_spec_dispatch_and_retains():
    ir = emit_llvm(
        "def format_pair(pair, prefix):\n"
        " a, b = pair\n"
        " if prefix:\n"
        "  return f'{prefix}:{a}-{b}'\n"
        " return (a, b)\n"
        "def run():\n"
        " p = ('left', 'right')\n"
        " out = format_pair(p, 'tag')\n"
        " return out\n"
    )
    run_body = ir.split('define internal ptr @"mule_fn_run.body"(')[1].split("\n}")[0]
    direct_helper = ir.split('define internal ptr @"mule.direct.mule_fn_format_pair.')[
        1
    ].split("\n}")[0]
    assert '@"mule.direct.mule_fn_format_pair.' in run_body
    assert '@"mule_fn_format_pair.spec.' not in direct_helper
    assert '@"mule_fn_format_pair.body"(' in direct_helper
    assert "@mule.fast.local(" not in run_body
    assert "@mule.fast.local.move(" not in run_body


def test_python_integer_annotation_propagates_arithmetic_and_masks():
    compilation = lower(
        "def compute(x: int, y: int):\n"
        " s = x + y\n"
        " q = x / y\n"
        " m = (x & 255) + (y & 255)\n"
        " f = x + 1.5\n"
        " t = 'a' * x\n"
        " b = x * b'a'\n"
        " n = -x\n"
        " return s, q, m, f, t, b, n\n"
    )
    fn = function(compilation, 1)
    assert fact(compilation, fn, stored_value(fn, "s").use()) == INTEGER
    assert fact(compilation, fn, stored_value(fn, "q").use()) == FLOAT
    masked = fact(compilation, fn, stored_value(fn, "m").use())
    assert masked == IntegerType(0, 510)
    assert masked.is_fixnum
    assert fact(compilation, fn, stored_value(fn, "f").use()) == FLOAT
    assert fact(compilation, fn, stored_value(fn, "t").use()) == TextType()
    assert fact(compilation, fn, stored_value(fn, "b").use()) == BytesType()
    assert fact(compilation, fn, stored_value(fn, "n").use()) == INTEGER


def test_boolean_and_integer_join_preserves_bounds_on_python_integer():
    compilation, fn = joined("True", 10)
    result = last_fact(compilation, fn, loads)
    assert result == UnionType(frozenset((BooleanType(1, 1), IntegerType(10, 10))))
    assert result.fits(PythonIntegerType(1, 10))
    assert not result.is_fixnum
    assert not result.needs_release
    assert result.fits(PYTHON_INTEGER)


def test_python_integer_branch_narrowing_recovers_fixnum_outside_boolean_domain():
    compilation = lower(
        "def narrow_int(x: int):\n"
        " if x > 1:\n"
        "  if x < 100:\n"
        "   y = x\n"
        "   return y\n"
        " return 0\n"
    )
    fn = function(compilation, 1)
    narrowed = fact(compilation, fn, stored_value(fn, "y").use())
    assert narrowed == IntegerType(2, 99)
    assert narrowed.is_fixnum


def test_real_annotation_propagates_numeric_and_float_operations():
    compilation = lower(
        "def compute(x: float, y: float):\n"
        " s = x + y\n"
        " q = x / y\n"
        " f = x + 1.0\n"
        " n = -x\n"
        " c = x < y\n"
        " return s, q, f, n, c\n"
    )
    fn = function(compilation, 1)
    assert fact(compilation, fn, stored_value(fn, "s").use()) == REAL
    assert fact(compilation, fn, stored_value(fn, "q").use()) == FLOAT
    assert fact(compilation, fn, stored_value(fn, "f").use()) == FLOAT
    assert fact(compilation, fn, stored_value(fn, "n").use()) == REAL
    assert fact(compilation, fn, stored_value(fn, "c").use()) == BooleanType()


def test_fixnum_variant_requires_numeric_use_and_specialized_operation():
    compilation = lower(
        "def empty_like(text):\n"
        " if isinstance(text, bytes):\n"
        "  return b''\n"
        " return ''\n"
        "def is_word(char):\n"
        " return char == '_' or char.isalnum()\n"
        "def eq_global(op):\n"
        " return op == TARGET\n"
        "def ascii_tolower(code):\n"
        " if 65 <= code <= 90:\n"
        "  return code + 32\n"
        " return code\n"
    )
    assert variants(compilation, function(compilation, 1)) == ()
    assert variants(compilation, function(compilation, 2)) == ()
    assert variants(compilation, function(compilation, 3)) == ()
    assert [
        context.id for context in variants(compilation, function(compilation, 4))
    ] == ["fixnum"]


def test_threshold_widening_preserves_literal_loop_bounds_above_1024():
    compilation = lower(
        "def count_to_2000():\n"
        " i = 0\n"
        " while i < 2000:\n"
        "  body_i = i\n"
        "  i += 1\n"
        " exit_i = i\n"
        " return exit_i\n"
    )
    fn = function(compilation, 1)
    assert fact(compilation, fn, stored_value(fn, "body_i").use()) == IntegerType(
        0, 1999
    )
    assert fact(compilation, fn, stored_value(fn, "exit_i").use()) == IntegerType(
        2000, 2000
    )


def test_relational_constraints_prove_difference_and_loop_exit_equality():
    compilation = lower(
        "def loop_rel(n: int):\n"
        " if 0 <= n <= 500:\n"
        "  i = 0\n"
        "  while i < n:\n"
        "   diff = n - i\n"
        "   i += 1\n"
        "  eq = (i == n)\n"
        "  return eq\n"
        " return False\n"
    )
    fn = function(compilation, 1)
    assert fact(compilation, fn, stored_value(fn, "diff").use()) == IntegerType(1, 500)
    assert fact(compilation, fn, stored_value(fn, "eq").use()) == BooleanType(1, 1)


def test_post_indexing_narrows_variable_and_offset_bounds():
    compilation = lower(
        "def index_bounds(i: int):\n"
        " if i >= 0:\n"
        "  t = (10, 20, 30, 40)\n"
        "  a = t[i]\n"
        "  b = t[i + 1]\n"
        "  after = i\n"
        "  return a + b + after\n"
        " return 0\n"
    )
    fn = function(compilation, 1)
    assert fact(compilation, fn, stored_value(fn, "after").use()) == PythonIntegerType(
        0, 2
    )


def test_nested_loops_drop_invalidated_outer_accumulator_relations():
    compilation = lower(
        "def scan_payload(text: str):\n"
        " n = len(text)\n"
        " i = 0\n"
        " val_sum = 0\n"
        " active = 0\n"
        " while i < n:\n"
        "  ch = text[i]\n"
        "  if ch == ',':\n"
        "   i += 1\n"
        "   while i < n and text[i] == ' ':\n"
        "    i += 1\n"
        "   if i < n:\n"
        "    start = i\n"
        "    i += 1\n"
        "    while i < n and text[i] <= '9':\n"
        "     i += 1\n"
        "    val_sum += int(text[start:i])\n"
        "  elif ch == 't':\n"
        "   active = 1\n"
        "   i += 4\n"
        "  else:\n"
        "   i += 1\n"
        " out_sum = val_sum\n"
        " out_active = active\n"
        " return out_sum, out_active\n"
    )
    fn = function(compilation, 1)
    assert fact(compilation, fn, stored_value(fn, "out_sum").use()) == INTEGER
    assert fact(compilation, fn, stored_value(fn, "out_active").use()) == IntegerType(
        0, 1
    )
