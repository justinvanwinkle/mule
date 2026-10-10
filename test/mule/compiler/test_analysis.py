"""Hand-built graphs distinguish proven representation facts from predictions."""

import pytest

from mule import ast
from mule.compiler import ir
from mule.compiler.analysis import analyze


def straight(instructions, *, parameters=(), locals_=()):
    instructions = list(instructions)
    consumed = {
        value for instruction in instructions for value in ir.operands(instruction)
    }
    for instruction in list(instructions):
        result = getattr(instruction, "result", None)
        if result is not None and result not in consumed:
            instructions.append(ir.Discard(result))
    instructions.append(ir.Constant("returned", None))
    function = ir.Function(
        "entry",
        parameters,
        locals_,
        [ir.Block("entry", instructions, ir.Return("returned"))],
    )
    return ir.Module([function], "entry")


@pytest.mark.parametrize(
    ("value", "kind", "fixnum", "immortal"),
    [
        (0, "integer", True, False),
        (-(2**62), "integer", True, False),
        (2**62 - 1, "integer", True, False),
        (-(2**62) - 1, "integer", False, False),
        (2**62, "integer", False, False),
        (True, "bool", False, True),
        (False, "bool", False, True),
        (None, "none", False, True),
        (1.0, "float", False, False),
        ("text", "text", False, False),
        (b"bytes", "bytes", False, False),
    ],
)
def test_constant_representation_is_proven(value, kind, fixnum, immortal):
    facts = analyze(straight([ir.Constant("value", value)])).types["entry"]["value"]
    assert facts.kind == kind
    assert facts.is_fixnum is fixnum
    assert facts.is_immortal is immortal


@pytest.mark.parametrize(
    ("operator", "left", "right", "minimum", "maximum", "fixnum"),
    [
        (ast.BinaryOperator.ADD, 3, 4, 7, 7, True),
        (ast.BinaryOperator.ADD, 2**62 - 1, 1, 2**62, 2**62, False),
        (ast.BinaryOperator.SUBTRACT, -(2**62), 1, -(2**62) - 1, -(2**62) - 1, False),
        (ast.BinaryOperator.MULTIPLY, 2**40, 2**40, 2**80, 2**80, False),
        (ast.BinaryOperator.BIT_AND, -(2**100), 255, 0, 0, True),
        (ast.BinaryOperator.RIGHT_SHIFT, -5, 1, -3, -3, True),
        (ast.BinaryOperator.LEFT_SHIFT, 1, 62, 2**62, 2**62, False),
    ],
)
def test_integer_ranges_detect_promotion(
    operator, left, right, minimum, maximum, fixnum
):
    module = straight(
        [
            ir.Constant("left", left),
            ir.Constant("right", right),
            ir.Binary("result", operator, "left", "right"),
        ]
    )
    result = analyze(module).types["entry"]["result"]
    assert (result.minimum, result.maximum) == (minimum, maximum)
    assert result.is_fixnum is fixnum


def test_bool_arithmetic_does_not_decode_heap_bool_as_integer_tag():
    module = straight(
        [
            ir.Constant("left", True),
            ir.Constant("right", False),
            ir.Binary("sum", ast.BinaryOperator.ADD, "left", "right"),
        ]
    )
    facts = analyze(module).types["entry"]
    assert not facts["left"].is_fixnum
    assert facts["left"].is_immortal
    assert facts["sum"].is_fixnum


def test_unknown_parameter_operator_can_return_arbitrary_object():
    module = straight(
        [
            ir.Load("parameter", "x", True),
            ir.Constant("one", 1),
            ir.Binary("result", ast.BinaryOperator.ADD, "parameter", "one"),
        ],
        parameters=("x",),
        locals_=("x",),
    )
    facts = analyze(module).types["entry"]
    assert facts["parameter"].kind == "unknown"
    assert facts["result"].kind == "unknown"


@pytest.mark.parametrize(
    "operator", [ast.UnaryOperator.NEGATIVE, ast.UnaryOperator.INVERT]
)
def test_unary_range_follows_integer_bounds(operator):
    module = straight(
        [ir.Constant("value", -(2**62)), ir.Unary("result", operator, "value")]
    )
    result = analyze(module).types["entry"]["result"]
    assert result.is_fixnum is (operator == ast.UnaryOperator.INVERT)


def joined(left, right, *, missing=False):
    first = [ir.Constant("left", left), ir.Store("x", "left", True)]
    second = (
        [] if missing else [ir.Constant("right", right), ir.Store("x", "right", True)]
    )
    return ir.Module(
        [
            ir.Function(
                "entry",
                ("condition",),
                ("condition", "x"),
                [
                    ir.Block(
                        "entry",
                        [ir.Load("condition", "condition", True)],
                        ir.Branch("condition", "left", "right"),
                    ),
                    ir.Block("left", first, ir.Jump("join")),
                    ir.Block("right", second, ir.Jump("join")),
                    ir.Block(
                        "join", [ir.Load("joined", "x", True)], ir.Return("joined")
                    ),
                ],
            )
        ],
        "entry",
    )


def test_branch_join_unions_integer_bounds():
    result = analyze(joined(-3, 8)).types["entry"]["joined"]
    assert result.is_fixnum
    assert (result.minimum, result.maximum) == (-3, 8)


def test_branch_join_preserves_null_possibility():
    result = analyze(joined(3, None, missing=True)).types["entry"]["joined"]
    assert result.may_be_unbound
    assert not result.is_fixnum
    assert not result.is_immortal


def test_bool_and_integer_join_is_not_proven_tagged():
    assert not analyze(joined(True, 1)).types["entry"]["joined"].is_fixnum


def test_bool_and_none_join_preserves_immortality():
    assert analyze(joined(True, None)).types["entry"]["joined"].is_immortal


def loop(*, bounded):
    test = [ir.Load("test", "x", True)]
    if bounded:
        test += [
            ir.Constant("limit", 10),
            ir.Compare("test_result", ast.ComparisonOperator.LESS, "test", "limit"),
        ]
        condition = "test_result"
    else:
        condition = "test"
    return ir.Module(
        [
            ir.Function(
                "entry",
                (),
                ("x",),
                [
                    ir.Block(
                        "entry",
                        [ir.Constant("zero", 0), ir.Store("x", "zero", True)],
                        ir.Jump("test"),
                    ),
                    ir.Block("test", test, ir.Branch(condition, "body", "done")),
                    ir.Block(
                        "body",
                        [
                            ir.Load("old", "x", True),
                            ir.Constant("one", 1),
                            ir.Binary("new", ast.BinaryOperator.ADD, "old", "one"),
                            ir.Store("x", "new", True),
                        ],
                        ir.Jump("test"),
                    ),
                    ir.Block("done", [ir.Load("final", "x", True)], ir.Return("final")),
                ],
            )
        ],
        "entry",
    )


def test_unbounded_loop_widens_and_converges():
    result = analyze(loop(bounded=False)).types["entry"]["new"]
    assert result.kind == "integer"
    assert not result.is_fixnum
    assert result.maximum is None


def test_integer_branch_constraint_keeps_bounded_loop_body_tagged():
    result = analyze(loop(bounded=True)).types["entry"]["new"]
    assert result.is_fixnum
    assert result.maximum == 10


def test_clear_load_leaves_subsequent_local_unbound():
    module = straight(
        [
            ir.Constant("initial", 3),
            ir.Store("x", "initial", True),
            ir.Load("moved", "x", True, clear=True),
            ir.Load("empty", "x", True),
        ],
        locals_=("x",),
    )
    facts = analyze(module).types["entry"]
    assert facts["moved"].is_fixnum
    assert facts["empty"].may_be_unbound
    assert not facts["empty"].is_fixnum


def target_module(instructions):
    module = straight(instructions, locals_=("alias",))
    module.functions.append(
        ir.Function(
            "target",
            (),
            (),
            [ir.Block("entry", [ir.Constant("answer", 7)], ir.Return("answer"))],
        )
    )
    return module


def test_rebound_global_is_only_a_guarded_target_candidate():
    module = target_module(
        [
            ir.DefineFunction("definition", "target", "f"),
            ir.Store("f", "definition", False),
            ir.Constant("replacement", 3),
            ir.Store("f", "replacement", False),
            ir.Load("callee", "f", False),
            ir.Call("result", "callee", ()),
        ]
    )
    result = analyze(module)
    assert result.targets["entry"]["callee"] == "target"
    assert result.types["entry"]["callee"].kind == "integer"
    assert result.types["entry"]["result"].kind == "unknown"


def test_local_alias_propagates_guarded_candidate():
    module = target_module(
        [
            ir.DefineFunction("definition", "target", "f"),
            ir.Store("f", "definition", False),
            ir.Load("predicted", "f", False),
            ir.Store("alias", "predicted", True),
            ir.Load("callee", "alias", True),
            ir.Call("result", "callee", ()),
        ]
    )
    assert analyze(module).targets["entry"]["callee"] == "target"


def test_proven_function_identity_propagates_its_return_summary():
    module = target_module(
        [
            ir.DefineFunction("definition", "target", "f"),
            ir.Call("result", "definition", ()),
        ]
    )
    result = analyze(module).types["entry"]["result"]
    assert result.is_fixnum
    assert (result.minimum, result.maximum) == (7, 7)


@pytest.mark.parametrize(
    ("initial", "step", "kind"),
    [(0, -1, "unknown"), (-1, -1, "float"), (0, 1, "integer")],
)
def test_widened_integer_exponent_preserves_negative_power_type(initial, step, kind):
    module = ir.Module(
        [
            ir.Function(
                "entry",
                ("condition",),
                ("condition", "exponent"),
                [
                    ir.Block(
                        "entry",
                        [
                            ir.Constant("initial", initial),
                            ir.Store("exponent", "initial", True),
                        ],
                        ir.Jump("test"),
                    ),
                    ir.Block(
                        "test",
                        [
                            ir.Constant("base", 2),
                            ir.Load("exponent", "exponent", True),
                            ir.Binary(
                                "power", ast.BinaryOperator.POWER, "base", "exponent"
                            ),
                            ir.Discard("power"),
                            ir.Load("condition", "condition", True),
                        ],
                        ir.Branch("condition", "body", "done"),
                    ),
                    ir.Block(
                        "body",
                        [
                            ir.Load("old", "exponent", True),
                            ir.Constant("step", step),
                            ir.Binary("new", ast.BinaryOperator.ADD, "old", "step"),
                            ir.Store("exponent", "new", True),
                        ],
                        ir.Jump("test"),
                    ),
                    ir.Block("done", [ir.Constant("none", None)], ir.Return("none")),
                ],
            )
        ],
        "entry",
    )
    result = analyze(module).types["entry"]["power"]
    assert result.kind == kind
    assert not result.is_fixnum


def test_identical_branch_successors_do_not_apply_only_true_constraint():
    module = joined(-3, 8)
    function = module.functions[0]
    function.blocks[-1] = ir.Block(
        "join",
        [
            ir.Load("tested", "x", True),
            ir.Constant("bound", 0),
            ir.Compare(
                "condition_result", ast.ComparisonOperator.LESS, "tested", "bound"
            ),
        ],
        ir.Branch("condition_result", "done", "done"),
    )
    function.blocks.append(
        ir.Block("done", [ir.Load("joined", "x", True)], ir.Return("joined"))
    )
    result = analyze(module).types["entry"]["joined"]
    assert (result.minimum, result.maximum) == (-3, 8)


@pytest.mark.parametrize("right_target", ["target", "other", None])
def test_local_candidate_join_requires_same_target_on_every_path(right_target):
    right = (
        []
        if right_target is None
        else [
            ir.DefineFunction("right", right_target, "f"),
            ir.Store("alias", "right", True),
        ]
    )
    module = ir.Module(
        [
            ir.Function(
                "entry",
                ("condition",),
                ("condition", "alias"),
                [
                    ir.Block(
                        "entry",
                        [ir.Load("condition", "condition", True)],
                        ir.Branch("condition", "left", "right"),
                    ),
                    ir.Block(
                        "left",
                        [
                            ir.DefineFunction("left", "target", "f"),
                            ir.Store("alias", "left", True),
                        ],
                        ir.Jump("join"),
                    ),
                    ir.Block("right", right, ir.Jump("join")),
                    ir.Block(
                        "join",
                        [
                            ir.Load("callee", "alias", True),
                            ir.Call("result", "callee", ()),
                        ],
                        ir.Return("result"),
                    ),
                ],
            ),
            ir.Function(
                "target",
                (),
                (),
                [ir.Block("entry", [ir.Constant("none", None)], ir.Return("none"))],
            ),
            ir.Function(
                "other",
                (),
                (),
                [ir.Block("entry", [ir.Constant("none", None)], ir.Return("none"))],
            ),
        ],
        "entry",
    )
    result = analyze(module)
    assert result.targets["entry"].get("callee") == (
        "target" if right_target == "target" else None
    )
    assert not result.types["entry"]["result"].is_fixnum


def test_unknown_rich_comparison_may_return_non_boolean():
    module = straight(
        [
            ir.Load("left", "x", True),
            ir.Load("right", "y", True),
            ir.Compare("comparison", ast.ComparisonOperator.LESS, "left", "right"),
        ],
        parameters=("x", "y"),
        locals_=("x", "y"),
    )
    assert not analyze(module).types["entry"]["comparison"].is_immortal


def test_not_of_unknown_object_always_returns_immortal_boolean():
    module = straight(
        [
            ir.Load("value", "x", True),
            ir.Unary("negated", ast.UnaryOperator.NOT, "value"),
        ],
        parameters=("x",),
        locals_=("x",),
    )
    assert analyze(module).types["entry"]["negated"].is_immortal


def test_missing_immortal_assignment_keeps_unbound_load_check():
    result = analyze(joined(None, None, missing=True)).types["entry"]["joined"]
    assert result.may_be_unbound
    assert not result.is_immortal


def test_block_storage_order_does_not_control_propagation():
    module = joined(-3, 8)
    function = module.functions[0]
    function.blocks[1:] = reversed(function.blocks[1:])
    result = analyze(module).types["entry"]["joined"]
    assert (result.minimum, result.maximum) == (-3, 8)
    assert result.is_fixnum


def test_bounded_bit_mask_recovers_fixnum_after_loop_widening():
    module = loop(bounded=False)
    module.functions[0].blocks[-1] = ir.Block(
        "done",
        [
            ir.Load("final", "x", True),
            ir.Constant("mask", 255),
            ir.Binary("masked", ast.BinaryOperator.BIT_AND, "final", "mask"),
        ],
        ir.Return("masked"),
    )
    facts = analyze(module).types["entry"]
    assert not facts["final"].is_fixnum
    assert facts["masked"].is_fixnum
    assert (facts["masked"].minimum, facts["masked"].maximum) == (0, 255)


def test_assignment_invalidates_constraint_for_previous_local_load():
    module = joined(-3, 8)
    function = module.functions[0]
    function.locals += ("replacement",)
    for block, value in zip(function.blocks[1:3], (100, -100), strict=True):
        result = f"{block.label}_replacement"
        block.instructions += [
            ir.Constant(result, value),
            ir.Store("replacement", result, True),
        ]
    function.blocks[-1] = ir.Block(
        "join",
        [
            ir.Load("tested", "x", True),
            ir.Constant("bound", 0),
            ir.Compare("comparison", ast.ComparisonOperator.LESS, "tested", "bound"),
            ir.Load("replacement", "replacement", True),
            ir.Store("x", "replacement", True),
        ],
        ir.Branch("comparison", "true", "false"),
    )
    for label in ("true", "false"):
        function.blocks.append(
            ir.Block(label, [ir.Load(label, "x", True)], ir.Return(label))
        )
    facts = analyze(module).types["entry"]
    for label in ("true", "false"):
        assert (facts[label].minimum, facts[label].maximum) == (-100, 100)


def integer_calls(values):
    instructions = []
    for index, value in enumerate(values):
        instructions += [
            ir.DefineFunction(f"callee{index}", "increment", "increment"),
            ir.Constant(f"argument{index}", value),
            ir.Call(f"call{index}", f"callee{index}", (f"argument{index}",)),
            ir.Discard(f"call{index}"),
        ]
    module = straight(instructions)
    module.functions.append(
        ir.Function(
            "increment",
            ("x",),
            ("x",),
            [
                ir.Block(
                    "entry",
                    [
                        ir.Load("x", "x", True),
                        ir.Constant("one", 1),
                        ir.Binary("sum", ast.BinaryOperator.ADD, "x", "one"),
                    ],
                    ir.Return("sum"),
                )
            ],
        )
    )
    return module


def observed(result, function):
    return next(
        context for context in result.variants[function] if context.id == "observed"
    )


def test_callsite_ranges_join_in_optional_context_not_public_parameters():
    result = analyze(integer_calls((2, 8)))
    context = observed(result, "increment")
    assert (context.parameters[0].minimum, context.parameters[0].maximum) == (2, 8)
    assert (context.return_type.minimum, context.return_type.maximum) == (3, 9)
    assert context.types["x"].is_fixnum
    assert result.types["increment"]["x"].kind == "unknown"
    assert result.returns["increment"].kind == "unknown"


def test_proven_caller_uses_matching_context_return_range():
    result = analyze(integer_calls((2, 8)))
    for identifier in ("call0", "call1"):
        assert result.types["entry"][identifier].is_fixnum
        assert (
            result.types["entry"][identifier].minimum,
            result.types["entry"][identifier].maximum,
        ) == (3, 9)


def test_fixnum_context_cannot_assume_fixnum_arithmetic_result_on_overflow():
    result = analyze(integer_calls((2**62 - 1,)))
    context = observed(result, "increment")
    assert context.parameters[0].is_fixnum
    assert not context.return_type.is_fixnum
    assert context.return_type.maximum == 2**62
    full = next(
        context for context in result.variants["increment"] if context.id == "fixnum"
    )
    assert full.types["x"].is_fixnum
    assert not full.types["sum"].is_fixnum


def test_mixed_calls_do_not_assign_integer_facts_to_bool_float_public_entry():
    result = analyze(integer_calls((3, True, 2.0)))
    context = observed(result, "increment")
    assert (context.parameters[0].minimum, context.parameters[0].maximum) == (3, 3)
    assert result.types["entry"]["call0"].is_fixnum
    assert result.types["entry"]["call1"].kind == "unknown"
    assert result.types["entry"]["call2"].kind == "unknown"
    assert result.types["increment"]["x"].kind == "unknown"


def test_dynamic_call_has_guarded_fixnum_context_without_observed_integer_seed():
    module = integer_calls((3,))
    entry = module.functions[0]
    entry.parameters = entry.locals = ("argument",)
    entry.blocks[0].instructions[1] = ir.Load("argument0", "argument", True)
    result = analyze(module)
    assert [context.id for context in result.variants["increment"]] == ["fixnum"]
    assert result.types["entry"]["call0"].kind == "unknown"


def test_global_store_proves_function_until_an_arbitrary_call():
    module = target_module(
        [
            ir.DefineFunction("definition", "target", "f"),
            ir.Store("f", "definition", False),
            ir.Load("before", "f", False),
            ir.Call("first", "before", ()),
            ir.Discard("first"),
            ir.Load("after", "f", False),
            ir.Call("second", "after", ()),
        ]
    )
    result = analyze(module)
    assert result.types["entry"]["first"].is_fixnum
    assert result.types["entry"]["after"].kind == "unknown"
    assert result.types["entry"]["second"].kind == "unknown"


def test_loaded_callee_keeps_identity_when_argument_call_rebinds_globals():
    module = target_module(
        [
            ir.DefineFunction("definition", "target", "f"),
            ir.Store("f", "definition", False),
            ir.Load("loaded", "f", False),
            ir.Load("effect", "effect", False),
            ir.Call("argument", "effect", ()),
            ir.Call("result", "loaded", ("argument",)),
        ]
    )
    target = module.functions[1]
    target.parameters = target.locals = ("ignored",)
    assert analyze(module).types["entry"]["result"].is_fixnum


def test_module_definition_is_not_a_fact_at_another_function_entry():
    module = target_module(
        [
            ir.DefineFunction("definition", "target", "f"),
            ir.Store("f", "definition", False),
        ]
    )
    module.functions.append(
        ir.Function(
            "wrapper",
            (),
            (),
            [
                ir.Block(
                    "entry",
                    [ir.Load("callee", "f", False), ir.Call("result", "callee", ())],
                    ir.Return("result"),
                )
            ],
        )
    )
    result = analyze(module)
    assert result.targets["wrapper"]["callee"] == "target"
    assert result.returns["wrapper"].kind == "unknown"


@pytest.mark.parametrize("same_target", [False, True])
def test_joined_global_functions_require_unanimous_proven_identity(same_target):
    module = joined(-3, 8)
    function = module.functions[0]
    for block, target in zip(
        function.blocks[1:3],
        ("first", "first" if same_target else "second"),
        strict=True,
    ):
        value = f"{block.label}_function"
        block.instructions = [
            ir.DefineFunction(value, target, "f"),
            ir.Store("f", value, False),
        ]
    function.blocks[-1] = ir.Block(
        "join",
        [
            ir.Load("callee", "f", False),
            ir.Call("result", "callee", ()),
        ],
        ir.Return("result"),
    )
    for name, value in (("first", 7), ("second", "text")):
        module.functions.append(
            ir.Function(
                name,
                (),
                (),
                [
                    ir.Block(
                        "entry",
                        [ir.Constant("answer", value)],
                        ir.Return("answer"),
                    )
                ],
            )
        )
    result = analyze(module).types["entry"]["result"]
    assert result.is_fixnum is same_target
    if not same_target:
        assert result.kind == "unknown"


def test_unbound_argument_does_not_seed_or_prove_specialized_call():
    module = joined(3, None, missing=True)
    entry = module.functions[0]
    entry.blocks[-1] = ir.Block(
        "join",
        [
            ir.DefineFunction("callee", "increment", "increment"),
            ir.Load("argument", "x", True),
            ir.Call("result", "callee", ("argument",)),
        ],
        ir.Return("result"),
    )
    module.functions.append(integer_calls((3,)).functions[1])
    result = analyze(module)
    assert result.types["entry"]["argument"].may_be_unbound
    assert result.types["entry"]["result"].kind == "unknown"
    assert [context.id for context in result.variants["increment"]] == ["fixnum"]


def recursive_calls(*, speculative=False):
    module = straight(
        [
            ir.DefineFunction("callee", "recursive", "recursive"),
            ir.Constant("argument", 3),
            ir.Call("result", "callee", ("argument",)),
        ]
    )
    recurse = (
        ir.Load("callee", "recursive", False)
        if speculative
        else ir.DefineFunction("callee", "recursive", "recursive")
    )
    function = ir.Function(
        "recursive",
        ("n",),
        ("n",),
        [
            ir.Block(
                "entry",
                [
                    ir.Load("tested", "n", True),
                    ir.Constant("limit", 100),
                    ir.Compare(
                        "comparison", ast.ComparisonOperator.LESS, "tested", "limit"
                    ),
                ],
                ir.Branch("comparison", "recurse", "done"),
            ),
            ir.Block(
                "recurse",
                [
                    recurse,
                    ir.Load("old", "n", True),
                    ir.Constant("one", 1),
                    ir.Binary("new", ast.BinaryOperator.ADD, "old", "one"),
                    ir.Call("recursive_result", "callee", ("new",)),
                ],
                ir.Return("recursive_result"),
            ),
            ir.Block("done", [ir.Load("final", "n", True)], ir.Return("final")),
        ],
    )
    module.functions.append(function)
    if speculative:
        module.functions[0].blocks[0].instructions[:0] = [
            ir.DefineFunction("global", "recursive", "recursive"),
            ir.Store("recursive", "global", False),
        ]
    return module


def test_recursive_argument_propagation_widens_before_publication():
    result = analyze(recursive_calls())
    context = observed(result, "recursive")
    assert context.parameters[0].minimum == 3
    assert context.parameters[0].maximum == 2**62 - 1
    assert context.return_type.kind == "integer"
    assert context.return_type.is_fixnum
    assert result.types["recursive"]["tested"].kind == "unknown"


def test_recursive_global_candidate_return_remains_unknown():
    result = analyze(recursive_calls(speculative=True))
    context = observed(result, "recursive")
    assert context.parameters[0].is_fixnum
    assert context.types["new"].is_fixnum
    assert context.types["recursive_result"].kind == "unknown"
    assert context.return_type.kind == "unknown"


def test_context_ids_and_cardinality_are_stable():
    first = analyze(integer_calls((2, 8)))
    second = analyze(integer_calls((2, 8)))
    assert first.variants == second.variants
    assert all(len(contexts) <= 2 for contexts in first.variants.values())


def test_argument_and_return_types_propagate_through_multiple_local_calls():
    module = integer_calls((3,))
    module.functions[0].blocks[0].instructions[0] = ir.DefineFunction(
        "callee0", "wrapper", "wrapper"
    )
    module.functions.append(
        ir.Function(
            "wrapper",
            ("x",),
            ("x",),
            [
                ir.Block(
                    "entry",
                    [
                        ir.DefineFunction("callee", "increment", "increment"),
                        ir.Load("argument", "x", True),
                        ir.Call("result", "callee", ("argument",)),
                    ],
                    ir.Return("result"),
                )
            ],
        )
    )
    result = analyze(module)
    for name in ("wrapper", "increment"):
        context = observed(result, name)
        assert (context.parameters[0].minimum, context.parameters[0].maximum) == (3, 3)
        assert (context.return_type.minimum, context.return_type.maximum) == (4, 4)
        assert result.returns[name].kind == "unknown"
    assert result.types["entry"]["call0"].is_fixnum


def test_growing_recursive_return_interval_widens_without_fixnum_promise():
    module = recursive_calls()
    function = module.functions[1]
    function.blocks[0].instructions[1] = ir.Constant("limit", 0)
    function.blocks[0].instructions[2] = ir.Compare(
        "comparison", ast.ComparisonOperator.GREATER, "tested", "limit"
    )
    function.blocks[1].instructions[2] = ir.Constant("one", -1)
    function.blocks[1].instructions += [
        ir.Constant("increment", 1),
        ir.Binary("sum", ast.BinaryOperator.ADD, "recursive_result", "increment"),
    ]
    function.blocks[1].terminator = ir.Return("sum")
    function.blocks[2] = ir.Block("done", [ir.Constant("base", 1)], ir.Return("base"))
    result = analyze(module)
    context = observed(result, "recursive")
    assert context.parameters[0].is_fixnum
    assert context.return_type.kind == "integer"
    assert context.return_type.minimum == 1
    assert context.return_type.maximum is None
    assert not context.return_type.is_fixnum


def test_source_defined_global_call_gets_only_flow_proven_return_facts():
    from mule.compiler.frontend import lower

    module = lower(
        "def increment(x):\n    return x + 1\ny = increment(3)\nz = increment(4)\n"
    )
    result = analyze(module)
    entry = module.functions[0]
    calls = [
        instruction
        for block in entry.blocks
        for instruction in block.instructions
        if isinstance(instruction, ir.Call)
    ]
    assert result.types[entry.name][calls[0].result].is_fixnum
    assert result.types[entry.name][calls[1].result].kind == "unknown"


@pytest.mark.parametrize("effect", ["x + 1", "not x", "x == 1", "if x:\n    pass"])
def test_user_operator_or_truth_hook_invalidates_stored_global_function(effect):
    from mule.compiler.frontend import lower

    module = lower(f"def target():\n    return 7\n{effect}\nresult = target()\n")
    result = analyze(module)
    entry = module.functions[0]
    call = next(
        instruction
        for block in entry.blocks
        for instruction in block.instructions
        if isinstance(instruction, ir.Call)
    )
    assert result.types[entry.name][call.result].kind == "unknown"
