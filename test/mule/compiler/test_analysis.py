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
    assert result.types["entry"]["callee"].kind == "unknown"
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


def test_unknown_call_return_is_not_inferred_from_candidate_body():
    module = target_module(
        [
            ir.DefineFunction("definition", "target", "f"),
            ir.Call("result", "definition", ()),
        ]
    )
    assert analyze(module).types["entry"]["result"].kind == "unknown"
