"""Expression contexts that require parentheses or disallow unpacking."""

import pytest

from mule import ast

from .ast_query import assert_ast_equal


@pytest.mark.parametrize(
    "source",
    [
        pytest.param("((target) := value)", id="parenthesized-walrus-target"),
        pytest.param("await -value", id="unary-await-operand"),
        pytest.param("await await value", id="nested-unparenthesized-await"),
        pytest.param("value + yield other", id="yield-after-addition"),
        pytest.param("yield value", id="bare-yield-in-eval"),
        pytest.param("*items", id="bare-unpack-in-eval"),
        pytest.param("first, *rest", id="unparenthesized-unpack-in-eval"),
        pytest.param("{*items: value}", id="unpack-as-dictionary-key"),
        pytest.param("f(yield value)", id="bare-yield-call-argument"),
    ],
)
def test_expression_context_rejects_invalid_prefix(parse, source):
    with pytest.raises(SyntaxError):
        parse(source, mode="eval")


def test_return_requires_parentheses_around_yield(parse):
    with pytest.raises(SyntaxError):
        parse("def f(): return yield value")


@pytest.mark.parametrize(
    "source, expected",
    [
        pytest.param(
            "await (-value)",
            ast.Await(ast.Unary(ast.UnaryOperator.NEGATIVE, ast.Name("value"))),
            id="grouped-unary-await-operand",
        ),
        pytest.param(
            "await (await value)",
            ast.Await(ast.Await(ast.Name("value"))),
            id="grouped-nested-await",
        ),
        pytest.param(
            "f((yield value))",
            ast.Call(ast.Name("f"), [ast.CallArgument(ast.Yield(ast.Name("value")))]),
            id="grouped-yield-call-argument",
        ),
    ],
)
def test_parentheses_allow_nested_expression_forms(parse, source, expected):
    assert_ast_equal(parse(source, mode="eval"), ast.ExpressionInput(expected))


def test_assignment_rhs_accepts_yield_without_parentheses(parse):
    expected = ast.Module(
        [
            ast.FunctionDefinition(
                "f",
                [],
                [ast.Assignment([ast.Name("result")], ast.Yield(ast.Name("value")))],
            )
        ]
    )
    assert_ast_equal(parse("def f(): result = yield value"), expected)


def test_return_accepts_parenthesized_yield(parse):
    expected = ast.Module(
        [ast.FunctionDefinition("f", [], [ast.Return(ast.Yield(ast.Name("value")))])]
    )
    assert_ast_equal(parse("def f(): return (yield value)"), expected)
