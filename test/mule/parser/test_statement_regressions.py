"""Statement boundaries that differ from expression and target grammar."""

import pytest

from mule import ast as a

from .ast_query import assert_ast_equal


def name(identifier):
    return a.Name(identifier)


@pytest.mark.parametrize(
    "source, expected",
    [
        pytest.param(
            "first, *rest",
            a.Module(
                [
                    a.ExpressionStatement(
                        a.Tuple([name("first"), a.Unpack(name("rest"))])
                    )
                ]
            ),
            id="tuple-unpacking-expression-statement",
        ),
        pytest.param(
            "def values(): return *items,",
            a.Module(
                [
                    a.FunctionDefinition(
                        "values", [], [a.Return(a.Tuple([a.Unpack(name("items"))]))]
                    )
                ]
            ),
            id="tuple-unpacking-return",
        ),
        pytest.param(
            "with (item for item in items): pass",
            a.Module(
                [
                    a.With(
                        [
                            a.WithItem(
                                a.Comprehension(
                                    a.ComprehensionKind.GENERATOR,
                                    name("item"),
                                    [
                                        a.ComprehensionClause(
                                            name("item"), name("items")
                                        )
                                    ],
                                )
                            )
                        ],
                        [a.Pass()],
                    )
                ]
            ),
            id="generator-context-manager",
        ),
        pytest.param(
            "with (manager).context as value: pass",
            a.Module(
                [
                    a.With(
                        [
                            a.WithItem(
                                a.Attribute(name("manager"), "context"), name("value")
                            )
                        ],
                        [a.Pass()],
                    )
                ]
            ),
            id="grouped-context-receiver",
        ),
        pytest.param(
            "with (first, second) as value: pass",
            a.Module(
                [
                    a.With(
                        [
                            a.WithItem(
                                a.Tuple([name("first"), name("second")]), name("value")
                            )
                        ],
                        [a.Pass()],
                    )
                ]
            ),
            id="tuple-context-expression",
        ),
        pytest.param(
            "with (manager := acquire()): pass",
            a.Module(
                [
                    a.With(
                        [
                            a.WithItem(
                                a.NamedExpression(
                                    name("manager"), a.Call(name("acquire"))
                                )
                            )
                        ],
                        [a.Pass()],
                    )
                ]
            ),
            id="named-expression-context-manager",
        ),
        pytest.param(
            "match lambda x=1: x:\n    case _: pass\n",
            a.Module(
                [
                    a.Match(
                        a.Lambda([a.Parameter("x", default=a.Literal(1))], name("x")),
                        [a.MatchCase(a.CapturePattern(None), [a.Pass()])],
                    )
                ]
            ),
            id="lambda-match-subject-with-default",
        ),
        pytest.param(
            "match value := get():\n    case _: pass\n",
            a.Module(
                [
                    a.Match(
                        a.NamedExpression(name("value"), a.Call(name("get"))),
                        [a.MatchCase(a.CapturePattern(None), [a.Pass()])],
                    )
                ]
            ),
            id="named-expression-match-subject",
        ),
    ],
)
def test_statement_expression_boundaries(parse, source, expected):
    assert_ast_equal(parse(source), expected)


@pytest.mark.parametrize(
    "source",
    [
        pytest.param("result = *items", id="bare-unpacking-assignment-value"),
        pytest.param("result += *items", id="bare-unpacking-augmented-value"),
        pytest.param("return *items", id="bare-unpacking-return-value"),
        pytest.param("for item in *items: pass", id="bare-unpacking-iterable"),
    ],
)
def test_statement_values_require_tuple_for_unpacking(parse, source):
    with pytest.raises(SyntaxError):
        parse(source)
