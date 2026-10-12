import pytest

from mule import ast
from test.mule.parser import ast_query as q

from .ast_query import assert_ast_equal, at, nodes, one


def assignment_tree():
    return ast.Module(
        [
            ast.Assignment(
                [ast.Name("answer")],
                ast.Binary(
                    ast.BinaryOperator.ADD, ast.IntegerLiteral(1), ast.IntegerLiteral(2)
                ),
            )
        ]
    )


def test_nodes_selects_by_node_behavior_and_fields():
    tree = ast.Binary(ast.BinaryOperator.ADD, ast.Name("left"), ast.Name("right"))
    assert [node.identifier for node in nodes(tree, q.Name)] == ["left", "right"]
    assert len(nodes(tree, q.Name, identifier="left")) == 1


def test_nodes_includes_matching_root():
    tree = ast.Name("item")
    assert nodes(tree, q.Name, identifier="item") == [tree]


def test_nodes_without_matches_returns_empty_list():
    assert nodes(ast.Module([ast.Pass()]), q.Name) == []


def test_nodes_invalid_field_raises_attribute_error():
    with pytest.raises(AttributeError):
        nodes(ast.Name("item"), q.Name, missing="value")


def test_one_returns_unique_filtered_node():
    tree = ast.Binary(ast.BinaryOperator.ADD, ast.Name("first"), ast.Name("second"))
    assert one(tree, q.Name, identifier="second").identifier == "second"


@pytest.mark.parametrize(
    "tree", [ast.Pass(), ast.Tuple([ast.Name("a"), ast.Name("b")])]
)
def test_one_missing_or_ambiguous_node_fails(tree):
    with pytest.raises(AssertionError, match="Expected one matching node"):
        one(tree, q.Name)


@pytest.mark.parametrize(
    ("path", "expected"),
    [
        ("statements.0.targets.0.identifier", "answer"),
        ("statements.0.value.left.value", 1),
        ("statements.0.value.right.value", 2),
    ],
)
def test_at_follows_fields_and_list_indices(path, expected):
    assert at(assignment_tree(), path) == expected


def test_at_returns_ast_node():
    assert at(assignment_tree(), "statements.0.value") == ast.Binary(
        ast.BinaryOperator.ADD, ast.IntegerLiteral(1), ast.IntegerLiteral(2)
    )


@pytest.mark.parametrize("path", ["statements.5", "statements.0.missing"])
def test_at_invalid_path_reports_lookup_failure(path):
    with pytest.raises((IndexError, AttributeError)):
        at(assignment_tree(), path)


def test_equal_trees_accepts_every_field():
    assert_ast_equal(assignment_tree(), assignment_tree())


@pytest.mark.parametrize(
    "expected", [ast.IntegerLiteral(2), ast.Name("one"), ast.BooleanLiteral(True)]
)
def test_different_nodes_or_literal_types_fail_comparison(expected):
    with pytest.raises(AssertionError):
        assert_ast_equal(ast.IntegerLiteral(1), expected)


def test_source_locations_can_be_compared_explicitly():
    with pytest.raises(AssertionError):
        assert_ast_equal(
            ast.Name("x", span=ast.Span(ast.Position(1, 0), ast.Position(1, 1))),
            ast.Name("x"),
            locations=True,
        )


def test_default_comparison_ignores_source_locations():
    assert_ast_equal(
        ast.Name("x", span=ast.Span(ast.Position(1, 0), ast.Position(1, 1))),
        ast.Name("x"),
    )


def test_different_root_types_fail_comparison():
    with pytest.raises(AssertionError):
        assert_ast_equal(ast.Module(), ast.InteractiveInput())


def test_literal_selector_visits_all_concrete_literal_methods():
    literals: list[ast.Expression] = [
        ast.IntegerLiteral(1),
        ast.FloatLiteral(1.5),
        ast.ComplexLiteral(2j),
        ast.BooleanLiteral(True),
        ast.NoneLiteral(),
        ast.TextLiteral("text"),
        ast.BytesLiteral(b"bytes"),
        ast.EllipsisLiteral(),
    ]
    tree = ast.Tuple(literals)
    assert nodes(tree, q.Literal) == literals
    assert nodes(tree, q.IntegerLiteral) == [literals[0]]
    assert nodes(tree, q.BooleanLiteral) == [literals[3]]


def test_node_selector_collects_explicit_dispatch_in_preorder():
    tree = assignment_tree()
    assert nodes(tree, q.Node) == list(ast.walk(tree))


def test_location_ignoring_comparison_preserves_both_original_trees():
    actual_span = ast.Span(ast.Position(1, 0), ast.Position(1, 4))
    expected_span = ast.Span(ast.Position(2, 1), ast.Position(2, 5))
    actual = ast.ExpressionInput(ast.Name("item", span=actual_span), span=actual_span)
    expected = ast.ExpressionInput(
        ast.Name("item", span=expected_span), span=expected_span
    )
    assert_ast_equal(actual, expected)
    assert actual.span is actual_span
    assert actual.expression.span is actual_span
    assert expected.span is expected_span
    assert expected.expression.span is expected_span


def test_equal_scalar_values_in_different_literal_classes_remain_distinct():
    with pytest.raises(AssertionError):
        assert_ast_equal(ast.IntegerLiteral(1), ast.FloatLiteral(1.0))
