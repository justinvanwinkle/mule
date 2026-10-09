import pytest

from mule import ast

from .ast_query import assert_ast_equal, at, nodes, one


def assignment_tree():
    return ast.Module(
        [
            ast.Assignment(
                [ast.Name("answer")],
                ast.Binary(ast.BinaryOperator.ADD, ast.Literal(1), ast.Literal(2)),
            )
        ]
    )


def test_nodes_filters_by_type_and_fields():
    tree = ast.Binary(ast.BinaryOperator.ADD, ast.Name("left"), ast.Name("right"))
    assert [node.identifier for node in nodes(tree, ast.Name)] == ["left", "right"]
    assert len(nodes(tree, ast.Name, identifier="left")) == 1


def test_nodes_includes_matching_root():
    tree = ast.Name("item")
    assert nodes(tree, ast.Name, identifier="item") == [tree]


def test_nodes_without_matches_returns_empty_list():
    assert nodes(ast.Module([ast.Pass()]), ast.Name) == []


def test_nodes_invalid_field_raises_attribute_error():
    with pytest.raises(AttributeError):
        nodes(ast.Name("item"), ast.Name, missing="value")


def test_one_returns_unique_filtered_node():
    tree = ast.Binary(ast.BinaryOperator.ADD, ast.Name("first"), ast.Name("second"))
    assert one(tree, ast.Name, identifier="second").identifier == "second"


@pytest.mark.parametrize(
    "tree", [ast.Pass(), ast.Tuple([ast.Name("a"), ast.Name("b")])]
)
def test_one_missing_or_ambiguous_node_fails(tree):
    with pytest.raises(AssertionError, match="Expected one Name"):
        one(tree, ast.Name)


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
    assert isinstance(at(assignment_tree(), "statements.0.value"), ast.Binary)


@pytest.mark.parametrize("path", ["statements.5", "statements.0.missing"])
def test_at_invalid_path_reports_lookup_failure(path):
    with pytest.raises((IndexError, AttributeError)):
        at(assignment_tree(), path)


def test_equal_trees_accepts_every_field():
    assert_ast_equal(assignment_tree(), assignment_tree())


@pytest.mark.parametrize(
    "expected", [ast.Literal(2), ast.Name("one"), ast.Literal(True)]
)
def test_different_nodes_or_literal_types_fail_comparison(expected):
    with pytest.raises(AssertionError):
        assert_ast_equal(ast.Literal(1), expected)


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
