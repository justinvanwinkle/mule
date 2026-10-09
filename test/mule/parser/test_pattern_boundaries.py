"""Wildcard dispatch differs between closed patterns and mapping keys."""

import pytest

from mule import ast
from test.mule.parser.ast_query import assert_ast_equal, one


@pytest.mark.parametrize(
    "pattern",
    [
        pytest.param("_()", id="wildcard-class-without-arguments"),
        pytest.param("_(item)", id="wildcard-class-with-argument"),
        pytest.param("_.value", id="wildcard-qualified-value"),
        pytest.param("_.Point(item)", id="wildcard-qualified-class"),
    ],
)
def test_wildcard_cannot_be_a_class_or_value_pattern(parse, pattern):
    with pytest.raises(SyntaxError):
        parse(f"match subject:\n    case {pattern}: pass\n")


def test_standalone_wildcard_is_an_uncaptured_pattern(parse):
    pattern = one(parse("match subject:\n    case _: pass\n"), ast.CapturePattern)
    assert_ast_equal(pattern, ast.CapturePattern(None))


def test_mapping_key_can_qualify_an_underscore_name(parse):
    pattern = one(
        parse("match subject:\n    case {_.value: item}: pass\n"), ast.MappingPattern
    )
    assert_ast_equal(
        pattern,
        ast.MappingPattern(
            [
                ast.PatternEntry(
                    ast.Attribute(ast.Name("_"), "value"), ast.CapturePattern("item")
                )
            ]
        ),
    )
