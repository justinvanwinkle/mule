"""Typed literal expectations for tests parameterized over raw scalar values."""

from mule import ast


def literal(value, *, span=None):
    constructors = {
        int: ast.IntegerLiteral,
        float: ast.FloatLiteral,
        complex: ast.ComplexLiteral,
        bool: ast.BooleanLiteral,
        str: ast.TextLiteral,
        bytes: ast.BytesLiteral,
        type(None): ast.NoneLiteral,
        type(Ellipsis): ast.EllipsisLiteral,
    }
    return constructors[type(value)](value, span=span)
