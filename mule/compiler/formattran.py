"""IR1 transforms for formatting operations (``%format``)."""

from __future__ import annotations

from mule import ast

from .ctype import TEXT_TYPE
from .ir1opt import (
    _FOLD_LIMIT_BITS,
    constant_literal,
    lvar_type,
    replace_combination_with_constant,
    replace_combination_with_lvar,
)
from .knownfun import deftransform
from .node import Combination


class _EmptyTextLiteral(ast.Visitor):
    def visit_Node(self, node: ast.Node) -> bool:
        return False

    def visit_TextLiteral(self, node: ast.TextLiteral) -> bool:
        return node.value == ""


_EMPTY_TEXT_LITERAL = _EmptyTextLiteral()


def _is_empty_spec(lvar) -> bool:
    literal = constant_literal(lvar)
    return literal is not None and literal.accept(_EMPTY_TEXT_LITERAL)


class _FormatLiteralFold(ast.Visitor):
    def __init__(self, conversion: str | None):
        self.conversion = conversion

    def visit_Node(self, node: ast.Node) -> ast.TextLiteral | None:
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral) -> ast.TextLiteral | None:
        if self.conversion in (None, "s"):
            return ast.TextLiteral(node.value)
        return None

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral) -> ast.TextLiteral | None:
        if node.value.bit_length() > _FOLD_LIMIT_BITS:
            return None
        if self.conversion in (None, "s"):
            return ast.TextLiteral(str(node.value))
        if self.conversion == "r":
            return ast.TextLiteral(repr(node.value))
        return None

    def visit_BooleanLiteral(self, node: ast.BooleanLiteral) -> ast.TextLiteral | None:
        if self.conversion in (None, "s", "r"):
            return ast.TextLiteral("True" if node.value else "False")
        return None

    def visit_NoneLiteral(self, node: ast.NoneLiteral) -> ast.TextLiteral | None:
        if self.conversion in (None, "s", "r"):
            return ast.TextLiteral("None")
        return None


_FORMAT_LITERAL_FOLD = {
    None: _FormatLiteralFold(None),
    "s": _FormatLiteralFold("s"),
    "r": _FormatLiteralFold("r"),
}


@deftransform("%format")
def _transform_format(node: Combination) -> bool:
    if node.lvar is None or len(node.args) != 2 or node.info is None:
        return False
    value_lvar, spec_lvar = node.args
    if not _is_empty_spec(spec_lvar):
        return False
    conversion = node.info.conversion
    folder = _FORMAT_LITERAL_FOLD.get(conversion)
    if folder is not None:
        literal = constant_literal(value_lvar)
        if literal is not None:
            folded = literal.accept(folder)
            if folded is not None:
                folded.span = node.source.span if node.source is not None else None
                replace_combination_with_constant(node, folded)
                return True
    if conversion in (None, "s"):
        value_type = lvar_type(value_lvar)
        if value_type.fits_text(TEXT_TYPE) and not value_type.may_be_unbound:
            replace_combination_with_lvar(node, value_lvar)
            return True
    return False


__all__ = ["_transform_format"]
