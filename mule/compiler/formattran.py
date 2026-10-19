"""IR1 transforms for formatting operations (``%format`` and ``str``)."""

from __future__ import annotations

from mule import ast

from .ctype import INTEGER, TEXT_TYPE
from .ir1opt import (
    _FOLD_LIMIT_BITS,
    constant_literal,
    lvar_type,
    replace_combination_with_constant,
    replace_combination_with_lvar,
    retarget_known_combination,
)
from .knownfun import KNOWN, defderive, defknown, deftransform
from .node import (
    CallKind,
    Combination,
    FunctionalKind,
    GlobalVar,
    LeafVisitor,
    NodeVisitor,
    Ref,
)

if "str" not in KNOWN:
    defknown("str", "call")

_PROTOCOL_PREFIXES = (
    "mule_sequences_",
    "mule_format_",
    "mule_arguments_",
    "mule_native_class_",
    "mule_print_",
)


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


_FORMAT_LITERAL_NO_CONV = _FormatLiteralFold(None)

_FORMAT_LITERAL_FOLD = {
    None: _FORMAT_LITERAL_NO_CONV,
    "s": _FormatLiteralFold("s"),
    "r": _FormatLiteralFold("r"),
}


class _PredictableStrGlobal(NodeVisitor, LeafVisitor):
    """Match a ``Ref`` to an unmutated ``GlobalVar('str')`` outside protocols."""

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.lexenv.policy.safety >= 3:
            return None
        home = node.home_lambda
        if home is not None and (
            home.kind is FunctionalKind.TOPLEVEL
            or home.policy.safety >= 3
            or home.name.startswith(_PROTOCOL_PREFIXES)
        ):
            return None
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_GlobalVar(self, leaf: GlobalVar):
        if leaf.name == "str" and not leaf.sets:
            return leaf
        return None


_PREDICTABLE_STR_GLOBAL = _PredictableStrGlobal()


def is_predictable_str_call(node: Combination) -> bool:
    """Whether ``node`` is a positional call to builtin ``str``."""
    if node.kind is CallKind.KNOWN:
        return node.fun_info is KNOWN.get("str")
    if (
        node.kind not in (CallKind.FULL, CallKind.GUARDED)
        or len(node.fun.uses) != 1
        or (node.info is not None and node.info.keywords)
        or node.lexenv.policy.safety >= 3
    ):
        return False
    home = node.home_lambda
    if home is not None and (
        home.kind is FunctionalKind.TOPLEVEL
        or home.policy.safety >= 3
        or home.name.startswith(_PROTOCOL_PREFIXES)
    ):
        return False
    return node.fun.uses[0].accept(_PREDICTABLE_STR_GLOBAL) is not None


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


@deftransform("str")
def _transform_str(node: Combination) -> bool:
    if not is_predictable_str_call(node):
        return False
    span = node.source.span if node.source is not None else None
    if node.lvar is not None and len(node.args) == 0:
        replace_combination_with_constant(node, ast.TextLiteral("", span=span))
        return True
    if len(node.args) != 1:
        return False
    arg_lvar = node.args[0]
    if node.lvar is not None:
        literal = constant_literal(arg_lvar)
        if literal is not None:
            folded = literal.accept(_FORMAT_LITERAL_NO_CONV)
            if folded is not None:
                folded.span = span
                replace_combination_with_constant(node, folded)
                return True
    arg_type = lvar_type(arg_lvar)
    if arg_type.fits_text(TEXT_TYPE) and not arg_type.may_be_unbound:
        replace_combination_with_lvar(node, arg_lvar)
        return True
    if arg_type.fits(INTEGER) and not arg_type.may_be_unbound:
        decimal_info = KNOWN.get("_decimal_protocol")
        if decimal_info is None:
            from .protocols import PRIMITIVES

            decimal_info = PRIMITIVES["_decimal_protocol"]
        if decimal_info.derive_type is None:
            defderive("_decimal_protocol", lambda _node: TEXT_TYPE)
        retarget_known_combination(node, decimal_info, [arg_lvar])
        node.derived_type = TEXT_TYPE
        if node.lvar is not None:
            node.lvar.derived_type = None
        return True
    return False


__all__ = ["_transform_format", "_transform_str", "is_predictable_str_call"]
