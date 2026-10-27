"""IR1 type, boolean-negation, identity-comparison, equality-family, and
comparison-folding transforms, following SBCL's ``src/compiler/typetran.lisp``
and comparison helpers in ``src/compiler/srctran.lisp``.

Double boolean negation on a boolean value simplifies to the operand, negated
identity, containment, and exact total-order comparisons invert in place,
``is``/``is not`` against ``None`` or boolean singletons fold when operand
types and side-effect freedom prove the outcome statically, ``==``/``!=`` fold
across mutually disjoint built-in type families and mismatched tuple arities,
and integer-interval and literal-text comparisons fold when operand types
decide the relation.
"""

from __future__ import annotations

from mule import ast

from . import fndb
from .ctype import (
    BOOLEAN,
    BYTES_TYPE,
    INTEGER,
    PYTHON_INTEGER,
    TEXT_TYPE,
    BooleanType,
    ByteArrayType,
    BytesType,
    FloatType,
    FunctionType,
    IntegerType,
    ListType,
    NoneType,
    PrimitiveType,
    PythonIntegerType,
    RealType,
    TextType,
    TupleType,
    ValueType,
    ValueTypeVisitor,
)
from .ir1opt import (
    constant_lvar_value,
    lvar_type,
    replace_combination_with_constant,
)
from .ir1util import (
    _flush_operand,
    is_flushable_ref,
    replace_combination_with_lvar,
    retarget_known_combination,
    unlink_node,
)
from .knownfun import deftransform, fun_info
from .node import (
    CallKind,
    Combination,
    Lvar,
    NodeVisitor,
)

_INVERTED_COMPARISONS = {
    "%compare_is": "%compare_is_not",
    "%compare_is_not": "%compare_is",
    "%compare_in": "%compare_not_in",
    "%compare_not_in": "%compare_in",
}

_INVERTED_ORDER_COMPARISONS = {
    "%compare_equal": fndb.COMPARE[ast.ComparisonOperator.NOT_EQUAL],
    "%compare_not_equal": fndb.COMPARE[ast.ComparisonOperator.EQUAL],
    "%compare_less": fndb.COMPARE[ast.ComparisonOperator.GREATER_EQUAL],
    "%compare_less_equal": fndb.COMPARE[ast.ComparisonOperator.GREATER],
    "%compare_greater": fndb.COMPARE[ast.ComparisonOperator.LESS_EQUAL],
    "%compare_greater_equal": fndb.COMPARE[ast.ComparisonOperator.LESS],
}


def _has_invertible_total_order(left_type: ValueType, right_type: ValueType) -> bool:
    return any(
        left_type.fits(domain) and right_type.fits(domain)
        for domain in (PYTHON_INTEGER, TEXT_TYPE, BYTES_TYPE)
    )


class _KnownCombinationProducer(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN and node.fun_info is not None:
            return node
        return None


_KNOWN_COMBINATION_PRODUCER = _KnownCombinationProducer()


@deftransform("%unary_not")
def _transform_unary_not(node: Combination) -> bool:
    if len(node.args) != 1 or len(node.fun.uses) != 1:
        return False
    fun_ref = node.fun.uses[0]
    arg = node.args[0]
    if len(arg.uses) != 1:
        return False
    producer = arg.uses[0].accept(_KNOWN_COMBINATION_PRODUCER)
    if (
        producer is None
        or producer.prev is None
        or fun_ref.prev is None
        or fun_ref.next is not node.prev
        or producer.next is not fun_ref.prev
    ):
        return False
    name = producer.fun_info.name
    if name == "%unary_not" and len(producer.args) == 1:
        inner = producer.args[0]
        if not lvar_type(inner).fits(BOOLEAN):
            return False
        producer.args = []
        arg.dest = None
        inner.dest = node
        node.args = [inner]
        replace_combination_with_lvar(node, inner)
        _flush_operand(producer.fun)
        unlink_node(producer)
        return True
    inverted_name = _INVERTED_COMPARISONS.get(name)
    if inverted_name is not None and len(producer.args) == 2:
        left, right = producer.args
        producer.args = []
        _flush_operand(producer.fun)
        unlink_node(producer)
        retarget_known_combination(node, fun_info(inverted_name), [left, right])
        return True
    inverted_order = _INVERTED_ORDER_COMPARISONS.get(name)
    if inverted_order is not None and len(producer.args) == 2:
        left, right = producer.args
        if _has_invertible_total_order(lvar_type(left), lvar_type(right)):
            producer.args = []
            _flush_operand(producer.fun)
            unlink_node(producer)
            retarget_known_combination(node, inverted_order, [left, right])
            return True
    return False


class _SideEffectFreeRef(NodeVisitor):
    """Whether a node is a reference or pure display whose evaluation cannot
    raise or have side effects."""

    def visit_Node(self, node):
        return is_flushable_ref(node)

    def visit_Combination(self, node: Combination):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name in {"%build_tuple", "%build_list"}
            and all(_is_side_effect_free_ref(arg) for arg in node.args)
        )


_SIDE_EFFECT_FREE_REF = _SideEffectFreeRef()


def _is_side_effect_free_ref(lvar: Lvar) -> bool:
    if len(lvar.uses) != 1 or lvar_type(lvar).may_be_unbound:
        return False
    return lvar.uses[0].accept(_SIDE_EFFECT_FREE_REF)


_side_effect_free_operand = _is_side_effect_free_ref


def _replace_comparison_with_bool(node: Combination, value: bool) -> None:
    folded = ast.BooleanLiteral(value)
    folded.span = node.source.span if node.source is not None else None
    replace_combination_with_constant(node, folded)


class _IsNoneType(ValueTypeVisitor):
    def visit_ValueType(self, fact: ValueType) -> bool:
        return False

    def visit_NoneType(self, fact: NoneType) -> bool:
        return not fact.may_be_unbound


class _DisjointFromNone(ValueTypeVisitor):
    """Whether a bound value type is proven never to be ``None``.

    ``UnionType`` and ``ImmortalType`` inherit from ``ValueType``
    (not ``PrimitiveType``) and may be ``None``, so they fall back to
    ``visit_ValueType`` and return ``False``.
    """

    def visit_ValueType(self, fact: ValueType) -> bool:
        return False

    def visit_PrimitiveType(self, fact: PrimitiveType) -> bool:
        return not fact.may_be_unbound

    def visit_NoneType(self, fact: NoneType) -> bool:
        return False

    def visit_FunctionType(self, fact: FunctionType) -> bool:
        return not fact.may_be_unbound

    def visit_TupleType(self, fact: TupleType) -> bool:
        return not fact.may_be_unbound

    def visit_ListType(self, fact: ListType) -> bool:
        return not fact.may_be_unbound


class _EqualityFamily(ValueTypeVisitor):
    """Partition bound built-in value types into mutually disjoint equality
    families. Types from distinct families never compare equal with ``==``."""

    def visit_ValueType(self, fact: ValueType) -> str | None:
        return None

    def visit_TextType(self, fact: TextType) -> str | None:
        return None if fact.may_be_unbound else "text"

    def visit_BytesType(self, fact: BytesType) -> str | None:
        return None if fact.may_be_unbound else "bytes"

    def visit_ByteArrayType(self, fact: ByteArrayType) -> str | None:
        return None if fact.may_be_unbound else "bytes"

    def visit_NoneType(self, fact: NoneType) -> str | None:
        return None if fact.may_be_unbound else "none"

    def visit_IntegerType(self, fact: IntegerType) -> str | None:
        return None if fact.may_be_unbound else "number"

    def visit_BooleanType(self, fact: BooleanType) -> str | None:
        return None if fact.may_be_unbound else "number"

    def visit_PythonIntegerType(self, fact: PythonIntegerType) -> str | None:
        return None if fact.may_be_unbound else "number"

    def visit_FloatType(self, fact: FloatType) -> str | None:
        return None if fact.may_be_unbound else "number"

    def visit_RealType(self, fact: RealType) -> str | None:
        return None if fact.may_be_unbound else "number"

    def visit_TupleType(self, fact: TupleType) -> str | None:
        return None if fact.may_be_unbound else "tuple"

    def visit_ListType(self, fact: ListType) -> str | None:
        return None if fact.may_be_unbound else "list"


_IS_NONE_TYPE = _IsNoneType()
_DISJOINT_FROM_NONE = _DisjointFromNone()
_EQUALITY_FAMILY = _EqualityFamily()


def _identity_comparison(left_type: ValueType, right_type: ValueType) -> bool | None:
    if left_type.may_be_unbound or right_type.may_be_unbound:
        return None
    if left_type.accept(_IS_NONE_TYPE):
        if right_type.accept(_IS_NONE_TYPE):
            return True
        if right_type.accept(_DISJOINT_FROM_NONE):
            return False
        return None
    if right_type.accept(_IS_NONE_TYPE):
        if left_type.accept(_DISJOINT_FROM_NONE):
            return False
        return None
    left_bool = left_type.exact_boolean()
    if left_bool is not None:
        right_bool = right_type.exact_boolean()
        if right_bool is not None:
            return left_bool == right_bool
    left_text = left_type.exact_text()
    if left_text is not None:
        right_text = right_type.exact_text()
        if right_text is not None:
            if left_text != right_text:
                return False
            if (
                (left_type.is_interned_text and right_type.is_interned_text)
                or left_type.singleton_text() is not None
                or right_type.singleton_text() is not None
            ):
                return True
    return None


def _fold_identity_comparison(node: Combination, *, negate: bool) -> bool:
    if node.lvar is None or len(node.args) != 2:
        return False
    left, right = node.args
    if not (_is_side_effect_free_ref(left) and _is_side_effect_free_ref(right)):
        return False
    same = _identity_comparison(_lvar_text_type(left), _lvar_text_type(right))
    if same is None:
        return False
    _replace_comparison_with_bool(node, not same if negate else same)
    return True


@deftransform("%compare_is")
def _transform_compare_is(node: Combination) -> bool:
    return _fold_identity_comparison(node, negate=False)


@deftransform("%compare_is_not")
def _transform_compare_is_not(node: Combination) -> bool:
    return _fold_identity_comparison(node, negate=True)


def _disjoint_equality_comparison(
    left_type: ValueType, right_type: ValueType
) -> bool | None:
    left_family = left_type.accept(_EQUALITY_FAMILY)
    right_family = right_type.accept(_EQUALITY_FAMILY)
    if left_family is None or right_family is None:
        return None
    if left_family != right_family:
        return False
    left_arity = left_type.exact_length()
    right_arity = right_type.exact_length()
    if (
        left_family in ("tuple", "text")
        and left_arity is not None
        and right_arity is not None
        and left_arity != right_arity
    ):
        return False
    return None


def _fold_disjoint_equality_comparison(node: Combination, *, negate: bool) -> bool:
    if node.lvar is None or len(node.args) != 2:
        return False
    left, right = node.args
    if not (_is_side_effect_free_ref(left) and _is_side_effect_free_ref(right)):
        return False
    equal = _disjoint_equality_comparison(_lvar_text_type(left), _lvar_text_type(right))
    if equal is None:
        return False
    _replace_comparison_with_bool(node, not equal if negate else equal)
    return True


@deftransform("%compare_equal")
def _transform_compare_equal_disjoint(node: Combination) -> bool:
    return _fold_disjoint_equality_comparison(node, negate=False)


@deftransform("%compare_not_equal")
def _transform_compare_not_equal_disjoint(node: Combination) -> bool:
    return _fold_disjoint_equality_comparison(node, negate=True)


class _TextLiteralValue(ast.Visitor):
    def visit_Node(self, node) -> str | None:
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral) -> str | None:
        return node.value


_TEXT_LITERAL_VALUE = _TextLiteralValue()


def _lvar_text_type(lvar: Lvar) -> ValueType:
    value_type = lvar_type(lvar)
    if value_type.fits(TEXT_TYPE) and value_type.exact_text() is None:
        constant = constant_lvar_value(lvar)
        if constant is not None:
            literal = constant.accept(_TEXT_LITERAL_VALUE)
            if literal is not None:
                return TextType(literal)
    return value_type


def _text_literal_comparison(
    operator: ast.ComparisonOperator,
    left_type: ValueType,
    right_type: ValueType,
) -> bool | None:
    if not (left_type.fits(TEXT_TYPE) and right_type.fits(TEXT_TYPE)):
        return None
    left_literal, right_literal = left_type.exact_text(), right_type.exact_text()
    if left_literal is None or right_literal is None:
        return None
    if operator is ast.ComparisonOperator.LESS:
        return left_literal < right_literal
    if operator is ast.ComparisonOperator.LESS_EQUAL:
        return left_literal <= right_literal
    if operator is ast.ComparisonOperator.GREATER:
        return left_literal > right_literal
    if operator is ast.ComparisonOperator.GREATER_EQUAL:
        return left_literal >= right_literal
    if operator is ast.ComparisonOperator.EQUAL:
        return left_literal == right_literal
    if operator is ast.ComparisonOperator.NOT_EQUAL:
        return left_literal != right_literal
    return None


def _fold_text_comparison(node: Combination, operator: ast.ComparisonOperator) -> bool:
    if node.lvar is None or len(node.args) != 2:
        return False
    left, right = node.args
    if not (_is_side_effect_free_ref(left) and _is_side_effect_free_ref(right)):
        return False
    result = _text_literal_comparison(
        operator, _lvar_text_type(left), _lvar_text_type(right)
    )
    if result is None:
        return False
    _replace_comparison_with_bool(node, result)
    return True


@deftransform("%compare_less", (TEXT_TYPE, TEXT_TYPE))
def _transform_compare_less_text(node: Combination) -> bool:
    return _fold_text_comparison(node, ast.ComparisonOperator.LESS)


@deftransform("%compare_less_equal", (TEXT_TYPE, TEXT_TYPE))
def _transform_compare_less_equal_text(node: Combination) -> bool:
    return _fold_text_comparison(node, ast.ComparisonOperator.LESS_EQUAL)


@deftransform("%compare_greater", (TEXT_TYPE, TEXT_TYPE))
def _transform_compare_greater_text(node: Combination) -> bool:
    return _fold_text_comparison(node, ast.ComparisonOperator.GREATER)


@deftransform("%compare_greater_equal", (TEXT_TYPE, TEXT_TYPE))
def _transform_compare_greater_equal_text(node: Combination) -> bool:
    return _fold_text_comparison(node, ast.ComparisonOperator.GREATER_EQUAL)


@deftransform("%compare_equal", (TEXT_TYPE, TEXT_TYPE))
def _transform_compare_equal_text(node: Combination) -> bool:
    return _fold_text_comparison(node, ast.ComparisonOperator.EQUAL)


@deftransform("%compare_not_equal", (TEXT_TYPE, TEXT_TYPE))
def _transform_compare_not_equal_text(node: Combination) -> bool:
    return _fold_text_comparison(node, ast.ComparisonOperator.NOT_EQUAL)


def _integer_interval_comparison(
    operator: ast.ComparisonOperator,
    left_type: ValueType,
    right_type: ValueType,
) -> bool | None:
    if not (left_type.fits(INTEGER) and right_type.fits(INTEGER)):
        return None
    lmin, lmax = left_type.minimum, left_type.maximum
    rmin, rmax = right_type.minimum, right_type.maximum
    if operator is ast.ComparisonOperator.LESS:
        if lmax is not None and rmin is not None and lmax < rmin:
            return True
        if lmin is not None and rmax is not None and lmin >= rmax:
            return False
    elif operator is ast.ComparisonOperator.LESS_EQUAL:
        if lmax is not None and rmin is not None and lmax <= rmin:
            return True
        if lmin is not None and rmax is not None and lmin > rmax:
            return False
    elif operator is ast.ComparisonOperator.GREATER:
        if lmin is not None and rmax is not None and lmin > rmax:
            return True
        if lmax is not None and rmin is not None and lmax <= rmin:
            return False
    elif operator is ast.ComparisonOperator.GREATER_EQUAL:
        if lmin is not None and rmax is not None and lmin >= rmax:
            return True
        if lmax is not None and rmin is not None and lmax < rmin:
            return False
    elif operator is ast.ComparisonOperator.EQUAL:
        if (lmax is not None and rmin is not None and lmax < rmin) or (
            lmin is not None and rmax is not None and lmin > rmax
        ):
            return False
        if (
            lmin is not None
            and lmin == lmax
            and rmin is not None
            and rmin == rmax
            and lmin == rmin
        ):
            return True
    elif operator is ast.ComparisonOperator.NOT_EQUAL:
        if (lmax is not None and rmin is not None and lmax < rmin) or (
            lmin is not None and rmax is not None and lmin > rmax
        ):
            return True
        if (
            lmin is not None
            and lmin == lmax
            and rmin is not None
            and rmin == rmax
            and lmin == rmin
        ):
            return False
    return None


def _fold_integer_comparison(
    node: Combination, operator: ast.ComparisonOperator
) -> bool:
    if node.lvar is None or len(node.args) != 2:
        return False
    left, right = node.args
    if not (_is_side_effect_free_ref(left) and _is_side_effect_free_ref(right)):
        return False
    result = _integer_interval_comparison(operator, lvar_type(left), lvar_type(right))
    if result is None:
        return False
    _replace_comparison_with_bool(node, result)
    return True


@deftransform("%compare_less", (INTEGER, INTEGER))
def _transform_compare_less(node: Combination) -> bool:
    return _fold_integer_comparison(node, ast.ComparisonOperator.LESS)


@deftransform("%compare_less_equal", (INTEGER, INTEGER))
def _transform_compare_less_equal(node: Combination) -> bool:
    return _fold_integer_comparison(node, ast.ComparisonOperator.LESS_EQUAL)


@deftransform("%compare_greater", (INTEGER, INTEGER))
def _transform_compare_greater(node: Combination) -> bool:
    return _fold_integer_comparison(node, ast.ComparisonOperator.GREATER)


@deftransform("%compare_greater_equal", (INTEGER, INTEGER))
def _transform_compare_greater_equal(node: Combination) -> bool:
    return _fold_integer_comparison(node, ast.ComparisonOperator.GREATER_EQUAL)


@deftransform("%compare_equal", (INTEGER, INTEGER))
def _transform_compare_equal(node: Combination) -> bool:
    return _fold_integer_comparison(node, ast.ComparisonOperator.EQUAL)


@deftransform("%compare_not_equal", (INTEGER, INTEGER))
def _transform_compare_not_equal(node: Combination) -> bool:
    return _fold_integer_comparison(node, ast.ComparisonOperator.NOT_EQUAL)


__all__ = [
    "_disjoint_equality_comparison",
    "_fold_disjoint_equality_comparison",
    "_fold_identity_comparison",
    "_fold_integer_comparison",
    "_fold_text_comparison",
    "_has_invertible_total_order",
    "_identity_comparison",
    "_integer_interval_comparison",
    "_is_side_effect_free_ref",
    "_text_literal_comparison",
    "_transform_compare_equal",
    "_transform_compare_equal_disjoint",
    "_transform_compare_equal_text",
    "_transform_compare_greater",
    "_transform_compare_greater_equal",
    "_transform_compare_greater_equal_text",
    "_transform_compare_greater_text",
    "_transform_compare_is",
    "_transform_compare_is_not",
    "_transform_compare_less",
    "_transform_compare_less_equal",
    "_transform_compare_less_equal_text",
    "_transform_compare_less_text",
    "_transform_compare_not_equal",
    "_transform_compare_not_equal_disjoint",
    "_transform_compare_not_equal_text",
    "_transform_unary_not",
]
