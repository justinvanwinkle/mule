"""Modular arithmetic and associative integer constant transforms, following
``src/compiler/modarith.lisp`` and ``src/compiler/srctran.lisp``.

Registers ``@deftransform`` rules on ``(INTEGER, INTEGER)`` operations:

- Redundant bitmask elimination: ``x & mask -> x`` and ``mask & x -> x`` when
  ``mask = 2**k - 1`` (``mask > 0``) is a contiguous low-bit mask and the
  proven interval of ``x`` lies within ``[0, mask]``.
- Associative constant combination on exact integers:
  - ``(x & c1) & c2 -> x & (c1 & c2)``
  - ``(x | c1) | c2 -> x | (c1 | c2)``
  - ``(x ^ c1) ^ c2 -> x ^ (c1 ^ c2)``
  - ``(x ± c1) ± c2 -> x ± (c1 ± c2)`` (or ``x`` when the net offset is ``0``)
  - ``(x >> k1) >> k2 -> x >> (k1 + k2)`` and
    ``(x << k1) << k2 -> x << (k1 + k2)`` for non-negative shifts.
"""

from __future__ import annotations

from mule import ast

from . import fndb
from .ctype import INTEGER, IntegerType
from .ir1opt import (
    _FOLD_LIMIT_BITS,
    _flush_operand,
    constant_literal,
    lvar_type,
    replace_combination_with_lvar,
)
from .ir1util import (
    retarget_known_combination,
    unlink_node,
)
from .knownfun import deftransform
from .node import (
    CallKind,
    Combination,
    Lvar,
    NodeVisitor,
)
from .srctran import (
    _exact_int_literal,
    _rewrite_int_constant_lvar,
)

_MAX_COMBINED_SHIFT = 1024


class _KnownBinaryProducer(NodeVisitor):
    def __init__(self, names: frozenset[str]):
        self.names = names

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name in self.names
            and len(node.args) == 2
            and len(node.fun.uses) == 1
        ):
            return node
        return None


_COMMUTATIVE_INT_NAMES = frozenset({"%add", "%bit_and", "%bit_or", "%bit_xor"})


def _extract_inner_var_and_const(
    inner_node: Combination, block
) -> tuple[Lvar, Lvar, int] | None:
    arg0, arg1 = inner_node.args
    if lvar_type(arg0).fits(INTEGER):
        c1 = _exact_int_literal(arg1)
        if (
            c1 is not None
            and len(arg1.uses) == 1
            and arg1.uses[0].prev is not None
            and arg1.uses[0].block is block
        ):
            return arg0, arg1, c1
    if (
        inner_node.fun_info is not None
        and inner_node.fun_info.name in _COMMUTATIVE_INT_NAMES
        and lvar_type(arg1).fits(INTEGER)
    ):
        c1 = _exact_int_literal(arg0)
        if (
            c1 is not None
            and len(arg0.uses) == 1
            and arg0.uses[0].prev is not None
            and arg0.uses[0].block is block
        ):
            return arg1, arg0, c1
    return None


def _match_assoc_int_binary(
    node: Combination, inner_names: frozenset[str]
) -> tuple[Combination, Lvar, Lvar, int, Lvar, int] | None:
    if len(node.args) != 2 or len(node.fun.uses) != 1:
        return None
    left_lvar, right_lvar = node.args
    if len(left_lvar.uses) != 1 or len(right_lvar.uses) != 1:
        return None
    fun_ref = node.fun.uses[0]
    if fun_ref.prev is None or fun_ref.next is not node.prev:
        return None
    c2 = _exact_int_literal(right_lvar)
    if c2 is not None:
        inner_node = left_lvar.uses[0].accept(_KnownBinaryProducer(inner_names))
        right_ref = right_lvar.uses[0]
        if (
            inner_node is not None
            and inner_node.prev is not None
            and right_ref.prev is not None
            and inner_node.block is node.block
            and inner_node.next is right_ref.prev
            and right_ref.next is fun_ref.prev
        ):
            extracted = _extract_inner_var_and_const(inner_node, node.block)
            if extracted is not None:
                x_lvar, c1_lvar, c1 = extracted
                return inner_node, x_lvar, c1_lvar, c1, right_lvar, c2
    if node.fun_info is not None and node.fun_info.name in _COMMUTATIVE_INT_NAMES:
        c2 = _exact_int_literal(left_lvar)
        inner_node = right_lvar.uses[0].accept(_KnownBinaryProducer(inner_names))
        left_ref = left_lvar.uses[0]
        if (
            c2 is not None
            and inner_node is not None
            and inner_node.prev is not None
            and left_ref.prev is not None
            and left_ref.block is node.block
            and inner_node.block is node.block
            and inner_node.next is fun_ref.prev
        ):
            arg0, arg1 = inner_node.args
            c1 = _exact_int_literal(arg1)
            inner_fun_ref = inner_node.fun.uses[0]
            if (
                lvar_type(arg0).fits(INTEGER)
                and c1 is not None
                and len(arg1.uses) == 1
                and arg1.uses[0].prev is not None
                and inner_fun_ref.prev is not None
                and arg1.uses[0].next is inner_fun_ref.prev
                and inner_fun_ref.next is inner_node.prev
            ):
                return inner_node, arg0, left_lvar, c1, arg1, c2
    return None


def _replace_assoc_binary(
    node: Combination,
    inner_node: Combination,
    target_info,
    x_lvar: Lvar,
    flush_const_lvar: Lvar,
    keep_const_lvar: Lvar,
    combined: int,
) -> bool:
    if not _rewrite_int_constant_lvar(keep_const_lvar, combined):
        return False
    inner_node.args = []
    _flush_operand(inner_node.fun)
    _flush_operand(flush_const_lvar)
    unlink_node(inner_node)
    retarget_known_combination(node, target_info, [x_lvar, keep_const_lvar])
    return True


def _collapse_assoc_to_lvar(
    node: Combination,
    inner_node: Combination,
    x_lvar: Lvar,
    flush_const_lvar: Lvar,
    keep_const_lvar: Lvar,
) -> bool:
    inner_lvar = inner_node.lvar
    inner_node.args = []
    x_lvar.dest = node
    keep_const_lvar.dest = node
    node.args = [x_lvar, keep_const_lvar]
    _flush_operand(inner_lvar)
    _flush_operand(inner_node.fun)
    _flush_operand(flush_const_lvar)
    unlink_node(inner_node)
    replace_combination_with_lvar(node, x_lvar)
    return True


def _is_redundant_bitmask(val_lvar: Lvar, mask_lvar: Lvar) -> bool:
    mask = _exact_int_literal(mask_lvar)
    if mask is None or mask <= 0 or (mask & (mask + 1)) != 0:
        return False
    if constant_literal(val_lvar) is not None:
        return False
    val_type = lvar_type(val_lvar)
    return (
        val_type.fits(INTEGER)
        and val_type.exact_integer() is None
        and val_type.minimum is not None
        and val_type.maximum is not None
        and 0 <= val_type.minimum
        and val_type.maximum <= mask
    )


_BIT_AND_NAMES = frozenset({"%bit_and"})
_BIT_OR_NAMES = frozenset({"%bit_or"})
_BIT_XOR_NAMES = frozenset({"%bit_xor"})
_ADD_SUB_NAMES = frozenset({"%add", "%subtract"})
_RSHIFT_NAMES = frozenset({"%right_shift"})
_LSHIFT_NAMES = frozenset({"%left_shift"})


@deftransform("%bit_and", (INTEGER, INTEGER))
def _transform_bit_and_modarith(node: Combination) -> bool:
    left, right = node.args
    if _is_redundant_bitmask(left, right):
        replace_combination_with_lvar(node, left)
        return True
    if _is_redundant_bitmask(right, left):
        replace_combination_with_lvar(node, right)
        return True
    matched = _match_assoc_int_binary(node, _BIT_AND_NAMES)
    if matched is None:
        return False
    inner_node, x_lvar, c1_lvar, c1, right_lvar, c2 = matched
    combined = c1 & c2
    if combined.bit_length() > _FOLD_LIMIT_BITS:
        return False
    return _replace_assoc_binary(
        node,
        inner_node,
        fndb.BINARY[ast.BinaryOperator.BIT_AND],
        x_lvar,
        c1_lvar,
        right_lvar,
        combined,
    )


@deftransform("%bit_or", (INTEGER, INTEGER))
def _transform_bit_or_assoc(node: Combination) -> bool:
    matched = _match_assoc_int_binary(node, _BIT_OR_NAMES)
    if matched is None:
        return False
    inner_node, x_lvar, c1_lvar, c1, right_lvar, c2 = matched
    combined = c1 | c2
    if combined.bit_length() > _FOLD_LIMIT_BITS:
        return False
    return _replace_assoc_binary(
        node,
        inner_node,
        fndb.BINARY[ast.BinaryOperator.BIT_OR],
        x_lvar,
        c1_lvar,
        right_lvar,
        combined,
    )


@deftransform("%bit_xor", (INTEGER, INTEGER))
def _transform_bit_xor_assoc(node: Combination) -> bool:
    matched = _match_assoc_int_binary(node, _BIT_XOR_NAMES)
    if matched is None:
        return False
    inner_node, x_lvar, c1_lvar, c1, right_lvar, c2 = matched
    combined = c1 ^ c2
    if combined.bit_length() > _FOLD_LIMIT_BITS:
        return False
    return _replace_assoc_binary(
        node,
        inner_node,
        fndb.BINARY[ast.BinaryOperator.BIT_XOR],
        x_lvar,
        c1_lvar,
        right_lvar,
        combined,
    )


_INT64_TYPE = IntegerType(-(1 << 63), (1 << 63) - 1)


def _transform_add_subtract_assoc(node: Combination) -> bool:
    matched = _match_assoc_int_binary(node, _ADD_SUB_NAMES)
    if matched is None:
        return False
    inner_node, x_lvar, c1_lvar, c1, right_lvar, c2 = matched
    if lvar_type(x_lvar).exact_integer() is not None and not lvar_type(
        node.args[0]
    ).fits(_INT64_TYPE):
        return False
    inner_is_add = (
        inner_node.fun_info is not None and inner_node.fun_info.name == "%add"
    )
    outer_is_add = node.fun_info is not None and node.fun_info.name == "%add"
    delta = (c1 if inner_is_add else -c1) + (c2 if outer_is_add else -c2)
    if delta == 0:
        return _collapse_assoc_to_lvar(node, inner_node, x_lvar, c1_lvar, right_lvar)
    if delta > 0:
        if delta.bit_length() > _FOLD_LIMIT_BITS:
            return False
        return _replace_assoc_binary(
            node,
            inner_node,
            fndb.BINARY[ast.BinaryOperator.ADD],
            x_lvar,
            c1_lvar,
            right_lvar,
            delta,
        )
    magnitude = -delta
    if magnitude.bit_length() > _FOLD_LIMIT_BITS:
        return False
    return _replace_assoc_binary(
        node,
        inner_node,
        fndb.BINARY[ast.BinaryOperator.SUBTRACT],
        x_lvar,
        c1_lvar,
        right_lvar,
        magnitude,
    )


deftransform("%add", (INTEGER, INTEGER), _transform_add_subtract_assoc)
deftransform("%subtract", (INTEGER, INTEGER), _transform_add_subtract_assoc)


@deftransform("%right_shift", (INTEGER, INTEGER))
def _transform_right_shift_assoc(node: Combination) -> bool:
    matched = _match_assoc_int_binary(node, _RSHIFT_NAMES)
    if matched is None:
        return False
    inner_node, x_lvar, c1_lvar, c1, right_lvar, c2 = matched
    if c1 < 0 or c2 < 0 or c1 + c2 > _MAX_COMBINED_SHIFT:
        return False
    return _replace_assoc_binary(
        node,
        inner_node,
        fndb.BINARY[ast.BinaryOperator.RIGHT_SHIFT],
        x_lvar,
        c1_lvar,
        right_lvar,
        c1 + c2,
    )


@deftransform("%left_shift", (INTEGER, INTEGER))
def _transform_left_shift_assoc(node: Combination) -> bool:
    matched = _match_assoc_int_binary(node, _LSHIFT_NAMES)
    if matched is None:
        return False
    inner_node, x_lvar, c1_lvar, c1, right_lvar, c2 = matched
    if c1 < 0 or c2 < 0 or c1 + c2 > _MAX_COMBINED_SHIFT:
        return False
    return _replace_assoc_binary(
        node,
        inner_node,
        fndb.BINARY[ast.BinaryOperator.LEFT_SHIFT],
        x_lvar,
        c1_lvar,
        right_lvar,
        c1 + c2,
    )


__all__ = [
    "_collapse_assoc_to_lvar",
    "_is_redundant_bitmask",
    "_match_assoc_int_binary",
    "_replace_assoc_binary",
]
