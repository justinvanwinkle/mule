"""IR1 source-level arithmetic and bitwise transforms, following
``src/compiler/srctran.lisp``.

Registers type-gated ``@deftransform`` rules on known arithmetic, bitwise,
comparison, and sequence repetition/concatenation operations:

- Identity reductions (``x + 0 -> x``, ``0 + x -> x``, ``x - 0 -> x``,
  ``x * 1 -> x``, ``1 * x -> x``, ``x // 1 -> x``, ``x ** 1 -> x``,
  ``x | 0 -> x``, ``0 | x -> x``, ``x ^ 0 -> x``, ``0 ^ x -> x``,
  ``x & -1 -> x``, ``-1 & x -> x``, ``x << 0 -> x``, ``x >> 0 -> x``,
  ``+x -> x``, ``s + "" -> s``, ``"" + s -> s``, ``b + b"" -> b``,
  ``b"" + b -> b``, ``s * 1 -> s``, ``1 * s -> s``, ``b * 1 -> b``,
  ``1 * b -> b``, and boolean bitwise identities ``b | False -> b``,
  ``False | b -> b``, ``b ^ False -> b``, ``False ^ b -> b``,
  ``b & True -> b``, ``True & b -> b``).
- Absorbing and annihilation constants on pure bound integer operands
  (``x * 0 -> 0``, ``0 * x -> 0``, ``x % 1 -> 0``, ``x % -1 -> 0``,
  ``x & 0 -> 0``, ``0 & x -> 0``).
- Negative unit reductions on exact integers
  (``x * -1 -> -x``, ``-1 * x -> -x``, ``x // -1 -> -x``).
- Unary involutions on exact integers (``-(-x) -> x`` and ``~(~x) -> x``).
- Power-of-two strength reductions on exact integers
  (``x * 2**k -> x << k``, ``2**k * x -> x << k``, ``x // 2**k -> x >> k``,
  and ``x % 2**k -> x & (2**k - 1)`` for ``k >= 1``).
- Same-variable algebraic and reflexive simplifications on bound locals
  (``x - x -> 0``, ``x ^ x -> 0`` / ``False``, ``x & x -> x``, ``x | x -> x``,
  reflexive comparisons ``x == x``, ``x != x``, ``x < x``, ``x <= x``,
  ``x > x``, ``x >= x`` on ``int``/``bool``/``str``/``bytes``, and reflexive
  identity ``x is x -> True``, ``x is not x -> False``).

Python adaptation: ``bool`` is a subclass of ``int`` whose arithmetic operators
widen ``bool`` to ``int`` (for example, ``True + 0`` evaluates to ``1``, not
``True``, and ``-(-True)`` evaluates to ``1``). Integer transforms therefore
gate on ``INTEGER`` (which excludes ``BOOLEAN`` and ``PYTHON_INTEGER``) and
inspect syntactic ``ast.IntegerLiteral`` operands rather than ``BooleanLiteral``.
"""

from __future__ import annotations

from mule import ast

from . import fndb
from .ctype import (
    BOOLEAN,
    BYTES_TYPE,
    INTEGER,
    NONE_TYPE,
    PYTHON_INTEGER,
    REAL,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    ValueType,
)
from .ir1opt import (
    constant_lvar_value,
    is_imported_module_global,
    lvar_type,
    replace_combination_with_constant,
    splice_fun_args,
)
from .ir1util import (
    _flush_operand,
    delete_ref,
    is_flushable_ref,
    is_pure_bound_ref,
    replace_combination_with_lvar,
    retarget_known_combination,
    unlink_node,
)
from .knownfun import deftransform
from .node import (
    CallKind,
    Combination,
    Constant,
    GlobalVar,
    LambdaVar,
    LeafVisitor,
    Lvar,
    Node,
    NodeVisitor,
    Ref,
)


class _ExactIntLiteral(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral) -> int:
        return node.value


class _ExactBoolLiteral(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_BooleanLiteral(self, node: ast.BooleanLiteral) -> bool:
        return node.value


class _ExactTextLiteral(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral) -> str:
        return node.value


class _ExactBytesLiteral(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> bytes:
        return node.value


_EXACT_INT_LITERAL = _ExactIntLiteral()
_EXACT_BOOL_LITERAL = _ExactBoolLiteral()
_EXACT_TEXT_LITERAL = _ExactTextLiteral()
_EXACT_BYTES_LITERAL = _ExactBytesLiteral()


def _exact_int_literal(lvar: Lvar) -> int | None:
    if not is_pure_bound_ref(lvar):
        return None
    literal = constant_lvar_value(lvar)
    return literal.accept(_EXACT_INT_LITERAL) if literal is not None else None


def _exact_bool_literal(lvar: Lvar) -> bool | None:
    if not is_pure_bound_ref(lvar):
        return None
    literal = constant_lvar_value(lvar)
    return literal.accept(_EXACT_BOOL_LITERAL) if literal is not None else None


def _exact_text_literal(lvar: Lvar) -> str | None:
    if not is_pure_bound_ref(lvar):
        return None
    literal = constant_lvar_value(lvar)
    return literal.accept(_EXACT_TEXT_LITERAL) if literal is not None else None


def _exact_bytes_literal(lvar: Lvar) -> bytes | None:
    if not is_pure_bound_ref(lvar):
        return None
    literal = constant_lvar_value(lvar)
    return literal.accept(_EXACT_BYTES_LITERAL) if literal is not None else None


def _is_exact_int_type(vtype: ValueType) -> bool:
    return vtype.fits(INTEGER)


def _power_of_two_exponent(value: int | None) -> int | None:
    if value is not None and value > 1 and (value & (value - 1)) == 0:
        return value.bit_length() - 1
    return None


class _LocalVarRef(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref) -> LambdaVar | None:
        if node.final:
            return None
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf: LambdaVar) -> LambdaVar:
        return leaf


_LOCAL_VAR_REF = _LocalVarRef()


def _same_block_bound_refs(
    var: LambdaVar,
    left_use: Node,
    right_use: Node,
) -> bool:
    if not (is_flushable_ref(left_use) and is_flushable_ref(right_use)):
        return False
    if left_use.prev is None or right_use.prev is None:
        return False
    block = left_use.block
    if right_use.block is not block:
        return False
    seen_left = False
    for node in block.nodes():
        if node is left_use:
            if seen_left or left_use is right_use:
                return False
            seen_left = True
        elif node is right_use:
            return seen_left
        elif seen_left and node in var.sets:
            return False
    return False


def _same_bound_local(left: Lvar, right: Lvar) -> LambdaVar | None:
    if len(left.uses) != 1 or len(right.uses) != 1:
        return None
    left_use = left.uses[0]
    right_use = right.uses[0]
    left_var = left_use.accept(_LOCAL_VAR_REF)
    if left_var is None:
        return None
    right_var = right_use.accept(_LOCAL_VAR_REF)
    if left_var is not right_var:
        return None
    if lvar_type(left).may_be_unbound or lvar_type(right).may_be_unbound:
        return None
    if not _same_block_bound_refs(left_var, left_use, right_use):
        return None
    return left_var


_is_pure_bound_ref = is_pure_bound_ref
_delete_unused_local_ref = _flush_operand


def _replace_with_zero_and_cleanup(node: Combination, dropped_lvar: Lvar) -> bool:
    if node.lvar is None or not is_pure_bound_ref(dropped_lvar):
        return False
    zero = ast.IntegerLiteral(
        0, span=node.source.span if node.source is not None else None
    )
    replace_combination_with_constant(node, zero)
    return True


def _replace_with_constant_and_clean_locals(
    node: Combination, literal: ast.Literal
) -> bool:
    if node.lvar is None:
        return False
    literal.span = node.source.span if node.source is not None else None
    replace_combination_with_constant(node, literal)
    return True


def _replace_with_lvar_and_clean(node: Combination, value: Lvar) -> bool:
    replace_combination_with_lvar(node, value)
    return True


def _replace_with_left_and_clean_right(node: Combination) -> bool:
    return _replace_with_lvar_and_clean(node, node.args[0])


class _RewriteIntConstant(NodeVisitor, LeafVisitor):
    def __init__(self, value: int):
        self.value = value
        self._ref: Ref | None = None

    def visit_Node(self, node):
        return False

    def visit_Ref(self, node: Ref):
        self._ref = node
        ok = node.leaf.accept(self)
        self._ref = None
        return ok

    def visit_Leaf(self, leaf):
        return False

    def visit_Constant(self, leaf: Constant):
        ref = self._ref
        if ref is None:
            return False
        span = ref.source.span if ref.source is not None else leaf.value.span
        literal = ast.IntegerLiteral(self.value, span=span)
        if ref in leaf.refs:
            leaf.refs.remove(ref)
        new_leaf = Constant(literal)
        ref.leaf = new_leaf
        new_leaf.refs.append(ref)
        ref.derived_type = IntegerType(self.value, self.value)
        return True

    def visit_LambdaVar(self, leaf: LambdaVar):
        ref = self._ref
        if ref is None or ref.final:
            return False
        if any(s.value is None for s in leaf.sets):
            return False
        if ref in leaf.refs:
            leaf.refs.remove(ref)
        span = ref.source.span if ref.source is not None else None
        new_leaf = Constant(ast.IntegerLiteral(self.value, span=span))
        ref.leaf = new_leaf
        new_leaf.refs.append(ref)
        ref.derived_type = IntegerType(self.value, self.value)
        return True


def _rewrite_int_constant_lvar(lvar: Lvar, value: int) -> bool:
    if len(lvar.uses) != 1:
        return False
    if not lvar.uses[0].accept(_RewriteIntConstant(value)):
        return False
    lvar.derived_type = IntegerType(value, value)
    return True


_rewrite_constant_int = _rewrite_int_constant_lvar


def _reduce_unary_involution(node: Combination, name: str) -> bool:
    if len(node.args) != 1:
        return False
    inner_node = splice_fun_args(node.args[0], (name,), 1, splice=False)
    if inner_node is None or not _is_exact_int_type(lvar_type(inner_node.args[0])):
        return False
    splice_fun_args(node.args[0], (name,), 1)
    replace_combination_with_lvar(node, node.args[0])
    return True


# ---------------------------------------------------------------------------
# Identity reductions
# ---------------------------------------------------------------------------


@deftransform("%add", (INTEGER, INTEGER))
def _transform_add_int(node: Combination) -> bool:
    left, right = node.args
    if _exact_int_literal(right) == 0:
        return _replace_with_lvar_and_clean(node, left)
    if _exact_int_literal(left) == 0:
        return _replace_with_lvar_and_clean(node, right)
    return False


@deftransform("%add", (TEXT_TYPE, TEXT_TYPE))
def _transform_add_text(node: Combination) -> bool:
    left, right = node.args
    if _exact_text_literal(right) == "":
        return _replace_with_lvar_and_clean(node, left)
    if _exact_text_literal(left) == "":
        return _replace_with_lvar_and_clean(node, right)
    return False


@deftransform("%add", (BYTES_TYPE, BYTES_TYPE))
def _transform_add_bytes(node: Combination) -> bool:
    left, right = node.args
    if _exact_bytes_literal(right) == b"":
        return _replace_with_lvar_and_clean(node, left)
    if _exact_bytes_literal(left) == b"":
        return _replace_with_lvar_and_clean(node, right)
    return False


@deftransform("%subtract", (INTEGER, INTEGER))
def _transform_subtract_int(node: Combination) -> bool:
    left, right = node.args
    if _exact_int_literal(right) == 0:
        return _replace_with_lvar_and_clean(node, left)
    if _same_bound_local(left, right) is not None:
        return _replace_with_constant_and_clean_locals(node, ast.IntegerLiteral(0))
    return False


@deftransform("%multiply", (INTEGER, INTEGER))
def _transform_multiply_int(node: Combination) -> bool:
    left, right = node.args
    left_val = _exact_int_literal(left)
    right_val = _exact_int_literal(right)
    if right_val == 1:
        return _replace_with_lvar_and_clean(node, left)
    if left_val == 1:
        return _replace_with_lvar_and_clean(node, right)
    if right_val == 0 and left_val is None and _is_pure_bound_ref(left):
        return _replace_with_zero_and_cleanup(node, left)
    if left_val == 0 and right_val is None and _is_pure_bound_ref(right):
        return _replace_with_zero_and_cleanup(node, right)
    if right_val == -1 and left_val is None and len(right.uses) == 1:
        _delete_unused_local_ref(right)
        retarget_known_combination(node, fndb.UNARY[ast.UnaryOperator.NEGATIVE], [left])
        return True
    if left_val == -1 and right_val is None and len(left.uses) == 1:
        _delete_unused_local_ref(left)
        retarget_known_combination(
            node, fndb.UNARY[ast.UnaryOperator.NEGATIVE], [right]
        )
        return True
    right_exp = _power_of_two_exponent(right_val)
    if (
        right_exp is not None
        and left_val is None
        and _rewrite_constant_int(right, right_exp)
    ):
        retarget_known_combination(
            node, fndb.BINARY[ast.BinaryOperator.LEFT_SHIFT], [left, right]
        )
        return True
    left_exp = _power_of_two_exponent(left_val)
    if (
        left_exp is not None
        and right_val is None
        and _rewrite_constant_int(left, left_exp)
    ):
        retarget_known_combination(
            node, fndb.BINARY[ast.BinaryOperator.LEFT_SHIFT], [right, left]
        )
        return True
    return False


@deftransform("%multiply", (TEXT_TYPE, INTEGER))
def _transform_multiply_text_int(node: Combination) -> bool:
    left, right = node.args
    if _exact_int_literal(right) == 1:
        return _replace_with_lvar_and_clean(node, left)
    return False


@deftransform("%multiply", (INTEGER, TEXT_TYPE))
def _transform_multiply_int_text(node: Combination) -> bool:
    left, right = node.args
    if _exact_int_literal(left) == 1:
        return _replace_with_lvar_and_clean(node, right)
    return False


@deftransform("%multiply", (BYTES_TYPE, INTEGER))
def _transform_multiply_bytes_int(node: Combination) -> bool:
    left, right = node.args
    if _exact_int_literal(right) == 1:
        return _replace_with_lvar_and_clean(node, left)
    return False


@deftransform("%multiply", (INTEGER, BYTES_TYPE))
def _transform_multiply_int_bytes(node: Combination) -> bool:
    left, right = node.args
    if _exact_int_literal(left) == 1:
        return _replace_with_lvar_and_clean(node, right)
    return False


@deftransform("%floor_divide", (INTEGER, INTEGER))
def _transform_floor_divide_int(node: Combination) -> bool:
    left, right = node.args
    divisor = _exact_int_literal(right)
    if divisor == 1:
        return _replace_with_lvar_and_clean(node, left)
    if divisor == -1 and _exact_int_literal(left) is None and len(right.uses) == 1:
        _delete_unused_local_ref(right)
        retarget_known_combination(node, fndb.UNARY[ast.UnaryOperator.NEGATIVE], [left])
        return True
    exponent = _power_of_two_exponent(divisor)
    if exponent is not None and _rewrite_int_constant_lvar(right, exponent):
        retarget_known_combination(
            node, fndb.BINARY[ast.BinaryOperator.RIGHT_SHIFT], [left, right]
        )
        return True
    return False


@deftransform("%modulo", (INTEGER, INTEGER))
def _transform_modulo_int(node: Combination) -> bool:
    left, right = node.args
    divisor = _exact_int_literal(right)
    if (
        divisor in (1, -1)
        and _exact_int_literal(left) is None
        and _is_pure_bound_ref(left)
    ):
        _delete_unused_local_ref(right)
        return _replace_with_zero_and_cleanup(node, left)
    exponent = _power_of_two_exponent(divisor)
    if (
        divisor is not None
        and exponent is not None
        and _rewrite_int_constant_lvar(right, divisor - 1)
    ):
        retarget_known_combination(
            node, fndb.BINARY[ast.BinaryOperator.BIT_AND], [left, right]
        )
        return True
    return False


@deftransform("%power", (INTEGER, INTEGER))
def _transform_power_int(node: Combination) -> bool:
    left, right = node.args
    if _exact_int_literal(right) == 1:
        return _replace_with_lvar_and_clean(node, left)
    return False


@deftransform("%bit_or", (INTEGER, INTEGER))
def _transform_bit_or_int(node: Combination) -> bool:
    left, right = node.args
    if _exact_int_literal(right) == 0:
        return _replace_with_lvar_and_clean(node, left)
    if _exact_int_literal(left) == 0:
        return _replace_with_lvar_and_clean(node, right)
    if _same_bound_local(left, right) is not None:
        return _replace_with_left_and_clean_right(node)
    return False


@deftransform("%bit_or", (BOOLEAN, BOOLEAN))
def _transform_bit_or_bool(node: Combination) -> bool:
    left, right = node.args
    if _exact_bool_literal(right) is False:
        return _replace_with_lvar_and_clean(node, left)
    if _exact_bool_literal(left) is False:
        return _replace_with_lvar_and_clean(node, right)
    if _same_bound_local(left, right) is not None:
        return _replace_with_left_and_clean_right(node)
    return False


@deftransform("%bit_xor", (INTEGER, INTEGER))
def _transform_bit_xor_int(node: Combination) -> bool:
    left, right = node.args
    if _exact_int_literal(right) == 0:
        return _replace_with_lvar_and_clean(node, left)
    if _exact_int_literal(left) == 0:
        return _replace_with_lvar_and_clean(node, right)
    if _same_bound_local(left, right) is not None:
        return _replace_with_constant_and_clean_locals(node, ast.IntegerLiteral(0))
    return False


@deftransform("%bit_xor", (BOOLEAN, BOOLEAN))
def _transform_bit_xor_bool(node: Combination) -> bool:
    left, right = node.args
    if _exact_bool_literal(right) is False:
        return _replace_with_lvar_and_clean(node, left)
    if _exact_bool_literal(left) is False:
        return _replace_with_lvar_and_clean(node, right)
    if _same_bound_local(left, right) is not None:
        return _replace_with_constant_and_clean_locals(node, ast.BooleanLiteral(False))
    return False


@deftransform("%bit_and", (INTEGER, INTEGER))
def _transform_bit_and_int(node: Combination) -> bool:
    left, right = node.args
    left_val = _exact_int_literal(left)
    right_val = _exact_int_literal(right)
    if right_val == -1:
        return _replace_with_lvar_and_clean(node, left)
    if left_val == -1:
        return _replace_with_lvar_and_clean(node, right)
    if right_val == 0 and left_val is None and _is_pure_bound_ref(left):
        _delete_unused_local_ref(right)
        return _replace_with_zero_and_cleanup(node, left)
    if left_val == 0 and right_val is None and _is_pure_bound_ref(right):
        _delete_unused_local_ref(left)
        return _replace_with_zero_and_cleanup(node, right)
    if _same_bound_local(left, right) is not None:
        return _replace_with_left_and_clean_right(node)
    return False


@deftransform("%bit_and", (BOOLEAN, BOOLEAN))
def _transform_bit_and_bool(node: Combination) -> bool:
    left, right = node.args
    if _exact_bool_literal(right) is True:
        return _replace_with_lvar_and_clean(node, left)
    if _exact_bool_literal(left) is True:
        return _replace_with_lvar_and_clean(node, right)
    if _same_bound_local(left, right) is not None:
        return _replace_with_left_and_clean_right(node)
    return False


@deftransform("%left_shift", (INTEGER, INTEGER))
def _transform_left_shift_int(node: Combination) -> bool:
    left, right = node.args
    if _exact_int_literal(right) == 0:
        return _replace_with_lvar_and_clean(node, left)
    return False


@deftransform("%right_shift", (INTEGER, INTEGER))
def _transform_right_shift_int(node: Combination) -> bool:
    left, right = node.args
    if _exact_int_literal(right) == 0:
        return _replace_with_lvar_and_clean(node, left)
    return False


@deftransform("%unary_positive", (INTEGER,))
def _transform_unary_positive_int(node: Combination) -> bool:
    replace_combination_with_lvar(node, node.args[0])
    return True


@deftransform("%unary_negative", (INTEGER,))
def _transform_unary_negative_int(node: Combination) -> bool:
    return _reduce_unary_involution(node, "%unary_negative")


@deftransform("%unary_invert", (INTEGER,))
def _transform_unary_invert_int(node: Combination) -> bool:
    return _reduce_unary_involution(node, "%unary_invert")


# ---------------------------------------------------------------------------
# Reflexive comparisons and identity on bound locals
# ---------------------------------------------------------------------------

_REFLEXIVE_COMPARABLE_TYPES = (PYTHON_INTEGER, TEXT_TYPE, BYTES_TYPE)
_REFLEXIVE_COMPARE_RESULTS = (
    ("%compare_equal", True),
    ("%compare_not_equal", False),
    ("%compare_less", False),
    ("%compare_less_equal", True),
    ("%compare_greater", False),
    ("%compare_greater_equal", True),
)


def _is_reflexive_comparable_type(vtype: ValueType) -> bool:
    return any(vtype.fits(expected) for expected in _REFLEXIVE_COMPARABLE_TYPES)


def _transform_reflexive_compare(node: Combination, result: bool) -> bool:
    left, right = node.args
    if _same_bound_local(left, right) is None:
        return False
    if not (
        _is_reflexive_comparable_type(lvar_type(left))
        and _is_reflexive_comparable_type(lvar_type(right))
    ):
        return False
    return _replace_with_constant_and_clean_locals(node, ast.BooleanLiteral(result))


for _compare_name, _compare_result in _REFLEXIVE_COMPARE_RESULTS:
    for _comparable_type in _REFLEXIVE_COMPARABLE_TYPES:
        deftransform(
            _compare_name,
            (_comparable_type, _comparable_type),
            lambda node, res=_compare_result: _transform_reflexive_compare(node, res),
        )


@deftransform("%compare_is", (UNKNOWN, UNKNOWN))
def _transform_compare_is(node: Combination) -> bool:
    left, right = node.args
    if _same_bound_local(left, right) is not None:
        return _replace_with_constant_and_clean_locals(node, ast.BooleanLiteral(True))
    return False


@deftransform("%compare_is_not", (UNKNOWN, UNKNOWN))
def _transform_compare_is_not(node: Combination) -> bool:
    left, right = node.args
    if _same_bound_local(left, right) is not None:
        return _replace_with_constant_and_clean_locals(node, ast.BooleanLiteral(False))
    return False


@deftransform("min")
def _transform_min(node: Combination) -> bool:
    if len(node.args) != 2:
        return False
    left, right = node.args
    retarget_known_combination(node, fndb.MIN2, [left, right])
    return True


@deftransform("max")
def _transform_max(node: Combination) -> bool:
    if len(node.args) != 2:
        return False
    left, right = node.args
    retarget_known_combination(node, fndb.MAX2, [left, right])
    return True


def _is_unit_slice_step(step_lvar: Lvar) -> bool:
    if not is_pure_bound_ref(step_lvar):
        return False
    if lvar_type(step_lvar).fits_none(NONE_TYPE):
        return True
    return _exact_int_literal(step_lvar) == 1


def _is_slice_bound_lvar(bound_lvar: Lvar) -> bool:
    bound_type = lvar_type(bound_lvar)
    if bound_type.is_fixnum:
        return True
    return bound_type.fits_none(NONE_TYPE) and is_pure_bound_ref(bound_lvar)


@deftransform("int", (TEXT_TYPE,))
def _transform_int_text(node: Combination) -> bool:
    retarget_known_combination(node, fndb.PARSE_INT_TEXT, [node.args[0]])
    return True


@deftransform("%parse_int_text")
def _transform_parse_int_slice(node: Combination) -> bool:
    if len(node.args) != 1:
        return False
    slice_node = splice_fun_args(node.args[0], ("%getslice",), 4, splice=False)
    if slice_node is None:
        return False
    seq_lvar, start_lvar, stop_lvar, step_lvar = slice_node.args
    if (
        not lvar_type(seq_lvar).fits_text(TEXT_TYPE)
        or not _is_slice_bound_lvar(start_lvar)
        or not _is_slice_bound_lvar(stop_lvar)
        or not _is_unit_slice_step(step_lvar)
    ):
        return False
    splice_fun_args(node.args[0], ("%getslice",), 4)
    retarget_known_combination(
        node, fndb.PARSE_INT_SLICE, [node.args[0], node.args[1], node.args[2]]
    )
    return True


@deftransform("math.sqrt")
def _transform_math_sqrt(node: Combination) -> bool:
    if len(node.args) != 1 or not lvar_type(node.args[0]).fits(REAL):
        return False
    retarget_known_combination(node, fndb.MATH_SQRT, [node.args[0]])
    return True


class _MathModuleGlobalRef(NodeVisitor, LeafVisitor):
    """Match a single-use ``Ref`` to a module-level ``import math`` global."""

    def __init__(self, block):
        self.block = block
        self._ref: Ref | None = None

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if (
            node.prev is None
            or node.block is not self.block
            or node.lexenv.policy.safety >= 3
        ):
            return None
        self._ref = node
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_GlobalVar(self, leaf: GlobalVar):
        if is_imported_module_global(leaf, "math"):
            return self._ref
        return None


class _ResolveMathSqrtProducer(NodeVisitor):
    """Match a ``%resolve_method`` for ``math.sqrt`` in ``block``."""

    def __init__(self, block):
        self.block = block

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.RESOLVE_METHOD
            or node.info is None
            or node.info.name != "sqrt"
            or len(node.args) != 1
            or len(node.args[0].uses) != 1
            or node.prev is None
            or node.block is not self.block
            or node.lexenv.policy.safety >= 3
        ):
            return None
        recv_ref = node.args[0].uses[0].accept(_MathModuleGlobalRef(self.block))
        if recv_ref is None:
            return None
        return node, recv_ref


@deftransform("%call_resolved")
def _transform_call_resolved_math_sqrt(node: Combination) -> bool:
    if (
        len(node.args) != 2
        or (node.info is not None and node.info.keywords)
        or node.lexenv.policy.safety >= 3
        or (node.home_lambda is not None and node.home_lambda.policy.safety >= 3)
    ):
        return False
    resolved, arg = node.args
    if len(resolved.uses) != 1 or not lvar_type(arg).fits(REAL):
        return False
    matched = resolved.uses[0].accept(_ResolveMathSqrtProducer(node.block))
    if matched is None:
        return False
    resolve_node, recv_ref = matched
    recv_lvar = resolve_node.args[0]
    delete_ref(recv_ref)
    unlink_node(recv_ref)
    recv_lvar.dest = None
    resolve_node.args = []
    _flush_operand(resolve_node.fun)
    unlink_node(resolve_node)
    resolved.dest = None
    node.args = [arg]
    retarget_known_combination(node, fndb.MATH_SQRT, [arg])
    return True


__all__ = [
    "UNKNOWN",
    "_delete_unused_local_ref",
    "_exact_bool_literal",
    "_exact_bytes_literal",
    "_exact_int_literal",
    "_exact_text_literal",
    "_is_exact_int_type",
    "_is_pure_bound_ref",
    "_is_slice_bound_lvar",
    "_is_unit_slice_step",
    "_power_of_two_exponent",
    "_replace_with_zero_and_cleanup",
    "_rewrite_constant_int",
    "_rewrite_int_constant_lvar",
    "_same_bound_local",
    "replace_combination_with_constant",
    "unlink_node",
]
