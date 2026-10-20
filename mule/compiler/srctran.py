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
    PYTHON_INTEGER,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    ValueType,
)
from .ir1opt import (
    constant_literal,
    lvar_type,
    replace_combination_with_constant,
)
from .ir1util import (
    _flush_operand,
    delete_ref,
    flush_combination,
    replace_combination_with_lvar,
    retarget_known_combination,
    unlink_node,
)
from .knownfun import deftransform
from .node import (
    CallKind,
    Combination,
    Constant,
    KnownFunction,
    Lambda,
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
    literal = constant_literal(lvar)
    if literal is not None:
        return literal.accept(_EXACT_INT_LITERAL)
    if not _is_pure_bound_ref(lvar):
        return None
    vtype = lvar_type(lvar)
    if not vtype.fits(INTEGER):
        return None
    return vtype.exact_integer()


def _exact_bool_literal(lvar: Lvar) -> bool | None:
    literal = constant_literal(lvar)
    if literal is not None:
        return literal.accept(_EXACT_BOOL_LITERAL)
    if not _is_pure_bound_ref(lvar):
        return None
    vtype = lvar_type(lvar)
    if not vtype.fits(BOOLEAN):
        return None
    if vtype.minimum == 1 and vtype.maximum == 1:
        return True
    if vtype.minimum == 0 and vtype.maximum == 0:
        return False
    return None


def _exact_text_literal(lvar: Lvar) -> str | None:
    literal = constant_literal(lvar)
    if literal is not None:
        return literal.accept(_EXACT_TEXT_LITERAL)
    if not _is_pure_bound_ref(lvar):
        return None
    vtype = lvar_type(lvar)
    if not vtype.fits(TEXT_TYPE):
        return None
    return vtype.interned_text()


def _exact_bytes_literal(lvar: Lvar) -> bytes | None:
    literal = constant_literal(lvar)
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
    left_type: ValueType,
) -> bool:
    if any(s.value is None for s in var.sets):
        return False
    if left_use.prev is None or right_use.prev is None:
        return False
    block = left_use.block
    if right_use.block is not block:
        return False
    seen_set_before_left = False
    seen_left = False
    seen_right = False
    for node in block.nodes():
        if node is left_use:
            if seen_left or left_use is right_use:
                return False
            seen_left = True
        elif node is right_use:
            if not seen_left:
                return False
            seen_right = True
            break
        elif seen_left and node in var.sets:
            return False
        elif not seen_left and node in var.sets and node.value is not None:
            seen_set_before_left = True
    if not seen_right:
        return False
    if (
        var.type is None
        and left_type is UNKNOWN
        and var.home is not None
        and var not in var.home.vars
    ):
        if seen_set_before_left:
            return True
        if var.home.bind is not None:
            bind_block = var.home.bind.block
            if bind_block is not block and any(
                s.block is bind_block and s.value is not None for s in var.sets
            ):
                return True
        return False
    return True


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
    left_type = lvar_type(left)
    right_type = lvar_type(right)
    if left_type.may_be_unbound or right_type.may_be_unbound:
        return None
    if not _same_block_bound_refs(left_var, left_use, right_use, left_type):
        return None
    return left_var


class _IsPureBoundRef(NodeVisitor, LeafVisitor):
    def __init__(self, lvar: Lvar):
        self.lvar = lvar

    def visit_Node(self, node):
        return False

    def visit_Ref(self, node: Ref) -> bool:
        if node.final:
            return False
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return False

    def visit_Constant(self, leaf: Constant) -> bool:
        return True

    def visit_KnownFunction(self, leaf: KnownFunction) -> bool:
        return True

    def visit_Lambda(self, leaf: Lambda) -> bool:
        return True

    def visit_LambdaVar(self, leaf: LambdaVar) -> bool:
        if any(s.value is None for s in leaf.sets):
            return False
        return not lvar_type(self.lvar).may_be_unbound


def _is_pure_bound_ref(lvar: Lvar) -> bool:
    if len(lvar.uses) != 1:
        return False
    return lvar.uses[0].accept(_IsPureBoundRef(lvar))


class _DeleteUnusedLocalRef(NodeVisitor, LeafVisitor):
    def __init__(self):
        self._ref: Ref | None = None

    def visit_Ref(self, node: Ref):
        if node.prev is not None and node.lvar is None and not node.final:
            self._ref = node
            node.leaf.accept(self)
            self._ref = None

    def visit_LambdaVar(self, leaf: LambdaVar):
        ref = self._ref
        if ref is not None:
            delete_ref(ref)
            unlink_node(ref)


_DELETE_UNUSED_LOCAL_REF = _DeleteUnusedLocalRef()


def _delete_unused_local_ref(lvar: Lvar) -> None:
    uses = list(lvar.uses)
    _flush_operand(lvar)
    for use in uses:
        use.accept(_DELETE_UNUSED_LOCAL_REF)


def _replace_with_zero_and_cleanup(node: Combination, dropped_lvar: Lvar) -> bool:
    if node.lvar is None:
        return False
    _delete_unused_local_ref(dropped_lvar)
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
    left, right = node.args
    _delete_unused_local_ref(left)
    _delete_unused_local_ref(right)
    literal.span = node.source.span if node.source is not None else None
    replace_combination_with_constant(node, literal)
    return True


def _replace_with_lvar_and_clean(node: Combination, value: Lvar) -> bool:
    for arg in node.args:
        if arg is not value:
            _delete_unused_local_ref(arg)
    replace_combination_with_lvar(node, value)
    if node.lvar is None and value.dest is None:
        _delete_unused_local_ref(value)
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
        literal = ast.IntegerLiteral(self.value, span=leaf.value.span)
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


class _UnaryProducer(NodeVisitor):
    def __init__(self, name: str):
        self.name = name

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name == self.name
            and len(node.args) == 1
        ):
            return node
        return None


def _reduce_unary_involution(node: Combination, name: str) -> bool:
    if len(node.args) != 1 or len(node.fun.uses) != 1:
        return False
    fun_ref = node.fun.uses[0]
    arg = node.args[0]
    if len(arg.uses) != 1:
        return False
    inner_node = arg.uses[0].accept(_UnaryProducer(name))
    if (
        inner_node is None
        or inner_node.prev is None
        or fun_ref.prev is None
        or fun_ref.next is not node.prev
        or inner_node.next is not fun_ref.prev
    ):
        return False
    inner = inner_node.args[0]
    if not _is_exact_int_type(lvar_type(inner)):
        return False
    inner_node.args = []
    inner.dest = node
    node.args = [inner]
    arg.dest = None
    flush_combination(inner_node)
    replace_combination_with_lvar(node, inner)
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


__all__ = [
    "UNKNOWN",
    "_delete_unused_local_ref",
    "_exact_bool_literal",
    "_exact_bytes_literal",
    "_exact_int_literal",
    "_exact_text_literal",
    "_is_exact_int_type",
    "_is_pure_bound_ref",
    "_power_of_two_exponent",
    "_replace_with_zero_and_cleanup",
    "_rewrite_constant_int",
    "_rewrite_int_constant_lvar",
    "_same_bound_local",
    "replace_combination_with_constant",
    "unlink_node",
]
