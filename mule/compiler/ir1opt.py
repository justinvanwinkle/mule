"""IR1 optimization, following ``src/compiler/ir1opt.lisp``.

``ir1_optimize`` walks a component's blocks and applies the rewrites that
depend only on the graph and the known-function database: folding a known
foldable call on literal operands, flushing an unused flushable call, and
deleting the dead arm of a branch on a literal test. ``ir1_optimize_until_done``
repeats while rewrites request reoptimization, within the same iteration limit
SBCL uses. Type-dependent rewrites attach here as transforms once the facts
established by ``constraint.py`` and ``locall.py`` are consulted per node.

Python semantics shape the effect attributes in ``fndb.py``: nearly every
operator may run user code, so binary arithmetic folding is limited to integer
and boolean literals; unary signs also fold complex constants. Immutable tuples
of supported constants also fold, while a reference to a possibly unbound name
is never deleted because reading it raises.
"""

from __future__ import annotations

import functools

from mule import ast

from . import fndb
from .ir1util import (
    aver,
    delete_ref,
    flush_dest,
    reoptimize_lvar,
    unlink_blocks,
    unlink_node,
)
from .node import (
    CallKind,
    Combination,
    Component,
    Constant,
    CtranKind,
    Exit,
    If,
    LeafVisitor,
    NodeVisitor,
    Ref,
    linked,
)

MAX_OPTIMIZE_ITERATIONS = 3

_FOLDABLE_BINARY = {
    ast.BinaryOperator.ADD,
    ast.BinaryOperator.SUBTRACT,
    ast.BinaryOperator.MULTIPLY,
    ast.BinaryOperator.BIT_AND,
    ast.BinaryOperator.BIT_OR,
    ast.BinaryOperator.BIT_XOR,
}
_FOLDABLE_COMPARE = {
    ast.ComparisonOperator.EQUAL,
    ast.ComparisonOperator.NOT_EQUAL,
    ast.ComparisonOperator.LESS,
    ast.ComparisonOperator.LESS_EQUAL,
    ast.ComparisonOperator.GREATER,
    ast.ComparisonOperator.GREATER_EQUAL,
}
_BINARY_NAMES = {f"%{operator.name.lower()}": operator for operator in _FOLDABLE_BINARY}
_COMPARE_NAMES = {
    f"%compare_{operator.name.lower()}": operator for operator in _FOLDABLE_COMPARE
}
_UNARY_NAMES = {
    f"%unary_{operator.name.lower()}": operator
    for operator in (
        ast.UnaryOperator.POSITIVE,
        ast.UnaryOperator.NEGATIVE,
        ast.UnaryOperator.INVERT,
    )
}
# Folded integers stay within the bounds literal syntax reaches comfortably.
_FOLD_LIMIT_BITS = 128


def ir1_optimize_until_done(component: Component) -> bool:
    """Optimize until nothing requests another pass or the limit is reached.
    Returns whether any pass changed the component."""
    count = 0
    reoptimized = False
    while True:
        component.reoptimize = False
        changed = ir1_optimize(component)
        reoptimized |= changed
        if not component.reoptimize:
            return reoptimized
        count += 1
        if count >= MAX_OPTIMIZE_ITERATIONS:
            return reoptimized


def ir1_optimize(component: Component) -> bool:
    changed = False
    for block in list(component.blocks()):
        if block.delete_p or block.component is None:
            continue
        if not (block.reoptimize or block.flush_p):
            continue
        was_flush = block.flush_p
        block.reoptimize = False
        block.flush_p = False
        changed |= _optimize_block(block)
        if was_flush or block.flush_p:
            block.flush_p = False
            _flush_dead_operands(block)
    return changed


class _MakeSliceProducer(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name == "%make_slice"
            and len(node.args) == 3
        ):
            return node
        return None


_MAKE_SLICE_PRODUCER = _MakeSliceProducer()


class _RetargetKnownRef(NodeVisitor):
    def __init__(self, info):
        self.info = info

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node in node.leaf.refs:
            node.leaf.refs.remove(node)
        node.leaf = self.info
        self.info.refs.append(node)


def _optimize_getslice(node: Combination) -> bool:
    if len(node.args) != 2:
        return False
    slice_lvar = node.args[1]
    if len(slice_lvar.uses) != 1:
        return False
    slice_node = slice_lvar.uses[0].accept(_MAKE_SLICE_PRODUCER)
    if (
        slice_node is None
        or slice_node.prev is None
        or slice_node.next is not node.prev
    ):
        return False
    start_lvar, stop_lvar, step_lvar = slice_node.args
    for arg in (start_lvar, stop_lvar, step_lvar):
        arg.dest = node
    node.args = [node.args[0], start_lvar, stop_lvar, step_lvar]
    node.fun_info = fndb.GETSLICE
    if len(node.fun.uses) == 1:
        node.fun.uses[0].accept(_RetargetKnownRef(fndb.GETSLICE))
    slice_node.args = []
    slice_node.lvar = None
    slice_lvar.uses.clear()
    slice_lvar.dest = None
    _flush_operand(slice_node.fun)
    unlink_node(slice_node)
    if node.lvar is not None:
        reoptimize_lvar(node.lvar)
    return True


class _OptimizeNode(NodeVisitor):
    """Applies one node's rewrite; returns whether the graph changed."""

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node):
        if node.kind is not CallKind.KNOWN:
            return False
        if node.lvar is None and node.fun_info.flushable:
            flush_combination(node)
            return True
        if node.fun_info.name == "%getitem":
            return _optimize_getslice(node)
        if node.fun_info.name == "%build_tuple" and node.fun_info.foldable:
            return _constant_fold_tuple(node)
        if node.fun_info.name in _UNARY_NAMES:
            return _constant_fold_unary(node)
        if node.fun_info.name in _BINARY_NAMES or node.fun_info.name in _COMPARE_NAMES:
            return _constant_fold(node)
        return False

    def visit_If(self, node):
        return _optimize_if(node)


_OPTIMIZE_NODE = _OptimizeNode()


def _optimize_block(block) -> bool:
    changed = False
    for node in list(block.nodes()):
        if node.prev is None:
            continue
        changed |= node.accept(_OPTIMIZE_NODE)
    return changed


class _ConstantValue(NodeVisitor, LeafVisitor):
    """The literal a node evaluates to, or None when it is not a constant."""

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_Constant(self, leaf):
        return leaf.value


_CONSTANT_VALUE = _ConstantValue()


def constant_literal(lvar):
    """The literal ``lvar`` always delivers, or None when it is not a constant."""
    if len(lvar.uses) != 1:
        return None
    return lvar.uses[0].accept(_CONSTANT_VALUE)


class _ImmutableLiteral(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Tuple(self, node):
        return all(element.accept(self) for element in node.elements)

    def visit_NoneLiteral(self, node):
        return True

    def visit_BooleanLiteral(self, node):
        return True

    def visit_IntegerLiteral(self, node):
        return True

    def visit_FloatLiteral(self, node):
        return True

    def visit_TextLiteral(self, node):
        return True

    def visit_BytesLiteral(self, node):
        return True


_IMMUTABLE_LITERAL = _ImmutableLiteral()


def _immutable_literal(literal):
    return literal is not None and literal.accept(_IMMUTABLE_LITERAL)


def _constant_fold_tuple(node: Combination) -> bool:
    """Capture immutable evaluated constants, never mutable or dynamic operands."""
    if node.lvar is None:
        return False
    literals = [constant_literal(arg) for arg in node.args]
    if not all(_immutable_literal(literal) for literal in literals):
        return False
    folded = ast.Tuple(literals)
    folded.span = node.source.span if node.source is not None else None
    replace_combination_with_constant(node, folded)
    return True


def _constant_fold(node: Combination) -> bool:
    """Replace a known operation on integer/boolean literals with its value."""
    if node.lvar is None:
        return False
    literals = [constant_literal(arg) for arg in node.args]
    if any(literal is None for literal in literals):
        return False
    domains = [literal.accept(_FOLD_DOMAIN_OF) for literal in literals]
    if any(domain is None for domain in domains):
        return False
    domain = functools.reduce(
        lambda left, right: None if left is None else left.join(right), domains
    )
    if domain is None:
        return False
    values = [literal.value for literal in literals]
    name = linked(node.fun_info, node, "function info").name
    if name in _BINARY_NAMES:
        if domain is _TEXT_DOMAIN:
            return False
        operator = _BINARY_NAMES[name]
        left, right = values
        if operator is ast.BinaryOperator.ADD:
            result = left + right
        elif operator is ast.BinaryOperator.SUBTRACT:
            result = left - right
        elif operator is ast.BinaryOperator.MULTIPLY:
            result = left * right
        elif operator is ast.BinaryOperator.BIT_AND:
            result = left & right
        elif operator is ast.BinaryOperator.BIT_OR:
            result = left | right
        else:
            result = left ^ right
        folded = domain.literal(operator, result)
        if folded is None:
            return False
    elif name in _COMPARE_NAMES:
        operator = _COMPARE_NAMES[name]
        left, right = values
        folded = ast.BooleanLiteral(
            {
                ast.ComparisonOperator.EQUAL: left == right,
                ast.ComparisonOperator.NOT_EQUAL: left != right,
                ast.ComparisonOperator.LESS: left < right,
                ast.ComparisonOperator.LESS_EQUAL: left <= right,
                ast.ComparisonOperator.GREATER: left > right,
                ast.ComparisonOperator.GREATER_EQUAL: left >= right,
            }[operator]
        )
    else:
        return False
    folded.span = node.source.span if node.source is not None else None
    replace_combination_with_constant(node, folded)
    return True


def _constant_fold_unary(node: Combination) -> bool:
    if node.lvar is None:
        return False
    literal = constant_literal(node.args[0])
    if literal is None:
        return False
    operator = _UNARY_NAMES[linked(node.fun_info, node, "function info").name]
    folded = literal.accept(_UnaryLiteralFold(operator))
    if folded is None:
        return False
    folded.span = node.source.span if node.source is not None else None
    replace_combination_with_constant(node, folded)
    return True


class _UnaryLiteralFold(ast.Visitor):
    def __init__(self, operator):
        self.operator = operator

    def visit_Node(self, node):
        return None

    def visit_IntegerLiteral(self, node):
        if self.operator is ast.UnaryOperator.POSITIVE:
            value = +node.value
        elif self.operator is ast.UnaryOperator.NEGATIVE:
            value = -node.value
        else:
            value = ~node.value
        if value.bit_length() > _FOLD_LIMIT_BITS:
            return None
        return ast.IntegerLiteral(value)

    def visit_BooleanLiteral(self, node):
        return self.visit_IntegerLiteral(ast.IntegerLiteral(int(node.value)))

    def visit_FloatLiteral(self, node):
        if self.operator is ast.UnaryOperator.INVERT:
            return None
        value = (
            -node.value if self.operator is ast.UnaryOperator.NEGATIVE else +node.value
        )
        return ast.FloatLiteral(value)

    def visit_ComplexLiteral(self, node):
        if self.operator is ast.UnaryOperator.INVERT:
            return None
        value = (
            -node.value if self.operator is ast.UnaryOperator.NEGATIVE else +node.value
        )
        return ast.ComplexLiteral(value)


class _IntegerDomain:
    """Folding over integers: results stay within the folding bit limit."""

    def join(self, other: _IntegerDomain) -> _IntegerDomain | None:
        return None if other is _TEXT_DOMAIN else self

    def literal(self, operator: ast.BinaryOperator, result: int) -> ast.Literal | None:
        if result.bit_length() > _FOLD_LIMIT_BITS:
            return None
        return ast.IntegerLiteral(result)


class _BooleanDomain(_IntegerDomain):
    """Booleans are integers closed under the bitwise operators."""

    def join(self, other: _IntegerDomain) -> _IntegerDomain | None:
        return None if other is _TEXT_DOMAIN else other

    def literal(self, operator: ast.BinaryOperator, result: int) -> ast.Literal | None:
        if operator in _BITWISE:
            return ast.BooleanLiteral(bool(result))
        return super().literal(operator, result)


class _TextDomain(_IntegerDomain):
    """Text literals fold only under comparison with other text literals."""

    def join(self, other: _IntegerDomain) -> _IntegerDomain | None:
        return self if other is self else None

    def literal(self, operator: ast.BinaryOperator, result) -> ast.Literal | None:
        return None


_INTEGER_DOMAIN = _IntegerDomain()
_BOOLEAN_DOMAIN = _BooleanDomain()
_TEXT_DOMAIN = _TextDomain()
_BITWISE = {
    ast.BinaryOperator.BIT_AND,
    ast.BinaryOperator.BIT_OR,
    ast.BinaryOperator.BIT_XOR,
}


class _FoldDomainOf(ast.Visitor):
    """Only integer and boolean literals fold."""

    def visit_Node(self, node):
        return None

    def visit_IntegerLiteral(self, node):
        return _INTEGER_DOMAIN

    def visit_BooleanLiteral(self, node):
        return _BOOLEAN_DOMAIN

    def visit_TextLiteral(self, node):
        return _TEXT_DOMAIN


_FOLD_DOMAIN_OF = _FoldDomainOf()


class _FlushDeadReference(NodeVisitor, LeafVisitor):
    """Deletes an unused reference whose evaluation has no effect."""

    def visit_Ref(self, node):
        if node.lvar is None and not node.final and node.leaf.accept(self):
            delete_ref(node)
            unlink_node(node)

    def visit_Leaf(self, leaf):
        return False

    def visit_Constant(self, leaf):
        return True

    def visit_KnownFunction(self, leaf):
        return True

    def visit_Lambda(self, leaf):
        return True


_FLUSH_DEAD_REFERENCE = _FlushDeadReference()


def _flush_operand(lvar) -> None:
    if lvar is None:
        return
    uses = list(lvar.uses)
    flush_dest(lvar)
    for use in uses:
        if use.prev is not None:
            use.accept(_FLUSH_DEAD_REFERENCE)


def replace_combination_with_constant(node: Combination, literal) -> None:
    """Substitute a constant reference for a combination and flush its
    operands (``replace-combination-with-constant``)."""
    lvar = linked(node.lvar, node, "lvar")
    leaf = Constant(literal)
    ref = Ref(node.lexenv, node.source, leaf)
    leaf.refs.append(ref)
    node.lvar = None
    lvar.uses.remove(node)
    _flush_operand(node.fun)
    for arg in node.args:
        _flush_operand(arg)
    prev = linked(node.prev, node, "preceding control position")
    block = linked(prev.block, prev, "block")
    following = node.next
    # Splice the reference in where the combination was.
    prev.next = ref
    ref.prev = prev
    if following is not None:
        ref.next = following
        following.use = ref
    else:
        block.last = ref
    node.prev = node.next = None
    ref.lvar = lvar
    lvar.uses.append(ref)
    reoptimize_lvar(lvar)


def flush_combination(node: Combination) -> None:
    """Delete an unused flushable call and any constant references it consumed."""
    _flush_operand(node.fun)
    for arg in node.args:
        _flush_operand(arg)
    unlink_node(node)


def _flush_dead_operands(block) -> None:
    """Delete references whose value is unused and whose evaluation has no
    effect: constants, known-function references, and lambda references."""
    for node in list(block.nodes()):
        if node.prev is None:
            continue
        node.accept(_FLUSH_DEAD_REFERENCE)


class _LiteralTruth(ast.Visitor):
    """A literal's truth value, or None when folding must not decide it."""

    def visit_Node(self, node):
        return None

    def visit_BooleanLiteral(self, node):
        return bool(node.value)

    def visit_IntegerLiteral(self, node):
        return bool(node.value)

    def visit_NoneLiteral(self, node):
        return False

    def visit_TextLiteral(self, node):
        return bool(node.value)

    def visit_BytesLiteral(self, node):
        return bool(node.value)

    def visit_FloatLiteral(self, node):
        return node.value != 0.0


_LITERAL_TRUTH = _LiteralTruth()


def _literal_truth(literal):
    return literal.accept(_LITERAL_TRUTH)


def _optimize_if(node: If) -> bool:
    """Replace a branch on a literal test by a jump to the live arm."""
    literal = constant_literal(node.test)
    if literal is None:
        return False
    truth = _literal_truth(literal)
    if truth is None:
        return False
    block = node.block
    live = node.consequent if truth else node.alternative
    dead = node.alternative if truth else node.consequent
    if dead is not live:
        unlink_blocks(block, dead)
    _flush_operand(node.test)
    prev = linked(node.prev, node, "preceding control position")
    if prev.kind is CtranKind.INSIDE_BLOCK:
        previous = linked(prev.use, prev, "use")
        previous.next = None
        block.last = previous
        node.prev = None
    else:
        filler = Exit(node.lexenv, node.source)
        prev.next = filler
        filler.prev = prev
        block.last = filler
        node.prev = None
    aver(block.succ == [live])
    linked(block.component, block, "component").reanalyze = True
    return True


__all__ = [
    "MAX_OPTIMIZE_ITERATIONS",
    "flush_combination",
    "ir1_optimize",
    "ir1_optimize_until_done",
    "replace_combination_with_constant",
]
