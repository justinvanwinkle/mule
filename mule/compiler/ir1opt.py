"""IR1 optimization, following ``src/compiler/ir1opt.lisp``.

``ir1_optimize`` walks a component's blocks and applies the rewrites that
depend only on the graph and the known-function database: folding a known
foldable call on literal operands, flushing an unused flushable call, and
deleting the dead arm of a branch on a literal test. ``ir1_optimize_until_done``
repeats while rewrites request reoptimization, within the same iteration limit
SBCL uses. Type-dependent rewrites attach here as transforms once the facts
established by ``constraint.py`` and ``locall.py`` are consulted per node.

Python semantics shape the effect attributes in ``fndb.py``: nearly every
operator may run user code, so arithmetic folding is limited to integer and
boolean literals. Immutable tuples of supported constants also fold, while
a reference to a possibly unbound name is never deleted because reading it raises.
"""

from __future__ import annotations

from mule import ast

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
    KnownFunction,
    Lambda,
    Ref,
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
        block.reoptimize = False
        block.flush_p = False
        changed |= _optimize_block(block)
    return changed


def _optimize_block(block) -> bool:
    changed = False
    for node in list(block.nodes()):
        if node.prev is None:
            continue
        if isinstance(node, Combination) and node.kind is CallKind.KNOWN:
            if node.lvar is None and node.fun_info.flushable:
                flush_combination(node)
                changed = True
                continue
            if node.fun_info.name == "%build_tuple" and node.fun_info.foldable:
                changed |= _constant_fold_tuple(node)
                continue
            if (
                node.fun_info.name in _BINARY_NAMES
                or node.fun_info.name in _COMPARE_NAMES
            ):
                changed |= _constant_fold(node)
        elif isinstance(node, If):
            changed |= _optimize_if(node)
    return changed


def _literal(lvar):
    use = lvar.uses[0] if len(lvar.uses) == 1 else None
    if isinstance(use, Ref) and isinstance(use.leaf, Constant):
        return use.leaf.value
    return None


def _immutable_literal(literal):
    if isinstance(literal, ast.Tuple):
        return all(_immutable_literal(element) for element in literal.elements)
    return isinstance(
        literal,
        (
            ast.NoneLiteral,
            ast.BooleanLiteral,
            ast.IntegerLiteral,
            ast.FloatLiteral,
            ast.TextLiteral,
            ast.BytesLiteral,
        ),
    )


def _constant_fold_tuple(node: Combination) -> bool:
    """Capture immutable evaluated constants, never mutable or dynamic operands."""
    if node.lvar is None:
        return False
    literals = [_literal(arg) for arg in node.args]
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
    literals = [_literal(arg) for arg in node.args]
    if any(literal is None for literal in literals):
        return False
    if not all(
        isinstance(literal, (ast.IntegerLiteral, ast.BooleanLiteral))
        for literal in literals
    ):
        return False
    values = [literal.value for literal in literals]
    name = node.fun_info.name
    booleans = all(isinstance(literal, ast.BooleanLiteral) for literal in literals)
    if name in _BINARY_NAMES:
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
        if isinstance(result, bool):
            folded = ast.BooleanLiteral(result)
        elif booleans and operator in (
            ast.BinaryOperator.BIT_AND,
            ast.BinaryOperator.BIT_OR,
            ast.BinaryOperator.BIT_XOR,
        ):
            folded = ast.BooleanLiteral(bool(result))
        else:
            if result.bit_length() > _FOLD_LIMIT_BITS:
                return False
            folded = ast.IntegerLiteral(result)
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


def replace_combination_with_constant(node: Combination, literal) -> None:
    """Substitute a constant reference for a combination and flush its
    operands (``replace-combination-with-constant``)."""
    lvar = node.lvar
    leaf = Constant(literal)
    ref = Ref(node.lexenv, node.source, leaf)
    leaf.refs.append(ref)
    node.lvar = None
    lvar.uses.remove(node)
    flush_dest(node.fun)
    for arg in node.args:
        flush_dest(arg)
    prev = node.prev
    block = prev.block
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
    _flush_dead_operands(block)


def flush_combination(node: Combination) -> None:
    """Delete an unused flushable call and any constant references it consumed."""
    flush_dest(node.fun)
    for arg in node.args:
        flush_dest(arg)
    block = node.block
    unlink_node(node)
    _flush_dead_operands(block)


def _flush_dead_operands(block) -> None:
    """Delete references whose value is unused and whose evaluation has no
    effect: constants, known-function references, and lambda references."""
    for node in list(block.nodes()):
        if node.prev is None:
            continue
        if (
            isinstance(node, Ref)
            and node.lvar is None
            and not node.final
            and isinstance(node.leaf, (Constant, KnownFunction, Lambda))
        ):
            delete_ref(node)
            unlink_node(node)


def _literal_truth(literal):
    if isinstance(literal, (ast.BooleanLiteral, ast.IntegerLiteral)):
        return bool(literal.value)
    if isinstance(literal, ast.NoneLiteral):
        return False
    if isinstance(literal, (ast.TextLiteral, ast.BytesLiteral)):
        return bool(literal.value)
    if isinstance(literal, ast.FloatLiteral):
        return literal.value != 0.0
    return None


def _optimize_if(node: If) -> bool:
    """Replace a branch on a literal test by a jump to the live arm."""
    literal = _literal(node.test)
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
    flush_dest(node.test)
    prev = node.prev
    if prev.kind is CtranKind.INSIDE_BLOCK:
        previous = prev.use
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
    block.component.reanalyze = True
    _flush_dead_operands(block)
    return True


__all__ = [
    "MAX_OPTIMIZE_ITERATIONS",
    "flush_combination",
    "ir1_optimize",
    "ir1_optimize_until_done",
    "replace_combination_with_constant",
]
