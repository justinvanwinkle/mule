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
from .constraint import constant_type
from .ctype import (
    BOTTOM,
    FUNCTION_TYPE,
    UNKNOWN,
    BooleanType,
    IntegerType,
    NoneType,
    TextType,
    ValueType,
    ValueTypeVisitor,
)
from .ir1util import (
    _FLUSH_DEAD_REFERENCE,
    _flush_operand,
    aver,
    flush_combination,
    reoptimize_lvar,
    replace_combination_with_lvar,
    retarget_known_combination,
    unlink_blocks,
    unlink_node,
)
from .knownfun import KNOWN, defoptimizer
from .node import (
    CallKind,
    Cast,
    Combination,
    Component,
    Constant,
    CtranKind,
    Exit,
    GlobalVar,
    If,
    LeafVisitor,
    Lvar,
    Node,
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


class _DeriveNodeType(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return UNKNOWN

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return leaf.type if leaf.type is not None else UNKNOWN

    def visit_Constant(self, leaf: Constant):
        return constant_type(leaf.value)

    def visit_Lambda(self, leaf):
        return FUNCTION_TYPE

    def visit_LambdaVar(self, leaf):
        if leaf.type is not None:
            return leaf.type
        if leaf.optimistic_type is not None and leaf.optimistic_type is not BOTTOM:
            return leaf.optimistic_type
        return UNKNOWN

    def visit_Cast(self, node: Cast):
        if node.value is None:
            return UNKNOWN
        return node.contract.refine(lvar_type(node.value))

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.derive_type is not None
        ):
            derived = node.fun_info.derive_type(node)
            if derived is not None:
                return derived
        return fndb.derive_builtin_call_type(node)

    def visit_Exit(self, node: Exit):
        return lvar_type(node.value) if node.value is not None else UNKNOWN


_DERIVE_NODE_TYPE = _DeriveNodeType()


class _NodeDerivedType(NodeVisitor):
    def visit_Node(self, node):
        return UNKNOWN

    def visit_ValuedNode(self, node):
        if node.derived_type is None:
            node.derived_type = node.accept(_DERIVE_NODE_TYPE)
        return node.derived_type


_NODE_DERIVED_TYPE = _NodeDerivedType()


def node_derived_type(node: Node) -> ValueType:
    """The derived value type of ``node``, computed and cached on demand."""
    return node.accept(_NODE_DERIVED_TYPE)


def lvar_type(lvar: Lvar) -> ValueType:
    """The union of the derived types of ``lvar``'s producing nodes."""
    if lvar.derived_type is not None:
        return lvar.derived_type
    if not lvar.uses:
        return BOTTOM
    derived = node_derived_type(lvar.uses[0])
    for use in lvar.uses[1:]:
        derived = derived.join(node_derived_type(use))
    lvar.derived_type = derived
    return derived


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


@defoptimizer("%getitem")
def _optimize_getslice(node: Combination) -> bool:
    if len(node.args) != 2 or len(node.fun.uses) != 1:
        return False
    fun_ref = node.fun.uses[0]
    slice_lvar = node.args[1]
    if len(slice_lvar.uses) != 1:
        return False
    slice_node = slice_lvar.uses[0].accept(_MAKE_SLICE_PRODUCER)
    if (
        slice_node is None
        or slice_node.prev is None
        or fun_ref.prev is None
        or fun_ref.next is not node.prev
        or slice_node.next is not fun_ref.prev
    ):
        return False
    start_lvar, stop_lvar, step_lvar = slice_node.args
    slice_node.args = []
    retarget_known_combination(
        node, fndb.GETSLICE, [node.args[0], start_lvar, stop_lvar, step_lvar]
    )
    _flush_operand(slice_node.fun)
    unlink_node(slice_node)
    return True


_PROTOCOL_PREFIXES = (
    "mule_sequences_",
    "mule_format_",
    "mule_arguments_",
    "mule_native_class_",
    "mule_print_",
)


class _PredictableGlobalFunInfo(NodeVisitor, LeafVisitor):
    """Return the ``FunInfo`` of an unmutated builtin global reference, or None."""

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.lexenv.policy.safety >= 3:
            return None
        home = node.home_lambda
        if home is not None and (
            home.policy.safety >= 3 or home.name.startswith(_PROTOCOL_PREFIXES)
        ):
            return None
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_GlobalVar(self, leaf: GlobalVar):
        if leaf.sets:
            return None
        return KNOWN.get(leaf.name)


_PREDICTABLE_GLOBAL_FUN_INFO = _PredictableGlobalFunInfo()


class _OptimizeNode(NodeVisitor):
    """Applies one node's rewrite; returns whether the graph changed."""

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN:
            if (
                node.kind not in (CallKind.FULL, CallKind.GUARDED)
                or len(node.fun.uses) != 1
                or (node.info is not None and node.info.keywords)
                or node.lexenv.policy.safety >= 3
            ):
                return False
            home = node.home_lambda
            if home is not None and (
                home.policy.safety >= 3 or home.name.startswith(_PROTOCOL_PREFIXES)
            ):
                return False
            info = node.fun.uses[0].accept(_PREDICTABLE_GLOBAL_FUN_INFO)
            if info is None:
                return False
            return self._run_transforms(node, info)
        info = linked(node.fun_info, node, "function info")
        if node.lvar is None and info.flushable:
            flush_combination(node)
            return True
        if info.name == "%build_tuple" and info.foldable and _constant_fold_tuple(node):
            return True
        if info.name in _UNARY_NAMES and _constant_fold_unary(node):
            return True
        if (
            info.name in _BINARY_NAMES or info.name in _COMPARE_NAMES
        ) and _constant_fold(node):
            return True
        if info.optimizer is not None and info.optimizer(node):
            return True
        return self._run_transforms(node, info)

    @staticmethod
    def _run_transforms(node: Combination, info) -> bool:
        for transform in info.transforms:
            if transform.policy is not None and not transform.policy(node):
                continue
            if transform.arg_types is not None:
                if len(node.args) != len(transform.arg_types):
                    continue
                if not all(
                    lvar_type(arg).fits(expected)
                    for arg, expected in zip(
                        node.args, transform.arg_types, strict=True
                    )
                ):
                    continue
            if transform.fun(node):
                return True
        return False

    def visit_If(self, node):
        return _optimize_if(node)


_OPTIMIZE_NODE = _OptimizeNode()


class _PrepareNodeOptimize(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_ValuedNode(self, node):
        old_type = node.derived_type
        if node.reoptimize:
            node.derived_type = None
        return old_type


_PREPARE_NODE_OPTIMIZE = _PrepareNodeOptimize()


class _UpdateDerivedType(NodeVisitor):
    def __init__(self, old_type: ValueType | None, force: bool):
        self.old_type = old_type
        self.force = force

    def visit_Node(self, node):
        return None

    def visit_ValuedNode(self, node):
        if not self.force and node.derived_type is not None:
            new_type = node.derived_type
        else:
            new_type = node.accept(_DERIVE_NODE_TYPE)
            node.derived_type = new_type
        if new_type != self.old_type and node.lvar is not None:
            node.lvar.derived_type = None
            reoptimize_lvar(node.lvar)


def _optimize_block(block) -> bool:
    changed = False
    for node in list(block.nodes()):
        if node.prev is None:
            continue
        was_reoptimize = node.reoptimize
        old_type = node.accept(_PREPARE_NODE_OPTIMIZE)
        node.reoptimize = False
        changed |= node.accept(_OPTIMIZE_NODE)
        if node.prev is None:
            continue
        if was_reoptimize or node.reoptimize or old_type is None:
            node.reoptimize = False
            node.accept(_UpdateDerivedType(old_type, True))
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


class _SingletonLiteral(ValueTypeVisitor):
    """The AST literal a singleton value type proves, or None."""

    def visit_ValueType(self, fact: ValueType):
        return None

    def visit_IntegerType(self, fact: IntegerType):
        exact = fact.exact_integer()
        return ast.IntegerLiteral(exact) if exact is not None else None

    def visit_BooleanType(self, fact: BooleanType):
        if fact.may_be_unbound:
            return None
        if fact.minimum == 1 and fact.maximum == 1:
            return ast.BooleanLiteral(True)
        if fact.minimum == 0 and fact.maximum == 0:
            return ast.BooleanLiteral(False)
        return None

    def visit_NoneType(self, fact: NoneType):
        return None if fact.may_be_unbound else ast.NoneLiteral()

    def visit_TextType(self, fact: TextType):
        text = fact.interned_text()
        if text is not None:
            return ast.TextLiteral(text)
        if not fact.may_be_unbound and fact.literal is not None:
            return ast.TextLiteral(fact.literal)
        return None


_SINGLETON_LITERAL = _SingletonLiteral()


def constant_lvar_value(lvar: Lvar):
    """The constant literal ``lvar`` delivers, either from a single ``Constant``
    reference or from a singleton derived type."""
    literal = constant_literal(lvar)
    if literal is not None:
        return literal
    return lvar_type(lvar).accept(_SINGLETON_LITERAL)


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


def replace_combination_with_constant(node: Combination, literal) -> None:
    """Substitute a constant reference for a combination and flush its
    operands (``replace-combination-with-constant``)."""
    lvar = linked(node.lvar, node, "lvar")
    leaf = Constant(literal)
    ref = Ref(node.lexenv, node.source, leaf)
    ref.derived_type = constant_type(literal)
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
    "constant_literal",
    "constant_lvar_value",
    "flush_combination",
    "ir1_optimize",
    "ir1_optimize_until_done",
    "lvar_type",
    "node_derived_type",
    "replace_combination_with_constant",
    "replace_combination_with_lvar",
    "retarget_known_combination",
]
