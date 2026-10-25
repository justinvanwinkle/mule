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
    flushable_lambda_var_ref_p,
    is_pure_bound_ref,
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
    FunctionalKind,
    GlobalVar,
    If,
    LambdaVar,
    LeafVisitor,
    Lvar,
    Node,
    NodeVisitor,
    Ref,
    ValuedNode,
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
        derived = node.leaf.accept(self)
        if node.derived_type is not None and (
            derived is UNKNOWN
            or (
                node.derived_type.as_bound().fits(derived.as_bound())
                if derived.may_be_unbound
                else node.derived_type.fits(derived)
            )
        ):
            return node.derived_type
        return derived

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
        derived = UNKNOWN
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.derive_type is not None
        ):
            result = node.fun_info.derive_type(node)
            if result is not None:
                derived = result
        if node.derived_type is not None and (
            derived is UNKNOWN
            or (
                node.derived_type.as_bound().fits(derived.as_bound())
                if derived.may_be_unbound
                else node.derived_type.fits(derived)
            )
        ):
            return node.derived_type
        return derived

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


class _ImportedModuleOrigin(NodeVisitor, LeafVisitor):
    """Resolve the module name an imported module object lvar originates from."""

    def __init__(self):
        self._active_aliases: set[LambdaVar] = set()

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.IMPORT
            and node.info is not None
        ):
            return node.info.module_name
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf.home is not None and leaf in leaf.home.vars:
            return None
        if leaf in self._active_aliases or len(leaf.sets) != 1:
            return None
        val = leaf.sets[0].value
        if val is None or len(val.uses) != 1:
            return None
        self._active_aliases.add(leaf)
        try:
            return val.uses[0].accept(self)
        finally:
            self._active_aliases.remove(leaf)


class _ImportedAttrOrigin(NodeVisitor, LeafVisitor):
    """Resolve ``(module_name, attr_name)`` for a ``from mod import attr`` store."""

    def __init__(self):
        self._active_aliases: set[LambdaVar] = set()

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.GETATTR
            and node.info is not None
            and len(node.args) == 1
            and len(node.args[0].uses) == 1
        ):
            mod_name = node.args[0].uses[0].accept(_ImportedModuleOrigin())
            if mod_name is not None:
                return mod_name, node.info.name
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf.home is not None and leaf in leaf.home.vars:
            return None
        if leaf in self._active_aliases or len(leaf.sets) != 1:
            return None
        val = leaf.sets[0].value
        if val is None or len(val.uses) != 1:
            return None
        self._active_aliases.add(leaf)
        try:
            return val.uses[0].accept(self)
        finally:
            self._active_aliases.remove(leaf)


def is_imported_module_global(leaf: GlobalVar, module_name: str) -> bool:
    """Whether ``leaf`` is bound once at module top level to ``import <module_name>``."""
    if len(leaf.sets) != 1:
        return False
    store = leaf.sets[0]
    if (
        store.home_lambda is None
        or store.home_lambda.kind is not FunctionalKind.TOPLEVEL
        or store.value is None
        or len(store.value.uses) != 1
    ):
        return False
    return store.value.uses[0].accept(_ImportedModuleOrigin()) == module_name


class _PredictableGlobalFunInfo(NodeVisitor, LeafVisitor):
    """Return the ``FunInfo`` of an unmutated builtin or imported global reference."""

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
        if not leaf.sets:
            return KNOWN.get(leaf.name)
        if len(leaf.sets) == 1:
            store = leaf.sets[0]
            if (
                store.home_lambda is not None
                and store.home_lambda.kind is FunctionalKind.TOPLEVEL
                and store.value is not None
                and len(store.value.uses) == 1
            ):
                origin = store.value.uses[0].accept(_ImportedAttrOrigin())
                if origin is not None:
                    return KNOWN.get(f"{origin[0]}.{origin[1]}")
        return None


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
        return node.derived_type


_PREPARE_NODE_OPTIMIZE = _PrepareNodeOptimize()


def derive_node_type(
    node: ValuedNode, ctype: ValueType, *, from_scratch: bool = False
) -> None:
    """Record ``ctype`` on ``node.derived_type`` (monotonically unless
    ``from_scratch``) and trigger ``reoptimize_lvar(node.lvar)`` when changed
    (``derive-node-type``, ``ir1opt.lisp``)."""
    old_type = node.derived_type
    if not from_scratch and old_type is not None and old_type is not UNKNOWN:
        if ctype is UNKNOWN or (
            old_type.as_bound().fits(ctype.as_bound())
            if ctype.may_be_unbound
            else old_type.fits(ctype)
        ):
            new_type = old_type
        else:
            new_type = ctype
    else:
        new_type = ctype
    if new_type != old_type:
        node.derived_type = new_type
        if node.lvar is not None:
            reoptimize_lvar(node.lvar)


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
            reoptimize_lvar(node.lvar)


def _optimize_block(block) -> bool:
    changed = False
    for node in list(block.nodes()):
        if node.prev is None:
            continue
        was_reoptimize = node.reoptimize
        old_type = node.accept(_PREPARE_NODE_OPTIMIZE)
        node.reoptimize = False
        node_changed = node.accept(_OPTIMIZE_NODE)
        if node_changed:
            changed = True
            if (home := node.home_lambda) is not None:
                home.ir1_revision += 1
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


class _ImmutableLiteralKey(ast.Visitor):
    """Canonical key for an immutable AST literal, or None."""

    def visit_Node(self, node):
        return None

    def visit_Tuple(self, node: ast.Tuple):
        keys = [element.accept(self) for element in node.elements]
        if any(key is None for key in keys):
            return None
        return ("tuple", tuple(keys))

    def visit_NoneLiteral(self, node: ast.NoneLiteral):
        return ("none",)

    def visit_BooleanLiteral(self, node: ast.BooleanLiteral):
        return ("bool", node.value)

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral):
        return ("int", node.value)

    def visit_TextLiteral(self, node: ast.TextLiteral):
        return ("str", node.value)

    def visit_BytesLiteral(self, node: ast.BytesLiteral):
        return ("bytes", node.value)


_IMMUTABLE_LITERAL_KEY = _ImmutableLiteralKey()


class _BoundConstantValue(_ConstantValue):
    """The literal a proven-bound local or constant ``Ref`` delivers (``lvar-constant``)."""

    def __init__(self):
        self._seen: set[LambdaVar] = set()
        self._ref: Ref | None = None

    def resolve_lvar(self, lvar: Lvar):
        if len(lvar.uses) != 1 or lvar_type(lvar).may_be_unbound:
            return None
        return lvar.uses[0].accept(self)

    def visit_Cast(self, node: Cast):
        if node.value is None:
            return None
        return self.resolve_lvar(node.value)

    def visit_Ref(self, node: Ref):
        if node.final or node.allow_unbound:
            return None
        prev_ref = self._ref
        self._ref = node
        try:
            return node.leaf.accept(self)
        finally:
            self._ref = prev_ref

    def visit_LambdaVar(self, leaf: LambdaVar):
        ref = self._ref
        if (
            ref is None
            or leaf in self._seen
            or not flushable_lambda_var_ref_p(leaf, ref)
            or (leaf.home is not None and leaf in leaf.home.vars)
            or not leaf.sets
        ):
            return None
        self._seen.add(leaf)
        try:
            first_store = leaf.sets[0]
            if first_store.value is None:
                return None
            first_literal = self.resolve_lvar(first_store.value)
            if first_literal is None:
                return None
            if len(leaf.sets) == 1:
                return first_literal
            first_key = first_literal.accept(_IMMUTABLE_LITERAL_KEY)
            if first_key is None:
                return None
            for store in leaf.sets[1:]:
                if store.value is None:
                    return None
                other = self.resolve_lvar(store.value)
                if other is None or other.accept(_IMMUTABLE_LITERAL_KEY) != first_key:
                    return None
            return first_literal
        finally:
            self._seen.discard(leaf)


_BOUND_CONSTANT_VALUE = _BoundConstantValue()


def bound_constant_literal(lvar):
    """The literal a proven-bound ``lvar`` delivers through a ``Constant`` or
    constant-assigned local ``LambdaVar``, or None (``lvar-constant``)."""
    return _BOUND_CONSTANT_VALUE.resolve_lvar(lvar)


class _SingletonLiteral(ValueTypeVisitor):
    """The AST literal a singleton value type proves, or None."""

    def visit_ValueType(self, fact: ValueType):
        return None

    def visit_IntegerType(self, fact: IntegerType):
        exact = fact.exact_integer()
        return ast.IntegerLiteral(exact) if exact is not None else None

    def visit_BooleanType(self, fact: BooleanType):
        exact = fact.exact_boolean()
        return ast.BooleanLiteral(exact) if exact is not None else None

    def visit_NoneType(self, fact: NoneType):
        return None if fact.may_be_unbound else ast.NoneLiteral()

    def visit_TextType(self, fact: TextType):
        text = fact.exact_text()
        return ast.TextLiteral(text) if text is not None else None


_SINGLETON_LITERAL = _SingletonLiteral()


def constant_lvar_value(lvar: Lvar):
    """The constant literal ``lvar`` delivers, either from ``bound_constant_literal``
    or from a singleton derived type (``lvar-value``)."""
    literal = bound_constant_literal(lvar)
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
    from .ir1tran import find_constant_leaf

    lvar = linked(node.lvar, node, "lvar")
    node.lvar = None
    lvar.uses.remove(node)
    _flush_operand(node.fun)
    for arg in node.args:
        _flush_operand(arg)
    prev = linked(node.prev, node, "preceding control position")
    block = linked(prev.block, prev, "block")
    leaf = (
        find_constant_leaf(block.component.constants, literal)
        if block.component is not None
        else Constant(literal)
    )
    ref = Ref(node.lexenv, node.source, leaf)
    ref.derived_type = constant_type(literal)
    leaf.refs.append(ref)
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
    """Delete nodes whose value is unused and whose evaluation has no
    effect, walking backwards so producers exposed by flushing a call
    are deleted in the same pass (``flush-dead-code``)."""
    for node in reversed(list(block.nodes())):
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
    """Replace a branch on a known-truth test or identical arms by a jump to
    the live arm (``ir1-optimize-if``, ``ir1opt.lisp``)."""
    literal = constant_literal(node.test)
    pure_test = literal is not None or is_pure_bound_ref(node.test)
    if not pure_test:
        return False
    if literal is None:
        literal = constant_lvar_value(node.test)
    if literal is not None:
        truth = _literal_truth(literal)
    else:
        truth = lvar_type(node.test).truth_value()
    if truth is None:
        if node.consequent is node.alternative:
            truth = True
        else:
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
    "bound_constant_literal",
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
