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
    BYTES_TYPE,
    FUNCTION_TYPE,
    NONE_TYPE,
    PYTHON_INTEGER,
    REAL,
    TEXT_TYPE,
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
    delete_ref,
    delete_set,
    flush_combination,
    flushable_lambda_var_ref_p,
    is_flushable_ref,
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
    PhysicalFieldInfo,
    Ref,
    Set,
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


class _OkLvarLambdaVar(NodeVisitor, LeafVisitor):
    """Match a proven-bound single-assignment local ``LambdaVar`` reference
    (``ok-lvar-lambda-var``, ``src/compiler/ir1util.lisp``)."""

    def __init__(self, fast: bool):
        self.fast = fast
        self._ref: Ref | None = None

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        if node.final or node.allow_unbound:
            return None
        self._ref = node
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        ref = self._ref
        if (
            ref is None
            or not flushable_lambda_var_ref_p(leaf, ref)
            or leaf.home is None
            or leaf.home.coroutine_body
            or leaf in leaf.home.vars
            or len(leaf.sets) != 1
            or leaf.sets[0].value is None
            or (not self.fast and len(leaf.refs) != 1)
        ):
            return None
        return leaf


def ok_lvar_lambda_var(lvar: Lvar, *, fast: bool = False) -> LambdaVar | None:
    """Return the local ``LambdaVar`` referenced by ``lvar`` when it has a
    single binding assignment and (unless ``fast``) a single reference."""
    if len(lvar.uses) != 1 or lvar_type(lvar).may_be_unbound:
        return None
    return lvar.uses[0].accept(_OkLvarLambdaVar(fast))


class _InterveningSetVarAllowed(LeafVisitor):
    def __init__(self, forbidden_vars: frozenset[LambdaVar] = frozenset()):
        self.forbidden_vars = forbidden_vars

    def visit_Leaf(self, leaf) -> bool:
        return False

    def visit_LambdaVar(self, leaf: LambdaVar) -> bool:
        if (
            leaf in self.forbidden_vars
            or leaf.indirect
            or any(r.home_lambda is not leaf.home for r in leaf.refs)
        ):
            return False
        return leaf.home is not None and not leaf.home.coroutine_body


_INTERVENING_SET_VAR_ALLOWED = _InterveningSetVarAllowed()


def _is_effectless_known_call(step: Combination) -> bool:
    if step.kind is not CallKind.KNOWN or step.fun_info is None:
        return False
    info = step.fun_info
    if info.flushable and not info.unsafe:
        return True
    name = info.name
    if name == "%make_slice":
        return True
    if name == "%getslice" and len(step.args) == 4:
        seq_t = lvar_type(step.args[0])
        start_t = lvar_type(step.args[1])
        stop_t = lvar_type(step.args[2])
        step_t = lvar_type(step.args[3])
        return (
            seq_t.fits_text(TEXT_TYPE)
            and not seq_t.may_be_unbound
            and (start_t.fits(PYTHON_INTEGER) or start_t.fits_none(NONE_TYPE))
            and not start_t.may_be_unbound
            and (stop_t.fits(PYTHON_INTEGER) or stop_t.fits_none(NONE_TYPE))
            and not stop_t.may_be_unbound
            and (step_t.fits_none(NONE_TYPE) or step_t.exact_integer() == 1)
            and not step_t.may_be_unbound
        )
    if name in ("%add", "%subtract", "%multiply"):
        return len(step.args) == 2 and all(
            lvar_type(arg).fits(REAL) for arg in step.args
        )
    if name in ("%bit_and", "%bit_or", "%bit_xor"):
        return len(step.args) == 2 and all(
            lvar_type(arg).fits(PYTHON_INTEGER) for arg in step.args
        )
    if name in ("%unary_positive", "%unary_negative"):
        return len(step.args) == 1 and lvar_type(step.args[0]).fits(REAL)
    if name == "%unary_invert":
        return len(step.args) == 1 and lvar_type(step.args[0]).fits(PYTHON_INTEGER)
    if name in ("%compare_is", "%compare_is_not"):
        return True
    if name in ("%compare_equal", "%compare_not_equal"):
        return len(step.args) == 2 and all(
            lvar_type(arg).fits(REAL)
            or lvar_type(arg).fits(TEXT_TYPE)
            or lvar_type(arg).fits(BYTES_TYPE)
            or lvar_type(arg).fits(NONE_TYPE)
            for arg in step.args
        )
    if name in (
        "%compare_less",
        "%compare_less_equal",
        "%compare_greater",
        "%compare_greater_equal",
    ):
        if len(step.args) != 2:
            return False
        t0, t1 = lvar_type(step.args[0]), lvar_type(step.args[1])
        return (
            (t0.fits(REAL) and t1.fits(REAL))
            or (t0.fits(TEXT_TYPE) and t1.fits(TEXT_TYPE))
            or (t0.fits(BYTES_TYPE) and t1.fits(BYTES_TYPE))
        )
    return False


class _InterveningStepAllowed(NodeVisitor):
    def __init__(self, flushable: bool, effectless_calls: bool):
        self.flushable = flushable
        self.effectless_calls = effectless_calls

    def visit_Node(self, node) -> bool:
        return False

    def visit_Ref(self, node: Ref) -> bool:
        return is_flushable_ref(node)

    def visit_Cast(self, node: Cast) -> bool:
        return node.value is not None and (
            not node.type_check
            or not node.safety
            or lvar_type(node.value).fits(node.contract.checked_type)
        )

    def visit_Combination(self, node: Combination) -> bool:
        return self.effectless_calls and _is_effectless_known_call(node)

    def visit_Set(self, node: Set) -> bool:
        if self.flushable:
            return False
        return node.var.accept(_INTERVENING_SET_VAR_ALLOWED)


def almost_immediately_used_p(
    lvar: Lvar,
    node: Node | None = None,
    *,
    flushable: bool = False,
    effectless_calls: bool = True,
) -> bool:
    """Whether ``node`` (defaulting to ``lvar``'s single use) reaches
    ``lvar.dest`` in the same block with only non-interfering intervening nodes
    (``almost-immediately-used-p``, ``src/compiler/ir1util.lisp``)."""
    if node is None:
        if len(lvar.uses) != 1:
            return False
        node = lvar.uses[0]
    dest = lvar.dest
    if (
        dest is None
        or node.prev is None
        or dest.prev is None
        or node.block is not dest.block
    ):
        return False
    allowed = _InterveningStepAllowed(flushable, effectless_calls)
    ctran = node.next
    while ctran is not dest.prev:
        if ctran is None or ctran.next is None:
            return False
        step = ctran.next
        if not step.accept(allowed):
            return False
        ctran = step.next
    return True


class _MatchKnownCombination(NodeVisitor):
    def __init__(self, names: frozenset[str], arg_count: int | None):
        self.names = names
        self.arg_count = arg_count

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name in self.names
            and (self.arg_count is None or len(node.args) == self.arg_count)
        ):
            return node
        return None


def lvar_matches(
    lvar: Lvar,
    fun_names: tuple[str, ...] | frozenset[str],
    *,
    arg_count: int | None = None,
    local_alias: bool = True,
) -> Combination | None:
    """Match a known ``Combination`` producing ``lvar`` directly or through a
    single-assignment local ``LambdaVar`` (``lvar-matches``,
    ``src/compiler/combination-match.lisp``)."""
    if len(lvar.uses) != 1:
        return None
    names = frozenset(fun_names)
    matcher = _MatchKnownCombination(names, arg_count)
    direct = lvar.uses[0].accept(matcher)
    if direct is not None:
        return direct
    if not local_alias:
        return None
    var = ok_lvar_lambda_var(lvar, fast=True)
    if var is None:
        return None
    val_lvar = var.sets[0].value
    if val_lvar is None or len(val_lvar.uses) != 1:
        return None
    return val_lvar.uses[0].accept(matcher)


def splice_fun_args(
    lvar: Lvar,
    fun_names: tuple[str, ...] | frozenset[str],
    arg_count: int | None = None,
    *,
    splice: bool = True,
    flushable: bool = False,
    effectless_calls: bool = True,
    allow_local: bool = True,
) -> Combination | None:
    """Match and optionally splice a known ``Combination`` producing ``lvar``
    (directly or through a single-use local ``LambdaVar``) into ``lvar.dest``
    (``splice-fun-args``, ``src/compiler/ir1util.lisp``)."""
    if len(lvar.uses) != 1 or lvar.dest is None:
        return None
    names = frozenset(fun_names)
    matcher = _MatchKnownCombination(names, arg_count)
    use = lvar.uses[0]
    producer = use.accept(matcher)
    var: LambdaVar | None = None
    if producer is not None:
        if not almost_immediately_used_p(
            lvar,
            producer,
            flushable=flushable,
            effectless_calls=effectless_calls,
        ):
            return None
    elif allow_local:
        var = ok_lvar_lambda_var(lvar, fast=False)
        if var is None:
            return None
        set_node = var.sets[0]
        val_lvar = set_node.value
        if val_lvar is None or len(val_lvar.uses) != 1:
            return None
        producer = val_lvar.uses[0].accept(matcher)
        if producer is None:
            return None
        if not almost_immediately_used_p(
            val_lvar,
            producer,
            flushable=flushable,
            effectless_calls=effectless_calls,
        ) or not almost_immediately_used_p(
            lvar,
            set_node,
            flushable=flushable,
            effectless_calls=effectless_calls,
        ):
            return None
    else:
        return None
    if not splice:
        return producer
    dest = lvar.dest
    idx = dest.args.index(lvar)
    spliced_args = list(producer.args)
    for arg in spliced_args:
        arg.dest = dest
    dest.args[idx : idx + 1] = spliced_args
    producer.args = []
    if var is not None:
        set_node = var.sets[0]
        val_lvar = linked(set_node.value, set_node, "set value")
        producer.lvar = None
        val_lvar.uses.clear()
        val_lvar.dest = None
        set_node.value = None
        delete_set(set_node)
        unlink_node(set_node)
        use.lvar = None
        lvar.uses.clear()
        lvar.dest = None
        delete_ref(use)
        unlink_node(use)
        if (
            not var.refs
            and not var.sets
            and var.home is not None
            and var in var.home.locals
        ):
            var.home.locals.remove(var)
    else:
        producer.lvar = None
        lvar.uses.clear()
        lvar.dest = None
    _flush_operand(producer.fun)
    unlink_node(producer)
    dest.reoptimize = True
    if dest.lvar is not None:
        reoptimize_lvar(dest.lvar)
    if dest.prev is not None and dest.block is not None:
        dest.block.reoptimize = True
        dest.block.flush_p = True
        if dest.block.component is not None:
            dest.block.component.reoptimize = True
    return producer


@defoptimizer("%getitem")
def _optimize_getslice(node: Combination) -> bool:
    if len(node.args) != 2:
        return False
    if splice_fun_args(node.args[1], ("%make_slice",), 3) is None:
        return False
    retarget_known_combination(node, fndb.GETSLICE, list(node.args))
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


def _is_explicit_constant_lvar(lvar: Lvar) -> bool:
    if not is_pure_bound_ref(lvar):
        return False
    if constant_literal(lvar) is not None:
        return True
    var = ok_lvar_lambda_var(lvar, fast=True)
    if var is None:
        return False
    store = var.sets[0]
    return (
        store.value is not None
        and is_pure_bound_ref(store.value)
        and constant_literal(store.value) is not None
    )


def _canonicalize_commutative_constant(node: Combination, info) -> bool:
    """Canonicalize ``const op x -> x op const`` on commutative operations when
    ``const`` is a pure bound constant and ``x`` is not (``associate-arguments``,
    ``src/compiler/srctran.lisp``)."""
    if len(node.args) != 2:
        return False
    left, right = node.args
    if not _is_explicit_constant_lvar(left) or _is_explicit_constant_lvar(right):
        return False
    name = info.name
    t0, t1 = lvar_type(left), lvar_type(right)
    if name in ("%add", "%multiply", "%bit_and", "%bit_or", "%bit_xor"):
        if not (t0.fits(PYTHON_INTEGER) and t1.fits(PYTHON_INTEGER)):
            return False
    elif name in ("%compare_equal", "%compare_not_equal"):
        if not all(
            t.fits(REAL) or t.fits(TEXT_TYPE) or t.fits(BYTES_TYPE) or t.fits(NONE_TYPE)
            for t in (t0, t1)
        ):
            return False
    else:
        return False
    node.args = [right, left]
    return True


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
            if not self._run_transforms(node, info):
                return False
            if node.prev is not None and node.kind is CallKind.KNOWN:
                self.visit_Combination(node)
            return True
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
        canonicalized = _canonicalize_commutative_constant(node, info)
        if info.optimizer is not None and info.optimizer(node):
            return True
        if self._run_transforms(node, info):
            return True
        if info.flushable and _fold_call_derived_to_constant(node):
            return True
        return canonicalized

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


def bound_constant_literal(lvar):
    """The literal a proven-bound ``lvar`` delivers through a ``Constant`` or
    constant-assigned local ``LambdaVar``, or None (``lvar-constant``)."""
    return _BoundConstantValue().resolve_lvar(lvar)


class _TextLiteralValue(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_TextLiteral(self, node):
        return node.value


_TEXT_LITERAL_VALUE = _TextLiteralValue()


def bound_constant_text(lvar):
    """The text of a proven-bound constant, independent of representation facts."""
    literal = bound_constant_literal(lvar)
    return None if literal is None else literal.accept(_TEXT_LITERAL_VALUE)


def transform_physical_field_ref(node: Combination) -> bool:
    if len(node.args) != 2:
        return False
    name = bound_constant_text(node.args[1])
    if name is None:
        return False
    retarget_known_combination(node, fndb.PHYSICAL_FIELD_REF, [node.args[0]])
    node.info = PhysicalFieldInfo(name)
    return True


def transform_physical_field_set(node: Combination) -> bool:
    if len(node.args) != 3:
        return False
    name = bound_constant_text(node.args[1])
    if name is None:
        return False
    retarget_known_combination(
        node, fndb.PHYSICAL_FIELD_SET, [node.args[0], node.args[2]]
    )
    node.info = PhysicalFieldInfo(name)
    return True


class _SingletonLiteral(ValueTypeVisitor):
    """The AST literal a singleton value type proves, or None."""

    def visit_ValueType(self, fact: ValueType):
        return None

    def visit_IntegerType(self, fact: IntegerType):
        exact = fact.exact_integer()
        if exact is None or exact.bit_length() > _FOLD_LIMIT_BITS:
            return None
        return ast.IntegerLiteral(exact)

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


def _constant_operand_literal(lvar: Lvar):
    literal = constant_literal(lvar)
    if literal is not None:
        return literal
    if is_pure_bound_ref(lvar):
        return constant_lvar_value(lvar)
    return None


def _fold_call_derived_to_constant(node: Combination) -> bool:
    """Fold a flushable known call whose derived type is a singleton constant
    (``fold-call-derived-to-constant``, ``ir1opt.lisp:2450``)."""
    if node.lvar is None or not all(is_pure_bound_ref(arg) for arg in node.args):
        return False
    derived = node.accept(_DERIVE_NODE_TYPE)
    literal = derived.accept(_SINGLETON_LITERAL)
    if literal is None:
        return False
    literal.span = node.source.span if node.source is not None else None
    replace_combination_with_constant(node, literal)
    return True


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
    literals = [_constant_operand_literal(arg) for arg in node.args]
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
    literal = _constant_operand_literal(node.args[0])
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
    "almost_immediately_used_p",
    "bound_constant_literal",
    "constant_literal",
    "constant_lvar_value",
    "flush_combination",
    "ir1_optimize",
    "ir1_optimize_until_done",
    "lvar_matches",
    "lvar_type",
    "node_derived_type",
    "ok_lvar_lambda_var",
    "replace_combination_with_constant",
    "replace_combination_with_lvar",
    "retarget_known_combination",
    "splice_fun_args",
]
