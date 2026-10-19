"""Local-call analysis, following ``src/compiler/locall.lisp``.

SBCL converts a call whose function is a known lambda into a local call and
splits optional-argument entry points. Python module globals are mutable until
the module has executed, so Mule converts a call through a global name into a
**guarded** call: the predicted lambda is recorded on the combination, code
generation checks the callable's identity at the call site, and the general
path remains. A call whose callee identity is proven by flow analysis (the
binding was stored in the same function and nothing since could run user code)
additionally receives the callee's published return type.

Entry contexts replace SBCL's optional-dispatch entry points for now: each
lambda keeps its general entry (unknown parameters) and may gain a ``fixnum``
entry (every parameter a tagged integer) and an ``observed`` entry (the join of
integer intervals actual call sites supplied). Return types per entry start
empty and grow by union, exactly as ``accumulate-optimistic-arg-types`` /
``publish-optimistic-types`` do; nothing is published before the fixpoint.
Code generation emits one body per entry with that entry's facts; copying the
body (SBCL's inline expansion) becomes necessary once transforms depend on an
entry's facts.
"""

from __future__ import annotations

from collections import defaultdict
from contextlib import contextmanager
from dataclasses import dataclass

from mule import ast

from . import ir1opt, loop
from .annotation_types import AnnotationLedger
from .constraint import (
    _BINARY_OPERATORS,
    _COMPARISON_OPERATORS,
    _UNARY_OPERATORS,
    NO_FACT,
    Body,
    Fact,
    LambdaGraph,
    build_lambda_graph,
    constant_type,
    join_fact,
    propagate,
    widen_fact,
)
from .ctype import (
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    FIXNUM_MAX,
    FIXNUM_MIN,
    INTEGER,
    LIST_TYPE,
    NONE_TYPE,
    TEXT_TYPE,
    UNBOUND,
    UNKNOWN,
    IntegerType,
    TupleType,
    ValueType,
    ValueTypeVisitor,
)
from .dfo import find_dfo
from .fndb import LEN, flow_propagation
from .ir1util import clone_lambda_component, join_blocks_if_possible, made_function
from .knownfun import KNOWN
from .node import (
    CallKind,
    Combination,
    FunctionalKind,
    GlobalVar,
    If,
    Lambda,
    LambdaVar,
    LeafVisitor,
    NodeVisitor,
    Ref,
    Set,
    ValuedNode,
    linked,
)
from .seqtran import is_predictable_len_call, known_length_primitive

_PROTOCOL_PREFIXES = (
    "mule_sequences_",
    "mule_format_",
    "mule_arguments_",
    "mule_native_class_",
    "mule_print_",
)

_NUMERIC_KNOWN = (
    frozenset(_BINARY_OPERATORS)
    | (frozenset(_UNARY_OPERATORS) - {"%unary_not"})
    | (
        frozenset(_COMPARISON_OPERATORS)
        - {"%compare_is", "%compare_is_not", "%compare_in", "%compare_not_in"}
    )
    | {"%range_prepare", "%range_advance", "%make_slice", "%getslice"}
)
_NON_INTEGER_FIRST_OPERAND = frozenset(
    {
        "%getattr",
        "%setattr",
        "%delattr",
        "%resolve_method",
        "%getitem",
        "%getslice",
        "%setitem",
        "%delitem",
        "%iterate",
        "%for_next",
        "%unpack",
        "_text_length",
        "_text_codepoint",
        "_text_character",
        "_text_slice",
        "_text_join",
        "_text_from_codepoints",
        "_storage_length",
        "_storage_read",
        "_storage_write",
        "_storage_append",
        "_storage_insert",
        "_storage_delete",
        "_storage_tuple",
    }
)
_INDEX_SECOND_OPERAND = frozenset({"%getitem", "%setitem", "%delitem"})
_TEXT_METHODS = frozenset(
    {
        "find",
        "rfind",
        "split",
        "rsplit",
        "splitlines",
        "strip",
        "lstrip",
        "rstrip",
        "startswith",
        "endswith",
        "replace",
        "lower",
        "upper",
        "join",
        "partition",
        "rpartition",
        "encode",
        "isalpha",
        "isdigit",
        "isdecimal",
        "isnumeric",
        "isalnum",
        "isspace",
        "islower",
        "isupper",
        "istitle",
        "isidentifier",
        "isprintable",
        "isascii",
        "casefold",
        "capitalize",
        "title",
        "swapcase",
        "center",
        "ljust",
        "rjust",
        "zfill",
        "expandtabs",
        "removeprefix",
        "removesuffix",
        "format",
        "format_map",
    }
)


_LIST_METHODS = frozenset(
    {
        "append",
        "extend",
        "insert",
        "pop",
        "remove",
        "clear",
        "reverse",
        "sort",
    }
)


class _ParameterUse(NodeVisitor, LeafVisitor):
    """Classifies how one parameter (and local copies of it) is used."""

    def __init__(self, parameter):
        self.copies = {parameter}
        self.queue = [parameter]
        self.numeric = False
        self.disqualified = False
        self.text = False
        self.text_disqualified = False
        self.list_seq = False
        self.list_disqualified = False
        self.sequence_disqualified = False
        self.has_len = False
        self.has_getitem = False
        self.current = None

    def analyze(self, contract, full: ValueType) -> ValueType:
        while self.queue and not (
            self.disqualified and self.text_disqualified and self.list_disqualified
        ):
            var = self.queue.pop()
            for ref in var.refs:
                if ref.dest is not None and ref.lvar is not None:
                    self.current = ref.lvar
                    ref.dest.accept(self)
        if (
            self.numeric
            and not self.disqualified
            and (contract is None or not contract.contradicts(full))
        ):
            return full
        if (
            self.text
            and not self.text_disqualified
            and not self.numeric
            and (contract is None or not contract.contradicts(TEXT_TYPE))
        ):
            return TEXT_TYPE
        if (
            (self.list_seq or (self.has_len and self.has_getitem))
            and not self.list_disqualified
            and not self.numeric
            and not self.text
            and (contract is None or not contract.contradicts(LIST_TYPE))
        ):
            return LIST_TYPE
        return UNKNOWN

    def general_type(self, declared) -> ValueType:
        if declared is None:
            return UNKNOWN
        candidate = declared.general_parameter_type()
        if candidate is UNKNOWN:
            return UNKNOWN
        while self.queue and not (self.numeric or self.sequence_disqualified):
            var = self.queue.pop()
            for ref in var.refs:
                if ref.dest is not None and ref.lvar is not None:
                    self.current = ref.lvar
                    ref.dest.accept(self)
        if self.numeric or self.sequence_disqualified:
            return UNKNOWN
        return candidate

    def visit_Set(self, node):
        if node.value is self.current:
            node.var.accept(self)

    def visit_LambdaVar(self, leaf):
        if leaf not in self.copies:
            self.copies.add(leaf)
            self.queue.append(leaf)

    def visit_Combination(self, node):
        lvar = self.current
        if node.fun is lvar:
            self.disqualified = True
            self.text_disqualified = True
            self.list_disqualified = True
            self.sequence_disqualified = True
            return
        info = node.fun_info
        if info is None:
            if is_predictable_len_call(node) and node.args and node.args[0] is lvar:
                self.disqualified = True
                self.has_len = True
            return
        name = info.name
        if name == "%resolve_method" and node.args and node.args[0] is lvar:
            self.disqualified = True
            method_name = linked(node.info, node, "info").name
            if method_name in _TEXT_METHODS:
                self.text = True
            else:
                self.text_disqualified = True
            if method_name in _LIST_METHODS:
                self.list_seq = True
            elif method_name not in {"copy", "index", "count"}:
                self.list_disqualified = True
            return
        if node.args and node.args[0] is lvar and name in {"%setitem", "%delitem"}:
            self.disqualified = True
            self.text_disqualified = True
            self.list_seq = True
            return
        if (
            node.args
            and node.args[0] is lvar
            and name in {"%getattr", "%setattr", "%delattr"}
        ):
            self.disqualified = True
            self.list_disqualified = True
            return
        if node.args and node.args[0] is lvar and name in _NON_INTEGER_FIRST_OPERAND:
            self.disqualified = True
            if name in {"%getitem", "%getslice"}:
                self.has_getitem = True
            elif name == "_storage_length":
                self.has_len = True
            return
        if name == "%resolve_super_method" and lvar in node.args:
            self.disqualified = True
            self.text_disqualified = True
            self.list_disqualified = True
            self.sequence_disqualified = True
            return
        if (
            len(node.args) > 1
            and node.args[1] is lvar
            and name in {"%compare_in", "%compare_not_in"}
        ):
            self.disqualified = True
            return
        if (
            len(node.args) > 1
            and node.args[1] is lvar
            and name in _INDEX_SECOND_OPERAND
        ):
            self.numeric = True
            self.text_disqualified = True
            self.list_disqualified = True
            return
        if node.args and node.args[0] is lvar and name in {"%getitem", "%getslice"}:
            self.has_getitem = True
            return
        if name in _NUMERIC_KNOWN:
            self.numeric = True


class _PredictableGlobalFunInfo(NodeVisitor, LeafVisitor):
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


class _HasCombinationImprovement(NodeVisitor):
    """Whether ``candidate`` narrowed any combination's type or transform
    operands over ``general``."""

    def __init__(
        self, general: dict[object, ValueType], candidate: dict[object, ValueType]
    ):
        self.general = general
        self.candidate = candidate
        self.improved = False

    def visit_Combination(self, node):
        cand_type = self.candidate.get(node, UNKNOWN)
        gen_type = self.general.get(node, UNKNOWN)
        if cand_type != gen_type and not gen_type.fits(cand_type):
            self.improved = True
            return
        if is_predictable_len_call(node) and len(node.args[0].uses) == 1:
            arg_use = node.args[0].uses[0]
            if known_length_primitive(
                self.candidate.get(arg_use, UNKNOWN)
            ) is not known_length_primitive(self.general.get(arg_use, UNKNOWN)):
                self.improved = True
                return
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name
            in {
                "%getitem",
                "%getslice",
                "%setitem",
                "%delitem",
                "%resolve_method",
                "%iterate",
            }
            and node.args
            and len(node.args[0].uses) == 1
        ):
            arg_use = node.args[0].uses[0]
            cand_recv = self.candidate.get(arg_use, UNKNOWN)
            gen_recv = self.general.get(arg_use, UNKNOWN)
            if cand_recv != gen_recv and (
                cand_recv.is_exact_list or cand_recv.fits_text(TEXT_TYPE)
            ):
                self.improved = True
                return
        if node.kind is CallKind.KNOWN and node.fun_info is not None:
            info = node.fun_info
        elif (
            node.kind in (CallKind.FULL, CallKind.GUARDED)
            and len(node.fun.uses) == 1
            and not (node.info is not None and node.info.keywords)
            and node.lexenv.policy.safety < 3
        ):
            info = node.fun.uses[0].accept(_PREDICTABLE_GLOBAL_FUN_INFO)
        else:
            info = None
        if info is not None and (
            info.transforms
            or (
                info.optimizer is not None
                and info.optimizer.__name__ != "_optimize_getslice"
            )
        ):
            for arg in node.args:
                if len(arg.uses) == 1:
                    arg_use = arg.uses[0]
                    cand_arg = self.candidate.get(arg_use, UNKNOWN)
                    gen_arg = self.general.get(arg_use, UNKNOWN)
                    if (
                        cand_arg != gen_arg
                        and cand_arg is not UNKNOWN
                        and not gen_arg.fits(cand_arg)
                    ):
                        self.improved = True
                        return


def _specializes_body(
    graph: LambdaGraph,
    general: dict[object, ValueType],
    candidate: dict[object, ValueType],
) -> bool:
    visitor = _HasCombinationImprovement(general, candidate)
    for block in graph.blocks:
        for node in graph.block_nodes[block]:
            node.accept(visitor)
            if visitor.improved:
                return True
    return False


@dataclass(frozen=True, slots=True)
class EntryContext:
    """A guarded internal entry: facts valid once its parameter guards pass."""

    id: str
    parameters: tuple[ValueType, ...]
    types: dict[object, ValueType]
    targets: dict[object, Lambda]
    return_type: ValueType


@dataclass(slots=True)
class LambdaFacts:
    types: dict[object, ValueType]
    targets: dict[object, Lambda]
    variants: tuple[EntryContext, ...] = ()
    return_type: ValueType = UNKNOWN


def global_bindings(lambdas) -> dict[str, dict[str, Lambda]]:
    """Per namespace, the lambda each global name is bound to by a store of a
    freshly created function object anywhere in the module."""
    targets = defaultdict(dict)
    for lambda_ in lambdas:
        visitor = _GlobalFunctionStores(targets[lambda_.namespace])
        for block in lambda_.component.blocks():
            for node in block.nodes():
                node.accept(visitor)
    return targets


class _GlobalFunctionStores(NodeVisitor):
    def __init__(
        self,
        namespace: dict[str, Lambda],
        counts: dict[str, int] | None = None,
    ):
        self.namespace = namespace
        self.counts = counts

    def visit_Set(self, node):
        node.var.accept(_GlobalFunctionStore(self.namespace, self.counts, node))


class _GlobalFunctionStore(LeafVisitor):
    """Records a store of a fresh function object into a module global; a
    store into a local binds no module name."""

    def __init__(
        self,
        namespace: dict[str, Lambda],
        counts: dict[str, int] | None,
        store: Set,
    ):
        self.namespace = namespace
        self.counts = counts
        self.store = store

    def visit_GlobalVar(self, leaf):
        if self.counts is not None:
            self.counts[leaf.name] = self.counts.get(leaf.name, 0) + 1
        if self.store.value is None:
            return
        lambda_ = made_function(self.store.value.use())
        if lambda_ is not None:
            self.namespace[leaf.name] = lambda_


class _FoldUnaryConstant(ast.Visitor):
    def __init__(self, name: str):
        self.name = name

    def visit_Node(self, node):
        return None

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral):
        if self.name == "%unary_negative":
            return ast.IntegerLiteral(-node.value)
        if self.name == "%unary_positive":
            return ast.IntegerLiteral(+node.value)
        if self.name == "%unary_invert":
            return ast.IntegerLiteral(~node.value)
        return None


class _MadeConstant(NodeVisitor, LeafVisitor):
    """The constant literal an lvar or node evaluates to, or None."""

    def __init__(self):
        self.visited = set()

    def visit_Lvar(self, lvar):
        return lvar.uses[0].accept(self) if len(lvar.uses) == 1 else None

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_Constant(self, leaf):
        return leaf.value

    def visit_LambdaVar(self, leaf):
        if leaf in self.visited or len(leaf.sets) != 1:
            return None
        self.visited.add(leaf)
        value = leaf.sets[0].value
        return (
            value.uses[0].accept(self)
            if value is not None and len(value.uses) == 1
            else None
        )

    def visit_Combination(self, node):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return None
        name = node.fun_info.name
        if name in ("%unary_negative", "%unary_positive", "%unary_invert"):
            if len(node.args) != 1 or len(node.args[0].uses) != 1:
                return None
            operand = node.args[0].uses[0].accept(self)
            if operand is None:
                return None
            return operand.accept(_FoldUnaryConstant(name))
        if name == "%build_tuple":
            elements = []
            for arg in node.args:
                if len(arg.uses) != 1:
                    return None
                item = arg.uses[0].accept(self)
                if item is None:
                    return None
                elements.append(item)
            return ast.Tuple(elements)
        return None


class _WidenUserConstantType(ValueTypeVisitor):
    def visit_ValueType(self, fact):
        return fact

    def visit_BooleanType(self, fact):
        return BOOLEAN

    def visit_IntegerType(self, fact):
        return IntegerType(FIXNUM_MIN, FIXNUM_MAX) if fact.is_fixnum else INTEGER

    def visit_TextType(self, fact):
        return TEXT_TYPE

    def visit_TupleType(self, fact):
        return TupleType(
            fact.arity,
            None
            if fact.elements is None
            else tuple(el.accept(self) for el in fact.elements),
        )


_WIDEN_USER_CONSTANT_TYPE = _WidenUserConstantType()


def _widen_user_constant_type(vtype: ValueType) -> ValueType:
    return vtype.accept(_WIDEN_USER_CONSTANT_TYPE)


class _GlobalConstantStores(NodeVisitor, LeafVisitor):
    def __init__(self, constants: dict[GlobalVar, ValueType], *, is_protocol: bool):
        self.constants = constants
        self.is_protocol = is_protocol
        self._store: Set | None = None

    def visit_Set(self, node: Set):
        self._store = node
        node.var.accept(self)
        self._store = None

    def visit_GlobalVar(self, leaf: GlobalVar):
        store = self._store
        if store is None or store.value is None or len(leaf.sets) != 1:
            return
        if leaf.sets[0] is not store:
            return
        home = store.home_lambda
        if home is None or home.kind is not FunctionalKind.TOPLEVEL:
            return
        literal = store.value.accept(_MadeConstant())
        if literal is None:
            return
        vtype = constant_type(literal)
        if vtype is UNKNOWN:
            return
        if self.is_protocol:
            if leaf.name.startswith("_"):
                self.constants[leaf] = vtype
            return
        if (
            home.policy.safety < 3
            and leaf.name.startswith("_")
            and not vtype.fits_none(NONE_TYPE)
        ):
            self.constants[leaf] = _widen_user_constant_type(vtype)


def collect_global_constants(target) -> dict[GlobalVar, ValueType]:
    """Unreassigned module-level private constant globals and their value types."""
    try:
        lambdas = list(target)
    except TypeError:
        lambdas = list(target.lambdas)
    seen = set(lambdas)
    for lambda_ in list(lambdas):
        current = lambda_
        while current.refs:
            parent = current.refs[0].home_lambda
            if parent is None or parent in seen:
                break
            seen.add(parent)
            lambdas.append(parent)
            current = parent
    is_protocol = any(
        lambda_.name.startswith(_PROTOCOL_PREFIXES) for lambda_ in lambdas
    )
    constants: dict[GlobalVar, ValueType] = {}
    visitor = _GlobalConstantStores(constants, is_protocol=is_protocol)
    visited_components = set()
    for lambda_ in lambdas:
        component = lambda_.component
        if component is None or component in visited_components:
            continue
        visited_components.add(component)
        for block in component.blocks():
            for node in block.nodes():
                node.accept(visitor)
    return constants


def _noop():
    return None


def _with_defined_arguments(arguments, continuation):
    for argument in arguments:
        defined = argument.when_defined(_noop)
        if defined is not None:
            return defined
    return continuation()


class _ClosedMakeFunctionCall(NodeVisitor, LeafVisitor):
    def __init__(self, target: Lambda, ref: Ref):
        self.target = target
        self.ref = ref

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not MAKE_FUNCTION
            or len(node.args) != 1
            or node.args[0] is not self.ref.lvar
            or node.lvar is None
            or len(node.lvar.uses) != 1
        ):
            return None
        dest = node.lvar.dest
        if dest is None:
            return None
        return dest.accept(_ClosedFunctionBinding(self.target))


class _ClosedFunctionBinding(NodeVisitor, LeafVisitor):
    def __init__(self, target: Lambda):
        self.target = target

    def visit_Node(self, node):
        return None

    def visit_Set(self, node: Set):
        if node.lvar is not None and node.lvar.dest is not None:
            return None
        return node.var.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf: LambdaVar):
        if len(leaf.sets) != 1 or leaf in leaf.home.vars:
            return None
        final_refs = [ref for ref in leaf.refs if ref.final]
        if (
            len(final_refs) == 1
            and final_refs[0].lvar is not None
            and len(final_refs[0].lvar.uses) == 1
            and final_refs[0].dest is not None
        ):
            forwarded = final_refs[0].dest.accept(self)
            if forwarded is not None:
                return forwarded
        if not leaf.refs:
            return None
        call_refs = []
        for ref in leaf.refs:
            if (
                ref.lvar is None
                or len(ref.lvar.uses) != 1
                or ref.dest is None
                or not ref.dest.accept(_IsClosedPositionalCall(self.target, ref.lvar))
            ):
                return None
            call_refs.append(ref)
        if not any(ref.home_lambda is not self.target for ref in call_refs):
            return None
        return call_refs


class _IsClosedPositionalCall(NodeVisitor):
    def __init__(self, target: Lambda, fun_lvar):
        self.target = target
        self.fun_lvar = fun_lvar

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.fun is self.fun_lvar
            and node.kind in (CallKind.FULL, CallKind.GUARDED)
            and (node.info is None or not node.info.keywords)
            and len(node.args) == len(self.target.vars)
        )


def _collect_closed_lambdas(lambdas) -> dict[Lambda, tuple[Ref, ...]]:
    """Return nested functions whose function object never escapes and whose
    every reference is the callee of a direct positional call."""
    closed: dict[Lambda, tuple[Ref, ...]] = {}
    for lambda_ in lambdas:
        if (
            lambda_.kind is not FunctionalKind.NORMAL
            or not lambda_.vars
            or not lambda_.signature.positional_body
            or lambda_.signature.default_parameters
            or len(lambda_.refs) != 1
        ):
            continue
        ref = lambda_.refs[0]
        if ref.lvar is None or len(ref.lvar.uses) != 1 or ref.dest is None:
            continue
        call_refs = ref.dest.accept(_ClosedMakeFunctionCall(lambda_, ref))
        if call_refs is not None:
            closed[lambda_] = tuple(call_refs)
    return closed


def analyze(lambdas, annotations: AnnotationLedger) -> dict[Lambda, LambdaFacts]:
    """Solve local-call arguments and results, then publish entry contexts.

    General entries model externally supplied values. Observed integer
    contexts and an all-fixnum context describe optional guarded entries, never
    changes to a function's public contract. Return summaries start empty, grow
    by union, and widen cyclic intervals before publication.
    """
    lambdas = list(lambdas)
    graphs: dict[Lambda, LambdaGraph] = {
        lambda_: build_lambda_graph(lambda_) for lambda_ in lambdas
    }
    closed_lambdas = _collect_closed_lambdas(lambdas)
    global_constants = collect_global_constants(lambdas)
    old_constant_ref_types: dict[object, ValueType | None] = {}
    for leaf, vtype in global_constants.items():
        for ref in leaf.refs:
            home = ref.home_lambda
            if (
                home is not None
                and home.kind is not FunctionalKind.TOPLEVEL
                and home.policy.safety < 3
                and ref.lexenv.policy.safety < 3
            ):
                old_constant_ref_types[ref] = ref.derived_type
                ref.derived_type = vtype
    global_targets = defaultdict(dict)
    global_counts: dict[str, dict[str, int]] = defaultdict(dict)
    for lambda_ in lambdas:
        visitor = _GlobalFunctionStores(
            global_targets[lambda_.namespace], global_counts[lambda_.namespace]
        )
        graph = graphs[lambda_]
        for block in graph.blocks:
            for node in graph.block_nodes[block]:
                node.accept(visitor)
    proven_by_namespace: dict[str, frozenset[str]] = {
        ns: frozenset(
            name
            for name, target in targets.items()
            if global_counts[ns].get(name) == 1
            and target.name.startswith(_PROTOCOL_PREFIXES)
        )
        for ns, targets in global_targets.items()
    }
    rebound_by_namespace: dict[str, frozenset[str]] = {
        ns: frozenset(counts) for ns, counts in global_counts.items()
    }
    captured_vars: list = []
    seen_captured: set = set()
    for lambda_ in lambdas:
        if lambda_.environment is not None:
            for var in lambda_.environment.closure:
                if var not in seen_captured:
                    seen_captured.add(var)
                    captured_vars.append(var)
    for var in captured_vars:
        if not var.sets or (
            var in var.home.vars and var.name not in var.home.local_annotations
        ):
            var.optimistic_type = UNKNOWN
        else:
            var.optimistic_type = BOTTOM
    var_changes = defaultdict(int)
    full = IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    parameters: dict[tuple[Lambda, str], tuple[ValueType, ...]] = {}
    for lambda_ in lambdas:
        signature = lambda_.signature
        if lambda_.vars and not signature.variadic:
            parameters[lambda_, "general"] = tuple(
                _ParameterUse(var).general_type(lambda_.local_annotations.get(var.name))
                if contract is None
                and ("local", lambda_, var.name) not in annotations.ignored
                else UNKNOWN
                for var, contract in zip(
                    lambda_.vars, signature.annotations, strict=True
                )
            )
            spec_params = tuple(
                _ParameterUse(var).analyze(contract, full)
                for var, contract in zip(
                    lambda_.vars, signature.annotations, strict=True
                )
            )
            if all(param is not UNKNOWN for param in spec_params):
                parameters[lambda_, "fixnum"] = spec_params
            else:
                partial_params = tuple(
                    param
                    if param is not UNKNOWN
                    and (contract is not None or lambda_.vars[0].name != "self")
                    else UNKNOWN
                    for param, contract in zip(
                        spec_params, signature.annotations, strict=True
                    )
                )
                if any(param is not UNKNOWN for param in partial_params):
                    parameters[lambda_, "fixnum"] = partial_params
        else:
            parameters[lambda_, "general"] = (UNKNOWN,) * len(lambda_.vars)
    summaries: dict[tuple[Lambda, str], ValueType] = dict.fromkeys(parameters, BOTTOM)
    return_changes = defaultdict(int)
    argument_changes = defaultdict(int)
    closed_seeded: set[Lambda] = set()
    bodies: dict[tuple[Lambda, str], Body] = {}
    # What each body's last propagation read: its parameters and, for every
    # Each entry records ``(expected_parameters, consulted_calls, closure_types,
    # consulted_targets)`` for the most recent propagation of ``key``. A body
    # whose inputs and resolved call return types are unchanged would propagate
    # identically.
    inputs: dict[tuple[Lambda, str], tuple] = {}

    def lookup_return_type(
        target, arguments, context_parameters, context_summaries
    ) -> ValueType:
        obs = context_parameters.get((target, "observed"))
        fix = context_parameters.get((target, "fixnum"))
        gen = context_parameters.get((target, "general"))
        order_contexts = (
            ("fixnum", "observed", "general")
            if obs is not None
            and fix is not None
            and all(a.fits(b) or a is UNKNOWN for a, b in zip(obs, fix, strict=True))
            and not all(a.fits(b) for a, b in zip(obs, fix, strict=True))
            else ("observed", "fixnum", "general")
        )
        for context in order_contexts:
            key = target, context
            expected = context_parameters.get(key)
            if expected is not None and all(
                actual.fits(wanted)
                for actual, wanted in zip(arguments, expected, strict=True)
            ):
                if (
                    context != "general"
                    and gen is not None
                    and all(
                        actual.fits(wanted)
                        for actual, wanted in zip(arguments, gen, strict=True)
                    )
                    and all(
                        g.fits(e) for g, e in zip(gen, expected, strict=True)
                    )
                ):
                    return context_summaries[target, "general"]
                return context_summaries[key]
        return UNKNOWN

    def closure_inputs(lambda_):
        if lambda_.environment is None:
            return ()
        return tuple(var.optimistic_type for var in lambda_.environment.closure)

    # The canonical (callee, arguments) queries and return types consulted
    # during the current propagation.
    consulted: dict[tuple[Lambda, tuple], ValueType] = {}

    while True:
        changed = False
        previous_parameters = dict(parameters)

        def resolve(
            callee: Fact,
            arguments,
            location=None,
            keywords=(),
            context_parameters=previous_parameters,
            context_summaries=summaries,
        ) -> ValueType:
            return callee.type.when_defined(
                lambda: _with_defined_arguments(
                    arguments,
                    lambda: resolve_defined(
                        callee,
                        arguments,
                        context_parameters,
                        context_summaries,
                        location,
                        keywords,
                    ),
                )
            )

        def resolve_defined(
            callee, arguments, context_parameters, context_summaries, location, keywords
        ):
            # A name's predicted target is never sufficient to publish a return
            # type. Only an already-loaded value with proven identity is local.
            if not callee.proven or callee.target is None:
                return UNKNOWN
            target = callee.target
            order = target.call_order(keywords, len(arguments))
            if order is None:
                return UNKNOWN
            arguments = tuple(
                arguments[index] if index >= 0 else UNKNOWN for index in order
            )
            for index, (actual, contract, var) in enumerate(
                zip(arguments, target.signature.annotations, target.vars, strict=True)
            ):
                if contract is not None:
                    annotations.check(
                        contract, actual, location, ("local", target, var.name)
                    )
                else:
                    declared = target.local_annotations.get(var.name)
                    if declared is not None and declared.contradicts(actual):
                        annotations.ignored.add(("local", target, var.name))
                        gen = parameters.get((target, "general"))
                        if gen is not None and gen[index] is not UNKNOWN:
                            updated = list(gen)
                            updated[index] = UNKNOWN
                            parameters[target, "general"] = tuple(updated)
            result = lookup_return_type(
                target, arguments, context_parameters, context_summaries
            )
            consulted[target, arguments] = result
            return result

        for key, expected in previous_parameters.items():
            lambda_ = key[0]
            recorded = inputs.get(key)
            current_closure = closure_inputs(lambda_)
            if (
                recorded is not None
                and recorded[0] == expected
                and recorded[2] == current_closure
                and all(
                    lookup_return_type(target, args, previous_parameters, summaries)
                    == result
                    for (target, args), result in recorded[1].items()
                )
            ):
                body = bodies[key]
            else:
                consulted.clear()
                proven_globals = (
                    proven_by_namespace.get(lambda_.namespace, frozenset())
                    if lambda_.kind is not FunctionalKind.TOPLEVEL
                    else frozenset()
                )
                with flow_propagation():
                    body = propagate(
                        lambda_,
                        global_targets[lambda_.namespace],
                        expected,
                        resolve,
                        annotations,
                        graph=graphs[lambda_],
                        proven_globals=proven_globals,
                        rebound_globals=rebound_by_namespace.get(
                            lambda_.namespace, frozenset()
                        ),
                    )
                bodies[key] = body
                inputs[key] = (
                    expected,
                    dict(consulted),
                    current_closure,
                    frozenset(target for target, _ in consulted),
                )
            returned = summaries[key].join(body.return_type)
            if returned != summaries[key]:
                return_changes[key] += 1
                if return_changes[key] > 3:
                    returned = widen_fact(Fact(summaries[key]), Fact(returned)).type
                if returned != summaries[key]:
                    summaries[key] = returned
                    changed = True

        for var in captured_vars:
            if not var.sets or (
                var in var.home.vars and var.name not in var.home.local_annotations
            ):
                continue
            joined = BOTTOM
            for store in var.sets:
                home_body = bodies.get((store.home_lambda, "general"))
                if home_body is None:
                    continue
                if store.value is None:
                    joined = joined.join(UNBOUND)
                else:
                    joined = joined.join(
                        home_body.facts.get(store.value.use(), NO_FACT).type
                    )
            if joined != var.optimistic_type:
                var_changes[var] += 1
                if var_changes[var] > 3:
                    joined = widen_fact(Fact(var.optimistic_type), Fact(joined)).type
                if joined != var.optimistic_type:
                    var.optimistic_type = joined
                    changed = True

        # The broad guard context is not evidence that callers actually supplied
        # that broad interval. Propagate observations through general/observed
        # entries, including recursive calls, independently of that context.
        seeds: dict[Lambda, tuple] = {}
        narrowed_targets: set[Lambda] = set()
        for target, call_refs in closed_lambdas.items():
            external_calls = []
            all_calls = []
            missing_external = False
            for ref in call_refs:
                caller = ref.home_lambda
                caller_body = bodies.get((caller, "general"))
                if caller_body is None:
                    if caller is not target:
                        missing_external = True
                    continue
                matching = [
                    call
                    for call in caller_body.calls
                    if call.target is target
                    and call.proven
                    and len(call.arguments) == len(target.vars)
                ]
                if not matching:
                    if caller is not target:
                        missing_external = True
                    continue
                all_calls.extend(matching)
                if caller is not target:
                    external_calls.extend(matching)
            if missing_external or not external_calls:
                continue
            source_calls = (
                external_calls if target not in closed_seeded else all_calls
            )
            joined_args = source_calls[0].arguments
            for call in source_calls[1:]:
                joined_args = tuple(
                    old.join(new)
                    for old, new in zip(joined_args, call.arguments, strict=True)
                )
            gen_key = target, "general"
            old_gen = parameters[gen_key]
            candidate_gen = []
            for arg_type, contract, var in zip(
                joined_args,
                target.signature.annotations,
                target.vars,
                strict=True,
            ):
                if (
                    arg_type is BOTTOM
                    or arg_type is UNBOUND
                    or arg_type.may_be_unbound
                ):
                    arg_type = UNKNOWN
                if contract is not None and contract.contradicts(arg_type):
                    arg_type = UNKNOWN
                declared = (
                    target.local_annotations.get(var.name)
                    if contract is None
                    and ("local", target, var.name) not in annotations.ignored
                    else None
                )
                if declared is not None and declared.contradicts(arg_type):
                    arg_type = UNKNOWN
                candidate_gen.append(arg_type)
            new_gen = tuple(candidate_gen)
            if target not in closed_seeded:
                closed_seeded.add(target)
                merged_gen = tuple(
                    b if a is UNKNOWN or b.fits(a) else a.join(b)
                    for a, b in zip(old_gen, new_gen, strict=True)
                )
            else:
                merged_gen = tuple(
                    a.join(b) for a, b in zip(old_gen, new_gen, strict=True)
                )
                if merged_gen != old_gen:
                    argument_changes[gen_key] += 1
                    if argument_changes[gen_key] > 3:
                        merged_gen = tuple(
                            widen_fact(Fact(a), Fact(b)).type
                            for a, b in zip(old_gen, merged_gen, strict=True)
                        )
            if merged_gen != old_gen:
                if not all(
                    a.fits(b) for a, b in zip(old_gen, merged_gen, strict=True)
                ):
                    narrowed_targets.add(target)
                parameters[gen_key] = merged_gen
                changed = True
        for key, body in bodies.items():
            if key[1] == "fixnum":
                continue
            for call in body.calls:
                target = call.target
                if (
                    target is None
                    or not target.signature.positional_body
                    or not target.vars
                    or len(call.arguments) != len(target.vars)
                ):
                    continue
                for index, (actual, contract, var) in enumerate(
                    zip(
                        call.arguments,
                        target.signature.annotations,
                        target.vars,
                        strict=True,
                    )
                ):
                    if contract is None:
                        declared = target.local_annotations.get(var.name)
                        if declared is not None and declared.contradicts(actual):
                            annotations.ignored.add(("local", target, var.name))
                            gen = parameters.get((target, "general"))
                            if gen is not None and gen[index] is not UNKNOWN:
                                updated = list(gen)
                                updated[index] = UNKNOWN
                                parameters[target, "general"] = tuple(updated)
                                narrowed_targets.add(target)
                                changed = True
                raw_domains = tuple(
                    domain
                    if (domain := type_.dispatch_domain()) is not None
                    and (contract is None or not contract.contradicts(domain))
                    else UNKNOWN
                    for type_, contract in zip(
                        call.arguments,
                        target.signature.annotations,
                        strict=True,
                    )
                )
                if all(domain is UNKNOWN for domain in raw_domains):
                    continue
                fix_params = parameters.get((target, "fixnum"))
                domains = tuple(
                    domain
                    if domain is not UNKNOWN
                    else (
                        fix_param
                        if fix_params is not None
                        and (fix_param := fix_params[idx]) in (LIST_TYPE, TEXT_TYPE)
                        else UNKNOWN
                    )
                    for idx, domain in enumerate(raw_domains)
                )
                if target not in seeds:
                    seeds[target] = domains
                else:
                    seeds[target] = tuple(
                        old.join(new)
                        for old, new in zip(seeds[target], domains, strict=True)
                    )
        for target, seed in seeds.items():
            key = target, "observed"
            old = parameters.get(key)
            if old is not None:
                seed = tuple(
                    b
                    if a is UNKNOWN and b is not UNKNOWN and argument_changes[key] == 0
                    else a.join(b)
                    for a, b in zip(old, seed, strict=True)
                )
                if old != seed:
                    argument_changes[key] += 1
                    if argument_changes[key] > 3:
                        seed = tuple(
                            widen_fact(Fact(a), Fact(b)).type.dispatch_domain()
                            or UNKNOWN
                            for a, b in zip(old, seed, strict=True)
                        )
            if old != seed:
                if old is None or not all(
                    a.fits(b) for a, b in zip(old, seed, strict=True)
                ):
                    narrowed_targets.add(target)
                parameters[key] = seed
                summaries.setdefault(key, BOTTOM)
                changed = True
        if narrowed_targets:
            # Propagate invalidation transitively along the recorded call graph
            # so only functions that transitively depend on a newly created or
            # narrowed observed context reset their provisional return summary.
            affected = set(narrowed_targets)
            while True:
                added = {
                    key[0]
                    for key, recorded in inputs.items()
                    if key[0] not in affected and affected.intersection(recorded[3])
                }
                if not added:
                    break
                affected.update(added)
            for key, expected in parameters.items():
                if key == (key[0], "observed") and key[0] in narrowed_targets:
                    summaries[key] = BOTTOM
                    return_changes.pop(key, None)
                elif key[0] in affected:
                    recorded = inputs.get(key)
                    if (
                        recorded is None
                        or recorded[0] != expected
                        or affected.intersection(recorded[3])
                    ):
                        summaries[key] = BOTTOM
                        return_changes.pop(key, None)
        if not changed:
            break

    for var in captured_vars:
        if var.optimistic_type is None or var.optimistic_type is BOTTOM:
            var.optimistic_type = UNKNOWN
        else:
            var.optimistic_type = var.optimistic_type.normalized_bottom()

    for ref, old_type in old_constant_ref_types.items():
        ref.derived_type = old_type

    results: dict[Lambda, LambdaFacts] = {}
    for lambda_ in lambdas:
        graph = graphs[lambda_]
        general = bodies[lambda_, "general"]
        facts = LambdaFacts(
            general.types(),
            general.targets(),
            return_type=summaries[lambda_, "general"].normalized_bottom(),
        )
        entries = []
        obs_params = parameters.get((lambda_, "observed"))
        fix_params = parameters.get((lambda_, "fixnum"))
        variant_order = (
            ("fixnum", "observed")
            if obs_params is not None
            and fix_params is not None
            and all(
                a.fits(b) or a is UNKNOWN
                for a, b in zip(obs_params, fix_params, strict=True)
            )
            and not all(a.fits(b) for a, b in zip(obs_params, fix_params, strict=True))
            else ("observed", "fixnum")
        )
        for identifier in variant_order:
            key = lambda_, identifier
            if key not in parameters:
                continue
            body = bodies[key]
            candidate_types = body.types()
            ret_type = summaries[key].normalized_bottom()
            if identifier == "fixnum" and not _specializes_body(
                graph, facts.types, candidate_types
            ):
                continue
            if (
                identifier == "observed"
                and not any(param.is_fixnum for param in parameters[key])
                and not _specializes_body(graph, facts.types, candidate_types)
                and ret_type == facts.return_type
            ):
                continue
            if (
                identifier == "observed"
                and parameters[key] == parameters.get((lambda_, "fixnum"))
                and _specializes_body(
                    graph, facts.types, bodies[lambda_, "fixnum"].types()
                )
            ):
                continue
            entries.append(
                EntryContext(
                    identifier,
                    parameters[key],
                    candidate_types,
                    body.targets(),
                    ret_type,
                )
            )
        facts.variants = tuple(entries)
        results[lambda_] = facts
        _mark_guarded_calls(graph, general)
        _publish_derived_types(lambda_, graph, facts.types)
    return results


def _lvar_current_type(lvar) -> ValueType | None:
    if lvar.derived_type is not None:
        return lvar.derived_type
    if not lvar.uses or any(use.derived_type is None for use in lvar.uses):
        return None
    derived = lvar.uses[0].derived_type
    for use in lvar.uses[1:]:
        derived = derived.join(use.derived_type)
    return derived


def _recompute_lvar_type(lvar) -> ValueType:
    if not lvar.uses:
        lvar.derived_type = BOTTOM
        return BOTTOM
    derived = (
        lvar.uses[0].derived_type if lvar.uses[0].derived_type is not None else UNKNOWN
    )
    for use in lvar.uses[1:]:
        use_type = use.derived_type if use.derived_type is not None else UNKNOWN
        derived = derived.join(use_type)
    lvar.derived_type = derived
    return derived


class _PublishNodeTypes(NodeVisitor):
    def __init__(self, types: dict[object, ValueType]):
        self.types = types
        self.old_lvar_types: dict[object, ValueType | None] = {}
        self.lvars: list = []

    def visit_ValuedNode(self, node):
        lvar = node.lvar
        if lvar is not None and lvar not in self.old_lvar_types:
            self.old_lvar_types[lvar] = _lvar_current_type(lvar)
            self.lvars.append(lvar)
        node.derived_type = self.types.get(node, UNKNOWN)


class _ReoptimizeNarrowedKnownCalls(NodeVisitor):
    def __init__(
        self,
        lambda_: Lambda,
        old_lvar_types: dict[object, ValueType | None],
    ):
        self.lambda_ = lambda_
        self.old_lvar_types = old_lvar_types
        self.reoptimized = False

    def visit_Combination(self, node):
        if node.kind is CallKind.KNOWN and node.fun_info is not None:
            info = node.fun_info
        elif is_predictable_len_call(node):
            info = LEN
        elif (
            node.kind in (CallKind.FULL, CallKind.GUARDED)
            and len(node.fun.uses) == 1
            and not (node.info is not None and node.info.keywords)
            and node.lexenv.policy.safety < 3
        ):
            info = node.fun.uses[0].accept(_PREDICTABLE_GLOBAL_FUN_INFO)
            if info is None:
                return
        else:
            return
        if not (
            info.transforms
            or (
                info.optimizer is not None
                and info.optimizer.__name__ != "_optimize_getslice"
            )
        ):
            return
        if any(
            (new_type := _recompute_lvar_type(arg)) != self.old_lvar_types.get(arg)
            and new_type is not UNKNOWN
            for arg in node.args
        ):
            node.reoptimize = True
            node.block.reoptimize = True
            if self.lambda_.component is not None:
                self.lambda_.component.reoptimize = True
            self.reoptimized = True


def _publish_derived_types(
    lambda_: Lambda, graph: LambdaGraph, types: dict[object, ValueType]
) -> bool:
    """Write ``general`` flow-derived types back onto IR1 nodes and lvars, and
    request IR1 reoptimization when a known call with transforms receives a
    narrowed operand."""
    publisher = _PublishNodeTypes(types)
    for block in graph.blocks:
        for node in graph.block_nodes[block]:
            node.accept(publisher)
    for lvar in publisher.lvars:
        _recompute_lvar_type(lvar)
    reoptimizer = _ReoptimizeNarrowedKnownCalls(lambda_, publisher.old_lvar_types)
    for block in graph.blocks:
        for node in graph.block_nodes[block]:
            node.accept(reoptimizer)
    return reoptimizer.reoptimized


def _would_reoptimize_for_context(
    lambda_: Lambda, context: EntryContext
) -> bool:
    component = lambda_.component
    if component is None or context.id not in ("fixnum", "observed"):
        return False
    for block in component.blocks():
        for node in block.nodes():
            if node.accept(_NodeHasContextTransform(context)):
                return True
    return False


class _NodeHasContextTransform(NodeVisitor):
    def __init__(self, context: EntryContext):
        self.context = context

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN and node.fun_info is not None:
            info = node.fun_info
        elif is_predictable_len_call(node):
            info = LEN
        elif (
            node.kind in (CallKind.FULL, CallKind.GUARDED)
            and len(node.fun.uses) == 1
            and not (node.info is not None and node.info.keywords)
            and node.lexenv.policy.safety < 3
        ):
            info = node.fun.uses[0].accept(_PREDICTABLE_GLOBAL_FUN_INFO)
            if info is None:
                return False
        else:
            return False
        if not (
            info.transforms
            or (
                info.optimizer is not None
                and info.optimizer.__name__ != "_optimize_getslice"
            )
        ):
            return False
        for arg in node.args:
            if not arg.uses:
                continue
            cand_arg = self.context.types.get(arg.uses[0], UNKNOWN)
            for use in arg.uses[1:]:
                cand_arg = cand_arg.join(self.context.types.get(use, UNKNOWN))
            gen_arg = _lvar_current_type(arg) or UNKNOWN
            if (
                cand_arg is not UNKNOWN
                and cand_arg != gen_arg
                and not gen_arg.fits(cand_arg)
            ):
                return True
        return False


class _CollectNodeTypesAndTargets(NodeVisitor):
    def __init__(
        self,
        base_types: dict[object, ValueType],
        base_targets: dict[object, Lambda],
    ):
        self.types = dict(base_types)
        self.targets = dict(base_targets)

    def visit_ValuedNode(self, node):
        if node not in self.types and node.derived_type is not None:
            self.types[node] = node.derived_type

    def visit_Combination(self, node: Combination):
        self.visit_ValuedNode(node)
        if (
            node not in self.targets
            and node.kind is CallKind.GUARDED
            and node.predicted is not None
        ):
            self.targets[node] = node.predicted


@contextmanager
def specialized_ir1_context(lambda_: Lambda, context: EntryContext):
    """Run entry-context-specific IR1 optimization on a cloned component for
    ``context`` and restore the canonical IR1 component on exit."""
    if not _would_reoptimize_for_context(lambda_, context):
        yield
        return
    cloned_component, node_map, restore = clone_lambda_component(lambda_)
    old_types = context.types
    old_targets = context.targets
    try:
        cloned_types = {
            node_map.get(node, node): vtype for node, vtype in old_types.items()
        }
        cloned_targets = {
            node_map.get(node, node): target for node, target in old_targets.items()
        }
        graph = build_lambda_graph(lambda_)
        if _publish_derived_types(lambda_, graph, cloned_types):
            ir1opt.ir1_optimize_until_done(cloned_component)
            join_blocks_if_possible(cloned_component)
            find_dfo(cloned_component)
            loop.analyze(cloned_component)
            collector = _CollectNodeTypesAndTargets(cloned_types, cloned_targets)
            for block in cloned_component.blocks():
                for node in block.nodes():
                    node.accept(collector)
            cloned_types = collector.types
            cloned_targets = collector.targets
        context.types = cloned_types
        context.targets = cloned_targets
        yield
    finally:
        context.types = old_types
        context.targets = old_targets
        restore()


def _mark_guarded_calls(graph: LambdaGraph, body: Body) -> None:
    """Record predicted targets on full calls so later phases and code
    generation can guard them; the general path always remains."""
    visitor = _MarkGuardedCall(body)
    for block in graph.blocks:
        for node in graph.block_nodes[block]:
            node.accept(visitor)


class _MarkGuardedCall(NodeVisitor):
    def __init__(self, body: Body):
        self.body = body

    def visit_Combination(self, node):
        if node.kind not in (CallKind.FULL, CallKind.GUARDED):
            return
        fact = self.body.facts.get(node.fun.use())
        if fact is None or fact.target is None:
            node.kind = CallKind.FULL
            node.predicted = None
            node.proven = False
            return
        node.kind = CallKind.GUARDED
        node.predicted = fact.target
        node.proven = fact.proven


__all__ = [
    "EntryContext",
    "LambdaFacts",
    "analyze",
    "collect_global_constants",
    "global_bindings",
    "join_fact",
    "specialized_ir1_context",
]
