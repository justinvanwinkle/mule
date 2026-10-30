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
from collections.abc import Mapping
from contextlib import contextmanager
from dataclasses import dataclass, field, replace
from threading import local

from mule import ast

from . import ir1opt, loop
from .annotation_types import AnnotationLedger
from .constraint import (
    BOOTSTRAP_BUILTIN_TARGETS,
    BOOTSTRAP_CLASS_TARGETS,
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
    ANY_CLASS_TYPE,
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    BUILTIN_CLASS_TYPES,
    BYTEARRAY_TYPE,
    BYTES_TYPE,
    DICT_TYPE,
    EMPTY_LIST_TYPE,
    FIXNUM_MAX,
    FIXNUM_MIN,
    FLOAT,
    FROZENSET_TYPE,
    INTEGER,
    LIST_TYPE,
    NONE_TYPE,
    PYTHON_INTEGER,
    STORAGE_CLASS_TYPES,
    TEXT_TYPE,
    UNBOUND,
    UNKNOWN,
    ClassType,
    DictType,
    FunctionType,
    InstanceType,
    IntegerType,
    ListType,
    TupleType,
    UniqueObjectType,
    ValueType,
    ValueTypeVisitor,
)
from .dfo import find_dfo
from .fndb import (
    BINARY,
    BUILD_LIST,
    BUILD_TUPLE,
    CALL_RESOLVED,
    CLASS_DELETE,
    CLASS_STORE,
    COMPARE,
    CURRENT_GLOBALS,
    GETATTR,
    GETSLICE,
    HAS_BINDING,
    IMPORT,
    IMPORT_STAR,
    INPLACE_BINARY,
    LEN,
    MAKE_CLASS,
    MAKE_FUNCTION,
    PHYSICAL_FIELD_REF,
    PHYSICAL_FIELD_SET,
    PROTOCOLS,
    RANGE_PREPARE,
    RESOLVE_METHOD,
    RESOLVE_SUPER_METHOD,
    SETATTR,
    TEXT_SLICE_COMPARE,
    UNARY,
    UNPACK_REF,
    flow_propagation,
)
from .ir1tran import _IS_FINAL_ANNOTATION
from .ir1util import (
    block_exception_target,
    clone_lambda_component,
    join_blocks_if_possible,
    known_calls,
    made_function,
)
from .node import (
    BasicVar,
    CallKind,
    Cast,
    ClassConstructorTarget,
    Combination,
    Enclose,
    ForeignCallTarget,
    FunctionalKind,
    GlobalVar,
    Lambda,
    LambdaVar,
    LeafVisitor,
    Lvar,
    NodeVisitor,
    Ref,
    Set,
    linked,
)
from .seqtran import (
    _PREDICTABLE_RANGE_CALLEE_REF,
    _UNMUTATED_RANGE_GLOBAL_REF,
    is_predictable_len_call,
    known_length_primitive,
)

DELATTR = PROTOCOLS["delete_attribute"]
DELITEM = PROTOCOLS["delitem"]
GETITEM = PROTOCOLS["getitem"]
ITERATE = PROTOCOLS["iterate"]
SETITEM = PROTOCOLS["setitem"]

_SET_DEL_ITEM = frozenset({SETITEM, DELITEM})
_GET_SET_DEL_ATTR = frozenset({GETATTR, SETATTR, DELATTR})
_GET_ITEM_OR_SLICE = frozenset({GETITEM, GETSLICE})
_CONTAINMENT_COMPARE = frozenset(
    {
        COMPARE[ast.ComparisonOperator.IN],
        COMPARE[ast.ComparisonOperator.NOT_IN],
    }
)
_SPECIALIZABLE_CONTAINER_OPS = frozenset(
    {
        GETATTR,
        SETATTR,
        GETITEM,
        GETSLICE,
        SETITEM,
        DELITEM,
        RESOLVE_METHOD,
        ITERATE,
        COMPARE[ast.ComparisonOperator.IN],
        COMPARE[ast.ComparisonOperator.NOT_IN],
        BINARY[ast.BinaryOperator.BIT_AND],
        BINARY[ast.BinaryOperator.BIT_OR],
        BINARY[ast.BinaryOperator.SUBTRACT],
        BINARY[ast.BinaryOperator.BIT_XOR],
        INPLACE_BINARY[ast.BinaryOperator.BIT_AND],
        INPLACE_BINARY[ast.BinaryOperator.BIT_OR],
        INPLACE_BINARY[ast.BinaryOperator.SUBTRACT],
        INPLACE_BINARY[ast.BinaryOperator.BIT_XOR],
    }
)


def _is_predictable_range_call(node: Combination) -> bool:
    if (
        node.kind not in (CallKind.FULL, CallKind.GUARDED)
        or not (1 <= len(node.args) <= 3)
        or len(node.fun.uses) != 1
        or (node.info is not None and node.info.keywords)
        or node.lexenv.policy.safety >= 3
    ):
        return False
    home = node.home_lambda
    if home is not None and (home.policy.safety >= 3 or home.is_protocol):
        return False
    fun_use = node.fun.uses[0]
    return fun_use.accept(_PREDICTABLE_RANGE_CALLEE_REF) or fun_use.accept(
        _UNMUTATED_RANGE_GLOBAL_REF
    )


class _ParameterUse(NodeVisitor, LeafVisitor):
    """Classifies how one parameter (and local copies of it) is used."""

    def __init__(
        self,
        parameter,
        storage_methods: Mapping[tuple[str, str], ValueType] | None = None,
    ):
        self.copies = {parameter}
        self.queue = [parameter]
        self.storage_methods = storage_methods
        self.numeric = False
        self.disqualified = False
        self.text_disqualified = False
        self.list_disqualified = False
        self.sequence_disqualified = False
        self.current = None

    def analyze(self, contract, full: ValueType, declared=None) -> ValueType:
        effective = contract if contract is not None else declared
        while self.queue and not (
            self.disqualified
            and self.text_disqualified
            and self.list_disqualified
            and self.sequence_disqualified
        ):
            var = self.queue.pop()
            for ref in var.refs:
                if ref.dest is not None and ref.lvar is not None:
                    self.current = ref.lvar
                    ref.dest.accept(self)
        if effective is not None:
            spec_type = effective.speculative_parameter_type(full)
            if spec_type.fits(INTEGER) or spec_type.fits_float(FLOAT):
                return spec_type if not self.disqualified else UNKNOWN
            if spec_type.fits_text(TEXT_TYPE):
                return spec_type if not self.text_disqualified else UNKNOWN
            if spec_type.is_exact_list:
                return spec_type if not self.list_disqualified else UNKNOWN
            if spec_type is not UNKNOWN:
                return spec_type if not self.sequence_disqualified else UNKNOWN
        if (
            self.numeric
            and not self.disqualified
            and (effective is None or not effective.contradicts(full))
        ):
            return full
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
            elif _is_predictable_range_call(node) and lvar in node.args:
                self.numeric = True
                self.text_disqualified = True
                self.list_disqualified = True
                self.sequence_disqualified = True
            return
        if info is RANGE_PREPARE:
            if node.info is not None and (
                node.info.direct or lvar in node.info.arguments
            ):
                self.numeric = True
                self.text_disqualified = True
                self.list_disqualified = True
                self.sequence_disqualified = True
            else:
                self.disqualified = True
            return
        if info is RESOLVE_METHOD and node.args and node.args[0] is lvar:
            self.disqualified = True
            method_name = linked(node.info, node, "info").name
            if self.storage_methods is not None and (
                ("TextStorage", method_name) in self.storage_methods
                or ("ListStorage", method_name) in self.storage_methods
            ):
                if ("TextStorage", method_name) not in self.storage_methods:
                    self.text_disqualified = True
                if ("ListStorage", method_name) not in self.storage_methods:
                    self.list_disqualified = True
            else:
                self.text_disqualified = True
                self.list_disqualified = True
            return
        if node.args and node.args[0] is lvar and info in _SET_DEL_ITEM:
            self.disqualified = True
            self.text_disqualified = True
            return
        if node.args and node.args[0] is lvar and info in _GET_SET_DEL_ATTR:
            self.disqualified = True
            self.list_disqualified = True
            return
        if node.args and node.args[0] is lvar and info.non_integer_first_arg:
            self.disqualified = True
            if (
                info is TEXT_SLICE_COMPARE
                and len(node.args) > 3
                and node.args[3] is lvar
            ):
                self.text_disqualified = True
                self.list_disqualified = True
                self.sequence_disqualified = True
            return
        if info is RESOLVE_SUPER_METHOD and lvar in node.args:
            self.disqualified = True
            self.text_disqualified = True
            self.list_disqualified = True
            self.sequence_disqualified = True
            return
        if len(node.args) > 1 and node.args[1] is lvar and info in _CONTAINMENT_COMPARE:
            self.disqualified = True
            return
        if (
            len(node.args) > 1
            and node.args[1] is lvar
            and (info.index_second_arg or info is PROTOCOLS["_list_fill"])
        ):
            self.numeric = True
            self.text_disqualified = True
            self.list_disqualified = True
            self.sequence_disqualified = True
            return
        if info is GETSLICE and lvar in node.args[1:]:
            self.numeric = True
            self.text_disqualified = True
            self.list_disqualified = True
            self.sequence_disqualified = True
            return
        if node.args and node.args[0] is lvar and info in _GET_ITEM_OR_SLICE:
            return
        if info.numeric:
            self.numeric = True


class _PredictableGlobalFunInfo(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.lexenv.policy.safety >= 3:
            return None
        home = node.home_lambda
        if home is not None and (home.policy.safety >= 3 or home.is_protocol):
            return None
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_GlobalVar(self, leaf: GlobalVar):
        return ir1opt._PREDICTABLE_GLOBAL_FUN_INFO.visit_GlobalVar(leaf)


_PREDICTABLE_GLOBAL_FUN_INFO = _PredictableGlobalFunInfo()


class _InstancePrediction(ValueTypeVisitor):
    def visit_ValueType(self, fact):
        return False

    def visit_InstanceType(self, fact):
        return True

    def visit_UnionType(self, fact):
        return any(member.accept(self) for member in fact.members)


_INSTANCE_PREDICTION = _InstancePrediction()


class _SpecializesBody(NodeVisitor):
    """Whether ``candidate`` narrows a numeric operation, branch, or transform
    over ``general``."""

    def __init__(
        self,
        general: dict[object, ValueType],
        candidate: dict[object, ValueType],
        general_targets: dict[object, Lambda] | None = None,
        candidate_targets: dict[object, Lambda] | None = None,
    ):
        self.general = general
        self.candidate = candidate
        self.general_targets = general_targets or {}
        self.candidate_targets = candidate_targets or {}

    def visit_Node(self, node):
        return False

    def visit_If(self, node):
        if len(node.test.uses) != 1:
            return False
        test_use = node.test.uses[0]
        return self.candidate.get(test_use, UNKNOWN) in (
            BOOLEAN_TRUE,
            BOOLEAN_FALSE,
        ) and self.general.get(test_use, UNKNOWN) not in (
            BOOLEAN_TRUE,
            BOOLEAN_FALSE,
        )

    def visit_Combination(self, node):
        cand_type = self.candidate.get(node, UNKNOWN)
        gen_type = self.general.get(node, UNKNOWN)
        if (
            cand_type != gen_type
            and cand_type is not UNKNOWN
            and not gen_type.fits(cand_type)
        ):
            return True
        call_key = node
        if node.kind in (CallKind.FULL, CallKind.GUARDED) and len(node.fun.uses) == 1:
            call_key = node.fun.uses[0]
        target = self.candidate_targets.get(call_key)
        if (
            target is not None
            and target.as_lambda() is not None
            and self.general_targets.get(call_key) is target
        ):
            # Forwarding a narrower argument can select a useful callee entry
            # even when a generic return fallback keeps the result unknown.
            for argument in node.args:
                if len(argument.uses) != 1:
                    continue
                source = argument.uses[0]
                candidate = self.candidate.get(source, UNKNOWN)
                general = self.general.get(source, UNKNOWN)
                if (
                    candidate not in (UNKNOWN, BOTTOM)
                    and candidate != general
                    and not general.fits(candidate)
                ):
                    return True
        numeric_args = (
            node.info.arguments
            if node.fun_info is RANGE_PREPARE
            and node.info is not None
            and not node.info.direct
            else node.args
        )
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.numeric
            and numeric_args
            and all(len(arg.uses) == 1 for arg in numeric_args)
        ):
            cand_args = [
                self.candidate.get(arg.uses[0], UNKNOWN) for arg in numeric_args
            ]
            gen_args = [self.general.get(arg.uses[0], UNKNOWN) for arg in numeric_args]
            if (
                all(arg.is_fixnum for arg in cand_args)
                and not all(arg.is_fixnum for arg in gen_args)
            ) or (
                all(arg.fits_float(FLOAT) for arg in cand_args)
                and not all(arg.fits_float(FLOAT) for arg in gen_args)
            ):
                return True
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and (
                node.fun_info.index_second_arg
                or node.fun_info is PROTOCOLS["_list_fill"]
            )
            and len(node.args) > 1
            and len(node.args[1].uses) == 1
        ):
            idx_use = node.args[1].uses[0]
            if (
                self.candidate.get(idx_use, UNKNOWN).is_fixnum
                and not self.general.get(idx_use, UNKNOWN).is_fixnum
            ):
                return True
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is GETSLICE
            and len(node.args) > 1
        ):
            for bound_arg in node.args[1:]:
                if len(bound_arg.uses) == 1:
                    bound_use = bound_arg.uses[0]
                    if (
                        self.candidate.get(bound_use, UNKNOWN).is_fixnum
                        and not self.general.get(bound_use, UNKNOWN).is_fixnum
                    ):
                        return True
        if is_predictable_len_call(node) and len(node.args[0].uses) == 1:
            arg_use = node.args[0].uses[0]
            if known_length_primitive(
                self.candidate.get(arg_use, UNKNOWN)
            ) is not known_length_primitive(self.general.get(arg_use, UNKNOWN)):
                return True
        if (
            node.kind is CallKind.KNOWN
            and (
                node.fun_info in _SPECIALIZABLE_CONTAINER_OPS
                or node.fun_info in (PHYSICAL_FIELD_REF, PHYSICAL_FIELD_SET)
                or (
                    node.fun_info is not None
                    and node.fun_info.name in ("_instance_field", "_set_instance_field")
                )
                or node.fun_info is PROTOCOLS["unpack"]
                or node.fun_info is UNPACK_REF
            )
            and node.args
        ):
            recv_idx = (
                1
                if node.fun_info
                in (
                    COMPARE[ast.ComparisonOperator.IN],
                    COMPARE[ast.ComparisonOperator.NOT_IN],
                )
                else 0
            )
            if recv_idx < len(node.args) and len(node.args[recv_idx].uses) == 1:
                arg_use = node.args[recv_idx].uses[0]
                cand_recv = self.candidate.get(arg_use, UNKNOWN)
                gen_recv = self.general.get(arg_use, UNKNOWN)
                if (
                    cand_recv != gen_recv
                    and cand_recv is not UNKNOWN
                    and not gen_recv.fits(cand_recv)
                    and (
                        node.fun_info
                        not in (
                            BINARY[ast.BinaryOperator.BIT_AND],
                            BINARY[ast.BinaryOperator.BIT_OR],
                            BINARY[ast.BinaryOperator.SUBTRACT],
                            BINARY[ast.BinaryOperator.BIT_XOR],
                            INPLACE_BINARY[ast.BinaryOperator.BIT_AND],
                            INPLACE_BINARY[ast.BinaryOperator.BIT_OR],
                            INPLACE_BINARY[ast.BinaryOperator.SUBTRACT],
                            INPLACE_BINARY[ast.BinaryOperator.BIT_XOR],
                        )
                        or cand_recv.fits_set()
                    )
                ):
                    return True
        if (
            node.kind in (CallKind.FULL, CallKind.GUARDED)
            and len(node.fun.uses) == 1
            and not (node.info is not None and node.info.keywords)
            and node.lexenv.policy.safety < 3
        ):
            info = node.fun.uses[0].accept(_PREDICTABLE_GLOBAL_FUN_INFO)
            if info is not None and info.transforms:
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
                            return True
        return False


def _specializes_body(
    graph: LambdaGraph,
    general: dict[object, ValueType],
    candidate: dict[object, ValueType],
    general_targets: dict[object, Lambda] | None = None,
    candidate_targets: dict[object, Lambda] | None = None,
) -> bool:
    # A context whose argument types resolve calls the general body cannot
    # resolve (methods of a typed receiver) specializes those calls.
    if (
        candidate_targets is not None
        and general_targets is not None
        and any(
            general_targets.get(node) is not target
            for node, target in candidate_targets.items()
        )
    ):
        return True
    visitor = _SpecializesBody(general, candidate, general_targets, candidate_targets)
    for block in graph.blocks:
        for node in graph.block_nodes[block]:
            if node.accept(visitor):
                return True
    return False


@dataclass(slots=True)
class EntryContext:
    """A guarded internal entry: facts valid once its parameter guards pass."""

    id: str
    parameters: tuple[ValueType, ...]
    types: dict[object, ValueType]
    targets: dict[object, Lambda]
    return_type: ValueType
    element_contracts: dict[object, object] = field(default_factory=dict)
    in_bounds_nodes: frozenset[object] = frozenset()
    escape_facts: object = None
    base_escape_facts: object = None


@dataclass(slots=True)
class LambdaFacts:
    types: dict[object, ValueType]
    targets: dict[object, Lambda]
    variants: tuple[EntryContext, ...] = ()
    return_type: ValueType = UNKNOWN
    parameters: tuple[ValueType, ...] = ()
    element_contracts: dict[object, object] = field(default_factory=dict)
    in_bounds_nodes: frozenset[object] = frozenset()
    final_bindings: tuple[GlobalVar, ...] = ()

    def ready_final_facts(self, types, targets):
        """Facts valid after the body's final-binding readiness guard."""
        ready_types, ready_targets = dict(types), dict(targets)
        for binding in self.final_bindings:
            for ref in binding.refs:
                if ref in types:
                    ready_types[ref] = FunctionType(target=binding.final_function)
                    ready_targets[ref] = binding.final_function
        return ready_types, ready_targets

    def method_return_type(self, receiver: ValueType) -> ValueType:
        """Predict a method result without narrowing its ordinary arguments.

        A method summary applies to its owning receiver class, unlike an
        ordinary callable summary. Other parameters must still admit every
        value accepted by the general entry.
        """
        result = self.return_type
        if not self.parameters:
            return result
        arguments = (receiver, *self.parameters[1:])
        for variant in self.variants:
            if (
                len(arguments) == len(variant.parameters)
                and all(
                    actual.fits(expected)
                    for actual, expected in zip(
                        arguments, variant.parameters, strict=True
                    )
                )
                and variant.return_type is not BOTTOM
                and (result is UNKNOWN or variant.return_type.fits(result))
            ):
                result = variant.return_type
        return result


def global_bindings(
    lambdas, *, unique_definitions=False
) -> dict[str, dict[str, Lambda]]:
    """Per namespace, the lambda each global name is bound to by a store of a
    freshly created function object anywhere in the module."""
    targets = defaultdict(dict)
    definitions = defaultdict(dict)
    for lambda_ in lambdas:
        visitor = _GlobalFunctionStores(
            targets[lambda_.namespace],
            definitions=definitions[lambda_.namespace] if unique_definitions else None,
        )
        for block in lambda_.component.blocks():
            for node in block.nodes():
                node.accept(visitor)
    if not unique_definitions:
        return targets
    return {
        namespace: {
            name: origin
            for name, origins in bindings.items()
            if len(origins) == 1 and (origin := next(iter(origins))) is not None
        }
        for namespace, bindings in definitions.items()
    }


class _GlobalFunctionStores(NodeVisitor):
    def __init__(
        self,
        namespace: dict[str, Lambda],
        counts: dict[str, int] | None = None,
        definitions: dict[str, set[Lambda]] | None = None,
        leaves: dict[str, GlobalVar] | None = None,
    ):
        self.namespace = namespace
        self.counts = counts
        self.definitions = definitions
        self.leaves = leaves

    def visit_Set(self, node):
        node.var.accept(
            _GlobalFunctionStore(
                self.namespace, self.counts, node, self.definitions, self.leaves
            )
        )


class _GlobalFunctionStore(LeafVisitor):
    """Records a store of a fresh function object into a module global; a
    store into a local binds no module name."""

    def __init__(
        self,
        namespace: dict[str, Lambda],
        counts: dict[str, int] | None,
        store: Set,
        definitions: dict[str, set[Lambda]] | None = None,
        leaves: dict[str, GlobalVar] | None = None,
    ):
        self.namespace = namespace
        self.counts = counts
        self.store = store
        self.definitions = definitions
        self.leaves = leaves

    def visit_GlobalVar(self, leaf):
        if self.counts is not None:
            self.counts[leaf.name] = self.counts.get(leaf.name, 0) + 1
        if self.store.value is None:
            if self.definitions is not None:
                self.definitions.setdefault(leaf.name, set()).add(None)
            return
        lambda_ = (
            leaf.final_function
            if self.store.final_binding
            else made_function(self.store.value.use())
        )
        if lambda_ is not None:
            self.namespace[leaf.name] = lambda_
            if self.leaves is not None:
                self.leaves[leaf.name] = leaf
            if self.definitions is not None and lambda_.source_name == leaf.name:
                self.definitions.setdefault(leaf.name, set()).add(lambda_)


_UNARY_CONSTANT_OPS = frozenset(
    {
        UNARY[ast.UnaryOperator.NEGATIVE],
        UNARY[ast.UnaryOperator.POSITIVE],
        UNARY[ast.UnaryOperator.INVERT],
    }
)


class _FoldUnaryConstant(ast.Visitor):
    def __init__(self, info):
        self.info = info

    def visit_Node(self, node):
        return None

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral):
        if self.info is UNARY[ast.UnaryOperator.NEGATIVE]:
            return ast.IntegerLiteral(-node.value)
        if self.info is UNARY[ast.UnaryOperator.POSITIVE]:
            return ast.IntegerLiteral(+node.value)
        if self.info is UNARY[ast.UnaryOperator.INVERT]:
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

    def visit_Cast(self, node: Cast):
        return self.visit_Lvar(node.value)

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
        info = node.fun_info
        if info in _UNARY_CONSTANT_OPS:
            if len(node.args) != 1 or len(node.args[0].uses) != 1:
                return None
            operand = node.args[0].uses[0].accept(self)
            if operand is None:
                return None
            return operand.accept(_FoldUnaryConstant(info))
        if info is BUILD_TUPLE:
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

    def visit_ListType(self, fact):
        elem = fact.element.accept(self)
        return fact if elem == fact.element else replace(fact, element=elem)

    def visit_DictType(self, fact):
        key = fact.key.accept(self)
        value = fact.value.accept(self)
        if key == fact.key and value == fact.value:
            return fact
        return replace(fact, key=key, value=value)

    def visit_SetType(self, fact):
        elem = fact.element.accept(self)
        return fact if elem == fact.element else replace(fact, element=elem)


_MUTATING_CONTAINER_METHODS = frozenset(
    {
        "append",
        "clear",
        "discard",
        "extend",
        "insert",
        "pop",
        "popitem",
        "remove",
        "setdefault",
        "update",
    }
)


class _RefMutatesContainer(NodeVisitor):
    def __init__(self, target_lvar, *, allow_setitem: bool = False):
        self.target_lvar = target_lvar
        self.allow_setitem = allow_setitem

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return False
        info = node.fun_info
        if (
            info is SETITEM
            and not self.allow_setitem
            and len(node.args) == 3
            and node.args[0] is self.target_lvar
        ):
            return True
        if info is DELITEM and len(node.args) == 2 and node.args[0] is self.target_lvar:
            return True
        return (
            info is RESOLVE_METHOD
            and node.info is not None
            and node.info.name in _MUTATING_CONTAINER_METHODS
            and len(node.args) == 1
            and node.args[0] is self.target_lvar
        )


def _global_container_is_mutated(leaf: GlobalVar) -> bool:
    for ref in leaf.refs:
        if (
            ref.lvar is not None
            and ref.dest is not None
            and ref.dest.accept(_RefMutatesContainer(ref.lvar))
        ):
            return True
    return False


def _default_param_container_is_mutated(var: LambdaVar) -> bool:
    for ref in var.refs:
        if (
            ref.lvar is not None
            and ref.dest is not None
            and ref.dest.accept(_RefMutatesContainer(ref.lvar, allow_setitem=True))
        ):
            return True
    return False


_WIDEN_USER_CONSTANT_TYPE = _WidenUserConstantType()


def _widen_user_constant_type(vtype: ValueType) -> ValueType:
    return vtype.accept(_WIDEN_USER_CONSTANT_TYPE)


class _GlobalCalleeRefName(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_GlobalVar(self, leaf: GlobalVar):
        return leaf.name


_GLOBAL_CALLEE_REF_NAME = _GlobalCalleeRefName()


class _ProtocolSingletonStoreType(NodeVisitor):
    def __init__(
        self,
        var_name: str,
        class_targets: dict[str, ClassConstructorTarget] | None,
    ):
        self.var_name = var_name
        self.class_targets = class_targets

    def visit_Node(self, node):
        return UNKNOWN

    def visit_Combination(self, node: Combination):
        if node.args:
            return UNKNOWN
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.derive_type is not None
        ):
            derived = node.fun_info.derive_type(node)
            return derived.widen_constant() if derived is not None else UNKNOWN
        if (
            node.kind in (CallKind.FULL, CallKind.GUARDED)
            and self.class_targets is not None
            and len(node.fun.uses) == 1
        ):
            callee_name = node.fun.uses[0].accept(_GLOBAL_CALLEE_REF_NAME)
            if callee_name is not None and callee_name in self.class_targets:
                cls_target = self.class_targets[callee_name]
                if cls_target.new_target is None and cls_target.init_target is None:
                    return UniqueObjectType(self.var_name)
        return UNKNOWN


class _UnreboundBuiltinCalleeName(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_GlobalVar(self, leaf: GlobalVar):
        return leaf.name if not leaf.sets else None


_UNREBOUND_BUILTIN_CALLEE_NAME = _UnreboundBuiltinCalleeName()

_FROZEN_CONTAINER_BUILTIN_TYPES: dict[str, ValueType] = {
    "frozenset": FROZENSET_TYPE,
    "tuple": TupleType(),
    "bytes": BYTES_TYPE,
}


class _FrozenContainerStoreType(NodeVisitor, LeafVisitor):
    def __init__(self):
        self._visited: set[LambdaVar] = set()

    def visit_Node(self, node):
        return UNKNOWN

    def visit_Leaf(self, leaf):
        return UNKNOWN

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Cast(self, node: Cast):
        return node.value.uses[0].accept(self) if len(node.value.uses) == 1 else UNKNOWN

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf in self._visited or len(leaf.sets) != 1:
            return UNKNOWN
        self._visited.add(leaf)
        value = leaf.sets[0].value
        return (
            value.uses[0].accept(self)
            if value is not None and len(value.uses) == 1
            else UNKNOWN
        )

    def visit_Combination(self, node: Combination):
        if (
            node.kind not in (CallKind.FULL, CallKind.GUARDED)
            or len(node.fun.uses) != 1
        ):
            return UNKNOWN
        callee_name = node.fun.uses[0].accept(_UNREBOUND_BUILTIN_CALLEE_NAME)
        if callee_name is None:
            return UNKNOWN
        return _FROZEN_CONTAINER_BUILTIN_TYPES.get(callee_name, UNKNOWN)


_READONLY_GLOBALS_METHODS = frozenset({"get", "keys", "values", "items", "copy"})

_IMPLICIT_MODULE_GLOBALS = frozenset(
    {
        "__doc__",
        "__speed__",
        "__safety__",
        "__debug_level__",
        "__mule_builtin_implementation__",
    }
)


class _IsResolvedGetOnSysModules(NodeVisitor):
    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is RESOLVE_METHOD
            and node.info is not None
            and node.info.name == "get"
            and len(node.args) == 1
            and len(node.args[0].uses) == 1
            and node.args[0].uses[0].accept(_IS_SYS_MODULES_DICT)
        )


_IS_RESOLVED_GET_ON_SYS_MODULES = _IsResolvedGetOnSysModules()


class _IsSysModulesEntry(NodeVisitor, LeafVisitor):
    def __init__(self):
        self._visited: set[LambdaVar] = set()

    def visit_Node(self, node):
        return False

    def visit_Leaf(self, leaf):
        return False

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf in self._visited or len(leaf.sets) != 1:
            return False
        self._visited.add(leaf)
        value = leaf.sets[0].value
        return (
            value.uses[0].accept(self)
            if value is not None and len(value.uses) == 1
            else False
        )

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return False
        if node.fun_info is GETITEM and len(node.args) == 2:
            return len(node.args[0].uses) == 1 and node.args[0].uses[0].accept(
                _IS_SYS_MODULES_DICT
            )
        if node.fun_info is CALL_RESOLVED and len(node.args) >= 1:
            return len(node.args[0].uses) == 1 and node.args[0].uses[0].accept(
                _IS_RESOLVED_GET_ON_SYS_MODULES
            )
        return False


class _IsSysModulesDict(NodeVisitor, LeafVisitor):
    def __init__(self):
        self._visited: set[BasicVar] = set()

    def visit_Node(self, node):
        return False

    def visit_Leaf(self, leaf):
        return False

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def _visit_var(self, leaf: BasicVar) -> bool:
        if leaf in self._visited:
            return False
        value_sets = [s for s in leaf.sets if s.value is not None]
        if len(value_sets) != 1:
            return False
        self._visited.add(leaf)
        value = value_sets[0].value
        return (
            value.uses[0].accept(self)
            if value is not None and len(value.uses) == 1
            else False
        )

    def visit_GlobalVar(self, leaf: GlobalVar):
        return self._visit_var(leaf)

    def visit_LambdaVar(self, leaf: LambdaVar):
        return self._visit_var(leaf)

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is GETATTR
            and node.info is not None
            and node.info.name == "modules"
            and len(node.args) == 1
            and len(node.args[0].uses) == 1
        ):
            return node.args[0].uses[0].accept(_ImportedModuleOrigin()) == "sys"
        return False


_IS_SYS_MODULES_DICT = _IsSysModulesDict()


class _IsDirectCallResolved(NodeVisitor):
    def __init__(self, resolved_lvar, receiver_lvar):
        self.resolved_lvar = resolved_lvar
        self.receiver_lvar = receiver_lvar

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is CALL_RESOLVED
            and len(node.args) >= 1
            and node.args[0] is self.resolved_lvar
            and self.receiver_lvar not in node.args[1:]
        )


class _ReadOnlyGlobalsConsumer(NodeVisitor, LeafVisitor):
    def __init__(self, target_lvar):
        self.target_lvar = target_lvar

    def visit_Node(self, node):
        return False

    def visit_Set(self, node: Set):
        return node.value is self.target_lvar and node.var.accept(self)

    def visit_Leaf(self, leaf):
        return False

    def visit_LambdaVar(self, leaf: LambdaVar):
        if len(leaf.sets) != 1 or (leaf.home is not None and leaf in leaf.home.vars):
            return False
        return all(
            ref.lvar is not None
            and ref.lvar.dest is not None
            and ref.lvar.dest.accept(_ReadOnlyGlobalsConsumer(ref.lvar))
            for ref in leaf.refs
        )

    def visit_Combination(self, node: Combination):
        if (
            node.kind in (CallKind.FULL, CallKind.GUARDED)
            and len(node.args) == 1
            and node.args[0] is self.target_lvar
            and len(node.fun.uses) == 1
            and node.fun.uses[0].accept(_UNREBOUND_BUILTIN_CALLEE_NAME) == "globals"
        ):
            if node.lvar is None or node.lvar.dest is None:
                return True
            return node.lvar.dest.accept(_ReadOnlyGlobalsConsumer(node.lvar))
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return False
        info = node.fun_info
        if info is PROTOCOLS["globals_dict"] and len(node.args) == 1:
            if node.args[0] is not self.target_lvar:
                return False
            if node.lvar is None or node.lvar.dest is None:
                return True
            return node.lvar.dest.accept(_ReadOnlyGlobalsConsumer(node.lvar))
        if info is GETITEM and len(node.args) == 2:
            return node.args[0] is self.target_lvar
        if info in _CONTAINMENT_COMPARE and len(node.args) == 2:
            return node.args[1] is self.target_lvar
        if info is ITERATE and len(node.args) == 1:
            return node.args[0] is self.target_lvar
        if info is LEN and len(node.args) == 1:
            return node.args[0] is self.target_lvar
        if (
            info is RESOLVE_METHOD
            and node.info is not None
            and node.info.name in _READONLY_GLOBALS_METHODS
            and len(node.args) == 1
            and node.args[0] is self.target_lvar
            and node.lvar is not None
            and node.lvar.dest is not None
        ):
            return node.lvar.dest.accept(
                _IsDirectCallResolved(node.lvar, self.target_lvar)
            )
        return False


class _MutatesModuleNamespace(NodeVisitor):
    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN and node.fun_info is not None:
            info = node.fun_info
            if info in (IMPORT_STAR, PROTOCOLS["call_with_eval_context"]):
                return True
            if info is CURRENT_GLOBALS:
                if node.lvar is None or node.lvar.dest is None:
                    return False
                return not node.lvar.dest.accept(_ReadOnlyGlobalsConsumer(node.lvar))
            if info in (SETATTR, DELATTR) and len(node.args) >= 1:
                return len(node.args[0].uses) == 1 and node.args[0].uses[0].accept(
                    _IsSysModulesEntry()
                )
            if (
                info is GETATTR
                and node.info is not None
                and node.info.name == "__dict__"
                and len(node.args) == 1
                and len(node.args[0].uses) == 1
                and node.args[0].uses[0].accept(_IsSysModulesEntry())
            ):
                if node.lvar is None or node.lvar.dest is None:
                    return False
                return not node.lvar.dest.accept(_ReadOnlyGlobalsConsumer(node.lvar))
            return False
        if node.kind in (CallKind.FULL, CallKind.GUARDED) and len(node.fun.uses) == 1:
            callee = node.fun.uses[0].accept(_GLOBAL_CALLEE_REF_NAME)
            if callee in ("eval", "exec"):
                return True
            if (
                callee in ("setattr", "delattr")
                and node.args
                and len(node.args[0].uses) == 1
                and node.args[0].uses[0].accept(_IsSysModulesEntry())
            ):
                return True
        return False


_MUTATES_MODULE_NAMESPACE = _MutatesModuleNamespace()


def has_module_namespace_mutations(lambdas) -> bool:
    visited_components = set()
    for lambda_ in lambdas:
        component = lambda_.component
        if component is None or component in visited_components:
            continue
        visited_components.add(component)
        if component.mutates_module_namespace:
            return True
        for block in component.blocks():
            for node in block.nodes():
                if node.accept(_MUTATES_MODULE_NAMESPACE):
                    return True
    return False


_SAFE_TOPLEVEL_KNOWN_CALLS = frozenset(
    {
        BUILD_LIST,
        BUILD_TUPLE,
        CLASS_DELETE,
        CLASS_STORE,
        CURRENT_GLOBALS,
        HAS_BINDING,
        IMPORT,
        MAKE_CLASS,
        MAKE_FUNCTION,
        PROTOCOLS["extend_arguments"],
        *COMPARE.values(),
        *UNARY.values(),
    }
)


def _may_call_module_function(node) -> bool:
    return node.accept(_MAY_CALL_MODULE_FUNCTION)


class _MayCallModuleFunction(NodeVisitor):
    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN:
            if node.fun_info in (
                PROTOCOLS["call_expanded"],
                PROTOCOLS["call_expanded_positional"],
                CALL_RESOLVED,
                RESOLVE_METHOD,
                RESOLVE_SUPER_METHOD,
            ):
                return True
            return node.fun_info not in _SAFE_TOPLEVEL_KNOWN_CALLS
        return True


_MAY_CALL_MODULE_FUNCTION = _MayCallModuleFunction()


def _reachable_without(start, target, excluded=None) -> bool:
    visited = set()
    if excluded is not None:
        visited.add(excluded)
    stack = [start]
    while stack:
        current = stack.pop()
        if current in visited:
            continue
        if current is target:
            return True
        visited.add(current)
        stack.extend(current.succ)
    return False


class _EnclosedLambdas(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return ()

    def visit_Leaf(self, leaf):
        return ()

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Lambda(self, leaf: Lambda):
        return (leaf,)

    def visit_Enclose(self, node: Enclose):
        return tuple(node.funs)


_ENCLOSED_LAMBDAS = _EnclosedLambdas()


def _has_callable_enclosing_lambda(
    ref: Ref, entry: Lambda, callable_lambdas: set[Lambda]
) -> bool:
    cur = ref.home_lambda
    visited: set[Lambda] = set()
    while cur is not None and cur is not entry and cur not in visited:
        if cur in callable_lambdas:
            return True
        visited.add(cur)
        if cur.refs:
            cur = cur.refs[0].home_lambda
        else:
            break
    return False


def _unconditional_toplevel_stores(entry: Lambda) -> set[Set]:
    component = entry.component
    if component is None or entry.bind is None or entry.return_ is None:
        return set()
    entry_block = entry.bind.block
    return_block = entry.return_.block
    if entry_block is None or return_block is None:
        return set()
    blocks = tuple(component.blocks())
    if return_block not in blocks:
        return set()
    unconditional_blocks = set()
    for candidate in blocks:
        if block_exception_target(candidate) is not None:
            continue
        if candidate is not entry_block and _reachable_without(
            entry_block, return_block, excluded=candidate
        ):
            continue
        in_cycle = any(_reachable_without(succ, candidate) for succ in candidate.succ)
        if not in_cycle:
            unconditional_blocks.add(candidate)
    unconditional_stores: set[Set] = set()
    seen_refs: set[BasicVar] = set()
    enclosed_lambdas: set[Lambda] = set()
    callable_lambdas: set[Lambda] = set()
    for block in blocks:
        in_unconditional = block in unconditional_blocks
        for node in block.nodes():
            enclosed_lambdas.update(node.accept(_ENCLOSED_LAMBDAS))
            if in_unconditional:
                store_var = node.accept(_SET_GLOBAL_VAR)
                if (
                    store_var is not None
                    and store_var not in seen_refs
                    and not (
                        callable_lambdas
                        and any(
                            _has_callable_enclosing_lambda(ref, entry, callable_lambdas)
                            for ref in store_var.refs
                        )
                    )
                ):
                    unconditional_stores.add(node)
            ref_var = node.accept(_REF_GLOBAL_VAR)
            if ref_var is not None:
                seen_refs.add(ref_var)
            if enclosed_lambdas and _may_call_module_function(node):
                callable_lambdas.update(enclosed_lambdas)
    return unconditional_stores


class _SetGlobalVar(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Set(self, node: Set):
        return node.var.accept(self)

    def visit_GlobalVar(self, leaf: GlobalVar):
        return leaf


_SET_GLOBAL_VAR = _SetGlobalVar()


class _RefGlobalVar(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_GlobalVar(self, leaf: GlobalVar):
        return leaf


_REF_GLOBAL_VAR = _RefGlobalVar()


class _GlobalConstantStores(NodeVisitor, LeafVisitor):
    def __init__(
        self,
        constants: dict[GlobalVar, ValueType],
        *,
        is_protocol: bool,
        class_targets: dict[str, ClassConstructorTarget] | None = None,
        unconditional_stores: set[Set] | None = None,
    ):
        self.constants = constants
        self.is_protocol = is_protocol
        self.class_targets = class_targets
        self.unconditional_stores = unconditional_stores
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
        if not self.is_protocol and leaf.name in _IMPLICIT_MODULE_GLOBALS:
            return
        home = store.home_lambda
        if home is None or home.kind is not FunctionalKind.TOPLEVEL:
            return
        if (
            self.unconditional_stores is not None
            and store not in self.unconditional_stores
        ):
            return
        annotation_expr = home.annotation_expressions.get(leaf.name)
        is_final = annotation_expr is not None and annotation_expr.accept(
            _IS_FINAL_ANNOTATION
        )
        literal = store.value.accept(_MadeConstant())
        if literal is not None:
            vtype = constant_type(literal)
        elif len(store.value.uses) == 1:
            vtype = store.value.uses[0].accept(_FrozenContainerStoreType())
            if vtype is UNKNOWN and self.is_protocol:
                vtype = store.value.uses[0].accept(
                    _ProtocolSingletonStoreType(leaf.name, self.class_targets)
                )
        else:
            return
        vtype = vtype.widen_mutable_containers()
        if vtype is UNKNOWN:
            return
        if self.is_protocol:
            self.constants[leaf] = vtype
            return
        if home.policy.safety < 3 and (is_final or not vtype.fits_none(NONE_TYPE)):
            self.constants[leaf] = (
                vtype if is_final else _widen_user_constant_type(vtype)
            )


class _ClassBaseNames(NodeVisitor, LeafVisitor):
    """Extract base class global names from the ``bases`` operand of ``%make_class``."""

    def visit_Node(self, node) -> tuple[str, ...]:
        return ()

    def visit_Leaf(self, leaf) -> tuple[str, ...]:
        return ()

    def visit_Ref(self, node: Ref) -> tuple[str, ...]:
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar) -> tuple[str, ...]:
        if (
            len(leaf.sets) == 1
            and leaf.sets[0].value is not None
            and len(leaf.sets[0].value.uses) == 1
        ):
            return leaf.sets[0].value.uses[0].accept(self)
        return ()

    def visit_Combination(self, node: Combination) -> tuple[str, ...]:
        if node.kind is CallKind.KNOWN and node.fun_info is BUILD_TUPLE:
            return tuple(
                base_name
                for arg in node.args
                if len(arg.uses) == 1
                and (base_name := arg.uses[0].accept(_GLOBAL_CALLEE_REF_NAME))
                is not None
            )
        return ()


_CLASS_BASE_NAMES = _ClassBaseNames()


class _CompleteReferenceSlots(ast.Visitor):
    """Whether a declaration's physical names match its compiler field prefix."""

    def visit_Node(self, node):
        return False

    def visit_TextLiteral(self, node):
        # Runtime class creation adds a mangled slot after compiler fields.
        # Until that extra field is modeled, descendants cannot use our prefix.
        return not (node.value.startswith("__") and not node.value.endswith("__"))

    def visit_Tuple(self, node):
        return all(element.accept(self) for element in node.elements)

    def visit_List(self, node):
        return all(element.accept(self) for element in node.elements)


_COMPLETE_REFERENCE_SLOTS = _CompleteReferenceSlots()


class _ModuleClassCollector(NodeVisitor):
    """Collect module-level class definitions and their method lambdas."""

    def __init__(self):
        self.methods: dict[str, dict[str, Lambda]] = defaultdict(dict)
        self.public_names: dict[str, str] = {}
        self.bases: dict[str, tuple[str, ...]] = {}
        # Identities whose definitions pass class keywords (a metaclass
        # among them), so calling the class may construct anything.
        self.keyword_classes: set[str] = set()
        self.definitions: dict[str, int] = defaultdict(int)
        # Names a class body binds to anything other than a plain function.
        self.other_bindings: dict[str, set[str]] = defaultdict(set)
        self.fields: dict[str, tuple[str, ...]] = {}
        self.native_classes: set[str] = set()
        self.unproven_slot_classes: set[str] = set()

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return
        if node.fun_info is CLASS_STORE and len(node.args) == 2:
            # (class namespace, value): a function stored in a class body.
            info = linked(node.info, node, "info")
            value = node.args[1]
            if info.name in ("__slots__", "__mule_slots__"):
                declaration = ir1opt.bound_constant_literal(value)
                if declaration is None or not declaration.accept(
                    _COMPLETE_REFERENCE_SLOTS
                ):
                    self.unproven_slot_classes.add(info.identity)
            lam = made_function(value.uses[0]) if len(value.uses) == 1 else None
            if lam is not None and info.name not in (
                "__init_subclass__",
                "__class_getitem__",
            ):
                self.methods[info.identity][info.name] = lam
            else:
                # Any other binding (a decorated function, a descriptor, a
                # rebinding, or an implicit classmethod) shadows a method of
                # the same name.
                self.methods[info.identity].pop(info.name, None)
                self.other_bindings[info.identity].add(info.name)
        elif node.fun_info is CLASS_DELETE:
            info = linked(node.info, node, "info")
            self.methods[info.identity].pop(info.name, None)
        elif node.fun_info is MAKE_CLASS:
            info = linked(node.info, node, "class definition")
            if info.class_id is not None:
                self.native_classes.add(info.identity)
            self.fields[info.identity] = info.fields
            self.public_names[info.identity] = info.public_name
            self.definitions[info.public_name] += 1
            if info.has_keywords:
                self.keyword_classes.add(info.identity)
            self.bases[info.public_name] = (
                node.args[0].uses[0].accept(_CLASS_BASE_NAMES)
                if node.args and len(node.args[0].uses) == 1
                else ()
            )
            if len(self.bases[info.public_name]) != info.base_count:
                # A base the analysis cannot name may define anything.
                self.keyword_classes.add(info.identity)


def _collect_module_classes(
    lambdas,
) -> dict[str, dict[str, ClassConstructorTarget]]:
    return _collect_class_tables(lambdas)[0]


@dataclass(slots=True)
class ClassFacts:
    """What analysis knows about one user class whose instances it types:
    its resolved methods (including inherited ones defined in the module),
    its single-inheritance chain of class identities, the names shadowed on
    the class by non-method bindings, and its guarded instance field types."""

    identity: str
    methods: dict[str, Lambda]
    fields: dict[str, ValueType]
    chain: tuple[str, ...] = ()
    shadowed: frozenset[str] = frozenset()
    public_name: str = ""
    is_protocol: bool = False
    fixed_slots: tuple[str, ...] = ()
    constructor_proven: bool = True
    widened_fields: set[str] = field(default_factory=set)


class _ProtocolSelfPrediction(ValueTypeVisitor):
    """Resolve instance hints through the method's actual declaring owner.

    General entries still treat this as a prediction, not a runtime layout
    proof. Semantic hints without an instance identity retain their meaning.
    """

    def __init__(self, owner: ClassFacts | None):
        self.owner = owner

    def visit_ValueType(self, fact):
        return fact

    def visit_InstanceType(self, fact):
        if self.owner is None:
            return UNKNOWN
        return InstanceType(
            self.owner.identity,
            may_be_unbound=fact.may_be_unbound,
            fixed_slots=self.owner.fixed_slots,
        )


class _SanitizeInstanceFieldType(ValueTypeVisitor):
    """Restricts a value type stored into an instance field to shapes whose
    consumers guard their runtime operations (instance/None unions, fixnums/ints,
    strings, and dicts)."""

    def visit_ValueType(self, fact):
        return UNKNOWN

    def visit_BottomType(self, fact):
        return BOTTOM

    def visit_NoneType(self, fact):
        return UNKNOWN if fact.may_be_unbound else NONE_TYPE

    def visit_IntegerType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        if fact.is_fixnum:
            return IntegerType(FIXNUM_MIN, FIXNUM_MAX)
        return PYTHON_INTEGER

    def visit_BooleanType(self, fact):
        return UNKNOWN if fact.may_be_unbound else BOOLEAN

    def visit_PythonIntegerType(self, fact):
        return UNKNOWN if fact.may_be_unbound else PYTHON_INTEGER

    def visit_TextType(self, fact):
        return UNKNOWN if fact.may_be_unbound else TEXT_TYPE

    def visit_InstanceType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        return fact.as_optional()

    def visit_UnionType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        return fact.map(lambda member: member.accept(self))

    def visit_DictType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        if fact.value is BOTTOM:
            return DictType(BOTTOM, BOTTOM, False, fact.proven)
        key = fact.key.accept(self) if fact.key is not BOTTOM else BOTTOM
        val = fact.value.accept(self)
        if val is BOTTOM or val is NONE_TYPE:
            return DICT_TYPE if fact.proven else UNKNOWN
        if val is UNKNOWN and not fact.proven:
            return UNKNOWN
        return DictType(key, val, False, fact.proven)


class _ReadLambdaVar(NodeVisitor, LeafVisitor):
    """Single-assignment local variable origin of a reference."""

    def __init__(self, seen=frozenset()):
        self.seen = seen

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf.indirect:
            return None
        definitions = [store.value for store in leaf.sets if store.value is not None]
        if len(definitions) > 1:
            return None
        if (
            leaf not in self.seen
            and len(definitions) == 1
            and len(definitions[0].uses) == 1
        ):
            origin = definitions[0].uses[0].accept(_ReadLambdaVar(self.seen | {leaf}))
            if origin is not None:
                return origin
        return leaf


_READ_LAMBDA_VAR = _ReadLambdaVar()


class _GetattrOnInstance(NodeVisitor, LeafVisitor):
    def __init__(self, seen=frozenset()):
        self.seen = seen

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        definitions = [store.value for store in leaf.sets if store.value is not None]
        if leaf.indirect or leaf in self.seen or not definitions:
            return None
        sub = _GetattrOnInstance(self.seen | {leaf})
        if len(definitions) == 1 and len(definitions[0].uses) == 1:
            return definitions[0].uses[0].accept(sub)
        first_attr = None
        first_recv_var = None
        for defn in definitions:
            if not defn.uses:
                return None
            for use in defn.uses:
                attr = use.accept(sub)
                if attr is None:
                    return None
                inst_lvar, field_name = attr
                recv_var = inst_lvar.use().accept(_READ_LAMBDA_VAR)
                if recv_var is None:
                    return None
                if first_attr is None:
                    first_attr = attr
                    first_recv_var = recv_var
                elif field_name != first_attr[1] or recv_var is not first_recv_var:
                    return None
        return first_attr

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN and node.fun_info is PHYSICAL_FIELD_REF:
            return node.args[0], linked(node.info, node, "physical field").name
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is GETATTR
            and len(node.args) == 1
            and node.info is not None
        ):
            return node.args[0], node.info.name
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name == "_instance_field"
            and len(node.args) == 2
        ):
            name = ir1opt.bound_constant_text(node.args[1])
            if name is None:
                name = ir1opt.lvar_type(node.args[1]).interned_text()
            if name is not None:
                return node.args[0], name
        return None


_GETATTR_ON_INSTANCE = _GetattrOnInstance()


_SANITIZE_INSTANCE_FIELD_TYPE = _SanitizeInstanceFieldType()


class _SanitizeProtocolFieldType(_SanitizeInstanceFieldType):
    def visit_IntegerType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        if fact.is_fixnum:
            return fact
        return PYTHON_INTEGER

    def visit_ListType(self, fact):
        if fact.may_be_unbound or fact.element is BOTTOM:
            return UNKNOWN
        elem = fact.element.accept(self)
        return ListType(elem)


_SANITIZE_PROTOCOL_FIELD_TYPE = _SanitizeProtocolFieldType()


class _StorageReadFromInstanceField(NodeVisitor, LeafVisitor):
    def __init__(self, seen=frozenset()):
        self.seen = seen

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        definitions = [store.value for store in leaf.sets if store.value is not None]
        if leaf.indirect or leaf in self.seen or not definitions:
            return None
        sub = _StorageReadFromInstanceField(self.seen | {leaf})
        field_name = None
        for defn in definitions:
            if not defn.uses:
                return None
            for use in defn.uses:
                name = use.accept(sub)
                if name is None or (field_name is not None and name != field_name):
                    return None
                field_name = name
        return field_name

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name == "_storage_read"
            and len(node.args) == 2
        ):
            attr = node.args[0].use().accept(_GETATTR_ON_INSTANCE)
            if attr is not None:
                return attr[1]
        return None


_STORAGE_READ_FROM_INSTANCE_FIELD = _StorageReadFromInstanceField()


class _EmptyListCopiedFromField(NodeVisitor, LeafVisitor):
    def __init__(self, field_name: str):
        self.field_name = field_name
        self.seen: set[LambdaVar] = set()

    def visit_Node(self, node):
        return False

    def visit_Leaf(self, leaf):
        return False

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        definitions = [store for store in leaf.sets if store.value is not None]
        if (
            leaf.indirect
            or leaf in self.seen
            or len(definitions) != 1
            or len(definitions[0].value.uses) != 1
        ):
            return False
        init_node = definitions[0].value.uses[0]
        if not init_node.accept(_IS_EMPTY_BUILD_LIST):
            # A single-read forwarding local does not add another mutation
            # path. Deletion ends its lifetime but does not define a new value.
            # A deletion also emits a resultless boundness-check reference.
            # It cannot expose the list; keep that check and count value uses.
            value_refs = [ref for ref in leaf.refs if ref.lvar is not None]
            if len(value_refs) != 1:
                return False
            self.seen.add(leaf)
            try:
                return init_node.accept(self)
            finally:
                self.seen.remove(leaf)
        saw_append = False
        for ref in leaf.refs:
            if ref.lvar is None or ref.lvar.dest is None:
                continue
            dest = ref.lvar.dest
            copied = dest.accept(_AppendsFromField(ref.lvar, self.field_name))
            if copied is False:
                return False
            if copied is True:
                saw_append = True
        return saw_append


class _IsEmptyBuildList(NodeVisitor):
    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is BUILD_LIST
            and len(node.args) == 0
        )


_IS_EMPTY_BUILD_LIST = _IsEmptyBuildList()


class _AppendsFromField(NodeVisitor):
    def __init__(self, list_lvar, field_name: str):
        self.list_lvar = list_lvar
        self.field_name = field_name

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name == "_storage_append"
            and len(node.args) == 2
            and node.args[0] is self.list_lvar
        ):
            src_field = node.args[1].use().accept(_STORAGE_READ_FROM_INSTANCE_FIELD)
            return src_field == self.field_name
        return None


class _DependsOnVars(NodeVisitor, LeafVisitor):
    """Whether a value may derive from one of ``targets``. A variable whose
    definitions cannot all be followed may derive from any of them."""

    def __init__(self, targets: set[LambdaVar]):
        self.targets = targets
        self.seen: set[LambdaVar] = set()

    def visit_Node(self, node):
        return False

    def visit_Leaf(self, leaf):
        return False

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf in self.targets:
            return True
        if leaf in self.seen:
            return False
        if leaf.indirect:
            return True
        self.seen.add(leaf)
        return any(
            store.value is None or any(use.accept(self) for use in store.value.uses)
            for store in leaf.sets
        )

    def visit_Combination(self, node: Combination):
        return any(use.accept(self) for arg in node.args for use in arg.uses)


class _DependsOnInstanceField(NodeVisitor, LeafVisitor):
    def __init__(self, field_name: str):
        self.field_name = field_name
        self.seen: set[LambdaVar] = set()

    def visit_Node(self, node):
        return False

    def visit_Leaf(self, leaf):
        return False

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf.indirect or leaf in self.seen:
            return False
        self.seen.add(leaf)
        definitions = [store.value for store in leaf.sets if store.value is not None]
        return any(use.accept(self) for defn in definitions for use in defn.uses)

    def visit_Combination(self, node: Combination):
        field = node.accept(_GETATTR_ON_INSTANCE)
        if field is not None:
            return field[1] == self.field_name
        return any(use.accept(self) for arg in node.args for use in arg.uses)


class _CollectSetattrSites(NodeVisitor):
    def __init__(
        self,
        sites: list[Combination],
        setitems: list[Combination],
        list_mutations: list[Combination] | None = None,
    ):
        self.sites = sites
        self.setitems = setitems
        self.list_mutations = list_mutations if list_mutations is not None else []

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN and (
            (node.fun_info is SETATTR and len(node.args) == 2 and node.info is not None)
            or (node.fun_info is PHYSICAL_FIELD_SET and len(node.args) == 2)
            or (
                node.fun_info is not None
                and node.fun_info.name == "_set_instance_field"
                and len(node.args) == 3
            )
        ):
            self.sites.append(node)
        elif (
            node.kind is CallKind.KNOWN
            and node.fun_info is PROTOCOLS.get("setitem")
            and len(node.args) == 3
        ):
            self.setitems.append(node)
        elif (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name in ("_storage_append", "_storage_write")
            and len(node.args) in (2, 3)
        ):
            self.list_mutations.append(node)


class _InstanceClasses(ValueTypeVisitor):
    """The complete class alternatives of an instance receiver fact."""

    def visit_ValueType(self, fact):
        return ()

    def visit_InstanceType(self, fact):
        return (fact.identity,) if not fact.may_be_unbound else ()

    def visit_UnionType(self, fact):
        if fact.may_be_unbound:
            return ()
        alternatives = [member.accept(self) for member in fact.members]
        if not all(alternatives):
            return ()
        return tuple(dict.fromkeys(cid for group in alternatives for cid in group))


_INSTANCE_CLASSES = _InstanceClasses()


def _entry_narrowed_parameters(
    lam: Lambda,
    ctx_name: str,
    parameters: dict[tuple[Lambda, str], tuple[ValueType, ...]],
) -> set[LambdaVar]:
    """The non-receiver parameters whose type in entry ``ctx_name`` is
    narrower than in the general entry that every other call reaches."""
    if ctx_name == "general":
        return set()
    return {
        var
        for var, entry_p, general_p in zip(
            lam.vars[1:],
            parameters.get((lam, ctx_name), ())[1:],
            parameters.get((lam, "general"), ())[1:],
            strict=False,
        )
        if entry_p != general_p
    }


def _update_class_field_types(
    class_facts: dict[str, ClassFacts],
    setattr_sites: list[tuple[Lambda, Combination]],
    setitem_sites: list[tuple[Lambda, Combination]],
    bodies: dict[tuple[Lambda, str], Body],
    parameters: dict[tuple[Lambda, str], tuple[ValueType, ...]],
    list_mutation_sites: list[tuple[Lambda, Combination]] = (),
) -> bool:
    if not class_facts or not setattr_sites:
        return False
    direct_stores: dict[str, dict[str, ValueType]] = defaultdict(dict)
    unattributed_stores: dict[str, ValueType] = {}
    stored_field_targets: dict[tuple[Lambda, LambdaVar], list[tuple[Lvar, str]]] = (
        defaultdict(list)
    )
    ambiguous_fields: set[str] = set()
    unknown_field_write = False
    for lam, node in setattr_sites:
        physical = (
            node.fun_info is not None and node.fun_info.name == "_set_instance_field"
        )
        field_name = (
            ir1opt.bound_constant_text(node.args[1]) if physical else node.info.name
        )
        if field_name is None:
            unknown_field_write = True
            continue
        recv_use = node.args[0].use()
        val_use = node.args[2 if physical else 1].use()
        stored_var = val_use.accept(_READ_LAMBDA_VAR)
        if stored_var is not None:
            stored_field_targets[(lam, stored_var)].append((node.args[0], field_name))
        found_context = False
        saw_typed_receiver = False
        best_stores: dict[str, ValueType] = {}
        unattributed_type = BOTTOM
        for ctx_name in ("observed", "fixnum", "general"):
            body = bodies.get((lam, ctx_name))
            if body is None:
                continue
            recv_fact = body.facts.get(recv_use, NO_FACT)
            val_body = body
            # The observed and fixnum entries are guarded specializations; any
            # call outside their parameter domains, including callers this
            # analysis never saw, runs the general entry. A field fact must
            # cover every store, so a value derived from a parameter those
            # entries narrow takes its general-entry fact, as SBCL's
            # propagate-local-call-args uses call-site argument types only
            # for a lambda without an external entry.
            narrowed = _entry_narrowed_parameters(lam, ctx_name, parameters)
            if (ctx_name == "observed" and lam.signature.default_parameters) or (
                narrowed and val_use.accept(_DependsOnVars(narrowed))
            ):
                val_body = bodies.get((lam, "general")) or body
            val_fact = val_body.facts.get(val_use, NO_FACT)
            if recv_fact.type is BOTTOM or val_fact.type is BOTTOM:
                continue
            found_context = True
            unattributed_type = unattributed_type.join(val_fact.type)
            for recv_cls in recv_fact.type.accept(_INSTANCE_CLASSES):
                if recv_cls in class_facts:
                    saw_typed_receiver = True
                    if recv_cls not in best_stores or best_stores[recv_cls] is UNKNOWN:
                        best_stores[recv_cls] = val_fact.type
        for recv_cls, stored_type in best_stores.items():
            cur_type = class_facts[recv_cls].fields.get(field_name, BOTTOM)
            if (
                stored_type is UNKNOWN
                and cur_type is not BOTTOM
                and val_use.accept(_GETATTR_ON_INSTANCE) is not None
                and not cur_type.fits(PYTHON_INTEGER)
            ):
                stored_type = cur_type
            elif (
                stored_type == LIST_TYPE
                and cur_type is BOTTOM
                and val_use.accept(_EmptyListCopiedFromField(field_name))
            ):
                stored_type = EMPTY_LIST_TYPE
            if stored_type is not UNKNOWN or (
                val_use.accept(_GETATTR_ON_INSTANCE) is None
                and not (
                    cur_type is BOTTOM
                    and val_use.accept(_DependsOnInstanceField(field_name))
                )
            ):
                prev = direct_stores[recv_cls].get(field_name, BOTTOM)
                direct_stores[recv_cls][field_name] = prev.join(stored_type)
        # Without receiver identity, this value can reach any existing candidate
        # for the attribute. It widens those candidates without creating owners.
        if found_context and not saw_typed_receiver:
            previous = unattributed_stores.get(field_name, BOTTOM)
            unattributed_stores[field_name] = previous.join(unattributed_type)

    for lam, node in setitem_sites:
        attr = node.args[0].use().accept(_GETATTR_ON_INSTANCE)
        if attr is not None:
            targets = (attr,)
        else:
            target_var = node.args[0].use().accept(_READ_LAMBDA_VAR)
            targets = (
                tuple(stored_field_targets.get((lam, target_var), ()))
                if target_var is not None
                else ()
            )
        if not targets:
            continue
        key_use = node.args[1].use()
        val_use = node.args[2].use()
        for inst_lvar, field_name in targets:
            inst_use = inst_lvar.use()
            for ctx_name in ("observed", "fixnum", "general"):
                body = bodies.get((lam, ctx_name))
                if body is None:
                    continue
                inst_fact = body.facts.get(inst_use, NO_FACT)
                key_fact = body.facts.get(key_use, NO_FACT)
                val_fact = body.facts.get(val_use, NO_FACT)
                if (
                    inst_fact.type is BOTTOM
                    or key_fact.type is BOTTOM
                    or val_fact.type is BOTTOM
                ):
                    continue
                for inst_cls in inst_fact.type.accept(_INSTANCE_CLASSES):
                    if inst_cls not in class_facts:
                        continue
                    cur_field = direct_stores[inst_cls].get(field_name, BOTTOM)
                    if cur_field.fits_dict():
                        direct_stores[inst_cls][field_name] = cur_field.with_entry(
                            key_fact.type, val_fact.type
                        )

    for lam, node in list_mutation_sites:
        attr = node.args[0].use().accept(_GETATTR_ON_INSTANCE)
        if attr is not None:
            targets = (attr,)
        else:
            target_var = node.args[0].use().accept(_READ_LAMBDA_VAR)
            targets = (
                tuple(stored_field_targets.get((lam, target_var), ()))
                if target_var is not None
                else ()
            )
        if not targets:
            continue
        val_use = node.args[-1].use()
        for inst_lvar, field_name in targets:
            inst_use = inst_lvar.use()
            for ctx_name in ("observed", "fixnum", "general"):
                body = bodies.get((lam, ctx_name))
                if body is None:
                    continue
                inst_fact = body.facts.get(inst_use, NO_FACT)
                val_fact = body.facts.get(val_use, NO_FACT)
                if inst_fact.type is BOTTOM or val_fact.type is BOTTOM:
                    continue
                for inst_cls in inst_fact.type.accept(_INSTANCE_CLASSES):
                    if inst_cls not in class_facts:
                        continue
                    cur_type = class_facts[inst_cls].fields.get(field_name, BOTTOM)
                    if (
                        val_fact.type is UNKNOWN
                        and cur_type is BOTTOM
                        and val_use.accept(_DependsOnInstanceField(field_name))
                    ):
                        continue
                    cur_field = direct_stores[inst_cls].get(field_name, BOTTOM)
                    if cur_field.fits_list():
                        direct_stores[inst_cls][field_name] = cur_field.with_element(
                            val_fact.type
                        )

    if unknown_field_write:
        ambiguous_fields.update(
            name for fields in direct_stores.values() for name in fields
        )
    changed = False
    for class_id, facts in class_facts.items():
        new_fields: dict[str, ValueType] = {}
        chain = facts.chain or (class_id,)
        sanitizer = (
            _SANITIZE_PROTOCOL_FIELD_TYPE
            if facts.is_protocol
            else _SANITIZE_INSTANCE_FIELD_TYPE
        )
        candidate_names = {
            name
            for anc_id in chain
            for name in direct_stores.get(anc_id, ())
            if class_facts[anc_id].constructor_proven
            and name not in ambiguous_fields
            and name not in facts.widened_fields
            and name not in facts.methods
            and name not in facts.shadowed
        }
        for name in candidate_names:
            joined = BOTTOM
            for anc_id in chain:
                if not class_facts[anc_id].constructor_proven:
                    continue
                anc_type = direct_stores.get(anc_id, {}).get(name, BOTTOM)
                if anc_type is not BOTTOM:
                    joined = joined.join(anc_type)
            joined = joined.join(unattributed_stores.get(name, BOTTOM))
            sanitized = joined.accept(sanitizer)
            if (
                sanitized is not UNKNOWN
                and sanitized is not BOTTOM
                and sanitized is not NONE_TYPE
                and sanitized != DictType(BOTTOM, BOTTOM, False)
            ):
                new_fields[name] = sanitized
        for name in facts.fields:
            if name not in new_fields:
                facts.widened_fields.add(name)
        if new_fields != facts.fields:
            facts.fields = new_fields
            changed = True
    return changed


def _collect_class_tables(lambdas):
    """Constructor targets by namespace and name, and the facts of every
    class whose instances analysis types, by identity."""
    by_ns: dict[str, dict[str, ClassConstructorTarget]] = defaultdict(dict)
    typed: dict[str, ClassFacts] = {}
    collectors: dict[str, _ModuleClassCollector] = defaultdict(_ModuleClassCollector)
    visited_components = set()
    for lambda_ in lambdas:
        component = lambda_.component
        if component is None or (lambda_.namespace, component) in visited_components:
            continue
        visited_components.add((lambda_.namespace, component))
        collector = collectors[lambda_.namespace]
        for block in component.blocks():
            for node in block.nodes():
                node.accept(collector)
    is_protocol = any(lam.is_protocol for lam in lambdas)
    for ns, collector in collectors.items():
        layouts = {}
        for class_id, cls_name in collector.public_names.items():
            _, chain, _ = _resolved_class_info(collector, class_id, cls_name)
            # Unknown and multiple bases do not establish a physical prefix.
            known_chain = all(
                not collector.bases.get(collector.public_names[cid], ())
                or all(
                    base == "object" or base in collector.public_names.values()
                    for base in collector.bases[collector.public_names[cid]]
                )
                for cid in chain
            ) and all(
                len(collector.bases.get(collector.public_names[cid], ())) <= 1
                for cid in chain
            )
            layouts[class_id] = (
                tuple(
                    dict.fromkeys(
                        name
                        for cid in reversed(chain)
                        for name in collector.fields.get(cid, ())
                    )
                )
                if known_chain
                and not any(cid in collector.native_classes for cid in chain)
                and not any(cid in collector.unproven_slot_classes for cid in chain)
                and not any(cid in collector.keyword_classes for cid in chain)
                and not any(
                    collector.definitions[collector.public_names[cid]] != 1
                    for cid in chain
                )
                else ()
            )
        all_class_ids = list(
            dict.fromkeys([*collector.public_names, *collector.methods])
        )
        for class_id in all_class_ids:
            cls_name = collector.public_names.get(class_id, class_id)
            methods = collector.methods.get(class_id, {})
            base_target = BOOTSTRAP_CLASS_TARGETS.get(cls_name) if is_protocol else None
            if base_target is not None:
                base_type = base_target.base_type
                cls_type = base_target.cls_type
                records_type_origin = base_target.records_type_origin
            else:
                constructed_type = (
                    InstanceType(class_id, fixed_slots=layouts.get(class_id, ()))
                    if _constructs_own_instances(collector, class_id, cls_name)
                    else UNKNOWN
                )
                base_type = (
                    STORAGE_CLASS_TYPES.get(cls_name, constructed_type)
                    if is_protocol
                    else constructed_type
                )
                constructed_class = (
                    ClassType((base_type,))
                    if base_type is not UNKNOWN
                    else ANY_CLASS_TYPE
                )
                cls_type = (
                    BUILTIN_CLASS_TYPES.get(cls_name, constructed_class)
                    if is_protocol
                    else constructed_class
                )
                records_type_origin = False
            by_ns[ns][cls_name] = ClassConstructorTarget(
                base_type=base_type,
                cls_type=cls_type,
                new_target=methods.get("__new__"),
                init_target=methods.get("__init__"),
                records_type_origin=records_type_origin,
                layout_type=InstanceType(
                    class_id, fixed_slots=layouts.get(class_id, ())
                ),
            )
            # A custom constructor can return another class without changing
            # this class's layout or the facts guarded method entries can use.
            if base_target is None or layouts.get(class_id):
                methods_map, chain, shadowed = _resolved_class_info(
                    collector, class_id, cls_name
                )
                typed[class_id] = ClassFacts(
                    class_id,
                    methods_map,
                    {},
                    chain,
                    shadowed,
                    public_name=cls_name,
                    is_protocol=is_protocol,
                    fixed_slots=layouts.get(class_id, ()),
                    constructor_proven=_constructs_own_instances(
                        collector, class_id, cls_name
                    ),
                )
    return by_ns, typed


def _resolved_class_info(
    collector, class_id: str, cls_name: str
) -> tuple[dict[str, Lambda], tuple[str, ...], frozenset[str]]:
    """The class's methods in method-resolution order over its module
    bases, its single-inheritance chain of class identities, and the
    non-method attribute names bound on the chain."""
    identities = {name: identity for identity, name in collector.public_names.items()}
    chain = [class_id]
    current = cls_name
    while True:
        bases = [base for base in collector.bases.get(current, ()) if base != "object"]
        if len(bases) != 1 or bases[0] not in identities:
            if bases:
                chain = [class_id]
            break
        current = bases[0]
        chain.append(identities[current])
    resolved: dict[str, Lambda] = {}
    shadowed: set[str] = set()
    for identity in reversed(chain):
        for name in collector.other_bindings.get(identity, ()):
            resolved.pop(name, None)
            shadowed.add(name)
        for name, lam in collector.methods.get(identity, {}).items():
            shadowed.discard(name)
            resolved[name] = lam
    return resolved, tuple(chain), frozenset(shadowed)


def _constructs_own_instances(collector, class_id: str, cls_name: str) -> bool:
    """Whether calling the class returns an exact instance of it, as
    ``type.__call__`` does through ``object.__new__``: one definition under
    the name, no class keywords or metaclass, and no ``__new__`` on the class
    or on a base defined in the module. Bases imported from elsewhere are
    not inspected, so a class with such a base is excluded."""
    if collector.definitions.get(cls_name, 0) != 1:
        return False
    if class_id in collector.keyword_classes:
        return False
    if "__new__" in collector.methods.get(class_id, {}):
        return False
    identities = {name: identity for identity, name in collector.public_names.items()}
    pending = list(collector.bases.get(cls_name, ()))
    seen = set()
    while pending:
        base = pending.pop()
        if base == "object" or base in seen:
            continue
        seen.add(base)
        base_id = identities.get(base)
        if (
            base_id is None
            or base_id in collector.keyword_classes
            or "__new__" in collector.methods.get(base_id, {})
        ):
            return False
        pending.extend(collector.bases.get(base, ()))
    return True


def collect_global_constants(
    target,
    class_targets_by_ns: dict[str, dict[str, ClassConstructorTarget]] | None = None,
) -> dict[GlobalVar, ValueType]:
    """Unreassigned module-level constant globals and their value types."""
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
    is_protocol = any(lambda_.is_protocol for lambda_ in lambdas)
    if class_targets_by_ns is None and is_protocol:
        class_targets_by_ns = _collect_module_classes(lambdas)
    lambdas_by_ns: dict[str, list[Lambda]] = defaultdict(list)
    for lambda_ in lambdas:
        lambdas_by_ns[lambda_.namespace].append(lambda_)
    mutated_namespaces = {
        ns
        for ns, ns_lambdas in lambdas_by_ns.items()
        if has_module_namespace_mutations(ns_lambdas)
    }
    unconditional_by_ns: dict[str, set[Set]] = {}
    for ns, ns_lambdas in lambdas_by_ns.items():
        if ns in mutated_namespaces:
            continue
        toplevels = [lam for lam in ns_lambdas if lam.kind is FunctionalKind.TOPLEVEL]
        if len(toplevels) == 1:
            unconditional_by_ns[ns] = _unconditional_toplevel_stores(toplevels[0])
    constants: dict[GlobalVar, ValueType] = {}
    visited_components = set()
    for lambda_ in lambdas:
        if lambda_.namespace in mutated_namespaces:
            continue
        component = lambda_.component
        if component is None or component in visited_components:
            continue
        visited_components.add(component)
        ns_classes = (
            class_targets_by_ns.get(lambda_.namespace)
            if class_targets_by_ns is not None
            else None
        )
        visitor = _GlobalConstantStores(
            constants,
            is_protocol=is_protocol,
            class_targets=ns_classes,
            unconditional_stores=unconditional_by_ns.get(lambda_.namespace),
        )
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
            or len(node.args) != 1 + len(self.target.signature.default_parameters)
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
        if len(leaf.sets) != 1 or (leaf.home is not None and leaf in leaf.home.vars):
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
            and self.target.call_order(
                node.info.keywords if node.info is not None else (),
                len(node.args),
            )
            is not None
        )


def _collect_closed_lambdas(lambdas) -> dict[Lambda, tuple[Ref, ...]]:
    """Return nested functions whose function object never escapes and whose
    every reference is the callee of a direct static call."""
    closed: dict[Lambda, tuple[Ref, ...]] = {}
    for lambda_ in lambdas:
        if (
            lambda_.kind is not FunctionalKind.NORMAL
            or not lambda_.vars
            or lambda_.signature.variadic
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


class _ImportedModuleOrigin(NodeVisitor, LeafVisitor):
    """Resolve the module name an imported module object lvar originates from."""

    def __init__(self):
        self._active_aliases: set[BasicVar] = set()

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return None
        if node.fun_info is IMPORT:
            return linked(node.info, node, "info").module_name
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def _visit_var(self, leaf: BasicVar):
        if leaf in self._active_aliases:
            return None
        value_sets = [s for s in leaf.sets if s.value is not None]
        if len(value_sets) != 1:
            return None
        val = value_sets[0].value
        if val is None or len(val.uses) != 1:
            return None
        self._active_aliases.add(leaf)
        try:
            return val.uses[0].accept(self)
        finally:
            self._active_aliases.remove(leaf)

    def visit_GlobalVar(self, leaf: GlobalVar):
        return self._visit_var(leaf)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf.home is not None and leaf in leaf.home.vars:
            return None
        return self._visit_var(leaf)


class _ImportBindingStores(NodeVisitor, LeafVisitor):
    """Collect ``from mod import orig as name`` global bindings in module bodies."""

    def __init__(self, imports: dict[str, tuple[str, str]]):
        self.imports = imports
        self._store: Set | None = None
        self._active_aliases: set[LambdaVar] = set()

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Set(self, node: Set):
        self._store = node
        node.var.accept(self)
        self._store = None

    def visit_GlobalVar(self, leaf: GlobalVar):
        store = self._store
        self._store = None
        if store is None or store.value is None or len(leaf.sets) != 1:
            return
        if leaf.sets[0] is not store:
            return
        home = store.home_lambda
        if home is None or home.kind is not FunctionalKind.TOPLEVEL:
            return
        if len(store.value.uses) != 1:
            return
        origin = store.value.uses[0].accept(self)
        if origin is not None:
            self.imports[leaf.name] = origin

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return None
        if node.fun_info is GETATTR and len(node.args) == 1:
            if len(node.args[0].uses) != 1:
                return None
            mod_name = node.args[0].uses[0].accept(_ImportedModuleOrigin())
            if mod_name is not None:
                return mod_name, linked(node.info, node, "info").name
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        store = self._store
        self._store = None
        # Parameter stores may be annotation assertions of the incoming value,
        # not definitions. Alias cycles likewise establish no import origin.
        if leaf.home is not None and leaf in leaf.home.vars:
            return None
        if leaf in self._active_aliases:
            return None
        self._active_aliases.add(leaf)
        try:
            if store is not None:
                value_sets = [s for s in leaf.sets if s.value is not None]
                if (
                    store.value is None
                    or len(value_sets) != 1
                    or value_sets[0] is not store
                ):
                    return None
                if len(store.value.uses) != 1:
                    return None
                origin = store.value.uses[0].accept(self)
                if origin is not None:
                    self.imports[leaf.name] = origin
                return None
            value_sets = [s for s in leaf.sets if s.value is not None]
            if len(value_sets) != 1:
                return None
            val = value_sets[0].value
            if val is None or len(val.uses) != 1:
                return None
            return val.uses[0].accept(self)
        finally:
            self._active_aliases.remove(leaf)


class _ActiveLocallState(local):
    def __init__(self):
        self.derivations: set[tuple[Lambda, str]] = set()
        self.specialized_contexts: set[tuple[Lambda, object]] = set()


_ACTIVE_STATE = _ActiveLocallState()
_MAX_DERIVATION_DEPTH = 6


class _MakeFunctionDefaults(NodeVisitor):
    def __init__(self, target: Lambda, ref: Ref):
        self.target = target
        self.ref = ref

    def visit_Node(self, node):
        return ()

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is MAKE_FUNCTION
            and len(node.args) > 1
            and node.args[0] is self.ref.lvar
        ):
            return tuple(node.args[1:])
        return ()


class _MutatesFunctionDefaults(NodeVisitor):
    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return False
        if node.fun_info is SETATTR:
            return node.info.name in ("__defaults__", "__kwdefaults__")
        if node.fun_info is GETATTR:
            return node.info.name == "__kwdefaults__"
        return node.fun_info is DELATTR


_MUTATES_FUNCTION_DEFAULTS = _MutatesFunctionDefaults()


def analyze(
    lambdas,
    annotations: AnnotationLedger,
    *,
    imported_function_types: Mapping[tuple[str, str], ValueType] | None = None,
    imported_targets: (
        Mapping[tuple[str, str], Lambda | ClassConstructorTarget | ForeignCallTarget]
        | None
    ) = None,
    storage_methods: Mapping[tuple[str, str], ValueType] | None = None,
    builtin_return_types: Mapping[str, ValueType] | None = None,
    builtin_targets: (
        Mapping[str, Lambda | ClassConstructorTarget | ForeignCallTarget] | None
    ) = None,
    state_cache: dict | None = None,
) -> dict[Lambda, LambdaFacts]:
    """Solve local-call arguments and results, then publish entry contexts.

    General entries model externally supplied values. Observed integer
    contexts and an all-fixnum context describe optional guarded entries, never
    changes to a function's public contract. Return summaries start empty, grow
    by union, and widen cyclic intervals before publication.
    """
    lambdas = list(lambdas)
    is_protocol_unit = any(lam.is_protocol for lam in lambdas)
    if not is_protocol_unit:
        if (
            storage_methods is None
            or imported_function_types is None
            or builtin_return_types is None
        ):
            from .sequence_protocol import stdlib_type_summary

            exported, methods, builtins = stdlib_type_summary()
            if imported_function_types is None:
                imported_function_types = exported
            if storage_methods is None:
                storage_methods = methods
            if builtin_return_types is None:
                builtin_return_types = builtins
        if imported_targets is None or builtin_targets is None:
            from .sequence_protocol import stdlib_target_summary

            exp_targets, b_targets = stdlib_target_summary()
            if imported_targets is None:
                imported_targets = exp_targets
            if builtin_targets is None:
                builtin_targets = b_targets
    else:
        if imported_targets is None:
            from .protocols import NATIVE_MODULE_TARGETS

            imported_targets = NATIVE_MODULE_TARGETS
        if builtin_targets is None:
            builtin_targets = BOOTSTRAP_BUILTIN_TARGETS
    prev_graphs: dict[Lambda, LambdaGraph] = (
        state_cache.get("graphs", {}) if state_cache is not None else {}
    )
    prev_revisions: dict[Lambda, int] = (
        state_cache.get("revisions", {}) if state_cache is not None else {}
    )
    graphs: dict[Lambda, LambdaGraph] = {}
    unchanged_lambdas: set[Lambda] = set()
    for lambda_ in lambdas:
        comp = lambda_.component
        current_blocks = tuple(comp.blocks()) if comp is not None else ()
        current_nodes = {block: tuple(block.nodes()) for block in current_blocks}
        prev_graph = prev_graphs.get(lambda_)
        if (
            prev_graph is not None
            and prev_revisions.get(lambda_) == lambda_.ir1_revision
            and prev_graph.blocks == current_blocks
            and prev_graph.block_nodes == current_nodes
        ):
            graphs[lambda_] = prev_graph
            unchanged_lambdas.add(lambda_)
        else:
            graphs[lambda_] = build_lambda_graph(lambda_)
    closed_lambdas = _collect_closed_lambdas(lambdas)
    class_targets_by_ns, class_facts = _collect_class_tables(lambdas)
    global_constants = collect_global_constants(lambdas, class_targets_by_ns)
    old_constant_ref_types: dict[Ref, ValueType | None] = {}
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
    global_leaves: dict[str, dict[str, GlobalVar]] = defaultdict(dict)
    import_bindings_by_ns: dict[str, dict[str, tuple[str, str]]] = defaultdict(dict)
    for lambda_ in lambdas:
        visitor = _GlobalFunctionStores(
            global_targets[lambda_.namespace],
            global_counts[lambda_.namespace],
            leaves=global_leaves[lambda_.namespace],
        )
        imp_visitor = _ImportBindingStores(import_bindings_by_ns[lambda_.namespace])
        graph = graphs[lambda_]
        for block in graph.blocks:
            for node in graph.block_nodes[block]:
                node.accept(visitor)
                node.accept(imp_visitor)
    lambda_set = set(lambdas)
    static_callees: dict[Lambda, list[Lambda]] = defaultdict(list)
    for target in lambdas:
        for ref in target.refs:
            caller = ref.home_lambda
            if caller is not None and caller is not target and caller in lambda_set:
                static_callees[caller].append(target)
    for target, call_refs in closed_lambdas.items():
        for ref in call_refs:
            caller = ref.home_lambda
            if caller is not None and caller is not target and caller in lambda_set:
                static_callees[caller].append(target)
    for ns, ns_targets in global_targets.items():
        ns_leaves = global_leaves[ns]
        for name, target_obj in ns_targets.items():
            target = target_obj.as_lambda()
            gvar = ns_leaves.get(name)
            if target is not None and gvar is not None and target in lambda_set:
                for ref in gvar.refs:
                    caller = ref.home_lambda
                    if (
                        caller is not None
                        and caller is not target
                        and caller in lambda_set
                    ):
                        static_callees[caller].append(target)
    ordered_lambdas: list[Lambda] = [
        lam for lam in lambdas if lam.kind is FunctionalKind.TOPLEVEL
    ]
    visited_order: set[Lambda] = set(ordered_lambdas)
    for root in lambdas:
        if root in visited_order:
            continue
        stack: list[tuple[Lambda, bool]] = [(root, False)]
        while stack:
            cur, done = stack.pop()
            if done:
                ordered_lambdas.append(cur)
                continue
            if cur in visited_order:
                continue
            visited_order.add(cur)
            stack.append((cur, True))
            for callee in reversed(static_callees.get(cur, ())):
                if callee not in visited_order:
                    stack.append((callee, False))
    lambda_order = {lam: idx for idx, lam in enumerate(ordered_lambdas)}
    unique_global_lambdas = {
        target
        for ns, ns_targets in global_targets.items()
        for name, target_obj in ns_targets.items()
        if global_counts[ns].get(name, 0) == 1
        and (target := target_obj.as_lambda()) is not None
        and target.source_name == name
    }
    can_reach: dict[Lambda, frozenset[Lambda]] = {}
    for lam in lambdas:
        seen_r: set[Lambda] = set()
        stack_r: list[Lambda] = list(static_callees.get(lam, ()))
        while stack_r:
            cur_r = stack_r.pop()
            if cur_r in seen_r:
                continue
            seen_r.add(cur_r)
            stack_r.extend(static_callees.get(cur_r, ()))
        can_reach[lam] = frozenset(seen_r)
    recursive_targets_by_caller: dict[Lambda, frozenset[Lambda]] = {}
    for lam in lambdas:
        rec_set: set[Lambda] = {lam} if lam in unique_global_lambdas else set()
        for target in static_callees.get(lam, ()):
            if (
                target is not lam
                and target in unique_global_lambdas
                and lam in can_reach.get(target, ())
            ):
                rec_set.add(target)
        recursive_targets_by_caller[lam] = frozenset(rec_set)
    imported_returns_by_ns: dict[str, dict[str, ValueType]] = defaultdict(dict)
    imported_targets_by_ns: dict[
        str, dict[str, Lambda | ClassConstructorTarget | ForeignCallTarget]
    ] = defaultdict(dict)
    for ns, bindings in import_bindings_by_ns.items():
        ns_returns = imported_returns_by_ns[ns]
        ns_targets = imported_targets_by_ns[ns]
        ns_counts = global_counts[ns]
        for local_name, (mod_name, orig_name) in bindings.items():
            if ns_counts.get(local_name, 0) > 1:
                continue
            if (
                imported_function_types is not None
                and (mod_name, orig_name) in imported_function_types
            ):
                ns_returns[local_name] = imported_function_types[mod_name, orig_name]
            if (
                imported_targets is not None
                and (mod_name, orig_name) in imported_targets
            ):
                ns_targets[local_name] = imported_targets[mod_name, orig_name]
    for ns, cls_targets in class_targets_by_ns.items():
        ns_targets = imported_targets_by_ns[ns]
        ns_counts = global_counts[ns]
        for cls_name, cls_target in cls_targets.items():
            if ns_counts.get(cls_name, 0) <= 1 and cls_name not in global_targets[ns]:
                ns_targets[cls_name] = cls_target
    proven_by_namespace: dict[str, frozenset[str]] = {
        ns: frozenset(
            name
            for name, target in {
                **imported_targets_by_ns.get(ns, {}),
                **targets,
            }.items()
            if (
                global_counts[ns].get(name, 0) <= 1
                and (fn := target.as_lambda()) is not None
                and fn.is_protocol
            )
        )
        for ns, targets in global_targets.items()
    }
    rebound_by_namespace: dict[str, frozenset[str]] = {
        ns: frozenset(counts) for ns, counts in global_counts.items()
    }
    namespaces = {lambda_.namespace for lambda_ in lambdas}
    global_constants_by_ns: dict[str, dict[str, ValueType]] = {}
    for ns in namespaces:
        rebound = rebound_by_namespace.get(ns, frozenset())
        ns_consts: dict[str, ValueType] = {
            cls_name: cls_type
            for cls_name, cls_type in BUILTIN_CLASS_TYPES.items()
            if cls_name not in rebound
        }
        ns_counts = global_counts[ns]
        for cls_name, cls_target in class_targets_by_ns.get(ns, {}).items():
            if ns_counts.get(cls_name, 0) <= 1:
                ns_consts[cls_name] = cls_target.cls_type
        for local_name, (mod_name, orig_name) in import_bindings_by_ns.get(
            ns, {}
        ).items():
            if ns_counts.get(local_name, 0) > 1:
                continue
            if (
                imported_targets is not None
                and (mod_name, orig_name) in imported_targets
            ):
                cls_const = imported_targets[mod_name, orig_name].global_constant_type()
                if cls_const is not UNKNOWN:
                    ns_consts[local_name] = cls_const
            if (
                imported_function_types is not None
                and (mod_name, orig_name) in imported_function_types
                and (
                    imported_targets is None
                    or (mod_name, orig_name) not in imported_targets
                )
            ):
                ns_consts[local_name] = imported_function_types[mod_name, orig_name]
        global_constants_by_ns[ns] = ns_consts
    for leaf, vtype in global_constants.items():
        if leaf.sets and leaf.sets[0].home_lambda is not None:
            ns = leaf.sets[0].home_lambda.namespace
            global_constants_by_ns.setdefault(ns, {})[leaf.name] = vtype
    lambdas_by_ns: dict[str, list[Lambda]] = defaultdict(list)
    for lambda_ in lambdas:
        lambdas_by_ns[lambda_.namespace].append(lambda_)
    mutated_namespaces = {
        ns
        for ns, ns_lambdas in lambdas_by_ns.items()
        if has_module_namespace_mutations(ns_lambdas)
    }
    unconditional_by_ns: dict[str, set[Set]] = {}
    inferred_global_constants: dict[str, dict[str, ValueType]] = (
        {
            ns: dict(prev_c)
            for ns, prev_c in state_cache.get("inferred_global_constants", {}).items()
            if ns not in mutated_namespaces
        }
        if state_cache is not None
        else {}
    )
    for ns, ns_lambdas in lambdas_by_ns.items():
        if ns in mutated_namespaces:
            continue
        toplevels = [lam for lam in ns_lambdas if lam.kind is FunctionalKind.TOPLEVEL]
        if len(toplevels) == 1 and toplevels[0].policy.safety < 3:
            unconditional_by_ns[ns] = _unconditional_toplevel_stores(toplevels[0])
            prev_c = inferred_global_constants.get(ns)
            if prev_c:
                ns_consts = global_constants_by_ns.setdefault(ns, {})
                ns_leaves = global_leaves.get(ns, {})
                for gname, vtype in prev_c.items():
                    if gname not in ns_consts:
                        ns_consts[gname] = vtype
                        leaf = ns_leaves.get(gname)
                        if leaf is not None:
                            for ref in leaf.refs:
                                home = ref.home_lambda
                                if (
                                    home is not None
                                    and home.kind is not FunctionalKind.TOPLEVEL
                                    and home.policy.safety < 3
                                    and ref.lexenv.policy.safety < 3
                                ):
                                    old_constant_ref_types.setdefault(
                                        ref, ref.derived_type
                                    )
                                    ref.derived_type = vtype
    defaults_mutated = any(
        node.accept(_MUTATES_FUNCTION_DEFAULTS)
        for graph in graphs.values()
        for block in graph.blocks
        for node in graph.block_nodes[block]
    )
    default_lvars_by_lam: dict[Lambda, tuple] = {}
    prev_default_types = (
        state_cache.get("default_types", {}) if state_cache is not None else {}
    )
    if not defaults_mutated:
        for lambda_ in lambdas:
            if lambda_.signature.default_parameters:
                make_defaults = [
                    d
                    for ref in lambda_.refs
                    if ref.dest is not None
                    and (d := ref.dest.accept(_MakeFunctionDefaults(lambda_, ref)))
                ]
                default_lvars = make_defaults[0] if len(make_defaults) == 1 else ()
                if default_lvars:
                    default_lvars_by_lam[lambda_] = default_lvars
                    if lambda_ in prev_default_types and len(
                        prev_default_types[lambda_]
                    ) == len(default_lvars):
                        lambda_.default_types = prev_default_types[lambda_]
                        continue
                    ns_consts = global_constants_by_ns.get(lambda_.namespace, {})
                    def_types: list[ValueType] = []
                    for def_lvar in default_lvars:
                        lit = def_lvar.accept(_MadeConstant())
                        if lit is not None:
                            def_types.append(constant_type(lit))
                        elif (
                            len(def_lvar.uses) == 1
                            and (
                                gname := def_lvar.uses[0].accept(
                                    _GLOBAL_CALLEE_REF_NAME
                                )
                            )
                            is not None
                        ):
                            def_types.append(ns_consts.get(gname, UNKNOWN))
                        else:
                            def_types.append(UNKNOWN)
                    lambda_.default_types = tuple(def_types)
    captured_vars: list = []
    seen_captured: set = set()
    for lambda_ in lambdas:
        if lambda_.environment is not None:
            for var in lambda_.environment.closure:
                if var not in seen_captured:
                    seen_captured.add(var)
                    captured_vars.append(var)
    closed_captures = {
        var
        for var in captured_vars
        if all(
            lambda_ in closed_lambdas
            for lambda_ in lambdas
            if lambda_.environment is not None and var in lambda_.environment.closure
        )
    }
    for var in captured_vars:
        if (
            (var.indirect and var not in closed_captures)
            or not var.sets
            or (var in var.home.vars and var.name not in var.home.local_annotations)
        ):
            var.optimistic_type = UNKNOWN
        else:
            var.optimistic_type = BOTTOM
    var_changes = defaultdict(int)
    full = IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    parameters: dict[tuple[Lambda, str], tuple[ValueType, ...]] = {}
    # External entries construct these exact containers before entering the
    # body; their contents and, for tuples, arity remain unknown.
    binding_types = {
        ast.ParameterKind.VAR_POSITIONAL: TupleType(),
        ast.ParameterKind.VAR_KEYWORD: DICT_TYPE,
    }
    physical_targets = {
        target
        for target in lambdas
        if any(
            True
            for operation in (
                "_instance_field",
                "_set_instance_field",
                "%physical_field_ref",
                "%physical_field_set",
            )
            for _ in known_calls(target, operation)
        )
    }
    declared_entry_lambdas: set[Lambda] = set()
    for lambda_ in lambdas:
        signature = lambda_.signature
        if lambda_.vars and not signature.variadic:
            gen_params = [
                _ParameterUse(var, storage_methods).general_type(
                    lambda_.local_annotations.get(var.name)
                )
                if contract is None
                and ("local", lambda_, var.name) not in annotations.ignored
                else UNKNOWN
                for var, contract in zip(
                    lambda_.vars, signature.annotations, strict=True
                )
            ]
            if gen_params[0] is UNKNOWN:
                self_type = lambda_.protocol_self_type()
                if self_type is not None:
                    gen_params[0] = self_type.accept(
                        _ProtocolSelfPrediction(class_facts.get(lambda_.method_class))
                    )
            parameters[lambda_, "general"] = tuple(gen_params)
            is_typed_method = (
                lambda_.method_class is not None
                and lambda_.method_class in class_facts
                and lambda_ in class_facts[lambda_.method_class].methods.values()
                and lambda_
                is not class_facts[lambda_.method_class].methods.get("__new__")
                and signature.kinds[0]
                in (
                    ast.ParameterKind.POSITIONAL_ONLY,
                    ast.ParameterKind.POSITIONAL_OR_KEYWORD,
                )
                and (
                    signature.annotations[0] is None
                    or (
                        signature.annotations[0].speculative_parameter_type(full)
                        is UNKNOWN
                        and not signature.annotations[0].contradicts(
                            InstanceType(lambda_.method_class)
                        )
                    )
                )
                and lambda_.vars[0].name not in lambda_.local_annotations
            )
            if lambda_.entry_parameter_types is not None:
                if len(lambda_.entry_parameter_types) != len(lambda_.vars):
                    raise ValueError(
                        f"entry parameter arity mismatch for {lambda_.name}"
                    )
                parameters[lambda_, "fixnum"] = lambda_.entry_parameter_types
                declared_entry_lambdas.add(lambda_)
            elif is_typed_method:
                recv_type = InstanceType(
                    lambda_.method_class,
                    fixed_slots=class_facts[lambda_.method_class].fixed_slots,
                )
                spec_params = (
                    recv_type,
                    *(
                        _ParameterUse(var, storage_methods).analyze(
                            contract,
                            full,
                            lambda_.local_annotations.get(var.name)
                            if ("local", lambda_, var.name) not in annotations.ignored
                            else None,
                        )
                        for var, contract in zip(
                            lambda_.vars[1:],
                            signature.annotations[1:],
                            strict=True,
                        )
                    ),
                )
                parameters[lambda_, "fixnum"] = spec_params
                declared_entry_lambdas.add(lambda_)
            else:
                spec_params = tuple(
                    _ParameterUse(var, storage_methods).analyze(
                        contract,
                        full,
                        lambda_.local_annotations.get(var.name)
                        if ("local", lambda_, var.name) not in annotations.ignored
                        else None,
                    )
                    for var, contract in zip(
                        lambda_.vars, signature.annotations, strict=True
                    )
                )
                decl_params = tuple(
                    param
                    if param is not UNKNOWN
                    and (
                        contract is not None
                        or (
                            var.name in lambda_.local_annotations
                            and ("local", lambda_, var.name) not in annotations.ignored
                        )
                    )
                    else UNKNOWN
                    for var, param, contract in zip(
                        lambda_.vars,
                        spec_params,
                        signature.annotations,
                        strict=True,
                    )
                )
                if any(param is not UNKNOWN for param in decl_params):
                    declared_entry_lambdas.add(lambda_)
                if all(param is not UNKNOWN for param in spec_params):
                    parameters[lambda_, "fixnum"] = spec_params
                else:
                    partial_params = tuple(
                        param
                        if param is not UNKNOWN
                        and (
                            contract is not None
                            or (
                                var.name in lambda_.local_annotations
                                and ("local", lambda_, var.name)
                                not in annotations.ignored
                            )
                            or (
                                lambda_.method_class is None
                                and lambda_ not in physical_targets
                            )
                        )
                        else UNKNOWN
                        for var, param, contract in zip(
                            lambda_.vars,
                            spec_params,
                            signature.annotations,
                            strict=True,
                        )
                    )
                    if any(param is not UNKNOWN for param in partial_params):
                        parameters[lambda_, "fixnum"] = partial_params
        else:
            gen_params = [binding_types.get(kind, UNKNOWN) for kind in signature.kinds]
            if lambda_.vars and gen_params[0] is UNKNOWN:
                self_type = lambda_.protocol_self_type()
                if self_type is not None:
                    gen_params[0] = self_type.accept(
                        _ProtocolSelfPrediction(class_facts.get(lambda_.method_class))
                    )
            parameters[lambda_, "general"] = tuple(gen_params)
    initial_general_parameters: dict[Lambda, tuple[ValueType, ...]] = {
        lam: parameters[lam, "general"] for lam in lambdas
    }
    summaries: dict[tuple[Lambda, str], ValueType] = dict.fromkeys(parameters, BOTTOM)
    return_changes = defaultdict(int)
    argument_changes = defaultdict(int)
    closed_seeded: set[Lambda] = set()
    closed_propagated: set[Lambda] = set()
    bodies: dict[tuple[Lambda, str], Body] = {}
    # What each body's last propagation read: its parameters and, for every
    # Each entry records ``(expected_parameters, consulted_calls, closure_types,
    # consulted_targets)`` for the most recent propagation of ``key``. A body
    # whose inputs and resolved call return types are unchanged would propagate
    # identically.
    inputs: dict[tuple[Lambda, str], tuple] = {}
    deriver_cache: dict[tuple[Lambda, tuple[ValueType, ...]], Body] = {}
    setattr_sites: list[tuple[Lambda, Combination]] = []
    setitem_sites: list[tuple[Lambda, Combination]] = []
    list_mutation_sites: list[tuple[Lambda, Combination]] = []
    if class_facts:
        for lambda_ in lambdas:
            graph = graphs[lambda_]
            site_collector = _CollectSetattrSites([], [], [])
            for block in graph.blocks:
                for node in graph.block_nodes[block]:
                    node.accept(site_collector)
            for site in site_collector.sites:
                setattr_sites.append((lambda_, site))
            for site in site_collector.setitems:
                setitem_sites.append((lambda_, site))
            for site in site_collector.list_mutations:
                list_mutation_sites.append((lambda_, site))
    field_changes = 0
    initial_pending_reset: set[tuple[Lambda, str]] = set()
    must_recompute: set[Lambda] = set(lambdas)
    if (
        state_cache is not None
        and "bodies" in state_cache
        and state_cache.get("ignored") == set(annotations.ignored)
    ):
        prev_bodies = state_cache["bodies"]
        prev_inputs = state_cache["inputs"]
        prev_summaries = state_cache["summaries"]
        prev_parameters = state_cache["parameters"]
        prev_deriver_cache = state_cache["deriver_cache"]
        prev_captured_types = state_cache.get("captured_types", {})
        prev_class_fields = state_cache.get("class_fields", {})
        prev_widened_fields = state_cache.get("class_widened_fields", {})
        directly_changed = set(lambdas) - unchanged_lambdas
        while True:
            added_direct: set[Lambda] = set()
            for target, call_refs in closed_lambdas.items():
                if target not in directly_changed and any(
                    ref.home_lambda in directly_changed for ref in call_refs
                ):
                    added_direct.add(target)
            for var in captured_vars:
                if var not in prev_captured_types or any(
                    store.home_lambda in directly_changed for store in var.sets
                ):
                    for lam in lambdas:
                        if (
                            lam not in directly_changed
                            and lam.environment is not None
                            and var in lam.environment.closure
                        ):
                            added_direct.add(lam)
            if not added_direct:
                break
            directly_changed.update(added_direct)
        affected = set(directly_changed)
        while True:
            added = {
                key[0]
                for key, recorded in prev_inputs.items()
                if key[0] not in affected and affected.intersection(recorded[3])
            }
            for target, call_refs in closed_lambdas.items():
                if target not in affected and any(
                    ref.home_lambda in affected for ref in call_refs
                ):
                    added.add(target)
            for var in captured_vars:
                if var not in prev_captured_types or any(
                    store.home_lambda in affected for store in var.sets
                ):
                    for lam in lambdas:
                        if (
                            lam not in affected
                            and lam.environment is not None
                            and var in lam.environment.closure
                        ):
                            added.add(lam)
            if not added:
                break
            affected.update(added)
        back_edge_affected = {
            callee
            for k, rec in prev_inputs.items()
            if k[0] in affected
            for callee in rec[3]
            if callee in affected
            and lambda_order.get(k[0], 0) <= lambda_order.get(callee, 0)
        }
        must_recompute = directly_changed | back_edge_affected
        if prev_class_fields:
            for cls_id, fields_map in prev_class_fields.items():
                if cls_id in class_facts:
                    class_facts[cls_id].fields = dict(fields_map)
        if prev_widened_fields:
            for cls_id, widened_set in prev_widened_fields.items():
                if cls_id in class_facts:
                    class_facts[cls_id].widened_fields = set(widened_set)
        for var in captured_vars:
            if var in prev_captured_types and all(
                store.home_lambda not in must_recompute for store in var.sets
            ):
                var.optimistic_type = prev_captured_types[var]
        affected_observed_targets = {
            target
            for (caller, _), prev_body in prev_bodies.items()
            if caller in must_recompute
            for call in prev_body.calls
            if call.target is not None
            and (target := call.target.as_lambda()) is not None
        }
        for key, prev_body in prev_bodies.items():
            lam, ctx_name = key
            if lam not in must_recompute:
                if (
                    ctx_name == "observed" or ctx_name.startswith("observed.")
                ) and lam in affected_observed_targets:
                    continue
                if key not in parameters and key in prev_parameters:
                    parameters[key] = prev_parameters[key]
                elif (
                    ctx_name == "general"
                    and lam in closed_lambdas
                    and key in prev_parameters
                ):
                    parameters[key] = prev_parameters[key]
                    closed_seeded.add(lam)
                if key in parameters and parameters[key] == prev_parameters.get(key):
                    bodies[key] = prev_body
                    inputs[key] = prev_inputs[key]
                    summaries[key] = prev_summaries[key]
                    if lam in affected:
                        initial_pending_reset.add(key)
        if not affected:
            deriver_cache.update(prev_deriver_cache)
        else:
            for d_key, d_body in prev_deriver_cache.items():
                if d_key[0] not in affected:
                    deriver_cache[d_key] = d_body

    def _observed_context_names(target: Lambda, ctx_params) -> tuple[str, ...]:
        base = ("observed",) if (target, "observed") in ctx_params else ()
        extras = sorted(
            ctx
            for lam, ctx in ctx_params
            if lam is target and ctx.startswith("observed.")
        )
        return (*base, *extras)

    def lookup_return_type(
        target, arguments, context_parameters, context_summaries
    ) -> ValueType:
        if needs_physical_fallback(target, arguments):
            # Instance facts can predict a mutable field's former class. The
            # guarded entry may reject it and execute the unrestricted body.
            return context_summaries.get((target, "general"), UNKNOWN)
        obs = context_parameters.get((target, "observed"))
        fix = context_parameters.get((target, "fixnum"))
        gen = context_parameters.get((target, "general"))
        obs_names = _observed_context_names(target, context_parameters)
        order_contexts = (
            ("fixnum", *obs_names, "general")
            if obs is not None
            and fix is not None
            and all(a.fits(b) or a is UNKNOWN for a, b in zip(obs, fix, strict=True))
            and not all(a.fits(b) for a, b in zip(obs, fix, strict=True))
            else (*obs_names, "fixnum", "general")
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
                    and all(g.fits(e) for g, e in zip(gen, expected, strict=True))
                ):
                    return context_summaries[target, "general"]
                return context_summaries[key]
        return UNKNOWN

    def needs_physical_fallback(target, arguments):
        return target in physical_targets and any(
            argument.accept(_INSTANCE_PREDICTION) for argument in arguments
        )

    def closure_inputs(lambda_):
        if lambda_.environment is None:
            return ()
        return tuple(var.optimistic_type for var in lambda_.environment.closure)

    post_graphs: dict[Lambda, LambdaGraph] = {}
    analysis_finished = False

    def _current_graph(target_lam: Lambda) -> LambdaGraph:
        if not analysis_finished:
            return graphs[target_lam]
        comp = target_lam.component
        cached_graph = post_graphs.get(target_lam)
        if cached_graph is not None and (comp is None or not comp.reoptimize):
            return cached_graph
        current_blocks = tuple(comp.blocks()) if comp is not None else ()
        current_nodes = {block: tuple(block.nodes()) for block in current_blocks}
        base_graph = cached_graph or graphs.get(target_lam)
        if (
            base_graph is not None
            and base_graph.blocks == current_blocks
            and base_graph.block_nodes == current_nodes
        ):
            post_graphs[target_lam] = base_graph
            return base_graph
        # Inserting a checked result changes def-use edges within existing
        # blocks. Cached propagation must include the new Cast producers.
        cached_graph = build_lambda_graph(target_lam)
        post_graphs[target_lam] = cached_graph
        deriver_cache.clear()
        return cached_graph

    def _resolve_deriver_call(
        callee: Fact,
        arguments,
        location=None,
        keywords=(),
        inferred_return: bool = False,
    ) -> ValueType:
        if callee.target is None:
            return UNKNOWN
        if not callee.proven:
            target = callee.target.as_lambda()
            if (
                not inferred_return
                or target is None
                or not target.policy.infers_return_contracts
            ):
                return UNKNOWN
        return callee.target.derive_return_type(arguments, keywords)

    def _proven_globals_for(lam: Lambda) -> frozenset[str]:
        if lam.kind is FunctionalKind.TOPLEVEL:
            return frozenset()
        base = proven_by_namespace.get(lam.namespace, frozenset())
        sname = lam.source_name
        if (
            lam.policy.recognizes_self_calls
            and lam.method_class is None
            and sname is not None
            and sname not in base
            and global_counts.get(lam.namespace, {}).get(sname, 0) <= 1
            and global_targets.get(lam.namespace, {}).get(sname) is lam
        ):
            return base | {sname}
        return base

    def _propagate_for_deriver(
        target_lam: Lambda, param_types: tuple[ValueType, ...], mode: str
    ) -> Body | None:
        graph = _current_graph(target_lam)
        if graph is graphs.get(target_lam):
            cur_clos = closure_inputs(target_lam)
            for ctx_name in ("general", "fixnum", "observed"):
                ctx_key = (target_lam, ctx_name)
                if (
                    param_types == parameters.get(ctx_key)
                    and (ctx_body := bodies.get(ctx_key)) is not None
                    and not ctx_body.return_type.has_unspecialized_part()
                    and (rec := inputs.get(ctx_key)) is not None
                    and rec[2] == cur_clos
                ):
                    return ctx_body
        cache_key = (target_lam, param_types)
        cached = deriver_cache.get(cache_key)
        if cached is not None:
            return cached
        active_derivations = _ACTIVE_STATE.derivations
        active_key = (target_lam, mode)
        if (
            active_key in active_derivations
            or len(active_derivations) >= _MAX_DERIVATION_DEPTH
        ):
            return None
        active_derivations.add(active_key)
        try:
            proven_globals = _proven_globals_for(target_lam)
            with flow_propagation():
                body = propagate(
                    target_lam,
                    global_targets[target_lam.namespace],
                    param_types,
                    _resolve_deriver_call,
                    AnnotationLedger(),
                    graph=graph,
                    proven_globals=proven_globals,
                    rebound_globals=rebound_by_namespace.get(
                        target_lam.namespace, frozenset()
                    ),
                    global_constants=global_constants_by_ns.get(target_lam.namespace),
                    imported_returns=(
                        imported_returns_by_ns.get(target_lam.namespace)
                        if target_lam.policy.safety < 3
                        else None
                    ),
                    imported_targets=(
                        imported_targets_by_ns.get(target_lam.namespace)
                        if target_lam.policy.safety < 3
                        else None
                    ),
                    module_returns=(
                        imported_function_types
                        if target_lam.policy.safety < 3
                        else None
                    ),
                    module_targets=(
                        imported_targets if target_lam.policy.safety < 3 else None
                    ),
                    storage_methods=storage_methods,
                    builtin_return_types=builtin_return_types,
                    builtin_targets=builtin_targets,
                    class_facts=class_facts,
                    recursive_targets=recursive_targets_by_caller.get(
                        target_lam, frozenset()
                    ),
                    unique_targets=frozenset(unique_global_lambdas),
                    is_specialized_context=True,
                    for_return_derivation=True,
                )
            if len(deriver_cache) > 4096:
                deriver_cache.clear()
            deriver_cache[cache_key] = body
            return body
        finally:
            active_derivations.discard(active_key)

    def _make_derivers(target_lam: Lambda):
        def _derive_ret(param_types: tuple[ValueType, ...]) -> ValueType:
            gen_summary = summaries.get((target_lam, "general"), UNKNOWN)
            if needs_physical_fallback(target_lam, param_types):
                return gen_summary.normalized_bottom()
            if not any(
                c is not None and c.contradicts(p)
                for c, p in zip(target_lam.signature.annotations, param_types)
            ) and (
                gen_summary in (BYTES_TYPE, BYTEARRAY_TYPE, FLOAT, NONE_TYPE)
                or gen_summary.instance_class() is not None
                or (
                    gen_summary == TEXT_TYPE
                    and not any(
                        p.fits_text(TEXT_TYPE) and p != TEXT_TYPE for p in param_types
                    )
                )
            ):
                rec = inputs.get((target_lam, "general"))
                if rec is not None and rec[2] == closure_inputs(target_lam):
                    return gen_summary
            body = _propagate_for_deriver(target_lam, param_types, "ret")
            if body is None:
                looked_up = lookup_return_type(
                    target_lam, param_types, parameters, summaries
                )
                if looked_up is not UNKNOWN:
                    return looked_up.normalized_bottom()
                return gen_summary.normalized_bottom()
            return body.return_type.normalized_bottom()

        def _derive_self(param_types: tuple[ValueType, ...]) -> ValueType | None:
            body = _propagate_for_deriver(target_lam, param_types, "self")
            if body is None:
                return None
            return body.self_type

        def _derive_branch(
            param_types: tuple[ValueType, ...], truth: bool
        ) -> tuple[ValueType, ...] | None:
            body = _propagate_for_deriver(target_lam, param_types, "branch")
            if body is None or not body.return_states:
                return None
            active_derivations = _ACTIVE_STATE.derivations
            active_key = (target_lam, "branch_narrow")
            if (
                active_key in active_derivations
                or len(active_derivations) >= _MAX_DERIVATION_DEPTH
            ):
                return None
            active_derivations.add(active_key)
            try:
                from .constraint import _Walk

                walk = _Walk(
                    target_lam,
                    global_targets[target_lam.namespace],
                    param_types,
                    _resolve_deriver_call,
                    AnnotationLedger(),
                    graph=_current_graph(target_lam),
                    proven_globals=_proven_globals_for(target_lam),
                    rebound_globals=rebound_by_namespace.get(
                        target_lam.namespace, frozenset()
                    ),
                    global_constants=global_constants_by_ns.get(target_lam.namespace),
                    imported_returns=imported_returns_by_ns.get(target_lam.namespace),
                    imported_targets=imported_targets_by_ns.get(target_lam.namespace),
                    module_returns=imported_function_types,
                    module_targets=imported_targets,
                    storage_methods=storage_methods,
                    builtin_return_types=builtin_return_types,
                    builtin_targets=builtin_targets,
                    class_facts=class_facts,
                )
                return walk.narrow_return_parameters(body, truth)
            finally:
                active_derivations.discard(active_key)

        return _derive_ret, _derive_self, _derive_branch

    for lambda_ in lambdas:
        if lambda_.kind is FunctionalKind.NORMAL:
            ret_d, self_d, branch_d = _make_derivers(lambda_)
            lambda_.type_deriver = ret_d
            lambda_.self_type_deriver = self_d
            lambda_.branch_deriver = branch_d

    # The canonical (callee, arguments) queries and return types consulted
    # during the current propagation.
    consulted: dict[tuple[Lambda, tuple], ValueType] = {}
    pending_reset: set[tuple[Lambda, str]] = set(initial_pending_reset)
    seeded_params: set[tuple[Lambda, str, int]] = set()
    narrowed_seed_params: set[tuple[Lambda, str, int]] = set()

    while True:
        changed = False
        narrowed_targets: set[Lambda] = set()
        previous_parameters = dict(parameters)

        def resolve(
            callee: Fact,
            arguments,
            location=None,
            keywords=(),
            inferred_return: bool = False,
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
                        inferred_return,
                    ),
                )
            )

        def resolve_defined(
            callee: Fact,
            arguments,
            context_parameters,
            context_summaries,
            location,
            keywords,
            inferred_return: bool = False,
        ):
            # A name's predicted target is never sufficient to publish a return
            # type. Only an already-loaded value with proven identity is local.
            if callee.target is None:
                return UNKNOWN
            target = callee.target.as_lambda()
            if not callee.proven:
                if (
                    not inferred_return
                    or target is None
                    or not target.policy.infers_return_contracts
                ):
                    return UNKNOWN
            elif target is None:
                return callee.target.derive_return_type(arguments, keywords)
            order = target.call_order(keywords, len(arguments))
            if order is None:
                if target.signature.variadic:
                    gen = context_parameters.get((target, "general"))
                    if gen is not None:
                        result = context_summaries.get((target, "general"), UNKNOWN)
                        consulted[target, gen] = result
                        return result
                return UNKNOWN
            default_types = target.default_types
            arguments = tuple(
                arguments[index]
                if index >= 0
                else (
                    default_types[def_idx]
                    if 0 <= (def_idx := -2 - index) < len(default_types)
                    else UNKNOWN
                )
                for index in order
            )
            if not callee.proven:
                if any(
                    contract is not None and contract.contradicts(actual)
                    for actual, contract in zip(
                        arguments, target.signature.annotations, strict=True
                    )
                ):
                    return UNKNOWN
            else:
                for index, (actual, contract, var) in enumerate(
                    zip(
                        arguments,
                        target.signature.annotations,
                        target.vars,
                        strict=True,
                    )
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
            if (
                result.has_unspecialized_part()
                and not needs_physical_fallback(target, arguments)
                and arguments != context_parameters.get((target, "general"))
            ):
                derived = target.derive_return_type(arguments)
                if (
                    derived is not UNKNOWN
                    and derived is not BOTTOM
                    and (result is UNKNOWN or derived.fits(result))
                ):
                    result = derived
            consulted[target, arguments] = result
            return result

        for lambda_ in ordered_lambdas:
            obs_ctxs = _observed_context_names(lambda_, previous_parameters)
            for ctx_name in (*obs_ctxs, "fixnum", "general"):
                key = lambda_, ctx_name
                expected = previous_parameters.get(key)
                if expected is None:
                    continue
                recorded = inputs.get(key)
                current_closure = closure_inputs(lambda_)
                if (
                    recorded is not None
                    and recorded[0] == expected
                    and recorded[2] == current_closure
                    and all(
                        (
                            new_res := lookup_return_type(
                                target, args, previous_parameters, summaries
                            )
                        )
                        == result
                        or (
                            new_res.has_unspecialized_part()
                            and args != previous_parameters.get((target, "general"))
                            and result is not UNKNOWN
                            and result is not BOTTOM
                            and target.derive_return_type(args) == result
                        )
                        for (target, args), result in recorded[1].items()
                    )
                ):
                    body = bodies[key]
                    pending_reset.discard(key)
                    old_summary = summaries[key]
                else:
                    consulted.clear()
                    proven_globals = _proven_globals_for(lambda_)
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
                            global_constants=global_constants_by_ns.get(
                                lambda_.namespace
                            ),
                            imported_returns=(
                                imported_returns_by_ns.get(lambda_.namespace)
                                if lambda_.policy.safety < 3
                                else None
                            ),
                            imported_targets=(
                                imported_targets_by_ns.get(lambda_.namespace)
                                if lambda_.policy.safety < 3
                                else None
                            ),
                            module_returns=(
                                imported_function_types
                                if lambda_.policy.safety < 3
                                else None
                            ),
                            module_targets=(
                                imported_targets if lambda_.policy.safety < 3 else None
                            ),
                            storage_methods=storage_methods,
                            builtin_return_types=builtin_return_types,
                            builtin_targets=builtin_targets,
                            class_facts=class_facts,
                            recursive_targets=recursive_targets_by_caller.get(
                                lambda_, frozenset()
                            ),
                            unique_targets=frozenset(unique_global_lambdas),
                            is_specialized_context=(ctx_name != "general"),
                        )
                    bodies[key] = body
                    if (
                        lambda_.kind is FunctionalKind.TOPLEVEL
                        and ctx_name == "general"
                        and lambda_.namespace in unconditional_by_ns
                    ):
                        ns = lambda_.namespace
                        ns_consts = global_constants_by_ns.setdefault(ns, {})
                        inf_consts = inferred_global_constants.setdefault(ns, {})
                        affected_by_consts: set[Lambda] = set()
                        for store in unconditional_by_ns[ns]:
                            leaf = store.accept(_SET_GLOBAL_VAR)
                            if (
                                leaf is None
                                or len(leaf.sets) != 1
                                or leaf in global_constants
                                or leaf.name in _IMPLICIT_MODULE_GLOBALS
                                or leaf.name in global_targets[ns]
                                or store.value is None
                            ):
                                continue
                            vtype = body.facts.get(store.value.use(), NO_FACT).type
                            if (
                                vtype is UNKNOWN
                                or vtype is BOTTOM
                                or vtype.may_be_unbound
                                or vtype.has_bottom_part()
                                or vtype.fits_none(NONE_TYPE)
                            ):
                                continue
                            if vtype.widen_mutable_containers() != vtype and (
                                _global_container_is_mutated(leaf)
                            ):
                                vtype = vtype.widen_mutable_containers()
                            vtype = _widen_user_constant_type(vtype)
                            if vtype is UNKNOWN:
                                continue
                            if ns_consts.get(leaf.name) != vtype:
                                ns_consts[leaf.name] = vtype
                                inf_consts[leaf.name] = vtype
                                for ref in leaf.refs:
                                    home = ref.home_lambda
                                    if (
                                        home is not None
                                        and home.kind is not FunctionalKind.TOPLEVEL
                                        and home.policy.safety < 3
                                        and ref.lexenv.policy.safety < 3
                                    ):
                                        old_constant_ref_types.setdefault(
                                            ref, ref.derived_type
                                        )
                                        ref.derived_type = vtype
                                        affected_by_consts.add(home)
                        if not defaults_mutated:
                            for lam in lambdas_by_ns[ns]:
                                default_lvars = default_lvars_by_lam.get(lam)
                                if not default_lvars:
                                    continue
                                num_req = len(lam.vars) - len(default_lvars)
                                def_types: list[ValueType] = []
                                for def_idx, def_lvar in enumerate(default_lvars):
                                    lit = def_lvar.accept(_MadeConstant())
                                    if lit is not None:
                                        def_types.append(constant_type(lit))
                                        continue
                                    def_type = body.facts.get(
                                        def_lvar.use(), NO_FACT
                                    ).type
                                    if (
                                        def_type is not UNKNOWN
                                        and def_type is not BOTTOM
                                        and not def_type.may_be_unbound
                                        and not def_type.has_bottom_part()
                                        and not def_type.fits_none(NONE_TYPE)
                                    ):
                                        param_var = (
                                            lam.vars[num_req + def_idx]
                                            if 0 <= num_req + def_idx < len(lam.vars)
                                            else None
                                        )
                                        if (
                                            def_type.widen_mutable_containers()
                                            != def_type
                                            and param_var is not None
                                            and _default_param_container_is_mutated(
                                                param_var
                                            )
                                        ):
                                            def_type = (
                                                def_type.widen_mutable_containers()
                                            )
                                        def_type = _widen_user_constant_type(def_type)
                                    elif (
                                        len(def_lvar.uses) == 1
                                        and (
                                            gname := def_lvar.uses[0].accept(
                                                _GLOBAL_CALLEE_REF_NAME
                                            )
                                        )
                                        is not None
                                    ):
                                        def_type = ns_consts.get(gname, UNKNOWN)
                                    else:
                                        def_type = UNKNOWN
                                    def_types.append(def_type)
                                new_defaults = tuple(def_types)
                                if lam.default_types != new_defaults:
                                    lam.default_types = new_defaults
                                    affected_by_consts.add(lam)
                                    for caller_lam, callees in static_callees.items():
                                        if lam in callees:
                                            affected_by_consts.add(caller_lam)
                        if affected_by_consts:
                            deriver_cache.clear()
                            for lam in affected_by_consts:
                                if lam.kind is not FunctionalKind.TOPLEVEL:
                                    narrowed_targets.add(lam)
                                    for c_name in (
                                        *_observed_context_names(lam, parameters),
                                        "fixnum",
                                        "general",
                                    ):
                                        inputs.pop((lam, c_name), None)
                            changed = True
                    if ctx_name == "general" and lambda_ in closed_seeded:
                        closed_propagated.add(lambda_)
                    inputs[key] = (
                        expected,
                        dict(consulted),
                        current_closure,
                        frozenset(target for target, _ in consulted),
                    )
                    old_summary = summaries[key]
                    if key in pending_reset:
                        pending_reset.discard(key)
                        summaries[key] = BOTTOM
                        return_changes.pop(key, None)
                returned = summaries[key].join(body.return_type)
                if returned != summaries[key]:
                    summaries[key] = returned
                    if returned != old_summary:
                        return_changes[key] += 1
                        if return_changes[key] > 3:
                            summaries[key] = widen_fact(
                                Fact(old_summary), Fact(returned)
                            ).type
                        if summaries[key] != old_summary:
                            changed = True

        for var in captured_vars:
            parameter = var in var.home.vars
            if var.indirect and var not in closed_captures:
                # Escaping cells expose replacement and deletion through
                # cell_contents, including replacement of nested containers.
                continue
            if parameter and var not in closed_captures:
                continue
            if not parameter and not var.sets:
                continue
            # Parameter initialization is a definition too. Only nonescaping
            # closures can use it without an environment-specific guard: an
            # escaping function exposes its cells to arbitrary external writes.
            joined = (
                parameters[var.home, "general"][
                    var.home.vars.index(var)
                ].widen_mutable_containers()
                if parameter
                else BOTTOM
            )
            for store in var.sets:
                home_body = bodies.get((store.home_lambda, "general"))
                if home_body is None:
                    continue
                if store.value is None:
                    joined = joined.join(UNBOUND)
                else:
                    # The fact describes the container when it was stored.
                    # A closure may read it after the defining frame or
                    # another closure mutated it, so only the binding's
                    # identity facts survive, not its contents.
                    joined = joined.join(
                        home_body.facts.get(
                            store.value.use(), NO_FACT
                        ).type.widen_mutable_containers()
                    )
            if joined != var.optimistic_type:
                var_changes[var] += 1
                if var_changes[var] > 3:
                    joined = widen_fact(Fact(var.optimistic_type), Fact(joined)).type
                if joined != var.optimistic_type:
                    previous_type = var.optimistic_type
                    var.optimistic_type = joined
                    deriver_cache.clear()
                    if (
                        previous_type is None
                        or previous_type is BOTTOM
                        or not previous_type.fits(joined)
                    ):
                        narrowed_targets.update(
                            lambda_
                            for lambda_ in lambdas
                            if lambda_.environment is not None
                            and var in lambda_.environment.closure
                        )
                    changed = True

        # The broad guard context is not evidence that callers actually supplied
        # that broad interval. Propagate observations through general/observed
        # entries, including recursive calls, independently of that context.
        seeds: dict[Lambda, tuple] = {}
        recursive_seeds: dict[Lambda, tuple] = {}
        dispatch_seed_groups: dict[Lambda, dict[tuple, tuple[ValueType, ...]]] = (
            defaultdict(dict)
        )
        for target, call_refs in closed_lambdas.items():
            target_reach = can_reach.get(target, frozenset())
            external_args: list[tuple[ValueType, ...]] = []
            seeded_all_args: list[tuple[ValueType, ...]] = []
            missing_external = False
            for ref in call_refs:
                caller = ref.home_lambda
                is_recursive_caller = caller is target or caller in target_reach
                caller_body = bodies.get((caller, "general"))
                if caller_body is None:
                    if not is_recursive_caller:
                        missing_external = True
                    continue
                matching_args = []
                for call in caller_body.calls:
                    if call.target is not target or not call.proven:
                        continue
                    bound_facts = call.bind_argument_facts(target)
                    if bound_facts is None:
                        continue
                    bound_types = tuple(f.type for f in bound_facts)
                    if all(arg is not BOTTOM for arg in bound_types):
                        matching_args.append(bound_types)
                if not matching_args:
                    if not is_recursive_caller:
                        missing_external = True
                    continue
                if not is_recursive_caller:
                    external_args.extend(matching_args)
                    seeded_all_args.extend(matching_args)
                elif caller in closed_propagated:
                    seeded_all_args.extend(matching_args)
            if missing_external or not external_args:
                continue
            source_args = (
                external_args if target not in closed_propagated else seeded_all_args
            )
            joined_args = source_args[0]
            for call_args in source_args[1:]:
                joined_args = tuple(
                    old.join(new)
                    for old, new in zip(joined_args, call_args, strict=True)
                )
            gen_key = target, "general"
            old_gen = parameters[gen_key]
            init_gen = initial_general_parameters[target]
            candidate_gen = []
            for arg_type, contract, var, init_p in zip(
                joined_args,
                target.signature.annotations,
                target.vars,
                init_gen,
                strict=True,
            ):
                if arg_type is BOTTOM or arg_type is UNBOUND or arg_type.may_be_unbound:
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
                if arg_type is UNKNOWN and init_p is not UNKNOWN:
                    arg_type = init_p
                elif init_p is not UNKNOWN and not arg_type.fits(init_p):
                    arg_type = init_p.join(arg_type)
                candidate_gen.append(arg_type)
            new_gen = tuple(candidate_gen)
            if any(p is not UNKNOWN for p in new_gen):
                closed_seeded.add(target)
            if target not in closed_propagated:
                merged_gen = new_gen
                for idx, p in enumerate(merged_gen):
                    if p is not UNKNOWN:
                        seeded_params.add((target, "general", idx))
            else:
                merged_gen = tuple(
                    b
                    if a is UNKNOWN
                    and b is not UNKNOWN
                    and (target, "general", idx) not in seeded_params
                    else a.join(b)
                    for idx, (a, b) in enumerate(zip(old_gen, new_gen, strict=True))
                )
                widened_seeded = any(
                    (target, "general", idx) in seeded_params and not b.fits(a)
                    for idx, (a, b) in enumerate(zip(old_gen, merged_gen, strict=True))
                )
                for idx, p in enumerate(merged_gen):
                    if p is not UNKNOWN:
                        seeded_params.add((target, "general", idx))
                if widened_seeded:
                    argument_changes[gen_key] += 1
                    if argument_changes[gen_key] > 3:
                        merged_gen = tuple(
                            b if a is UNKNOWN else widen_fact(Fact(a), Fact(b)).type
                            for a, b in zip(old_gen, merged_gen, strict=True)
                        )
            if merged_gen != old_gen:
                if not all(a.fits(b) for a, b in zip(old_gen, merged_gen, strict=True)):
                    narrowed_targets.add(target)
                parameters[gen_key] = merged_gen
                changed = True
        for key, body in bodies.items():
            caller, ctx_name = key
            if (caller, "observed") in bodies:
                if ctx_name != "observed" and not ctx_name.startswith("observed."):
                    continue
            elif (caller, "fixnum") in bodies and caller in declared_entry_lambdas:
                if ctx_name != "fixnum":
                    continue
            elif ctx_name != "general":
                continue
            for call in body.calls:
                target = call.target.as_lambda() if call.target is not None else None
                if target is None or target not in graphs or not target.vars:
                    # Imported targets still predict guarded calls, but their
                    # defining analysis owns their argument and entry contexts.
                    continue
                bound_facts = call.bind_argument_facts(target)
                if bound_facts is None or any(f.type is BOTTOM for f in bound_facts):
                    continue
                for index, (actual_fact, contract, var) in enumerate(
                    zip(
                        bound_facts,
                        target.signature.annotations,
                        target.vars,
                        strict=True,
                    )
                ):
                    if contract is None:
                        declared = target.local_annotations.get(var.name)
                        if declared is not None and declared.contradicts(
                            actual_fact.type
                        ):
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
                    if (domain := argument.argument_domain().dispatch_domain())
                    is not None
                    and (contract is None or not contract.contradicts(domain))
                    else UNKNOWN
                    for argument, contract in zip(
                        bound_facts,
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
                        and (
                            (fix_param := fix_params[idx])
                            in (
                                LIST_TYPE,
                                DICT_TYPE,
                                TEXT_TYPE,
                                BYTES_TYPE,
                                BYTEARRAY_TYPE,
                                FLOAT,
                            )
                            or fix_param.instance_class() is not None
                        )
                        else UNKNOWN
                    )
                    for idx, domain in enumerate(raw_domains)
                )
                is_unseeded_recursive = (
                    (caller is target or caller in can_reach.get(target, ()))
                    and (caller, "observed") not in bodies
                    and caller not in closed_propagated
                )
                dest_seeds = recursive_seeds if is_unseeded_recursive else seeds
                if not is_unseeded_recursive:
                    # Keep scalar representations separate: an int/float union
                    # loses the native operations available to each call.
                    dispatch_sig = tuple(
                        (
                            idx,
                            c_target,
                            dom.widen_constant()
                            if dom.is_fixnum or dom.fits_float(FLOAT)
                            else None,
                        )
                        for idx, dom in enumerate(domains)
                        if (c_target := dom.callable_target()) is not None
                        or dom.is_fixnum
                        or dom.fits_float(FLOAT)
                    )
                    if dispatch_sig:
                        grp = dispatch_seed_groups[target]
                        if dispatch_sig not in grp:
                            grp[dispatch_sig] = domains
                        else:
                            grp[dispatch_sig] = tuple(
                                old.join(new).dispatch_domain() or UNKNOWN
                                for old, new in zip(
                                    grp[dispatch_sig], domains, strict=True
                                )
                            )
                if target not in dest_seeds:
                    dest_seeds[target] = domains
                else:
                    dest_seeds[target] = tuple(
                        old.join(new).dispatch_domain() or UNKNOWN
                        for old, new in zip(dest_seeds[target], domains, strict=True)
                    )
        for target, rec_seed in recursive_seeds.items():
            if target not in seeds and (target, "observed") not in parameters:
                seeds[target] = rec_seed
        ctx_seeds: list[tuple[tuple[Lambda, str], tuple[ValueType, ...]]] = []
        for target, seed in seeds.items():
            grp = dispatch_seed_groups.get(target)
            if grp is not None and 1 < len(grp) <= 4:
                for idx_c, c_seed in enumerate(grp.values()):
                    c_name = "observed" if idx_c == 0 else f"observed.{idx_c}"
                    ctx_seeds.append(((target, c_name), c_seed))
            else:
                ctx_seeds.append(((target, "observed"), seed))
        for key, seed in ctx_seeds:
            target, ctx_name = key
            old = parameters.get(key)
            if old is None:
                for idx, p in enumerate(seed):
                    if p is not UNKNOWN:
                        seeded_params.add((target, ctx_name, idx))
            else:
                new_seed = []
                for idx, (a, b) in enumerate(zip(old, seed, strict=True)):
                    if (
                        a is UNKNOWN
                        and b is not UNKNOWN
                        and (target, ctx_name, idx) not in seeded_params
                    ):
                        new_seed.append(b)
                    elif (
                        b is not UNKNOWN
                        and b != a
                        and b.fits(a)
                        and (target, ctx_name, idx) not in narrowed_seed_params
                    ):
                        narrowed_seed_params.add((target, ctx_name, idx))
                        new_seed.append(b)
                    else:
                        new_seed.append(a.join(b).dispatch_domain() or UNKNOWN)
                seed = tuple(new_seed)
                widened_seeded = any(
                    (target, ctx_name, idx) in seeded_params and not b.fits(a)
                    for idx, (a, b) in enumerate(zip(old, seed, strict=True))
                )
                for idx, p in enumerate(seed):
                    if p is not UNKNOWN:
                        seeded_params.add((target, ctx_name, idx))
                if widened_seeded:
                    argument_changes[key] += 1
                    if argument_changes[key] > 3:
                        seed = tuple(
                            b
                            if a is UNKNOWN
                            else (
                                widen_fact(Fact(a), Fact(b)).type.dispatch_domain()
                                or UNKNOWN
                            )
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
            # so only functions that depend on narrowed arguments or captures
            # reset their provisional return summary.
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
            back_edge_targets = {
                callee
                for k, rec in inputs.items()
                if k[0] in affected
                for callee in rec[3]
                if callee in affected and lambda_order[k[0]] <= lambda_order[callee]
            }
            for key, expected in parameters.items():
                if (key[1] == "observed" or key[1].startswith("observed.")) and key[
                    0
                ] in narrowed_targets:
                    summaries[key] = BOTTOM
                    return_changes.pop(key, None)
                    pending_reset.discard(key)
                elif key[0] in affected:
                    recorded = inputs.get(key)
                    if (
                        recorded is None
                        or recorded[0] != expected
                        or recorded[2] != closure_inputs(key[0])
                        or key[0] in back_edge_targets
                    ):
                        summaries[key] = BOTTOM
                        return_changes.pop(key, None)
                        pending_reset.discard(key)
                    elif affected.intersection(recorded[3]):
                        pending_reset.add(key)
        if (
            field_changes < 3
            and not any(
                (lam, "observed") in parameters and (lam, "observed") not in bodies
                for lam, _ in setattr_sites
            )
            and _update_class_field_types(
                class_facts,
                setattr_sites,
                setitem_sites,
                bodies,
                parameters,
                list_mutation_sites,
            )
        ):
            field_changes += 1
            inputs.clear()
            deriver_cache.clear()
            pending_reset.update(parameters)
            changed = True
        if not changed:
            break

    for var in captured_vars:
        if var.optimistic_type is None or var.optimistic_type is BOTTOM:
            var.optimistic_type = UNKNOWN
        else:
            var.optimistic_type = var.optimistic_type.normalized_bottom()
    if state_cache is not None:
        state_cache["captured_types"] = {
            var: var.optimistic_type for var in captured_vars
        }

    for ref, old_type in old_constant_ref_types.items():
        ref.derived_type = old_type

    prev_results: dict[Lambda, LambdaFacts] = (
        state_cache.get("results", {}) if state_cache is not None else {}
    )
    prev_bodies_ref: dict[tuple[Lambda, str], Body] = (
        state_cache.get("bodies", {}) if state_cache is not None else {}
    )
    prev_summaries_ref: dict[tuple[Lambda, str], ValueType] = (
        state_cache.get("summaries", {}) if state_cache is not None else {}
    )
    prev_parameters_ref: dict[tuple[Lambda, str], tuple[ValueType, ...]] = (
        state_cache.get("parameters", {}) if state_cache is not None else {}
    )
    results: dict[Lambda, LambdaFacts] = {}
    for lambda_ in lambdas:
        obs_ctxs = _observed_context_names(lambda_, parameters)
        prev_obs_ctxs = _observed_context_names(lambda_, prev_parameters_ref)
        if (
            lambda_ not in must_recompute
            and lambda_ in prev_results
            and obs_ctxs == prev_obs_ctxs
            and all(
                bodies.get((lambda_, ctx)) is prev_bodies_ref.get((lambda_, ctx))
                and summaries.get((lambda_, ctx))
                == prev_summaries_ref.get((lambda_, ctx))
                and parameters.get((lambda_, ctx))
                == prev_parameters_ref.get((lambda_, ctx))
                for ctx in ("general", "fixnum", *obs_ctxs)
            )
        ):
            results[lambda_] = prev_results[lambda_]
            continue
        graph = graphs[lambda_]
        general = bodies[lambda_, "general"]
        facts = LambdaFacts(
            general.types(),
            general.targets(),
            return_type=summaries[lambda_, "general"].normalized_bottom(),
            parameters=parameters[lambda_, "general"],
            element_contracts=dict(general.element_contracts),
            in_bounds_nodes=general.in_bounds_nodes,
        )
        entries = []
        obs_params = parameters.get((lambda_, "observed"))
        fix_params = parameters.get((lambda_, "fixnum"))
        variant_order = (
            ("fixnum", *obs_ctxs)
            if obs_params is not None
            and fix_params is not None
            and all(
                a.fits(b) or a is UNKNOWN
                for a, b in zip(obs_params, fix_params, strict=True)
            )
            and not all(a.fits(b) for a, b in zip(obs_params, fix_params, strict=True))
            else (*obs_ctxs, "fixnum")
        )
        for identifier in variant_order:
            key = lambda_, identifier
            if key not in parameters:
                continue
            body = bodies[key]
            candidate_types = body.types()
            ret_type = summaries[key].normalized_bottom()
            is_observed_id = identifier == "observed" or identifier.startswith(
                "observed."
            )
            if (
                identifier == "fixnum"
                and lambda_.entry_parameter_types is None
                and not any(
                    param.has_float_specialization()
                    and not gen_p.has_float_specialization()
                    for param, gen_p in zip(
                        parameters[key], facts.parameters, strict=True
                    )
                )
                and body.element_contracts == facts.element_contracts
                and body.in_bounds_nodes == facts.in_bounds_nodes
                and not _specializes_body(
                    graph,
                    facts.types,
                    candidate_types,
                    facts.targets,
                    body.targets(),
                )
                and ret_type == facts.return_type
            ):
                continue
            if (
                is_observed_id
                and not any(
                    (param.is_fixnum and not gen_p.is_fixnum)
                    or (
                        param.has_float_specialization()
                        and not gen_p.has_float_specialization()
                    )
                    for param, gen_p in zip(
                        parameters[key], facts.parameters, strict=True
                    )
                )
                and body.element_contracts == facts.element_contracts
                and body.in_bounds_nodes == facts.in_bounds_nodes
                and not _specializes_body(
                    graph,
                    facts.types,
                    candidate_types,
                    facts.targets,
                    body.targets(),
                )
                and ret_type == facts.return_type
            ):
                continue
            if (
                is_observed_id
                and (lambda_, "fixnum") in parameters
                and (
                    bodies[lambda_, "fixnum"].element_contracts
                    != facts.element_contracts
                    or bodies[lambda_, "fixnum"].in_bounds_nodes
                    != facts.in_bounds_nodes
                    or _specializes_body(
                        graph,
                        facts.types,
                        bodies[lambda_, "fixnum"].types(),
                        facts.targets,
                        bodies[lambda_, "fixnum"].targets(),
                    )
                )
                and (
                    all(
                        obs_p is UNKNOWN or obs_p == fix_p
                        for obs_p, fix_p in zip(
                            parameters[key],
                            parameters[lambda_, "fixnum"],
                            strict=True,
                        )
                    )
                    or (
                        all(
                            obs_p.fits(fix_p)
                            for obs_p, fix_p in zip(
                                parameters[key],
                                parameters[lambda_, "fixnum"],
                                strict=True,
                            )
                        )
                        and not any(
                            (obs_p.is_fixnum and not fix_p.is_fixnum)
                            or (
                                obs_p.has_float_specialization()
                                and not fix_p.has_float_specialization()
                            )
                            for obs_p, fix_p in zip(
                                parameters[key],
                                parameters[lambda_, "fixnum"],
                                strict=True,
                            )
                        )
                        and body.element_contracts
                        == bodies[lambda_, "fixnum"].element_contracts
                        and body.in_bounds_nodes
                        == bodies[lambda_, "fixnum"].in_bounds_nodes
                        and not _specializes_body(
                            graph,
                            bodies[lambda_, "fixnum"].types(),
                            candidate_types,
                            bodies[lambda_, "fixnum"].targets(),
                            body.targets(),
                        )
                        and ret_type == summaries[lambda_, "fixnum"].normalized_bottom()
                    )
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
                    dict(body.element_contracts),
                    body.in_bounds_nodes,
                )
            )
        facts.variants = tuple(entries)
        if lambda_.kind is not FunctionalKind.TOPLEVEL:
            facts.final_bindings = tuple(
                leaf
                for leaf in global_leaves[lambda_.namespace].values()
                if leaf.final_function is not None
                and any(ref in facts.types for ref in leaf.refs)
            )
        results[lambda_] = facts
        _mark_guarded_calls(graph, general)
        _publish_derived_types(lambda_, graph, facts.types)
    analysis_finished = True
    if state_cache is not None:
        state_cache["graphs"] = graphs
        state_cache["revisions"] = {lam: lam.ir1_revision for lam in lambdas}
        state_cache["bodies"] = dict(bodies)
        state_cache["inputs"] = dict(inputs)
        state_cache["summaries"] = dict(summaries)
        state_cache["parameters"] = dict(parameters)
        state_cache["deriver_cache"] = dict(deriver_cache)
        state_cache["ignored"] = set(annotations.ignored)
        state_cache["results"] = dict(results)
        state_cache["class_fields"] = {
            cls_id: dict(cf.fields) for cls_id, cf in class_facts.items()
        }
        state_cache["class_widened_fields"] = {
            cls_id: set(cf.widened_fields) for cls_id, cf in class_facts.items()
        }
        state_cache["class_layouts"] = {
            cls_id: InstanceType(cls_id, fixed_slots=cf.fixed_slots)
            for cls_id, cf in class_facts.items()
        }
        state_cache["inferred_global_constants"] = {
            ns: dict(c) for ns, c in inferred_global_constants.items()
        }
        state_cache["default_types"] = {
            lam: lam.default_types for lam in lambdas if lam.default_types
        }
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

    def visit_If(self, node):
        new_type = _recompute_lvar_type(node.test)
        if new_type != self.old_lvar_types.get(node.test) and (
            new_type.truth_value() is not None
            or ir1opt.constant_lvar_value(node.test) is not None
        ):
            node.reoptimize = True
            node.block.reoptimize = True
            if self.lambda_.component is not None:
                self.lambda_.component.reoptimize = True
            self.reoptimized = True

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
            or info.flushable
            or info.flushable_test is not None
            or (info.optimizer is not None and info is not GETITEM)
        ):
            return
        checked_args = _checked_transform_args(info, node)
        if any(
            (new_type := _recompute_lvar_type(arg)) != self.old_lvar_types.get(arg)
            and new_type is not UNKNOWN
            for arg in checked_args
        ):
            node.reoptimize = True
            node.block.reoptimize = True
            if (
                info is GETSLICE
                and node.lvar is not None
                and node.lvar.dest is not None
            ):
                node.lvar.dest.accept(_ReoptimizeConsumer(self))
            if self.lambda_.component is not None:
                self.lambda_.component.reoptimize = True
            self.reoptimized = True


class _ResolveMethodReceiverLvar(NodeVisitor):
    def visit_Node(self, node):
        return ()

    def visit_Combination(self, node: Combination):
        if node.fun_info is RESOLVE_METHOD and node.args:
            return (node.args[0],)
        return ()


_RESOLVE_METHOD_RECEIVER_LVAR = _ResolveMethodReceiverLvar()


def _checked_transform_args(info, node: Combination):
    if info is RANGE_PREPARE and node.info is not None and not node.info.direct:
        return (*node.args, *node.info.arguments)
    if info is CALL_RESOLVED and node.args and len(node.args[0].uses) == 1:
        return (
            *node.args[0].uses[0].accept(_RESOLVE_METHOD_RECEIVER_LVAR),
            *node.args,
        )
    return node.args


class _ReoptimizeConsumer(NodeVisitor, LeafVisitor):
    def __init__(self, reoptimizer: _ReoptimizeNarrowedKnownCalls):
        self.reoptimizer = reoptimizer
        self.active: set[LambdaVar] = set()

    def visit_Node(self, node):
        if node.prev is not None and node.block is not None:
            node.reoptimize = True
            node.block.reoptimize = True
            self.reoptimizer.reoptimized = True

    def visit_Set(self, node):
        node.var.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf: LambdaVar):
        if len(leaf.sets) != 1 or leaf in self.active:
            return
        self.active.add(leaf)
        for ref in leaf.refs:
            if ref.lvar is not None and ref.lvar.dest is not None:
                ref.lvar.dest.accept(self)


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


def _would_reoptimize_for_context(lambda_: Lambda, context: EntryContext) -> bool:
    component = lambda_.component
    if component is None or context.id is None:
        return False
    if (
        context.id not in ("fixnum", "observed")
        and not context.id.startswith("observed.")
        and not context.id.startswith("borrow.fixnum.")
        and not (
            context.id.startswith("borrow.observed.") and context.convention is not None
        )
    ):
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
            or info.flushable
            or info.flushable_test is not None
            or (info.optimizer is not None and info is not GETITEM)
            or (
                loop.is_hoistable_op(info)
                and node.block.loop is not None
                and node.block.loop.is_natural
            )
        ):
            return False
        checked_args = _checked_transform_args(info, node)
        for arg in checked_args:
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
    active_contexts = _ACTIVE_STATE.specialized_contexts
    active_key = (lambda_, context.id)
    if active_key in active_contexts or not _would_reoptimize_for_context(
        lambda_, context
    ):
        yield
        return
    active_contexts.add(active_key)
    cloned_component, node_map, restore = clone_lambda_component(lambda_)
    old_types = context.types
    old_targets = context.targets
    old_element_contracts = context.element_contracts
    old_in_bounds_nodes = context.in_bounds_nodes
    old_escape_facts = context.escape_facts
    old_base_escape_facts = context.base_escape_facts
    try:
        cloned_types = {
            node_map.get(node, node): vtype for node, vtype in old_types.items()
        }
        cloned_targets = {
            node_map.get(node, node): target for node, target in old_targets.items()
        }
        cloned_element_contracts = {
            node_map.get(node, node): contract
            for node, contract in old_element_contracts.items()
        }
        cloned_in_bounds_nodes = frozenset(
            node_map.get(node, node) for node in old_in_bounds_nodes
        )
        graph = build_lambda_graph(lambda_)
        reopt = _publish_derived_types(lambda_, graph, cloned_types)
        if reopt:
            ir1opt.ir1_optimize_until_done(cloned_component)
            join_blocks_if_possible(cloned_component)
            find_dfo(cloned_component)
            loop.analyze(cloned_component)
        hoisted = loop.hoist_loop_invariants(cloned_component)
        if reopt or hoisted:
            if hoisted:
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
        context.element_contracts = cloned_element_contracts
        context.in_bounds_nodes = cloned_in_bounds_nodes
        context.base_escape_facts = old_escape_facts or old_base_escape_facts
        context.escape_facts = None
        yield
    finally:
        active_contexts.discard(active_key)
        context.types = old_types
        context.targets = old_targets
        context.element_contracts = old_element_contracts
        context.in_bounds_nodes = old_in_bounds_nodes
        context.escape_facts = old_escape_facts
        context.base_escape_facts = old_base_escape_facts
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
