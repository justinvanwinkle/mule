"""Parameter specialization predicates, module global constant collection, and closed lambda analysis."""

from __future__ import annotations

from collections import defaultdict
from collections.abc import Mapping
from dataclasses import dataclass, field, replace
from threading import local

from mule import ast

from . import byteops, ir1opt
from .constraint import (
    LambdaGraph,
    constant_type,
)
from .ctype import (
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    BYTES_TYPE,
    FIXNUM_MAX,
    FIXNUM_MIN,
    FLOAT,
    FROZENSET_TYPE,
    INTEGER,
    NONE_TYPE,
    TEXT_TYPE,
    UNKNOWN,
    FunctionType,
    IntegerType,
    TupleType,
    UniqueObjectType,
    ValueType,
    ValueTypeVisitor,
)
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
)
from .ir1tran import _IS_FINAL_ANNOTATION
from .ir1util import (
    block_exception_target,
    made_function,
)
from .locall_classes import _GLOBAL_CALLEE_REF_NAME, _collect_module_classes
from .node import (
    BasicVar,
    CallKind,
    Cast,
    ClassConstructorTarget,
    Combination,
    Enclose,
    FunctionalKind,
    GlobalVar,
    Lambda,
    LambdaVar,
    LeafVisitor,
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
        byteops.ITERATE,
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
        results: dict[Lambda, LambdaFacts] | None = None,
        parameters: dict[tuple[Lambda, str], tuple[ValueType, ...]] | None = None,
    ):
        self.general = general
        self.candidate = candidate
        self.general_targets = general_targets or {}
        self.candidate_targets = candidate_targets or {}
        self.results = results
        self.parameters = parameters

    def _callee_entry_parameters(
        self, callee: Lambda
    ) -> tuple[tuple[ValueType, ...], ...]:
        if self.results is not None and callee in self.results:
            return tuple(
                variant.parameters for variant in self.results[callee].variants
            )
        if self.parameters is not None:
            entries = []
            for key, params in self.parameters.items():
                lam, ctx = key
                if lam is callee and ctx != "general":
                    entries.append(params)
            return tuple(entries)
        return ()

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
        if (cand_type.is_fixnum and not gen_type.is_fixnum) or (
            cand_type.has_float_specialization()
            and not gen_type.has_float_specialization()
        ):
            return True
        call_key = None
        call_args = ()
        if node.kind in (CallKind.FULL, CallKind.GUARDED):
            call_key = node.fun.uses[0] if len(node.fun.uses) == 1 else node
            call_args = node.args
        elif (
            node.kind is CallKind.KNOWN and node.fun_info is CALL_RESOLVED and node.args
        ):
            call_key = node
            resolved = node.args[0]
            if len(resolved.uses) == 1 and resolved.uses[0].args:
                call_args = (resolved.uses[0].args[0], *node.args[1:])
            else:
                call_args = node.args[1:]
        target = self.candidate_targets.get(call_key) if call_key is not None else None
        if (
            target is not None
            and (callee_lam := target.as_lambda()) is not None
            and self.general_targets.get(call_key) is target
            and callee_lam is not node.home_lambda
        ):
            # Forwarding a narrower argument can select a useful callee entry
            # even when a generic return fallback keeps the result unknown.
            keywords = node.info.keywords if node.info is not None else ()
            order = callee_lam.call_order(keywords, len(call_args))
            callee_entries = self._callee_entry_parameters(callee_lam)
            if order is not None and callee_entries:
                for param_idx, arg_idx in enumerate(order):
                    if arg_idx < 0 or arg_idx >= len(call_args):
                        continue
                    argument = call_args[arg_idx]
                    if len(argument.uses) != 1:
                        continue
                    source = argument.uses[0]
                    candidate = self.candidate.get(source, UNKNOWN)
                    general = self.general.get(source, UNKNOWN)
                    if (
                        candidate in (UNKNOWN, BOTTOM)
                        or candidate == general
                        or general.fits(candidate)
                    ):
                        continue
                    for entry_params in callee_entries:
                        if param_idx >= len(entry_params):
                            continue
                        expected = entry_params[param_idx]
                        if expected is UNKNOWN:
                            continue
                        if candidate.may_dispatch_fit(
                            expected
                        ) != general.may_dispatch_fit(
                            expected
                        ) or candidate.dispatch_fits(expected) != general.dispatch_fits(
                            expected
                        ):
                            return True
            elif self.results is None and self.parameters is None:
                for argument in call_args:
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
                ):
                    if node.fun_info in (GETATTR, SETATTR):
                        if node.info is not None and cand_recv.fixed_slot_index(
                            node.info.name
                        ) != gen_recv.fixed_slot_index(node.info.name):
                            return True
                    elif node.fun_info is RESOLVE_METHOD:
                        if (
                            cand_recv.is_exact_list
                            or cand_recv.fits_text(TEXT_TYPE)
                            or cand_recv.fits_bytearray()
                            or cand_recv.is_byte_storage
                        ):
                            return True
                    elif node.fun_info in (
                        BINARY[ast.BinaryOperator.BIT_AND],
                        BINARY[ast.BinaryOperator.BIT_OR],
                        BINARY[ast.BinaryOperator.SUBTRACT],
                        BINARY[ast.BinaryOperator.BIT_XOR],
                        INPLACE_BINARY[ast.BinaryOperator.BIT_AND],
                        INPLACE_BINARY[ast.BinaryOperator.BIT_OR],
                        INPLACE_BINARY[ast.BinaryOperator.SUBTRACT],
                        INPLACE_BINARY[ast.BinaryOperator.BIT_XOR],
                    ):
                        if cand_recv.fits_set():
                            return True
                    elif node.fun_info in (
                        GETITEM,
                        GETSLICE,
                        SETITEM,
                        DELITEM,
                        ITERATE,
                        byteops.ITERATE,
                        COMPARE[ast.ComparisonOperator.IN],
                        COMPARE[ast.ComparisonOperator.NOT_IN],
                    ):
                        if (
                            cand_recv.is_exact_list
                            or cand_recv.is_exact_tuple
                            or cand_recv.fits_text(TEXT_TYPE)
                            or cand_recv.fits_bytes(BYTES_TYPE)
                            or cand_recv.fits_bytearray()
                            or cand_recv.is_byte_storage
                            or cand_recv.fits_iterator()
                        ):
                            return True
                    else:
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
    results: dict[Lambda, LambdaFacts] | None = None,
    parameters: dict[tuple[Lambda, str], tuple[ValueType, ...]] | None = None,
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
    visitor = _SpecializesBody(
        general,
        candidate,
        general_targets,
        candidate_targets,
        results,
        parameters,
    )
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
        vtype = vtype.conservative_type()
        if vtype is UNKNOWN:
            return
        if self.is_protocol:
            self.constants[leaf] = vtype
            return
        if home.policy.safety < 3 and (is_final or not vtype.fits_none(NONE_TYPE)):
            self.constants[leaf] = (
                vtype if is_final else _widen_user_constant_type(vtype)
            )


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
