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

from mule import ast

from . import ir1opt, loop
from .annotation_types import AnnotationLedger
from .constraint import (
    BOOTSTRAP_BUILTIN_TARGETS,
    NO_FACT,
    Body,
    Fact,
    LambdaGraph,
    build_lambda_graph,
    constant_type,
    propagate,
    widen_fact,
)
from .ctype import (
    BOTTOM,
    BUILTIN_CLASS_TYPES,
    BYTEARRAY_TYPE,
    BYTES_TYPE,
    DICT_TYPE,
    FIXNUM_MAX,
    FIXNUM_MIN,
    FLOAT,
    LIST_TYPE,
    NONE_TYPE,
    TEXT_TYPE,
    UNBOUND,
    UNKNOWN,
    InstanceType,
    IntegerType,
    TupleType,
    ValueType,
)
from .dfo import find_dfo
from .fndb import (
    CALL_RESOLVED,
    GETSLICE,
    LEN,
    RANGE_PREPARE,
    RESOLVE_METHOD,
)
from .ir1util import (
    clone_lambda_component,
    join_blocks_if_possible,
    known_calls,
    made_function,
)
from .locall_classes import (
    _CLASS_BASE_NAMES,
    _GLOBAL_CALLEE_REF_NAME,
    ClassFacts,
    _collect_class_tables,
    _CollectSetattrSites,
    _ProtocolSelfPrediction,
    _update_class_field_types,
)
from .locall_globals import (
    _ACTIVE_STATE,
    _IMPLICIT_MODULE_GLOBALS,
    _INSTANCE_PREDICTION,
    _MAX_DERIVATION_DEPTH,
    _MUTATES_FUNCTION_DEFAULTS,
    _PREDICTABLE_GLOBAL_FUN_INFO,
    _SET_GLOBAL_VAR,
    GETITEM,
    EntryContext,
    LambdaFacts,
    _collect_closed_lambdas,
    _default_param_container_is_mutated,
    _global_container_is_mutated,
    _GlobalFunctionStores,
    _ImportBindingStores,
    _MadeConstant,
    _MakeFunctionDefaults,
    _ParameterUse,
    _specializes_body,
    _unconditional_toplevel_stores,
    _widen_user_constant_type,
    _with_defined_arguments,
    collect_global_constants,
    global_bindings,
    has_module_namespace_mutations,
)
from .node import (
    CallKind,
    ClassConstructorTarget,
    Combination,
    ForeignCallTarget,
    FunctionalKind,
    GlobalVar,
    Lambda,
    LambdaVar,
    LeafVisitor,
    NodeVisitor,
    Ref,
    Set,
)
from .seqtran import (
    is_predictable_len_call,
)


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
            or imported_targets is None
            or builtin_targets is None
        ):
            from .driver import cached_builtins_interface

            interface = cached_builtins_interface()
            if imported_function_types is None:
                imported_function_types = interface.exported_types
            if storage_methods is None:
                storage_methods = interface.storage_methods
            if builtin_return_types is None:
                builtin_return_types = interface.builtin_return_types
            if imported_targets is None:
                imported_targets = interface.exported_targets
            if builtin_targets is None:
                builtin_targets = interface.builtin_targets
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
        if (
            lambda_.vars
            and not signature.variadic
            and lambda_.name != "__annotate__"
            and not lambda_.name.endswith(".__annotate__")
        ):
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
    if class_facts:
        for lambda_ in lambdas:
            graph = graphs[lambda_]
            site_collector = _CollectSetattrSites([])
            for block in graph.blocks:
                for node in graph.block_nodes[block]:
                    node.accept(site_collector)
            for site in site_collector.sites:
                setattr_sites.append((lambda_, site))
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
                    imported_function_types if target_lam.policy.safety < 3 else None
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
                        global_constants=global_constants_by_ns.get(lambda_.namespace),
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
                                or not (
                                    vtype.fits_list(LIST_TYPE)
                                    or vtype.fits_dict(DICT_TYPE)
                                    or vtype.fits_set()
                                    or vtype.fits_tuple(TupleType())
                                )
                            ):
                                continue
                            if vtype.conservative_type() != vtype and (
                                _global_container_is_mutated(leaf)
                            ):
                                vtype = vtype.conservative_type()
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
                                            def_type.conservative_type() != def_type
                                            and param_var is not None
                                            and _default_param_container_is_mutated(
                                                param_var
                                            )
                                        ):
                                            def_type = def_type.conservative_type()
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
                parameters[var.home, "general"][var.home.vars.index(var)]
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
                    joined = joined.join(
                        home_body.facts.get(store.value.use(), NO_FACT).type
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
                bodies,
                parameters,
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
    for lambda_ in ordered_lambdas:
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
                    (param.is_fixnum and not gen_p.is_fixnum)
                    or (param.is_exact_list and not gen_p.is_exact_list)
                    or (
                        param.has_float_specialization()
                        and not gen_p.has_float_specialization()
                    )
                    or getattr(param, "fixed_slots", ())
                    != getattr(gen_p, "fixed_slots", ())
                    for param, gen_p in zip(
                        parameters[key], facts.parameters, strict=True
                    )
                )
                and body.element_contracts == facts.element_contracts
                and body.in_bounds_nodes == facts.in_bounds_nodes
                and ret_type == facts.return_type
                and sum(len(graph.block_nodes[b]) for b in graph.blocks) > 80
            ):
                continue
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
                    results,
                    parameters,
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
                    results,
                    parameters,
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
                        results,
                        parameters,
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
                            results,
                            parameters,
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
        state_cache["class_facts"] = class_facts
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
        checked_args = checked_transform_args(info, node)
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


def checked_transform_args(info, node: Combination):
    if info is RANGE_PREPARE and node.info is not None and not node.info.direct:
        return (*node.args, *node.info.arguments)
    if info.name == "%byte_next" and node.info is not None:
        return (*node.args, node.info.source)
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
        checked_args = checked_transform_args(info, node)
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


class CollectNodeTypesAndTargets(NodeVisitor, LeafVisitor):
    def __init__(
        self,
        base_types: dict[object, ValueType],
        base_targets: dict[object, Lambda],
    ):
        self.types = dict(base_types)
        self.targets = dict(base_targets)

    def visit_ValuedNode(self, node):
        if node.derived_type is not None and (
            node not in self.types
            or (node.derived_type.is_fixnum and not self.types[node].is_fixnum)
        ):
            self.types[node] = node.derived_type

    def visit_Leaf(self, leaf) -> bool:
        return False

    def visit_LambdaVar(self, leaf: LambdaVar) -> bool:
        return (
            not leaf.indirect
            and bool(leaf.sets)
            and all(
                s.value is not None
                and (_lvar_current_type(s.value) or UNKNOWN).is_fixnum
                for s in leaf.sets
            )
        )

    def visit_Set(self, node: Set):
        if node.value is not None and node.var.accept(self):
            self.types[node] = _lvar_current_type(node.value) or UNKNOWN

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
            collector = CollectNodeTypesAndTargets(cloned_types, cloned_targets)
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
    "_CLASS_BASE_NAMES",
    "ClassFacts",
    "EntryContext",
    "LambdaFacts",
    "_ImportBindingStores",
    "analyze",
    "collect_global_constants",
    "global_bindings",
    "has_module_namespace_mutations",
    "made_function",
    "specialized_ir1_context",
]
