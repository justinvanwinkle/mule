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
from dataclasses import dataclass

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
    join_fact,
    propagate,
    widen_fact,
)
from .ctype import (
    BOTTOM,
    FIXNUM_MAX,
    FIXNUM_MIN,
    TEXT_TYPE,
    UNBOUND,
    UNKNOWN,
    IntegerType,
    ValueType,
)
from .fndb import LEN, flow_propagation
from .ir1util import made_function
from .node import (
    CallKind,
    FunctionalKind,
    Lambda,
    LeafVisitor,
    NodeVisitor,
    Set,
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


class _ParameterUse(NodeVisitor, LeafVisitor):
    """Classifies how one parameter (and local copies of it) is used."""

    def __init__(self, parameter):
        self.copies = {parameter}
        self.queue = [parameter]
        self.numeric = False
        self.disqualified = False
        self.text = False
        self.text_disqualified = False
        self.current = None

    def analyze(self, contract, full: ValueType) -> ValueType:
        while self.queue and not (self.disqualified and self.text_disqualified):
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
        return UNKNOWN

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
            return
        info = node.fun_info
        if info is None:
            if is_predictable_len_call(node) and node.args and node.args[0] is lvar:
                self.disqualified = True
            return
        name = info.name
        if name == "%resolve_method" and node.args and node.args[0] is lvar:
            self.disqualified = True
            if linked(node.info, node, "info").name in _TEXT_METHODS:
                self.text = True
            else:
                self.text_disqualified = True
            return
        if node.args and node.args[0] is lvar and name in {"%setitem", "%delitem"}:
            self.disqualified = True
            self.text_disqualified = True
            return
        if node.args and node.args[0] is lvar and name in _NON_INTEGER_FIRST_OPERAND:
            self.disqualified = True
            return
        if name == "%resolve_super_method" and lvar in node.args:
            self.disqualified = True
            self.text_disqualified = True
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
            return
        if node.args and node.args[0] is lvar and name in {"%getitem", "%getslice"}:
            return
        if name in _NUMERIC_KNOWN:
            self.numeric = True


class _HasCombinationImprovement(NodeVisitor):
    """Whether ``candidate`` narrowed any combination's type over ``general``."""

    def __init__(
        self, general: dict[object, ValueType], candidate: dict[object, ValueType]
    ):
        self.general = general
        self.candidate = candidate
        self.improved = False

    def visit_Combination(self, node):
        if self.candidate.get(node) != self.general.get(node):
            self.improved = True
            return
        if is_predictable_len_call(node) and len(node.args[0].uses) == 1:
            arg_use = node.args[0].uses[0]
            if known_length_primitive(
                self.candidate.get(arg_use, UNKNOWN)
            ) is not known_length_primitive(self.general.get(arg_use, UNKNOWN)):
                self.improved = True


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


def _noop():
    return None


def _with_defined_arguments(arguments, continuation):
    for argument in arguments:
        defined = argument.when_defined(_noop)
        if defined is not None:
            return defined
    return continuation()


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
        parameters[lambda_, "general"] = (UNKNOWN,) * len(lambda_.vars)
        signature = lambda_.signature
        if lambda_.vars and not signature.variadic:
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
    summaries: dict[tuple[Lambda, str], ValueType] = dict.fromkeys(parameters, BOTTOM)
    return_changes = defaultdict(int)
    argument_changes = defaultdict(int)
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
        order_contexts = (
            ("fixnum", "observed", "general")
            if obs is not None
            and fix is not None
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
            for actual, contract, var in zip(
                arguments, target.signature.annotations, target.vars, strict=True
            ):
                if contract is not None:
                    annotations.check(
                        contract, actual, location, ("local", target, var.name)
                    )
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
                domains = tuple(
                    domain
                    if (domain := type_.dispatch_domain()) is not None
                    and (contract is None or not contract.contradicts(domain))
                    else UNKNOWN
                    for type_, contract in zip(
                        call.arguments, target.signature.annotations, strict=True
                    )
                )
                if all(domain is UNKNOWN for domain in domains):
                    continue
                if target not in seeds:
                    seeds[target] = domains
                else:
                    seeds[target] = tuple(
                        old.join(new)
                        for old, new in zip(seeds[target], domains, strict=True)
                    )
        narrowed_targets: set[Lambda] = set()
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


__all__ = ["EntryContext", "LambdaFacts", "analyze", "global_bindings", "join_fact"]
