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
from .constraint import Body, Fact, join_fact, propagate, widen_fact
from .ctype import BOTTOM, FIXNUM_MAX, FIXNUM_MIN, UNKNOWN, IntegerType, ValueType
from .node import CallKind, Combination, GlobalVar, Lambda, Set, linked


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
        namespace = targets[lambda_.namespace]
        for block in lambda_.component.blocks():
            for node in block.nodes():
                if not isinstance(node, Set) or not isinstance(node.var, GlobalVar):
                    continue
                if node.value is None:
                    continue
                producer = node.value.use()
                if (
                    isinstance(producer, Combination)
                    and producer.kind is CallKind.KNOWN
                    and producer.fun_info.name == "%make_function"
                ):
                    namespace[node.var.name] = producer.args[0].use().leaf
    return targets


def _with_defined_arguments(arguments, continuation):
    if not arguments:
        return continuation()
    return arguments[0].when_defined(
        lambda: _with_defined_arguments(arguments[1:], continuation)
    )


def analyze(lambdas, annotations: AnnotationLedger) -> dict[Lambda, LambdaFacts]:
    """Solve local-call arguments and results, then publish entry contexts.

    General entries model externally supplied values. Observed integer
    contexts and an all-fixnum context describe optional guarded entries, never
    changes to a function's public contract. Return summaries start empty, grow
    by union, and widen cyclic intervals before publication.
    """
    lambdas = list(lambdas)
    global_targets = global_bindings(lambdas)
    full = IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    parameters: dict[tuple[Lambda, str], tuple[ValueType, ...]] = {}
    for lambda_ in lambdas:
        parameters[lambda_, "general"] = (UNKNOWN,) * len(lambda_.vars)
        signature = lambda_.signature
        if (
            lambda_.vars
            and not signature.variadic
            and not any(
                contract is not None and contract.contradicts(full)
                for contract in signature.annotations
            )
        ):
            parameters[lambda_, "fixnum"] = (full,) * len(lambda_.vars)
    summaries: dict[tuple[Lambda, str], ValueType] = dict.fromkeys(parameters, BOTTOM)
    return_changes = defaultdict(int)
    argument_changes = defaultdict(int)
    bodies: dict[tuple[Lambda, str], Body] = {}
    # What each body's last propagation read: its parameters and, for every
    # callee whose summary it consulted, that callee's contexts and summaries.
    # A body whose inputs are unchanged would propagate identically.
    inputs: dict[tuple[Lambda, str], tuple] = {}

    def callee_inputs(target, context_parameters, context_summaries):
        return tuple(
            (
                context_parameters.get((target, context)),
                context_summaries.get((target, context)),
            )
            for context in ("observed", "fixnum", "general")
        )

    # The callees whose summaries the current propagation consulted.
    consulted: set[Lambda] = set()

    while True:
        changed = False
        previous_summaries = dict(summaries)
        previous_parameters = dict(parameters)

        def resolve(
            callee: Fact,
            arguments,
            location=None,
            keywords=(),
            context_parameters=previous_parameters,
            context_summaries=previous_summaries,
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
            consulted.add(target)
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
            for context in ("observed", "fixnum", "general"):
                key = target, context
                expected = context_parameters.get(key)
                if expected is not None and all(
                    actual.fits(wanted)
                    for actual, wanted in zip(arguments, expected, strict=True)
                ):
                    return context_summaries[key]
            return UNKNOWN

        for key, expected in previous_parameters.items():
            lambda_ = key[0]
            recorded = inputs.get(key)
            if (
                recorded is not None
                and recorded[0] == expected
                and all(
                    callee_inputs(target, previous_parameters, previous_summaries)
                    == state
                    for target, state in recorded[1].items()
                )
            ):
                body = bodies[key]
            else:
                consulted.clear()
                body = propagate(
                    lambda_,
                    global_targets[lambda_.namespace],
                    expected,
                    resolve,
                    annotations,
                )
                bodies[key] = body
                inputs[key] = (
                    expected,
                    {
                        target: callee_inputs(
                            target, previous_parameters, previous_summaries
                        )
                        for target in consulted
                    },
                )
            returned = summaries[key].join(body.return_type)
            if returned != summaries[key]:
                return_changes[key] += 1
                if return_changes[key] > 3:
                    returned = widen_fact(Fact(summaries[key]), Fact(returned)).type
                if returned != summaries[key]:
                    summaries[key] = returned
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
                domains = tuple(type_.fixnum_domain() for type_ in call.arguments)
                if any(domain is None for domain in domains):
                    continue
                if any(
                    contract is not None and contract.contradicts(domain)
                    for domain, contract in zip(
                        domains, target.signature.annotations, strict=True
                    )
                ):
                    continue
                if target not in seeds:
                    seeds[target] = domains
                else:
                    seeds[target] = tuple(
                        old.join(new)
                        for old, new in zip(seeds[target], domains, strict=True)
                    )
        contexts_changed = False
        for target, seed in seeds.items():
            key = target, "observed"
            old = parameters.get(key)
            if old is not None:
                seed = tuple(a.join(b) for a, b in zip(old, seed, strict=True))
                if old != seed:
                    argument_changes[key] += 1
                    if argument_changes[key] > 3:
                        seed = tuple(
                            widen_fact(Fact(a), Fact(b)).type.fixnum_domain()
                            for a, b in zip(old, seed, strict=True)
                        )
            if old != seed:
                parameters[key] = seed
                summaries.setdefault(key, BOTTOM)
                changed = True
                contexts_changed = True
        if contexts_changed:
            # A new, narrower callee context can replace a broad summary used
            # provisionally by callers. Solve returns afresh for the current
            # context graph rather than permanently retaining that broad type.
            summaries = dict.fromkeys(parameters, BOTTOM)
            return_changes.clear()
        if not changed:
            break

    results: dict[Lambda, LambdaFacts] = {}
    for lambda_ in lambdas:
        general = bodies[lambda_, "general"]
        facts = LambdaFacts(
            general.types(),
            general.targets(),
            return_type=summaries[lambda_, "general"].normalized_bottom(),
        )
        entries = []
        for identifier in ("observed", "fixnum"):
            key = lambda_, identifier
            if key not in parameters:
                continue
            if identifier == "observed" and parameters[key] == parameters.get(
                (lambda_, "fixnum")
            ):
                continue
            body = bodies[key]
            entries.append(
                EntryContext(
                    identifier,
                    parameters[key],
                    body.types(),
                    body.targets(),
                    summaries[key].normalized_bottom(),
                )
            )
        facts.variants = tuple(entries)
        results[lambda_] = facts
        _mark_guarded_calls(lambda_, general)
    return results


def _mark_guarded_calls(lambda_: Lambda, body: Body) -> None:
    """Record predicted targets on full calls so later phases and code
    generation can guard them; the general path always remains."""
    for block in linked(lambda_.component, lambda_, "component").blocks():
        for node in block.nodes():
            if not isinstance(node, Combination) or node.kind not in (
                CallKind.FULL,
                CallKind.GUARDED,
            ):
                continue
            fact = body.facts.get(node.fun.use())
            if fact is None or fact.target is None:
                node.kind = CallKind.FULL
                node.predicted = None
                node.proven = False
                continue
            node.kind = CallKind.GUARDED
            node.predicted = fact.target
            node.proven = fact.proven


__all__ = ["EntryContext", "LambdaFacts", "analyze", "global_bindings", "join_fact"]
