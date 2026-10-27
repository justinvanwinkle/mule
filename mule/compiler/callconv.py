"""Internal argument and return locations, independent of whole-body lowering.

SBCL's ``return-info`` and known local-call conventions select machine value
locations without requiring every operation in the callee to be unboxed
(``ltn-analyze-return`` and ``ir2-convert-local-known-call``). Mule expresses
those locations as LLVM storage classes. Generic object entries retain their
object convention; a numeric entry can still use ordinary object operations.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field, replace
from typing import TYPE_CHECKING

from mule import ast

from . import vops
from .ctype import UNKNOWN, ValueTypeVisitor
from .ltn import primitive_type_of
from .node import CallKind, FunctionalKind, Lambda, LeafVisitor, NodeVisitor, linked
from .represent import can_use_double
from .vop import (
    BOXED_FLOAT,
    DESCRIPTOR,
    DOUBLE,
    IMMEDIATE,
    REFERENCE,
    SIGNED,
    StorageClass,
    TNRef,
)

if TYPE_CHECKING:
    from .ltn import EntryContext

OBJECT = (DESCRIPTOR, IMMEDIATE)


def _symbol(name: str) -> str:
    if re.fullmatch(r"[A-Za-z_.$][A-Za-z0-9_.$]*", name):
        return f"@{name}"
    return f'@"{name.replace(chr(34), "")}"'


class _TupleArity(ValueTypeVisitor):
    def visit_ValueType(self, fact):
        return None

    def visit_TupleType(self, fact):
        return None if fact.may_be_unbound else fact.exact_length()


class _TupleConstruction(NodeVisitor):
    def visit_Node(self, node):
        return False

    def visit_Combination(self, node):
        return node.kind is CallKind.KNOWN and node.fun_info.name == "%build_tuple"

    def visit_Ref(self, node):
        return node.leaf.accept(_DirectTupleLiteral())


class _DirectTupleLiteral(LeafVisitor):
    def visit_Leaf(self, leaf):
        return False

    def visit_Constant(self, leaf):
        return leaf.value.accept(_TupleLiteral())


class _TupleLiteral(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Tuple(self, node):
        return all(element.accept(_ScalarLiteral()) for element in node.elements)


class _ScalarLiteral(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_NoneLiteral(self, node):
        return True

    visit_BooleanLiteral = visit_NoneLiteral
    visit_IntegerLiteral = visit_NoneLiteral
    visit_FloatLiteral = visit_NoneLiteral
    visit_TextLiteral = visit_NoneLiteral
    visit_BytesLiteral = visit_NoneLiteral


@dataclass(frozen=True, slots=True)
class TupleReturnConvention:
    """Fixed owned object locations, selected from every return value's type."""

    arity: int
    direct: frozenset = frozenset()


def tuple_return_convention(lambda_, context=None):
    """Select known tuple returns without imposing a producer spelling."""
    if lambda_.kind is FunctionalKind.TOPLEVEL or lambda_.return_ is None:
        return None
    returned = lambda_.return_
    if not returned.result.uses:
        return None
    arity = None
    direct = set()
    for producer in returned.result.uses:
        fact = context.type_of(producer) if context is not None else UNKNOWN
        if fact is UNKNOWN:
            fact = producer.derived_type
        count = fact.accept(_TupleArity())
        if (
            count is None
            or not 1 <= count <= 8
            or (arity is not None and count != arity)
        ):
            return None
        arity = count
        # Only a construction delivered directly to RETURN may bypass its tuple.
        # A local read still owns a materialized tuple and is unpacked at RETURN.
        follows = producer.next is not None and producer.next.next is returned
        edge = (
            producer.block.last is producer
            and producer.block.succ == [returned.block]
            and returned.block.start.next is returned
        )
        if (follows or edge) and producer.accept(_TupleConstruction()):
            direct.add(producer)
    assert arity is not None
    return TupleReturnConvention(arity, frozenset(direct))


@dataclass(slots=True)
class TupleDelivery:
    """IR2-selected output locations and owners at each instruction."""

    calls: dict = field(default_factory=dict)
    call_assertions: dict = field(default_factory=dict)
    direct_assertions: dict = field(default_factory=dict)
    unpacks: set = field(default_factory=set)
    slot_unpacks: dict = field(default_factory=dict)
    sequence_available: dict = field(default_factory=dict)
    moves: set = field(default_factory=set)
    refs: dict = field(default_factory=dict)
    discard_loads: set = field(default_factory=set)
    temp_tns: set = field(default_factory=set)
    remaining: dict = field(default_factory=dict)
    max_arity: int = 0


@dataclass(frozen=True, slots=True)
class CallConvention:
    parameters: tuple[StorageClass, ...]
    returned: StorageClass

    def operand_restriction(self, vop, ref):
        """Representations this entry's boundary generator can consume or emit.

        A numeric return can extract a boxed value after object-sensitive uses.
        A raw float parameter must instead reach an entirely compatible local
        group: reconstructing its original object identity would not be safe.
        Fixnum parameters may use their existing tagged representation inside
        an object body, so their initialization retains the usual restriction.
        """
        if vop.info.name == "return" and not ref.write_p:
            if self.returned is DOUBLE:
                return (DESCRIPTOR, DOUBLE)
            if self.returned is SIGNED:
                return (IMMEDIATE, SIGNED)
        elif vop.info.name == "move-argument" and ref.write_p:
            if self.parameters[vop.codegen_info] is DOUBLE:
                return (DOUBLE,)
        return None


@dataclass(slots=True)
class NumericEntry:
    function: Lambda
    context: EntryContext
    symbol: str
    proven: bool = True
    result_contract: object = None


def _numeric_argument_fits(actual, expected) -> bool:
    # Boolean facts can fit integer bounds, but their singleton pointers do
    # not have the tagged fixnum representation that a scalar entry decodes.
    if expected.is_fixnum and not actual.is_fixnum:
        return False
    if (
        primitive_type_of(expected) is BOXED_FLOAT
        and primitive_type_of(actual) is not BOXED_FLOAT
    ):
        return False
    return actual.fits(expected)


def numeric_entry(
    module,
    function: Lambda,
    context: EntryContext,
    borrowed_parameters: frozenset[int] = frozenset(),
    *,
    probe_component=None,
) -> NumericEntry:
    """Select an internal ABI without changing the entry's semantic facts."""
    returned = (
        DOUBLE if primitive_type_of(context.return_type) is BOXED_FLOAT else SIGNED
    )
    parameters = tuple(
        DOUBLE
        if primitive_type_of(fact) is BOXED_FLOAT
        else SIGNED
        if fact.is_fixnum
        else DESCRIPTOR
        for fact in context.parameters
    )
    probe_key = function, context.id
    probe = module.numeric_entries.get(probe_key)
    if probe is not None:
        parameters = linked(probe.context.convention, probe, "convention").parameters
    elif DOUBLE in parameters and probe_component is not None:
        numeric_probe = replace(
            context, convention=CallConvention(parameters, returned)
        )
        # Signature probing does not select calls or retain cloned IR1 nodes.
        # Object-sensitive parameter uses must keep the incoming object itself.
        component = probe_component(function, numeric_probe)
        self_calls = tuple(
            vop
            for block in component.blocks
            for vop in block.vops
            if vop.info in (vops.FULL_CALL, vops.LOCAL_EXTENT_CALL)
            and vop.codegen_info.target is function
        )
        parameters = tuple(
            DESCRIPTOR
            if sc is DOUBLE
            and not can_use_double(
                component, component.environment.vars[var], ignore=self_calls
            )
            else sc
            for var, sc in zip(function.vars, parameters, strict=True)
        )
    borrowed_descriptors = frozenset(
        i for i in borrowed_parameters if parameters[i] is DESCRIPTOR
    )
    context_id = (
        "borrow."
        + str(context.id or "general")
        + "."
        + ".".join(map(str, sorted(borrowed_descriptors)))
        if borrowed_descriptors
        else context.id
    )
    key = function, context_id
    existing = module.numeric_entries.get(key)
    if existing is not None:
        return existing
    numeric = replace(
        context,
        id=context_id,
        borrowed_parameters=borrowed_descriptors,
        convention=CallConvention(parameters, returned),
    )
    entry = NumericEntry(
        function,
        numeric,
        _symbol(f"{function.name}.numeric.{context_id or 'general'}"),
    )
    module.numeric_entries[key] = entry
    return entry


def select_numeric_calls(module, component) -> bool:
    """Use scalar locations for proven or guarded calls and compatible consumers."""
    if component.context.plan is not None or component.context.borrowed_parameters:
        return False
    changed = False
    block_by_vop = {vop: block for block in component.blocks for vop in block.vops}
    pending = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info in (vops.FULL_CALL, vops.LOCAL_EXTENT_CALL)
    ]
    while pending:
        remaining = []
        progress = False
        for vop in pending:
            site = vop.codegen_info
            borrowed_args = frozenset(
                index
                for index, ref in enumerate(vop.args[1:])
                if ref.tn.borrowed_from is not None
                and (
                    ref.tn.sc.owned
                    if ref.tn.sc is not None
                    else ref.tn.primitive_type.admits(DESCRIPTOR)
                )
            )
            if borrowed_args and (
                not site.proven or not (borrowed_args <= site.borrowable_arguments)
            ):
                continue
            target = site.target
            if (
                site.keywords
                or target not in module.owned
                or (
                    target in module.native_plans
                    and (site.proven or module.native_plans[target].dependencies)
                )
                or target.call_order((), len(vop.args) - 1)
                != tuple(range(len(target.vars)))
            ):
                continue
            assertion_vop = None
            result_contract = None
            if site.proven:
                result = vop.results[0].tn
            else:
                if module.closure_of(target):
                    continue
                block = block_by_vop.get(vop)
                if block is None or vop not in block.vops:
                    continue
                idx = block.vops.index(vop)
                if (
                    idx + 1 >= len(block.vops)
                    or block.vops[idx + 1].info is not vops.TYPE_CHECK
                ):
                    continue
                assertion_vop = block.vops[idx + 1]
                delivered = vop.results[0].tn
                if (
                    assertion_vop.args[0].tn is not delivered
                    or len(component.writes.get(delivered, ())) != 1
                    or len(component.reads.get(delivered, ())) != 1
                ):
                    continue
                result = assertion_vop.results[0].tn
                if assertion_vop.codegen_info.safety:
                    result_contract = assertion_vop.codegen_info.contract
            fact = result.type or UNKNOWN
            ignore_vops = (vop, assertion_vop) if assertion_vop is not None else (vop,)
            if primitive_type_of(fact) is BOXED_FLOAT:
                if not can_use_double(component, result, ignore=ignore_vops):
                    remaining.append(vop)
                    continue
            elif not fact.is_fixnum:
                continue
            arguments = tuple(arg.tn.type or UNKNOWN for arg in vop.args[1:])
            general, variants = module.contexts(target)
            context = next(
                (
                    candidate
                    for candidate in (*variants, general)
                    if all(
                        _numeric_argument_fits(actual, expected)
                        for actual, expected in zip(
                            arguments, candidate.parameters, strict=True
                        )
                    )
                    and (
                        candidate.return_type.is_fixnum
                        if fact.is_fixnum
                        else primitive_type_of(candidate.return_type) is BOXED_FLOAT
                    )
                    and candidate.return_type.fits(fact)
                ),
                None,
            )
            if context is None:
                continue
            entry = module.numeric_entry(target, context, borrowed_args)
            convention = linked(entry.context.convention, entry, "convention")
            if not site.proven and any(
                sc not in (DOUBLE, SIGNED) for sc in convention.parameters
            ):
                continue
            if assertion_vop is not None:
                delivered = vop.results[0].tn
                block = block_by_vop[vop]
                block.vops.remove(assertion_vop)
                component.reads.pop(delivered, None)
                component.writes.pop(delivered, None)
                vop.results[0] = TNRef(result, True)
                component.writes[result] = [(vop, vop.results[0])]
            restrictions = tuple(
                (DESCRIPTOR, DOUBLE)
                if sc is DOUBLE
                else (IMMEDIATE, SIGNED)
                if sc is SIGNED
                else OBJECT
                for sc in convention.parameters
            )
            vop.info = replace(
                vops.LOCAL_CALL,
                args=(
                    (REFERENCE,) if site.local_environment else OBJECT,
                    *restrictions,
                ),
                arg_escapes=(
                    False,
                    *(
                        sc is DESCRIPTOR and i not in entry.context.borrowed_parameters
                        for i, sc in enumerate(convention.parameters)
                    ),
                ),
                results=(
                    (DOUBLE,) if convention.returned is DOUBLE else (IMMEDIATE, SIGNED),
                ),
            )
            vop.codegen_info = (
                entry
                if site.proven
                else replace(
                    entry,
                    proven=False,
                    result_contract=result_contract,
                )
            )
            progress = changed = True
        if not progress:
            break
        pending = remaining
    return changed
