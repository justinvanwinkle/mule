"""Internal argument and return locations, independent of whole-body lowering.

SBCL's ``return-info`` and known local-call conventions select machine value
locations without requiring every operation in the callee to be unboxed
(``ltn-analyze-return`` and ``ir2-convert-local-known-call``). Mule expresses
those locations as LLVM storage classes. Generic object entries retain their
object convention; a numeric entry can still use ordinary object operations.
"""

from __future__ import annotations

from dataclasses import dataclass, field

from mule import ast

from .ctype import UNKNOWN, ValueTypeVisitor
from .node import CallKind, FunctionalKind, LeafVisitor, NodeVisitor
from .vop import DESCRIPTOR, DOUBLE, IMMEDIATE, SIGNED, StorageClass


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
