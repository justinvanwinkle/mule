"""Flow-sensitive propagation of variable facts, following
``src/compiler/constraint.lisp``.

SBCL propagates sets of constraints (``(typep var type)``, ``(eql var x)``)
along control edges, intersects them at joins, and narrows each reference's
derived type from the constraints in force. Mule's adaptation propagates one
fact per variable (a ``ctype`` value type plus a predicted callable identity)
through the same forward dataflow: facts join at block entries, branch edges
narrow integer intervals from comparisons against constants, loop headers widen
growing intervals after a few visits, and error edges carry the join of every
state inside the raising block. Node result types are derived in the same walk
rather than in a separate bottom-up pass, so one worklist settles both.

Module globals are tracked within one lambda only: a store of a known function
object proves the binding's identity until any operation that could run user
code, and reading an unproven binding yields the module-level prediction that
local-call analysis may guard.
"""

from __future__ import annotations

import heapq
from collections import defaultdict
from collections.abc import Mapping
from dataclasses import dataclass, field

from mule import ast

from . import fndb
from .annotation_types import InferredReturnContract, ListContract, SetContract
from .ctype import (
    ANY_CLASS_TYPE,
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    BYTEARRAY_TYPE,
    BYTES_TYPE,
    CLASS_BOOL_TYPE,
    CLASS_BYTEARRAY_TYPE,
    CLASS_BYTES_TYPE,
    CLASS_DICT_TYPE,
    CLASS_FLOAT_TYPE,
    CLASS_FROZENSET_TYPE,
    CLASS_INT_TYPE,
    CLASS_LIST_TYPE,
    CLASS_OBJECT_TYPE,
    CLASS_SET_TYPE,
    CLASS_STR_TYPE,
    CLASS_TUPLE_TYPE,
    CLASS_TYPE_TYPE,
    DICT_TYPE,
    EMPTY_DICT_TYPE,
    EMPTY_FROZENSET_TYPE,
    EMPTY_LIST_TYPE,
    EMPTY_SET_TYPE,
    FIXNUM_MAX,
    FIXNUM_MIN,
    FLOAT,
    FROZENSET_TYPE,
    FUNCTION_TYPE,
    IMMORTAL,
    IMMORTAL_UNBOUND,
    INTEGER,
    INTERNED_TEXT_TYPE,
    ITERATOR_TYPE,
    LIST_TYPE,
    NONE_TYPE,
    PYTHON_INTEGER,
    REAL,
    SET_TYPE,
    TEXT_TYPE,
    UNBOUND,
    UNKNOWN,
    UNKNOWN_UNBOUND,
    FunctionType,
    InstanceType,
    IntegerType,
    IteratorType,
    ListType,
    TextType,
    TupleType,
    ValueType,
    ValueTypeVisitor,
)
from .fndb import INTERNED_TEXTS, SMALL_INTS, SMALL_TUPLES, constant_type
from .ir1util import block_exception_target, made_function, made_lambda
from .knownfun import defconstrain, defpropagate
from .node import (
    CallKind,
    Cast,
    ClassConstructorTarget,
    Combination,
    ForeignCallTarget,
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
    resolve_implementation_type,
)

_DEAD_EDGE = object()


class _AsTupleType(ValueTypeVisitor):
    def visit_ValueType(self, fact):
        return None

    def visit_TupleType(self, fact):
        return None if fact.may_be_unbound else fact


_AS_TUPLE_TYPE = _AsTupleType()


class _AllocatedImplementation(ValueTypeVisitor):
    def __init__(self, targets):
        self.targets = targets

    def visit_ValueType(self, fact):
        implementation = resolve_implementation_type(fact, self.targets)
        return fact if implementation is UNKNOWN else implementation

    def visit_UnionType(self, fact):
        if fact.may_be_unbound:
            return fact
        result = BOTTOM
        for member in fact.members:
            result = result.join(member.accept(self))
        return result


class _InstanceFieldType(ValueTypeVisitor):
    def __init__(self, class_facts, storage_methods, name):
        self.class_facts = class_facts
        self.storage_methods = storage_methods
        self.name = name

    def visit_ValueType(self, fact):
        return UNKNOWN

    def visit_InstanceType(self, fact):
        identity = fact.instance_class()
        if identity is None:
            return UNKNOWN
        facts = self.class_facts.get(identity)
        if facts is not None:
            return facts.fields.get(self.name, UNKNOWN)
        if self.storage_methods is None:
            return UNKNOWN
        return self.storage_methods.get((identity, self.name), UNKNOWN)

    def visit_DictType(self, fact):
        if not fact.proven or fact.may_be_unbound:
            return UNKNOWN
        for cf in self.class_facts.values():
            if cf.public_name in ("dict", "_Table"):
                res = cf.fields.get(self.name, UNKNOWN)
                if res is not UNKNOWN:
                    return res
        if self.storage_methods is not None:
            return self.storage_methods.get(("dict", self.name), UNKNOWN)
        return UNKNOWN

    def visit_SetType(self, fact):
        if not fact.proven or fact.may_be_unbound or self.name not in fact.fixed_slots:
            return UNKNOWN
        res = self.visit_InstanceType(fact)
        if res is not UNKNOWN:
            return res
        short_name = "frozenset" if fact.identity == "mule_dict_frozenset" else "set"
        for cf in self.class_facts.values():
            if cf.public_name == short_name:
                return cf.fields.get(self.name, UNKNOWN)
        if self.storage_methods is not None:
            return self.storage_methods.get((short_name, self.name), UNKNOWN)
        return UNKNOWN

    def visit_UnionType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        result = BOTTOM
        for member in fact.members:
            result = result.join(member.accept(self))
        return result


class _InstanceDeclaredField(ValueTypeVisitor):
    def __init__(self, class_facts, name: str):
        self.class_facts = class_facts
        self.name = name

    def visit_ValueType(self, fact):
        return None

    def visit_InstanceType(self, fact):
        if fact.may_be_unbound:
            return None
        identity = fact.instance_class()
        if identity is None:
            return None
        facts = self.class_facts.get(identity)
        if facts is None:
            return None
        contract = facts.declared_fields.get(self.name)
        if contract is None:
            return None
        return contract, (self.name in facts.unsafe_declared_fields)

    def visit_UnionType(self, fact):
        if fact.may_be_unbound or not fact.members:
            return None
        it = iter(fact.members)
        first = next(it).accept(self)
        if first is None:
            return None
        contract, is_unsafe = first
        for member in it:
            res = member.accept(self)
            if res is None or res[0] != contract:
                return None
            is_unsafe = is_unsafe or res[1]
        return contract, is_unsafe


@dataclass(frozen=True, slots=True)
class Fact:
    type: ValueType = UNKNOWN
    target: Lambda | ClassConstructorTarget | ForeignCallTarget | None = None
    proven: bool = False

    def as_fact(self, wrap):
        return self

    def with_type(self, type_: ValueType) -> Fact:
        if type_ is self.type or type_ == self.type:
            return self
        if self.target is None and not self.proven:
            return _fact(type_)
        return Fact(type_, self.target, self.proven)

    def conservative(self) -> Fact:
        """This fact without claims about mutable contents (SBCL CONSERVATIVE-TYPE)."""
        return self.with_type(self.type.conservative_type())

    def argument_domain(self) -> ValueType:
        """A guarded entry may test a predicted callable's code identity."""
        if self.target is not None and (target := self.target.as_lambda()) is not None:
            return FunctionType(target=target)
        return self.type


NO_FACT = Fact(BOTTOM)
UNBOUND_FACT = Fact(UNBOUND)
UNKNOWN_FACT = Fact(UNKNOWN)
UNKNOWN_UNBOUND_FACT = Fact(UNKNOWN_UNBOUND)
NONE_FACT = Fact(NONE_TYPE)
TEXT_FACT = Fact(TEXT_TYPE)
BYTES_FACT = Fact(BYTES_TYPE)
BOOLEAN_FACT = Fact(BOOLEAN)
BOOLEAN_FALSE_FACT = Fact(BOOLEAN_FALSE)
BOOLEAN_TRUE_FACT = Fact(BOOLEAN_TRUE)
FLOAT_FACT = Fact(FLOAT)
REAL_FACT = Fact(REAL)
INTEGER_FACT = Fact(INTEGER)
PYTHON_INTEGER_FACT = Fact(PYTHON_INTEGER)
LIST_FACT = Fact(LIST_TYPE)
EMPTY_LIST_FACT = Fact(EMPTY_LIST_TYPE)
DICT_FACT = Fact(DICT_TYPE)
EMPTY_DICT_FACT = Fact(EMPTY_DICT_TYPE)
SET_FACT = Fact(SET_TYPE)
EMPTY_SET_FACT = Fact(EMPTY_SET_TYPE)
FROZENSET_FACT = Fact(FROZENSET_TYPE)
EMPTY_FROZENSET_FACT = Fact(EMPTY_FROZENSET_TYPE)
IMMORTAL_FACT = Fact(IMMORTAL)
IMMORTAL_UNBOUND_FACT = Fact(IMMORTAL_UNBOUND)
_EMPTY_ITERATOR_TYPE = IteratorType(BOTTOM)
_EMPTY_ITERATOR_FACT = Fact(_EMPTY_ITERATOR_TYPE)
_EMPTY_DICT_ITEMS_ITERATOR_TYPE = IteratorType(TupleType(2, (BOTTOM, BOTTOM)))
_EMPTY_DICT_ITEMS_ITERATOR_FACT = Fact(_EMPTY_DICT_ITEMS_ITERATOR_TYPE)

_CANONICAL_FACTS: dict[int, Fact] = {
    id(fact.type): fact
    for fact in (
        NO_FACT,
        UNBOUND_FACT,
        UNKNOWN_FACT,
        UNKNOWN_UNBOUND_FACT,
        NONE_FACT,
        TEXT_FACT,
        BYTES_FACT,
        BOOLEAN_FACT,
        BOOLEAN_FALSE_FACT,
        BOOLEAN_TRUE_FACT,
        FLOAT_FACT,
        REAL_FACT,
        INTEGER_FACT,
        PYTHON_INTEGER_FACT,
        LIST_FACT,
        EMPTY_LIST_FACT,
        DICT_FACT,
        EMPTY_DICT_FACT,
        SET_FACT,
        EMPTY_SET_FACT,
        FROZENSET_FACT,
        EMPTY_FROZENSET_FACT,
        IMMORTAL_FACT,
        IMMORTAL_UNBOUND_FACT,
        _EMPTY_ITERATOR_FACT,
        _EMPTY_DICT_ITEMS_ITERATOR_FACT,
    )
}


def _fact(value_type: ValueType) -> Fact:
    canonical = _CANONICAL_FACTS.get(id(value_type))
    if canonical is not None:
        return canonical
    target = value_type.callable_target()
    if target is not None:
        return Fact(value_type, target, True)
    return Fact(value_type)


@dataclass(frozen=True, slots=True)
class CallSite:
    target: Lambda | ClassConstructorTarget | ForeignCallTarget | None
    argument_facts: tuple[Fact, ...]
    proven: bool = False
    keywords: tuple[str | None, ...] = ()

    @property
    def arguments(self) -> tuple[ValueType, ...]:
        return tuple(fact.type for fact in self.argument_facts)

    def bind_argument_facts(self, target: Lambda) -> tuple[Fact, ...] | None:
        if not self.keywords and len(self.argument_facts) == len(target.vars):
            if target.signature.variadic or not target.signature.positional_body:
                order = target.call_order((), len(self.argument_facts))
                if order is None:
                    return None
            else:
                return self.argument_facts
        else:
            order = target.call_order(self.keywords, len(self.argument_facts))
            if order is None:
                return None
        default_types = target.default_types
        bound: list[Fact] = []
        for index in order:
            if index >= 0:
                bound.append(self.argument_facts[index])
            else:
                def_idx = -2 - index
                def_type = (
                    default_types[def_idx]
                    if 0 <= def_idx < len(default_types)
                    else UNKNOWN
                )
                bound.append(_fact(def_type))
        return tuple(bound)


@dataclass(slots=True)
class Body:
    """What one propagation over a lambda established."""

    facts: dict[object, Fact]
    return_type: ValueType
    calls: tuple[CallSite, ...]
    self_type: ValueType | None = None
    return_states: tuple[tuple[ValuedNode, dict, dict, dict], ...] = ()
    # ``%call_resolved`` nodes whose receiver's class resolved the method.
    method_targets: dict[object, Lambda] | None = None
    element_contracts: dict[object, object] = field(default_factory=dict)
    in_bounds_nodes: frozenset[object] = frozenset()

    def types(self) -> dict[object, ValueType]:
        return {
            node: fact.type.normalized_bottom() for node, fact in self.facts.items()
        }

    def targets(self) -> dict[object, object]:
        targets = {
            node: fn
            for node, fact in self.facts.items()
            if fact.target is not None and (fn := fact.target.as_function()) is not None
        }
        if self.method_targets:
            targets.update(self.method_targets)
        return targets


def join_fact(left: Fact, right: Fact) -> Fact:
    # Joins mostly meet the same fact on both sides. Returning the existing
    # object keeps later joins and state comparisons on the identity path.
    if left is right or left == right:
        return left
    if left is NO_FACT:
        return right
    if right is NO_FACT:
        return left
    joined = left.type.merge_information(right.type, left, right, _combine)
    if joined is left or joined == left:
        return left
    if joined is right or joined == right:
        return right
    return joined


def _combine(a: Fact, b: Fact) -> Fact:
    target = a.target if a.target is b.target else None
    proven = (a.proven and b.proven) if target is not None else False
    joined_type = a.type.join(b.type)
    if (
        (joined_type is a.type or joined_type == a.type)
        and target is a.target
        and proven == a.proven
    ):
        return a
    if (
        (joined_type is b.type or joined_type == b.type)
        and target is b.target
        and proven == b.proven
    ):
        return b
    if target is None:
        return _fact(joined_type)
    return Fact(joined_type, target, proven)


def join_states(left: dict, right: dict) -> tuple[dict, bool]:
    """Join two variable states over the same keys. The flag reports whether
    the result differs from ``left``."""
    if left is right:
        return left, False
    joined = None
    for key, fact in left.items():
        other = right[key]
        if other is not fact:
            merged = join_fact(fact, other)
            if merged is not fact:
                if joined is None:
                    joined = left.copy()
                joined[key] = merged
    if joined is None:
        return left, False
    return joined, True


def widen_fact(old: Fact, new: Fact, thresholds: tuple[int, ...] = ()) -> Fact:
    widened = old.type.widen(new.type, thresholds)
    if widened is new.type:
        return new
    if new.target is None:
        return _fact(widened)
    return Fact(widened, new.target, new.proven)


_BASE_WIDENING_THRESHOLDS: tuple[int, ...] = (-1, 0, 1)


def _state_diff_bound(state: dict, u, v) -> int | None:
    if u not in state or v not in state:
        return None
    u_type = state[u].type
    v_type = state[v].type
    if (
        not u_type.fits(PYTHON_INTEGER)
        or u_type.may_be_unbound
        or u_type.maximum is None
        or not v_type.fits(PYTHON_INTEGER)
        or v_type.may_be_unbound
        or v_type.minimum is None
    ):
        return None
    return u_type.maximum - v_type.minimum


def _join_relations_list(
    rels_list: list[dict], states_list: list[dict] | None = None
) -> dict:
    if not rels_list:
        return {}
    if len(rels_list) == 1:
        return rels_list[0]
    if not any(rels_list):
        return {}
    if states_list is None:
        first = rels_list[0]
        if not first or any(not r for r in rels_list[1:]):
            return {}
        result = {}
        others = rels_list[1:]
        for pair, val in first.items():
            hi = val
            for other in others:
                other_val = other.get(pair)
                if other_val is None:
                    break
                hi = max(hi, other_val)
            else:
                result[pair] = hi
        return result
    candidates = set()
    for r in rels_list:
        if r:
            candidates.update(r.keys())
    result = {}
    n = len(rels_list)
    for pair in candidates:
        u, v = pair
        hi = None
        for idx in range(n):
            val = rels_list[idx].get(pair)
            if val is None:
                val = _state_diff_bound(states_list[idx], u, v)
                if val is None:
                    break
            hi = val if hi is None or val > hi else hi
        else:
            if hi is not None:
                result[pair] = hi
    return result


def _widen_relations(old_rels: dict, new_rels: dict) -> dict:
    if not old_rels or not new_rels:
        return {}
    return {
        pair: old_val
        for pair, val in new_rels.items()
        if (old_val := old_rels.get(pair)) is not None and val <= old_val
    }


def _tighten_relation(rels: dict, u, v, off: int) -> bool:
    """Record ``u - v <= off`` and its 1-step transitive consequences.
    Returns ``False`` when a negative cycle makes the path infeasible."""
    if u is v:
        return off >= 0
    cur = rels.get((u, v))
    if cur is not None and cur <= off:
        return True
    rels[u, v] = off
    rev = rels.get((v, u))
    if rev is not None and off + rev < 0:
        return False
    updates = []
    for (a, b), ab_off in rels.items():
        if b is u and a is not v:
            updates.append((a, v, ab_off + off))
        elif a is v and b is not u:
            updates.append((u, b, off + ab_off))
    for a, b, new_off in updates:
        prev = rels.get((a, b))
        if prev is None or new_off < prev:
            rels[a, b] = new_off
            rev_ab = rels.get((b, a))
            if rev_ab is not None and new_off + rev_ab < 0:
                return False
    return True


SMALL_INT_FACTS = {i: Fact(vt) for i, vt in SMALL_INTS.items()}
SMALL_TUPLE_FACTS = tuple(Fact(vt) for vt in SMALL_TUPLES)
UNBOUNDED_TUPLE = TupleType(None)
_UNBOUNDED_TUPLE_FACT = Fact(UNBOUNDED_TUPLE)
INTERNED_TEXT_FACTS = {s: Fact(vt) for s, vt in INTERNED_TEXTS.items()}
_INTERNED_TEXT_FACT = Fact(INTERNED_TEXT_TYPE)
_CANONICAL_FACTS[id(INTERNED_TEXT_TYPE)] = _INTERNED_TEXT_FACT
_CANONICAL_FACTS[id(UNBOUNDED_TUPLE)] = _UNBOUNDED_TUPLE_FACT
_CANONICAL_FACTS.update({id(fact.type): fact for fact in SMALL_INT_FACTS.values()})
_CANONICAL_FACTS.update({id(fact.type): fact for fact in SMALL_TUPLE_FACTS})
_CANONICAL_FACTS.update({id(fact.type): fact for fact in INTERNED_TEXT_FACTS.values()})


def _tuple_fact(
    arity: int | None, elements: tuple[ValueType, ...] | None = None
) -> Fact:
    if arity is None:
        return _UNBOUNDED_TUPLE_FACT
    if elements is not None and not all(el is UNKNOWN for el in elements):
        return Fact(TupleType(arity, elements))
    if 0 <= arity < 16:
        return SMALL_TUPLE_FACTS[arity]
    return Fact(TupleType(arity))


_GLOBAL = object()
_SEQ_LEN = "seq-len"
_RANGE_STOP = "range-stop"
_SEQ_LEN_BOUND_TYPE = IntegerType(0, (1 << 60) - 1)
_SEQ_LEN_BOUND_FACT = Fact(_SEQ_LEN_BOUND_TYPE)
_CANONICAL_FACTS[id(_SEQ_LEN_BOUND_TYPE)] = _SEQ_LEN_BOUND_FACT


class _IsBuiltinSequenceType(ValueTypeVisitor):
    def visit_ValueType(self, fact: ValueType) -> bool:
        return False

    def visit_TextType(self, fact: TextType) -> bool:
        return not fact.may_be_unbound

    def visit_BytesType(self, fact) -> bool:
        return not fact.may_be_unbound

    def visit_ByteArrayType(self, fact) -> bool:
        return not fact.may_be_unbound

    def visit_ListType(self, fact) -> bool:
        return not fact.may_be_unbound

    def visit_TupleType(self, fact: TupleType) -> bool:
        return not fact.may_be_unbound

    def visit_WordVectorType(self, fact) -> bool:
        return not fact.may_be_unbound


_IS_BUILTIN_SEQUENCE_TYPE = _IsBuiltinSequenceType()


class _IsMutableSequenceType(ValueTypeVisitor):
    def visit_ValueType(self, fact: ValueType) -> bool:
        return False

    def visit_ByteArrayType(self, fact) -> bool:
        return True

    def visit_ListType(self, fact) -> bool:
        return True


_IS_MUTABLE_SEQUENCE_TYPE = _IsMutableSequenceType()


class _IsMutableContainerType(ValueTypeVisitor):
    def visit_ValueType(self, fact: ValueType) -> bool:
        return False

    def visit_ListType(self, fact) -> bool:
        return True

    def visit_DictType(self, fact) -> bool:
        return True

    def visit_SetType(self, fact) -> bool:
        return fact.identity != "mule_dict_frozenset"


_IS_MUTABLE_CONTAINER_TYPE = _IsMutableContainerType()


class _IsSimpleSequenceType(ValueTypeVisitor):
    """Whether a sequence's length is fixed when it is created.

    SBCL uses vector-length constraints to elide bound checks only for a
    SIMPLE-ARRAY (%CHECK-BOUND and VECTOR-CONSTRAINT-LENGTH in
    equality-constraints.lisp): an adjustable vector's length may change
    through any reference. Python's text, bytes, and tuples are simple; lists
    and bytearrays are adjustable.
    """

    def visit_ValueType(self, fact: ValueType) -> bool:
        return False

    def visit_TextType(self, fact: TextType) -> bool:
        return not fact.may_be_unbound

    def visit_BytesType(self, fact) -> bool:
        return not fact.may_be_unbound

    def visit_TupleType(self, fact: TupleType) -> bool:
        return not fact.may_be_unbound

    def visit_WordVectorType(self, fact) -> bool:
        return not fact.may_be_unbound


_IS_SIMPLE_SEQUENCE_TYPE = _IsSimpleSequenceType()


class _LiteralSeqLength(NodeVisitor, LeafVisitor, ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.final:
            return None
        return node.leaf.accept(self)

    def visit_Cast(self, node: Cast):
        if node.value is not None and len(node.value.uses) == 1:
            return node.value.uses[0].accept(self)
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Constant(self, leaf):
        return leaf.value.accept(self)

    def visit_TextLiteral(self, node: ast.TextLiteral) -> int:
        return len(node.value)

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> int:
        return len(node.value)

    def visit_Tuple(self, node: ast.Tuple) -> int:
        return len(node.elements)


_LITERAL_SEQ_LENGTH = _LiteralSeqLength()


class _StateKey(LeafVisitor):
    """A local is its own state key; a global is keyed by name, since
    every reference converts to its own ``GlobalVar``."""

    def visit_Leaf(self, leaf):
        raise TypeError(f"{leaf!r} is not a variable with propagated facts")

    def visit_LambdaVar(self, leaf):
        return leaf

    def visit_GlobalVar(self, leaf):
        return (_GLOBAL, leaf.name)


_STATE_KEY = _StateKey()


def _key(var):
    return var.accept(_STATE_KEY)


class _SingleAssignedLambdaVar(LeafVisitor):
    def visit_Leaf(self, leaf):
        return False

    def visit_LambdaVar(self, leaf):
        if leaf.indirect:
            return False
        value_sets = [s for s in leaf.sets if s.value is not None]
        if len(value_sets) == 1:
            return True
        return bool(value_sets) and all(
            s.source is not None and s.source is value_sets[0].source
            for s in value_sets
        )


_SINGLE_ASSIGNED_LAMBDA_VAR = _SingleAssignedLambdaVar()


class _IsUnpackNode(NodeVisitor):
    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return node.fun_info is fndb.PROTOCOLS["unpack"]


_IS_UNPACK_NODE = _IsUnpackNode()


class _IsBinaryAddNode(NodeVisitor):
    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return node.kind is CallKind.KNOWN and node.fun_info in (
            fndb.BINARY[ast.BinaryOperator.ADD],
            fndb.INPLACE_BINARY[ast.BinaryOperator.ADD],
        )


_IS_BINARY_ADD_NODE = _IsBinaryAddNode()


class _InferredReturnDestination(NodeVisitor):
    def __init__(self, call: Combination):
        self.call = call

    def visit_Node(self, node):
        return False

    def visit_Return(self, node):
        return True

    def visit_Cast(self, node: Cast):
        return node.declaration == ("inferred-return", self.call)


class _UnreassignedRefVar(NodeVisitor, LeafVisitor):
    def __init__(self, headers: frozenset):
        self.headers = headers
        self.ref_block = None

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        self.ref_block = node.block
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf):
        if leaf.indirect:
            return None
        if not leaf.sets:
            return leaf
        if (
            len(leaf.sets) == 1
            and leaf.sets[0].block is self.ref_block
            and self.ref_block not in self.headers
        ):
            return leaf
        return None


class _LinearUpdate(NodeVisitor):
    """Extract ``(src_var, d_min, d_max)`` from ``src_var`` or ``src_var +/- d``."""

    def __init__(self, visitor: _Propagation):
        self.visitor = visitor

    def _origin_var(self, lvar):
        src_var = self.visitor.origins.get(lvar)
        if src_var is not None:
            return src_var
        if len(lvar.uses) == 1:
            var = lvar.uses[0].accept(
                _UnreassignedRefVar(self.visitor.walk.graph.headers)
            )
            if var is not None and var in self.visitor.walk.local_vars_set:
                return var
        return None

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.lvar is None:
            return None
        src_var = self._origin_var(node.lvar)
        if src_var is not None and src_var in self.visitor.walk.local_vars_set:
            t = self.visitor.type_of(node.lvar)
            if t.fits(PYTHON_INTEGER) and not t.may_be_unbound:
                return src_var, 0, 0
        return None

    def visit_Cast(self, node: Cast):
        if node.value is not None and len(node.value.uses) == 1:
            return node.value.uses[0].accept(self)
        return None

    def _extract_seq_len_linear(self, seq_lvar):
        src_var = self._origin_var(seq_lvar)
        if src_var is not None and self.visitor.has_vector_length(seq_lvar):
            src_var = self.visitor.walk.temp_source.get(src_var, src_var)
            seq_key = self.visitor.walk.seq_len_keys.get(src_var)
            if seq_key is not None:
                return seq_key, 0, 0
        if len(seq_lvar.uses) == 1:
            _, linear = seq_lvar.uses[0].accept(_SequenceProducerLength(self.visitor))
            if linear is not None:
                return linear
        return None

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN:
            if self.visitor.is_predictable_len(node):
                return self._extract_seq_len_linear(node.args[0])
            return None
        info = node.fun_info
        if (
            info is not None
            and info.name in ("_text_length", "_storage_length", "len")
            and len(node.args) == 1
        ):
            return self._extract_seq_len_linear(node.args[0])
        if info is fndb.RANGE_ADVANCE:
            state = node.info
            if state is not None:
                r_key = self.visitor.walk.range_stop_keys.get(state)
                if r_key is not None and r_key in self.visitor.state:
                    r_fact = self.visitor.state[r_key]
                    if not r_fact.type.may_be_unbound:
                        return r_key, None, -1
            if state is None or not (1 <= len(state.arguments) <= 3):
                return None
            if len(state.arguments) == 1:
                stop_lvar = state.arguments[0]
                positive_step = True
                negative_step = False
            else:
                stop_lvar = state.arguments[1]
                step_t = (
                    self.visitor.type_of(state.arguments[2])
                    if len(state.arguments) == 3
                    else SMALL_INTS[1]
                )
                positive_step = step_t.minimum is not None and step_t.minimum > 0
                negative_step = step_t.maximum is not None and step_t.maximum < 0
            if not (positive_step or negative_step) or len(stop_lvar.uses) != 1:
                return None
            stop_linear = stop_lvar.uses[0].accept(self)
            if stop_linear is None:
                return None
            bvar, bmin, bmax = stop_linear
            if positive_step:
                return bvar, None, (bmax - 1 if bmax is not None else None)
            return bvar, (bmin + 1 if bmin is not None else None), None
        if len(node.args) != 2:
            return None
        arg0, arg1 = node.args
        t0 = self.visitor.type_of(arg0)
        t1 = self.visitor.type_of(arg1)
        if not (
            t0.fits(PYTHON_INTEGER)
            and not t0.may_be_unbound
            and t1.fits(PYTHON_INTEGER)
            and not t1.may_be_unbound
        ):
            return None
        if (
            (
                info is fndb.BINARY[ast.BinaryOperator.RIGHT_SHIFT]
                and t1.exact_integer() == 1
            )
            or (
                info is fndb.BINARY[ast.BinaryOperator.FLOOR_DIVIDE]
                and t1.exact_integer() == 2
            )
        ) and len(arg0.uses) == 1:
            sum_node = arg0.uses[0]
            if (
                sum_node.accept(_IS_BINARY_ADD_NODE)
                and len(sum_node.args) == 2
                and self.visitor.relations
            ):
                term_a = _extract_linear_term(self.visitor, sum_node.args[0])
                term_b = _extract_linear_term(self.visitor, sum_node.args[1])
                if term_a is not None and term_b is not None:
                    u_var, c0 = term_a
                    v_var, c1 = term_b
                    uv = (
                        0
                        if u_var is v_var
                        else self.visitor.relations.get((u_var, v_var))
                    )
                    if uv is not None:
                        return v_var, None, (uv + c0 + c1) // 2
                    vu = self.visitor.relations.get((v_var, u_var))
                    if vu is not None:
                        return u_var, None, (vu + c0 + c1) // 2
        if info in (
            fndb.BINARY[ast.BinaryOperator.ADD],
            fndb.INPLACE_BINARY[ast.BinaryOperator.ADD],
        ):
            src0 = self._origin_var(arg0)
            if src0 is not None and src0 in self.visitor.walk.local_vars_set:
                return src0, t1.minimum, t1.maximum
            src1 = self._origin_var(arg1)
            if src1 is not None and src1 in self.visitor.walk.local_vars_set:
                return src1, t0.minimum, t0.maximum
            if len(arg0.uses) == 1:
                sub0 = arg0.uses[0].accept(self)
                if sub0 is not None:
                    bvar, bmin, bmax = sub0
                    d_min = (
                        bmin + t1.minimum
                        if bmin is not None and t1.minimum is not None
                        else None
                    )
                    d_max = (
                        bmax + t1.maximum
                        if bmax is not None and t1.maximum is not None
                        else None
                    )
                    return bvar, d_min, d_max
            if len(arg1.uses) == 1:
                sub1 = arg1.uses[0].accept(self)
                if sub1 is not None:
                    bvar, bmin, bmax = sub1
                    d_min = (
                        bmin + t0.minimum
                        if bmin is not None and t0.minimum is not None
                        else None
                    )
                    d_max = (
                        bmax + t0.maximum
                        if bmax is not None and t0.maximum is not None
                        else None
                    )
                    return bvar, d_min, d_max
        elif info in (
            fndb.BINARY[ast.BinaryOperator.SUBTRACT],
            fndb.INPLACE_BINARY[ast.BinaryOperator.SUBTRACT],
        ):
            src0 = self._origin_var(arg0)
            if src0 is not None and src0 in self.visitor.walk.local_vars_set:
                d_min = -t1.maximum if t1.maximum is not None else None
                d_max = -t1.minimum if t1.minimum is not None else None
                return src0, d_min, d_max
            if len(arg0.uses) == 1:
                sub0 = arg0.uses[0].accept(self)
                if sub0 is not None:
                    bvar, bmin, bmax = sub0
                    d_min = (
                        bmin - t1.maximum
                        if bmin is not None and t1.maximum is not None
                        else None
                    )
                    d_max = (
                        bmax - t1.minimum
                        if bmax is not None and t1.minimum is not None
                        else None
                    )
                    return bvar, d_min, d_max
        return None


class _SequenceProducerLength(NodeVisitor):
    """Derive a sequence length interval ``ValueType`` (and optional linear term
    ``(base_key, d_min, d_max)``) from a sequence-producing ``Combination``."""

    def __init__(self, visitor: _Propagation):
        self.visitor = visitor

    def visit_Node(self, node):
        return None, None

    def visit_Cast(self, node: Cast):
        if node.value is not None and len(node.value.uses) == 1:
            return node.value.uses[0].accept(self)
        return None, None

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return None, None
        name = node.fun_info.name
        visitor = self.visitor
        if name == "%add" and len(node.args) == 2:
            left_len = visitor.sequence_length_type(node.args[0])
            right_len = visitor.sequence_length_type(node.args[1])
            if left_len is None or right_len is None:
                return None, None
            sum_type = left_len.binary(ast.BinaryOperator.ADD, right_len)
            if not sum_type.fits(INTEGER):
                return None, None
            if sum_type.minimum is None or sum_type.minimum < 0:
                sum_type = IntegerType(0, sum_type.maximum)
            if sum_type.maximum is None or sum_type.maximum >= (1 << 60):
                sum_type = IntegerType(sum_type.minimum, (1 << 60) - 1)
            linear = None
            src0 = visitor.origins.get(node.args[0])
            src1 = visitor.origins.get(node.args[1])
            if (
                src0 is not None
                and visitor.has_vector_length(node.args[0])
                and (k0 := visitor.walk.seq_len_keys.get(src0)) is not None
            ):
                linear = (k0, right_len.minimum, right_len.maximum)
            elif (
                src1 is not None
                and visitor.has_vector_length(node.args[1])
                and (k1 := visitor.walk.seq_len_keys.get(src1)) is not None
            ):
                linear = (k1, left_len.minimum, left_len.maximum)
            return sum_type, linear
        if name == "_text_slice" and len(node.args) == 3:
            start_t = visitor.type_of(node.args[1])
            stop_t = visitor.type_of(node.args[2])
            if not (start_t.fits(PYTHON_INTEGER) and stop_t.fits(PYTHON_INTEGER)):
                return None, None
            diff_t = stop_t.binary(ast.BinaryOperator.SUBTRACT, start_t)
            lo = max(0, diff_t.minimum) if diff_t.minimum is not None else 0
            hi = (
                min((1 << 60) - 1, max(0, diff_t.maximum))
                if diff_t.maximum is not None
                else (1 << 60) - 1
            )
            return IntegerType(lo, hi), None
        if name == "%getslice" and len(node.args) == 4:
            seq_lvar, start_lvar, stop_lvar, step_lvar = node.args
            step_t = visitor.type_of(step_lvar)
            if not (step_t.fits_none(NONE_TYPE) or step_t.exact_integer() == 1):
                return None, None
            seq_len_t = visitor.sequence_length_type(seq_lvar)
            if seq_len_t is None:
                return None, None
            start_t = visitor.type_of(start_lvar)
            stop_t = visitor.type_of(stop_lvar)
            start_zero = start_t.fits_none(NONE_TYPE) or start_t.exact_integer() == 0
            start_nonneg = start_zero or (
                start_t.fits(PYTHON_INTEGER)
                and start_t.minimum is not None
                and start_t.minimum >= 0
            )
            stop_none = stop_t.fits_none(NONE_TYPE)
            stop_nonneg = (
                not stop_none
                and stop_t.fits(PYTHON_INTEGER)
                and stop_t.minimum is not None
                and stop_t.minimum >= 0
            )
            if not start_nonneg or not (stop_none or stop_nonneg):
                return None, None
            clamped_stop = (
                seq_len_t if stop_none else fndb.derive_min2_types(seq_len_t, stop_t)
            )
            raw_len = (
                clamped_stop
                if start_zero
                else clamped_stop.binary(ast.BinaryOperator.SUBTRACT, start_t)
            )
            lo = max(0, raw_len.minimum) if raw_len.minimum is not None else 0
            hi = (
                min((1 << 60) - 1, max(0, raw_len.maximum))
                if raw_len.maximum is not None
                else (1 << 60) - 1
            )
            return IntegerType(lo, hi), None
        if name == "%_list_fill" and len(node.args) == 2:
            cnt_t = visitor.type_of(node.args[1])
            if not cnt_t.fits(PYTHON_INTEGER) or cnt_t.may_be_unbound:
                return None, None
            lo = max(0, cnt_t.minimum) if cnt_t.minimum is not None else 0
            hi = (
                min((1 << 60) - 1, max(0, cnt_t.maximum))
                if cnt_t.maximum is not None
                else (1 << 60) - 1
            )
            linear = None
            if cnt_t.minimum is not None and cnt_t.minimum >= 0:
                cnt_var = visitor.origins.get(node.args[1])
                if cnt_var is not None and cnt_var in visitor.walk.local_vars_set:
                    linear = (cnt_var, 0, 0)
                elif len(node.args[1].uses) == 1:
                    linear = node.args[1].uses[0].accept(_LinearUpdate(visitor))
            return IntegerType(lo, hi), linear
        if name == "%multiply" and len(node.args) == 2:
            t0 = visitor.type_of(node.args[0])
            t1 = visitor.type_of(node.args[1])
            if t0.accept(_IS_BUILTIN_SEQUENCE_TYPE) and t1.fits(PYTHON_INTEGER):
                seq_lvar, cnt_t = node.args[0], t1
            elif t1.accept(_IS_BUILTIN_SEQUENCE_TYPE) and t0.fits(PYTHON_INTEGER):
                seq_lvar, cnt_t = node.args[1], t0
            else:
                return None, None
            seq_len_t = visitor.sequence_length_type(seq_lvar)
            if seq_len_t is None or cnt_t.may_be_unbound:
                return None, None
            cnt_lo = max(0, cnt_t.minimum) if cnt_t.minimum is not None else 0
            cnt_hi = max(0, cnt_t.maximum) if cnt_t.maximum is not None else None
            prod_t = seq_len_t.binary(
                ast.BinaryOperator.MULTIPLY, IntegerType(cnt_lo, cnt_hi)
            )
            lo = max(0, prod_t.minimum) if prod_t.minimum is not None else 0
            hi = (
                min((1 << 60) - 1, max(0, prod_t.maximum))
                if prod_t.maximum is not None
                else (1 << 60) - 1
            )
            return IntegerType(lo, hi), None
        return None, None


class _Propagation(NodeVisitor, LeafVisitor):
    """Processes one block's nodes against the current variable state."""

    def __init__(self, walk, state, origins):
        self.walk = walk
        self.state = state
        self.origins = origins
        self.relations: dict = {}
        self.edge_relations: dict | None = None
        self.type_of_origins: dict[object, object] = {}
        self.tuple_list_origins: dict[object, TupleType] = {}
        self.handler_state: dict[object, Fact] | None = None
        self.handler_relations: dict | None = None
        self.has_handler = False
        # Keys assigned since the handler state last absorbed the state.
        self.changed = set()
        self.state_mutated = False
        self.globals_invalidated = False
        self._current_ref: Ref | None = None

    def reset(
        self,
        state: dict[object, Fact],
        has_handler: bool,
        relations: dict | None = None,
    ) -> None:
        self.state = state
        self.relations = relations.copy() if relations is not None else {}
        self.edge_relations = None
        self.origins.clear()
        self.type_of_origins.clear()
        self.tuple_list_origins.clear()
        self.changed.clear()
        self.state_mutated = False
        self.globals_invalidated = False
        self.has_handler = has_handler
        self.handler_state = None
        self.handler_relations = (
            relations.copy() if has_handler and relations is not None else None
        )

    def assign(self, key, fact):
        if self.state.get(key) is not fact:
            if not self.state_mutated:
                if self.has_handler:
                    self.handler_state = self.state.copy()
                self.state = self.state.copy()
                self.state_mutated = True
            self.state[key] = fact
            if self.has_handler:
                self.changed.add(key)
            if key in self.walk.invalidated:
                self.globals_invalidated = False

    def fact_of(self, lvar) -> Fact:
        return self.walk.facts.get(lvar.use(), NO_FACT)

    def lvar_fact(self, lvar) -> Fact:
        return self.fact_of(lvar)

    def type_of(self, lvar) -> ValueType:
        return self.fact_of(lvar).type

    def narrowed_fact(self, current: Fact, narrowed_type: ValueType) -> Fact:
        if current.target is None:
            return _fact(narrowed_type)
        return Fact(narrowed_type, current.target, current.proven)

    def apply_element_contract(
        self,
        node,
        static_type: ValueType,
        elem_contract,
        *,
        trusted_static: bool = True,
    ) -> ValueType:
        if elem_contract is None or elem_contract.contradicts(static_type):
            self.walk.element_contracts.pop(node, None)
            return static_type
        if node.lvar is not None and elem_contract.has_element_contracts():
            self.walk.lvar_contracts[node.lvar] = elem_contract
        checked = elem_contract.checked_type
        if checked is UNKNOWN or (
            trusted_static and static_type is not UNKNOWN and static_type.fits(checked)
        ):
            self.walk.element_contracts.pop(node, None)
            return static_type
        if self.walk.lambda_.is_protocol:
            return static_type
        if self.walk.lambda_.policy.safety == 0:
            # Trust the declared type without scheduling an element check.
            return elem_contract.refine(static_type)
        self.walk.element_contracts[node] = elem_contract
        return elem_contract.refine(static_type)

    def has_vector_length(self, seq_lvar) -> bool:
        """Whether a variable's vector-length facts describe ``seq_lvar``.

        Only a simple sequence's length is fixed by its creation; any other
        reference may resize a list or bytearray (see _IsSimpleSequenceType).
        """
        return self.type_of(seq_lvar).accept(_IS_SIMPLE_SEQUENCE_TYPE)

    def sequence_length_type(self, lvar) -> ValueType | None:
        seq_type = self.type_of(lvar)
        if seq_type is BOTTOM:
            return BOTTOM
        if seq_type.may_be_unbound:
            return None
        exact_len = seq_type.exact_length()
        if exact_len is not None:
            return IntegerType(exact_len, exact_len)
        if len(lvar.uses) == 1:
            lit_len = lvar.uses[0].accept(_LITERAL_SEQ_LENGTH)
            if lit_len is not None:
                return IntegerType(lit_len, lit_len)
        src_var = self.origins.get(lvar)
        if src_var is not None and self.has_vector_length(lvar):
            src_var = self.walk.temp_source.get(src_var, src_var)
            seq_key = self.walk.seq_len_keys.get(src_var)
            if seq_key is not None and seq_key in self.state:
                len_fact = self.state[seq_key]
                if not len_fact.type.may_be_unbound and len_fact.type.fits(INTEGER):
                    return len_fact.type
        if len(lvar.uses) == 1:
            prod_len, _ = lvar.uses[0].accept(_SequenceProducerLength(self))
            if prod_len is not None:
                return prod_len
        if seq_type.accept(_IS_BUILTIN_SEQUENCE_TYPE):
            return _SEQ_LEN_BOUND_TYPE
        return None

    def is_proven_in_bounds(self, seq_lvar, idx_lvar) -> bool:
        seq_type = self.type_of(seq_lvar)
        idx_type = self.type_of(idx_lvar)
        if (
            seq_type is BOTTOM
            or idx_type is BOTTOM
            or seq_type.may_be_unbound
            or idx_type.may_be_unbound
            or not idx_type.is_fixnum
            or idx_type.minimum is None
            or idx_type.minimum < 0
        ):
            return False
        exact_len = seq_type.exact_length()
        if (
            exact_len is not None
            and idx_type.maximum is not None
            and idx_type.maximum < exact_len
        ):
            return True
        seq_len_t = self.sequence_length_type(seq_lvar)
        if (
            seq_len_t is not None
            and not seq_len_t.may_be_unbound
            and seq_len_t.minimum is not None
            and idx_type.maximum is not None
            and idx_type.maximum < seq_len_t.minimum
        ):
            return True
        if self.relations:
            idx_term = _extract_linear_term(self, idx_lvar)
            if idx_term is not None:
                seq_linear = _LinearUpdate(self)._extract_seq_len_linear(seq_lvar)
                if (
                    seq_linear is not None
                    and seq_linear[1] is not None
                    and seq_linear[1] == seq_linear[2]
                    and _relational_comparison(
                        ast.ComparisonOperator.LESS,
                        self.relations,
                        idx_term[0],
                        seq_linear[0],
                        idx_term[1] - seq_linear[1],
                    )
                    is BOOLEAN_TRUE
                ):
                    return True
        return False

    def record_in_bounds(self, node, seq_lvar, idx_lvar) -> None:
        self.check_in_bounds_access(node, seq_lvar, idx_lvar)

    def _narrow_var_interval(
        self, var, minimum: int | None, maximum: int | None
    ) -> None:
        if var is None or var not in self.state or var in self.walk.indirect:
            return
        cur = self.state[var]
        if not cur.type.fits(PYTHON_INTEGER) or cur.type.may_be_unbound:
            return
        narrowed = cur.type
        if minimum is not None:
            narrowed = narrowed.narrow(
                ast.ComparisonOperator.GREATER_EQUAL, minimum, True
            )
        if maximum is not None and narrowed is not BOTTOM:
            narrowed = narrowed.narrow(ast.ComparisonOperator.LESS_EQUAL, maximum, True)
        if narrowed is not BOTTOM and narrowed != cur.type:
            self.assign(var, self.narrowed_fact(cur, narrowed))

    def check_in_bounds_access(self, node, seq_lvar, idx_lvar) -> None:
        seq_type = self.type_of(seq_lvar)
        idx_type = self.type_of(idx_lvar)
        if (
            seq_type is BOTTOM
            or idx_type is BOTTOM
            or seq_type.may_be_unbound
            or idx_type.may_be_unbound
            or not idx_type.fits(PYTHON_INTEGER)
            or idx_type.minimum is None
            or idx_type.minimum < 0
        ):
            self.walk.in_bounds_nodes.discard(node)
            return
        in_bounds = self.is_proven_in_bounds(seq_lvar, idx_lvar)
        seq_key = None
        src_var = _LinearUpdate(self)._origin_var(seq_lvar)
        if src_var is not None and self.has_vector_length(seq_lvar):
            src_var = self.walk.temp_source.get(src_var, src_var)
            seq_key = self.walk.seq_len_keys.get(src_var)
        idx_var = None
        idx_d_min = None
        idx_d_max = None
        local = _LinearUpdate(self)._origin_var(idx_lvar)
        if local is not None and local in self.walk.local_vars_set:
            idx_var = self.walk.temp_source.get(local, local)
            idx_d_min = idx_d_max = 0
        elif len(idx_lvar.uses) == 1:
            linear = idx_lvar.uses[0].accept(_LinearUpdate(self))
            if linear is not None:
                idx_var, idx_d_min, idx_d_max = linear
                if idx_var in self.walk.temp_source:
                    idx_var = self.walk.temp_source[idx_var]
        if (
            not in_bounds
            and self.relations
            and seq_key is not None
            and idx_var is not None
            and idx_d_max is not None
        ):
            rel = self.relations.get((idx_var, seq_key))
            if rel is not None and rel + idx_d_max <= -1:
                in_bounds = True
        if in_bounds:
            self.walk.in_bounds_nodes.add(node)
        else:
            self.walk.in_bounds_nodes.discard(node)
        if (
            seq_key is not None
            and idx_var is not None
            and idx_d_max is not None
            and idx_d_min == idx_d_max
            and seq_type.accept(_IS_BUILTIN_SEQUENCE_TYPE)
            and not in_bounds
        ):
            _tighten_relation(self.relations, idx_var, seq_key, -1 - idx_d_max)

    def prove_argument_type(self, lvar, proven_type: ValueType) -> None:
        recv_var = self.origins.get(lvar)
        if recv_var is not None and recv_var not in self.walk.indirect:
            if self.type_of(lvar) is UNKNOWN:
                self.assign(recv_var, _fact(proven_type))
            elif proven_type.fits(INTEGER):
                self._narrow_var_interval(
                    recv_var, proven_type.minimum, proven_type.maximum
                )
        elif len(lvar.uses) == 1 and proven_type.fits(INTEGER):
            linear = lvar.uses[0].accept(_LinearUpdate(self))
            if linear is not None:
                base_var, d_min, d_max = linear
                lo = (
                    proven_type.minimum - d_max
                    if proven_type.minimum is not None and d_max is not None
                    else None
                )
                hi = (
                    proven_type.maximum - d_min
                    if proven_type.maximum is not None and d_min is not None
                    else None
                )
                self._narrow_var_interval(base_var, lo, hi)

    def resolve_call_target(self, node: Combination):
        callee_fact = self.fact_of(node.fun)
        if callee_fact.target is not None:
            if callee_fact.proven:
                return callee_fact.target
            imported = self.walk.imported_target(node.fun)
            return imported if imported is callee_fact.target else None
        imported = self.walk.imported_target(node.fun)
        if imported is not None:
            return imported
        return self.walk.builtin_target(node.fun)

    def invalidate(self):
        if not self.globals_invalidated and self.walk.invalidated:
            state = self.state
            has_handler = self.has_handler
            mutated = self.state_mutated
            for key, val in self.walk.invalidated.items():
                if state.get(key) is not val:
                    if not mutated:
                        if has_handler:
                            self.handler_state = state.copy()
                        state = self.state = state.copy()
                        mutated = self.state_mutated = True
                    state[key] = val
                    if has_handler:
                        self.changed.add(key)
            self.globals_invalidated = True

    def forget_origin(self, var):
        if self.origins:
            self.origins = {
                lvar: origin
                for lvar, origin in self.origins.items()
                if origin is not var
            }

    def note_handler_state(self):
        """Join the state after a node into what an error edge carries.
        Only keys assigned since the previous node can differ."""
        if self.handler_state is not None and self.changed:
            handler = self.handler_state
            state = self.state
            for key in self.changed:
                handler[key] = join_fact(handler[key], state[key])
        if self.handler_relations is not None:
            if not self.relations:
                self.handler_relations.clear()
            else:
                self.handler_relations = _join_relations_list(
                    [self.handler_relations, self.relations]
                )
        self.changed.clear()

    # Nodes

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        self._current_ref = node
        try:
            return node.leaf.accept(self)
        finally:
            self._current_ref = None

    def visit_Leaf(self, leaf):
        # A known function's reference is a compile-time operand with no fact.
        return None

    def visit_Constant(self, leaf):
        fact = leaf.cached_fact
        if fact is None:
            fact = leaf.cached_fact = _fact(constant_type(leaf.value))
        return fact

    def visit_GlobalVar(self, leaf):
        # Any code anywhere may reference a module global and mutate the
        # container it holds, so its references see only the conservative
        # type (SBCL NODE-CONSERVATIVE-TYPE in ir1opt.lisp).
        fact = self.state[(_GLOBAL, leaf.name)].conservative()
        ref = self._current_ref
        if ref is not None and ref.lvar is not None:
            self.origins[ref.lvar] = (_GLOBAL, leaf.name)
        target = fact.target or self.walk.global_targets.get(leaf.name)
        if target is None and self.walk.imported_targets is not None:
            target = self.walk.imported_targets.get(leaf.name)
        if target is None:
            target = self.walk.foreign_builtin_function(leaf.name)
        if (
            fact.type is UNKNOWN
            and ref is not None
            and ref.derived_type is not None
            and ref.derived_type is not UNKNOWN
            and self.walk.lambda_.policy.safety < 3
            and ref.lexenv.policy.safety < 3
        ):
            if target is not None:
                return Fact(ref.derived_type.conservative_type(), target, fact.proven)
            return _fact(ref.derived_type.conservative_type())
        if fact.target is None and target is not None:
            fact = Fact(fact.type, target)
        return fact

    def visit_LambdaVar(self, leaf):
        state = self.state
        if leaf not in state:
            # A free variable of an enclosing function: any value.
            return UNKNOWN_FACT
        fact = state[leaf]
        ref = linked(self._current_ref, leaf, "reference")
        if ref.allow_unbound and fact.type.may_be_unbound:
            return UNKNOWN_FACT
        if ref.final:
            self.forget_origin(leaf)
            self.assign(leaf, UNBOUND_FACT)
        if ref.lvar is not None:
            self.origins[ref.lvar] = leaf
            if leaf in self.walk.var_contracts:
                self.walk.lvar_contracts[ref.lvar] = self.walk.var_contracts[leaf]
        if (
            fact.target is None
            and not fact.type.may_be_unbound
            and self.walk.imported_targets is not None
            and len(leaf.sets) == 1
            and (leaf.home is None or leaf not in leaf.home.vars)
        ):
            target = self.walk.imported_targets.get(leaf.name)
            if target is not None:
                fact = Fact(fact.type, target)
        if len(leaf.refs) > 1:
            # SBCL NODE-CONSERVATIVE-TYPE (ir1opt.lisp): a type that describes
            # a mutable container's contents holds at a reference only when
            # it is the variable's sole reference. Through any other
            # reference, code the analysis cannot see may have changed them.
            fact = fact.conservative()
            if leaf in self.walk.var_contracts and not fact.type.may_be_unbound:
                fact = fact.with_type(self.walk.var_contracts[leaf].refine(fact.type))
        return fact

    def visit_Lambda(self, leaf):
        return Fact(FunctionType(target=leaf), leaf, True)

    def visit_Set(self, node: Set):
        key = _key(node.var)
        if key not in self.state:
            return None
        if self.walk.temp_source:
            for k, v in list(self.walk.temp_source.items()):
                if k is key or v is key:
                    del self.walk.temp_source[k]
        old = self.state[key]
        if node.value is None:
            self.assign(key, UNBOUND_FACT)
            self.type_of_origins.pop(key, None)
            if self.relations and key in self.walk.local_vars_set:
                self._drop_relations_for(key)
            seq_key = self.walk.seq_len_keys.get(key)
            if seq_key is not None:
                self.assign(seq_key, UNBOUND_FACT)
                if self.relations:
                    self._drop_relations_for(seq_key)
        else:
            fact = self.fact_of(node.value)
            if (
                fact.type is UNKNOWN
                and key in self.walk.invalidated
                and self.walk.invalidated[key].type is not UNKNOWN
            ):
                fact = self.walk.invalidated[key]
            if key in self.walk.local_vars_set:
                fact = self._update_relations_on_set(key, node.value, fact)
                if key.type is not None and fact.type.fits(PYTHON_INTEGER):
                    fact = Fact(
                        fact.type.annotated_integer(key.type),
                        fact.target,
                        fact.proven,
                    )
            seq_key = self.walk.seq_len_keys.get(key)
            if seq_key is not None:
                self._update_seq_len_on_set(seq_key, node.value, fact)
            contract = self.walk.local_contracts.get(key)
            if contract is not None:
                if fact.type is not UNKNOWN and fact.type.fits(contract.checked_type):
                    self.walk.var_contracts[key] = contract
                else:
                    self.walk.var_contracts.pop(key, None)
            elif key in self.walk.local_vars_set and not key.indirect:
                is_single = node.var.accept(_SINGLE_ASSIGNED_LAMBDA_VAR)
                is_non_param = key not in self.walk.lambda_.vars
                val_contract = self.walk.lvar_contracts.get(node.value)
                if val_contract is not None and (
                    is_single
                    or (
                        is_non_param
                        and all(
                            s.value is not None
                            and self.walk.lvar_contracts.get(s.value) == val_contract
                            for s in key.sets
                        )
                    )
                ):
                    self.walk.var_contracts[key] = val_contract
                else:
                    self.walk.var_contracts.pop(key, None)
                iter_contract = self.walk.iter_contracts.get(node.value)
                if iter_contract is not None and (
                    is_single
                    or (
                        is_non_param
                        and all(
                            s.value is not None
                            and self.walk.iter_contracts.get(s.value) == iter_contract
                            for s in key.sets
                        )
                    )
                ):
                    self.walk.iter_contracts[key] = iter_contract
                else:
                    self.walk.iter_contracts.pop(key, None)
            src_var = self.origins.get(node.value)
            if (
                src_var is not None
                and src_var in self.state
                and key in self.walk.local_vars_set
                and key.hidden
                and len(key.sets) == 1
                and src_var in self.walk.local_vars_set
            ):
                self.walk.temp_source[key] = src_var
            type_origin = self.type_of_origins.get(node.value) or (
                self.type_of_origins.get(src_var) if src_var is not None else None
            )
            if type_origin is not None:
                self.type_of_origins[key] = type_origin
            else:
                self.type_of_origins.pop(key, None)
            self.assign(key, fact)
        # Origins are locals, so this changes nothing for a global.
        self.forget_origin(node.var)
        return old

    def _drop_relations_for(self, key) -> None:
        rels = self.relations
        for pair in [pair for pair in rels if key in pair]:
            del rels[pair]

    def _update_relations_on_set(self, key, value_lvar, fact: Fact) -> Fact:
        rels = self.relations
        linear = (
            value_lvar.uses[0].accept(_LinearUpdate(self))
            if len(value_lvar.uses) == 1
            else None
        )
        if linear is not None:
            src_var, d_min, d_max = linear
            if src_var is key:
                for (u, v), off in list(rels.items()):
                    if u is key:
                        if d_max is not None:
                            rels[u, v] = off + d_max
                        else:
                            del rels[u, v]
                    elif v is key:
                        if d_min is not None:
                            rels[u, v] = off - d_min
                        else:
                            del rels[u, v]
                if fact.type.fits(PYTHON_INTEGER) and not fact.type.may_be_unbound:
                    narrowed = fact.type
                    for (u, v), off in rels.items():
                        if u is key and v in self.state:
                            v_type = self.state[v].type
                            if (
                                v_type.fits(PYTHON_INTEGER)
                                and not v_type.may_be_unbound
                                and v_type.maximum is not None
                            ):
                                narrowed = narrowed.narrow(
                                    ast.ComparisonOperator.LESS_EQUAL,
                                    v_type.maximum + off,
                                    True,
                                )
                                if narrowed is BOTTOM:
                                    break
                        elif v is key and u in self.state:
                            u_type = self.state[u].type
                            if (
                                u_type.fits(PYTHON_INTEGER)
                                and not u_type.may_be_unbound
                                and u_type.minimum is not None
                            ):
                                narrowed = narrowed.narrow(
                                    ast.ComparisonOperator.GREATER_EQUAL,
                                    u_type.minimum - off,
                                    True,
                                )
                                if narrowed is BOTTOM:
                                    break
                    if narrowed is not BOTTOM and narrowed != fact.type:
                        fact = self.narrowed_fact(fact, narrowed)
                return fact
            self._drop_relations_for(key)
            if d_max is not None:
                _tighten_relation(rels, key, src_var, d_max)
            if d_min is not None:
                _tighten_relation(rels, src_var, key, -d_min)
            return fact
        self._drop_relations_for(key)
        return fact

    def _update_seq_len_on_set(self, seq_key, value_lvar, fact: Fact) -> None:
        rels = self.relations
        if rels:
            self._drop_relations_for(seq_key)
        if (
            fact.type.may_be_unbound
            or fact.type.fits(PYTHON_INTEGER)
            or fact.type is FLOAT
            or fact.type is NONE_TYPE
        ):
            self.assign(seq_key, UNBOUND_FACT)
            return
        src_var = self.origins.get(value_lvar)
        if src_var is not None:
            src_var = self.walk.temp_source.get(src_var, src_var)
            src_seq_key = self.walk.seq_len_keys.get(src_var)
            if src_seq_key is not None and src_seq_key in self.state:
                src_len_fact = self.state[src_seq_key]
                self.assign(seq_key, src_len_fact)
                if not src_len_fact.type.may_be_unbound and src_seq_key is not seq_key:
                    _tighten_relation(rels, seq_key, src_seq_key, 0)
                    _tighten_relation(rels, src_seq_key, seq_key, 0)
                return
        linear = None
        seq_len_t = None
        if len(value_lvar.uses) == 1:
            seq_len_t, linear = value_lvar.uses[0].accept(_SequenceProducerLength(self))
        if seq_len_t is None:
            seq_len_t = self.sequence_length_type(value_lvar)
        if seq_len_t is None or seq_len_t is BOTTOM:
            self.assign(seq_key, UNBOUND_FACT)
            return
        self.assign(seq_key, _fact(seq_len_t))
        if linear is not None:
            base_key, d_min, d_max = linear
            if base_key is not seq_key:
                if d_max is not None:
                    _tighten_relation(rels, seq_key, base_key, d_max)
                if d_min is not None:
                    _tighten_relation(rels, base_key, seq_key, -d_min)

    def visit_Cast(self, node: Cast):
        value = self.type_of(node.value)
        if value is BOTTOM:
            return NO_FACT
        if node.declaration is not None and node.declaration[0] == "inferred-return":
            call = node.declaration[1]
            if call.kind in (CallKind.FULL, CallKind.GUARDED) and call.info is not None:
                callee_fact = self.fact_of(call.fun)
                if (
                    callee_fact.target is not None
                    and (target_lam := callee_fact.target.as_lambda()) is not None
                    and (
                        target_lam in self.walk.recursive_targets
                        or self.walk.is_specialized_context
                        or self.walk.for_return_derivation
                    )
                ):
                    arg_facts = tuple(self.fact_of(arg) for arg in call.args)
                    arg_types = tuple(f.type for f in arg_facts)
                    if any(t is BOTTOM for t in arg_types):
                        return NO_FACT
                    arg_domains = tuple(
                        FunctionType(target=lam)
                        if t is UNKNOWN
                        and f.target is not None
                        and (lam := f.target.as_lambda()) is not None
                        else t
                        for f, t in zip(arg_facts, arg_types, strict=True)
                    )
                    resolved = self.walk.resolve_call(
                        callee_fact,
                        tuple(t.widen_inferred_contract() for t in arg_domains),
                        call.info.location,
                        call.info.keywords,
                        True,
                    )
                    if resolved is BOTTOM:
                        return NO_FACT
                    if node.contract.fact is PYTHON_INTEGER and resolved.is_fixnum:
                        return _fact(resolved.widen_constant())
                    if (
                        (
                            self.walk.is_specialized_context
                            or self.walk.for_return_derivation
                        )
                        and resolved is not UNKNOWN
                        and not resolved.has_unspecialized_part()
                    ):
                        specialized_fact = InferredReturnContract(
                            target_lam.name, resolved.widen_inferred_contract()
                        ).fact
                        if specialized_fact.fits(node.contract.fact):
                            return _fact(specialized_fact)
        if self.walk.annotations.check(
            node.contract, value, node.location, node.declaration
        ):
            return _fact(value)
        if node.lvar is not None and node.contract.has_element_contracts():
            self.walk.lvar_contracts[node.lvar] = node.contract
        return _fact(node.contract.refine(value))

    def is_predictable_len(self, node: Combination) -> bool:
        if (
            node.kind not in (CallKind.FULL, CallKind.GUARDED)
            or len(node.args) != 1
            or node.info is None
            or node.info.keywords
            or self.walk.lambda_.kind is FunctionalKind.TOPLEVEL
            or self.walk.lambda_.policy.safety >= 3
            or node.lexenv.policy.safety >= 3
            or "len" in self.walk.rebound_globals
            or "len" in self.walk.global_targets
            or len(node.fun.uses) != 1
        ):
            return False
        if node.fun.uses[0].accept(_IMPORTED_CALLEE_NAME) != "len":
            return False
        return self.type_of(node.args[0]).accept(_IS_BUILTIN_SEQUENCE_TYPE)

    def visit_Combination(self, node: Combination):
        if node.kind in (CallKind.FULL, CallKind.GUARDED):
            info = linked(node.info, node, "info")
            arg_types = tuple(self.type_of(arg) for arg in node.args)
            if any(t is BOTTOM for t in arg_types):
                return NO_FACT
            if self.is_predictable_len(node):
                seq_len = self.sequence_length_type(node.args[0])
                if seq_len is not None:
                    return _fact(seq_len)
            arg_domains = tuple(
                self.fact_of(arg).argument_domain() for arg in node.args
            )
            callee_fact = self.fact_of(node.fun)
            target = self.resolve_call_target(node)
            resolved = self.walk.resolve_call(
                callee_fact,
                arg_domains,
                info.location,
                info.keywords,
            )
            if resolved is BOTTOM:
                return NO_FACT
            if (
                resolved is UNKNOWN
                and not callee_fact.proven
                and callee_fact.target is not None
                and (target_lam := callee_fact.target.as_lambda()) is not None
                and (
                    target_lam in self.walk.recursive_targets
                    or (
                        not self.walk.lambda_.is_protocol
                        and (
                            not self.walk.unique_targets
                            or target_lam in self.walk.unique_targets
                        )
                    )
                )
                and self.walk.lambda_.policy.infers_return_contracts
                and node.lexenv.policy.infers_return_contracts
                and target_lam.policy.infers_return_contracts
                and target_lam.namespace == self.walk.lambda_.namespace
                and node.lvar is not None
                and len(node.lvar.uses) == 1
                and node.lvar.dest is not None
                and (
                    (
                        self.walk.for_return_derivation
                        and not self.walk.lambda_.is_protocol
                    )
                    or not node.lvar.dest.accept(_InferredReturnDestination(node))
                )
                and len(node.fun.uses) == 1
                and (callee_name := node.fun.use().accept(_GLOBAL_CALLEE_NAME))
                is not None
                and self.walk.global_targets.get(callee_name) is target_lam
                and target_lam.source_name == callee_name
                and target_lam.code is not None
                and info.location is not None
                and target_lam.code.filename == info.location[0]
                and not (
                    self.walk.lambda_.kind is FunctionalKind.TOPLEVEL
                    and target_lam.code.firstlineno > info.location[1]
                )
            ):
                inferred_res = self.walk.resolve_call(
                    callee_fact,
                    tuple(t.widen_inferred_contract() for t in arg_domains),
                    info.location,
                    info.keywords,
                    True,
                )
                if inferred_res is BOTTOM:
                    return NO_FACT
                if inferred_res is not UNKNOWN:
                    resolved = InferredReturnContract(
                        target_lam.name, inferred_res.widen_inferred_contract()
                    ).fact
            if resolved is UNKNOWN and target is not None:
                resolved = target.propagate_call(
                    node, self, node.args, arg_types, info.keywords
                )
                if resolved is BOTTOM:
                    return NO_FACT
            if resolved is UNKNOWN and self.walk.imported_returns:
                callee_name = node.fun.use().accept(_IMPORTED_CALLEE_NAME)
                if callee_name is not None:
                    resolved = self.walk.imported_returns.get(callee_name, UNKNOWN)
            builtin_type = UNKNOWN
            if resolved is UNKNOWN:
                builtin_type = self.walk.builtin_return_type(
                    node.fun, arg_types, info.keywords
                )
                if builtin_type is BOTTOM:
                    return NO_FACT
                resolved = builtin_type
            if len(node.args) == 1 and not info.keywords and node.lvar is not None:
                arg_contract = self.walk.lvar_contracts.get(node.args[0])
                if arg_contract is not None:
                    iter_elem = arg_contract.iter_element_contract()
                    if iter_elem is not None:
                        preserved = self.walk.element_preserving_builtin_contract(
                            node.fun, iter_elem
                        )
                        if preserved is not None:
                            self.walk.lvar_contracts[node.lvar] = preserved
            fact = _fact(resolved)
            if not (
                builtin_type is not UNKNOWN
                and not any(t.may_run_user_code for t in arg_types)
            ):
                self.invalidate()
            return fact
        return self.derive_known(node)

    def derive_known(self, node: Combination):
        info = node.fun_info
        assert info is not None
        if info.propagate is not None:
            res = info.propagate(self, node)
            if res is not None:
                return res.as_fact(_fact)
        if info.derive_type is not None:
            value_type = info.derive_type(node)
            if value_type is not None:
                if info.may_call or info.unsafe:
                    self.invalidate()
                return _fact(value_type)
        if info.protocol is not None:
            self.invalidate()
            return UNKNOWN_FACT
        if info.result:
            return UNKNOWN_FACT
        return None

    def is_proven_fixnum_range(self, node: Combination) -> bool:
        state = linked(node.info, node, "info")
        if state.direct:
            return True
        if (
            self.walk.lambda_.policy.safety >= 3
            or node.lexenv.policy.safety >= 3
            or "range" in self.walk.rebound_globals
            or "range" in self.walk.global_targets
            or not node.args
            or len(node.args[0].uses) != 1
            or not node.args[0].uses[0].accept(_UNMUTATED_RANGE_REF)
        ):
            return False
        if not (1 <= len(state.arguments) <= 3):
            return False
        arg_types = [self.type_of(arg) for arg in state.arguments]
        if not all(
            not arg_t.may_be_unbound and arg_t.fits(PYTHON_INTEGER) and arg_t.is_fixnum
            for arg_t in arg_types
        ):
            return False
        if len(arg_types) == 3:
            step = arg_types[2]
            if not (
                (step.minimum is not None and step.minimum > 0)
                or (step.maximum is not None and step.maximum < 0)
            ):
                return False
        return True

    def range_type(self, state) -> IntegerType:
        minimum, maximum = state.minimum, state.maximum
        arguments = [self.type_of(arg) for arg in state.arguments]
        if arguments and all(arg.fits(PYTHON_INTEGER) for arg in arguments):
            if len(arguments) == 1:
                stop = arguments[0]
                minimum = max(minimum, 0)
                if stop.maximum is not None:
                    maximum = min(maximum, stop.maximum - 1)
            else:
                start, stop = arguments[0], arguments[1]
                step = arguments[2] if len(arguments) == 3 else SMALL_INTS[1]
                if step.minimum is not None and step.minimum > 0:
                    if start.minimum is not None:
                        minimum = max(minimum, start.minimum)
                    if stop.maximum is not None:
                        maximum = min(maximum, stop.maximum - 1)
                elif step.maximum is not None and step.maximum < 0:
                    if stop.minimum is not None:
                        minimum = max(minimum, stop.minimum + 1)
                    if start.maximum is not None:
                        maximum = min(maximum, start.maximum)
        if minimum > maximum:
            return IntegerType(state.minimum, state.maximum)
        return IntegerType(minimum, maximum)

    def visit_If(self, node: If):
        if self.type_of(node.test).may_run_user_code:
            self.invalidate()


class _ResolvedMethod(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if node.fun_info is fndb.RESOLVE_METHOD:
            return node.args[0], linked(node.info, node, "info").name
        return None


_RESOLVED_METHOD = _ResolvedMethod()


class _ImportedModuleReceiver(NodeVisitor, LeafVisitor):
    """Resolve the module name an unmutated imported module lvar originates from."""

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
        if node.lexenv.policy.safety >= 3:
            return None
        return node.leaf.accept(self)

    def visit_GlobalVar(self, leaf: GlobalVar):
        if len(leaf.sets) != 1:
            return None
        store = leaf.sets[0]
        if (
            store.home_lambda is None
            or store.home_lambda.kind is not FunctionalKind.TOPLEVEL
            or store.value is None
            or len(store.value.uses) != 1
        ):
            return None
        return store.value.uses[0].accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf.home is not None and leaf in leaf.home.vars:
            return None
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


class _BuildDictType(ValueTypeVisitor):
    def visit_ValueType(self, fact):
        return DICT_TYPE, True

    def visit_TupleType(self, fact):
        if fact.may_be_unbound:
            return DICT_TYPE, True
        if fact.arity == 0:
            return EMPTY_DICT_TYPE, False
        if fact.elements is not None and len(fact.elements) % 2 == 0:
            dict_type = EMPTY_DICT_TYPE
            may_call = False
            for key_t, val_t in zip(
                fact.elements[0::2], fact.elements[1::2], strict=True
            ):
                dict_type = dict_type.with_entry(key_t, val_t)
                if key_t.may_run_user_code:
                    may_call = True
            return dict_type, may_call
        return DICT_TYPE, True


_BUILD_DICT_TYPE = _BuildDictType()


class _BuildSetType(ValueTypeVisitor):
    def visit_ValueType(self, fact):
        return SET_TYPE, True

    def visit_TupleType(self, fact):
        if fact.may_be_unbound:
            return SET_TYPE, True
        if fact.arity == 0:
            return EMPTY_SET_TYPE, False
        if fact.elements is not None:
            set_type = EMPTY_SET_TYPE
            may_call = False
            for elem_t in fact.elements:
                set_type = set_type.with_element(elem_t)
                if elem_t.may_run_user_code:
                    may_call = True
            return set_type, may_call
        return SET_TYPE, True

    def visit_ListType(self, fact):
        if fact.may_be_unbound:
            return SET_TYPE, True
        if fact.element is BOTTOM:
            return EMPTY_SET_TYPE, False
        return (
            EMPTY_SET_TYPE.with_element(fact.element),
            fact.element.may_run_user_code,
        )


_BUILD_SET_TYPE = _BuildSetType()


class _SequenceIteratorVar(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.fun_info is not None
            and node.fun_info.name in ("%sequence_available", "%byte_available")
            and len(node.args) == 1
            and len(node.args[0].uses) == 1
        ):
            return node.args[0].uses[0].accept(self)
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf):
        return leaf


_SEQUENCE_ITERATOR_VAR = _SequenceIteratorVar()


def _loop_iterator_var(block):
    for pred in block.pred:
        for pred_node in pred.nodes():
            var = pred_node.accept(_SEQUENCE_ITERATOR_VAR)
            if var is not None:
                return var
    return None


class _UnmutatedRangeRef(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return False

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return False

    def visit_GlobalVar(self, leaf):
        return leaf.name == "range" and not leaf.sets

    def visit_LambdaVar(self, leaf):
        if len(leaf.sets) != 1:
            return False
        value = leaf.sets[0].value
        if value is None or len(value.uses) != 1:
            return False
        return value.uses[0].accept(self)


_UNMUTATED_RANGE_REF = _UnmutatedRangeRef()


_BINARY_OPERATORS = {
    **{f"%{operator.name.lower()}": operator for operator in ast.BinaryOperator},
    **{f"%i{operator.name.lower()}": operator for operator in ast.BinaryOperator},
}
_UNARY_OPERATORS = {
    f"%unary_{operator.name.lower()}": operator for operator in ast.UnaryOperator
}
_COMPARISON_OPERATORS = {
    f"%compare_{operator.name.lower()}": operator for operator in ast.ComparisonOperator
}
_SWAPPED = {
    ast.ComparisonOperator.LESS: ast.ComparisonOperator.GREATER,
    ast.ComparisonOperator.LESS_EQUAL: ast.ComparisonOperator.GREATER_EQUAL,
    ast.ComparisonOperator.GREATER: ast.ComparisonOperator.LESS,
    ast.ComparisonOperator.GREATER_EQUAL: ast.ComparisonOperator.LESS_EQUAL,
}
_NEGATED_COMPARISON = {
    ast.ComparisonOperator.LESS: ast.ComparisonOperator.GREATER_EQUAL,
    ast.ComparisonOperator.LESS_EQUAL: ast.ComparisonOperator.GREATER,
    ast.ComparisonOperator.GREATER: ast.ComparisonOperator.LESS_EQUAL,
    ast.ComparisonOperator.GREATER_EQUAL: ast.ComparisonOperator.LESS,
    ast.ComparisonOperator.EQUAL: ast.ComparisonOperator.NOT_EQUAL,
    ast.ComparisonOperator.NOT_EQUAL: ast.ComparisonOperator.EQUAL,
}


def _comparison_bound(
    operator: ast.ComparisonOperator, other_type: ValueType, truth: bool
) -> int | None:
    if other_type.may_be_unbound:
        return None
    effective = operator if truth else _NEGATED_COMPARISON.get(operator, operator)
    if effective in (
        ast.ComparisonOperator.LESS,
        ast.ComparisonOperator.LESS_EQUAL,
    ):
        return other_type.maximum
    if effective in (
        ast.ComparisonOperator.GREATER,
        ast.ComparisonOperator.GREATER_EQUAL,
    ):
        return other_type.minimum
    if effective in (
        ast.ComparisonOperator.EQUAL,
        ast.ComparisonOperator.NOT_EQUAL,
    ):
        return other_type.exact_integer()
    return None


def _extract_linear_term(visitor: _Propagation, lvar):
    local = visitor.origins.get(lvar)
    if local is not None and local in visitor.walk.local_vars_set:
        local = visitor.walk.temp_source.get(local, local)
        return local, 0
    if len(lvar.uses) == 1:
        linear = lvar.uses[0].accept(_LinearUpdate(visitor))
        if linear is not None:
            base_var, d_min, d_max = linear
            if (
                base_var in visitor.walk.relational_vars_set
                and d_min is not None
                and d_min == d_max
            ):
                return base_var, d_min
    return None


def _relational_comparison(
    operator: ast.ComparisonOperator,
    rels: dict,
    u_var,
    v_var,
    delta: int,
) -> ValueType | None:
    uv = 0 if u_var is v_var else rels.get((u_var, v_var))
    vu = 0 if u_var is v_var else rels.get((v_var, u_var))
    if operator is ast.ComparisonOperator.LESS:
        if uv is not None and uv + delta <= -1:
            return BOOLEAN_TRUE
        if vu is not None and vu - delta <= 0:
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.LESS_EQUAL:
        if uv is not None and uv + delta <= 0:
            return BOOLEAN_TRUE
        if vu is not None and vu - delta <= -1:
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.GREATER:
        if vu is not None and vu - delta <= -1:
            return BOOLEAN_TRUE
        if uv is not None and uv + delta <= 0:
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.GREATER_EQUAL:
        if vu is not None and vu - delta <= 0:
            return BOOLEAN_TRUE
        if uv is not None and uv + delta <= -1:
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.EQUAL:
        if uv is not None and vu is not None and uv + delta <= 0 and vu - delta <= 0:
            return BOOLEAN_TRUE
        if (uv is not None and uv + delta <= -1) or (
            vu is not None and vu - delta <= -1
        ):
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.NOT_EQUAL:
        if (uv is not None and uv + delta <= -1) or (
            vu is not None and vu - delta <= -1
        ):
            return BOOLEAN_TRUE
        if uv is not None and vu is not None and uv + delta <= 0 and vu - delta <= 0:
            return BOOLEAN_FALSE
    return None


def _apply_relations_to_state(rels: dict, state: dict, visitor: _Propagation) -> bool:
    if not rels:
        return False
    changed = False
    for (u, v), off in rels.items():
        if u not in state or v not in state:
            continue
        u_fact = state[u]
        v_fact = state[v]
        u_type = u_fact.type
        v_type = v_fact.type
        if (
            not u_type.fits(PYTHON_INTEGER)
            or u_type.may_be_unbound
            or not v_type.fits(PYTHON_INTEGER)
            or v_type.may_be_unbound
        ):
            continue
        if v_type.maximum is not None:
            u_narrowed = u_type.narrow(
                ast.ComparisonOperator.LESS_EQUAL, v_type.maximum + off, True
            )
            if u_narrowed is BOTTOM:
                state[_DEAD_EDGE] = True
                return True
            if u_narrowed != u_type:
                state[u] = visitor.narrowed_fact(u_fact, u_narrowed)
                u_fact = state[u]
                u_type = u_narrowed
                changed = True
        if u_type.minimum is not None:
            v_narrowed = v_type.narrow(
                ast.ComparisonOperator.GREATER_EQUAL, u_type.minimum - off, True
            )
            if v_narrowed is BOTTOM:
                state[_DEAD_EDGE] = True
                return True
            if v_narrowed != v_type:
                state[v] = visitor.narrowed_fact(v_fact, v_narrowed)
                changed = True
    return changed


def _make_binary_propagator(operator: ast.BinaryOperator):
    def _prop(visitor: _Propagation, node: Combination):
        args = node.args
        left, right = visitor.type_of(args[0]), visitor.type_of(args[1])
        if left is BOTTOM or right is BOTTOM:
            return NO_FACT
        res_type = left.binary(operator, right)
        if node.lvar is not None:
            left_contract = visitor.walk.lvar_contracts.get(args[0])
            if left_contract is not None:
                right_contract = visitor.walk.lvar_contracts.get(args[1])
                res_contract = left_contract.binary_result_contract(
                    operator, right_contract, right
                )
                if res_contract is not None:
                    visitor.walk.lvar_contracts[node.lvar] = res_contract
        if (
            operator is ast.BinaryOperator.SUBTRACT
            and visitor.relations
            and res_type.fits(PYTHON_INTEGER)
            and left.fits(PYTHON_INTEGER)
            and right.fits(PYTHON_INTEGER)
            and not left.may_be_unbound
            and not right.may_be_unbound
        ):
            left_term = _extract_linear_term(visitor, args[0])
            right_term = _extract_linear_term(visitor, args[1])
            if left_term is not None and right_term is not None:
                u_var, c0 = left_term
                v_var, c1 = right_term
                delta = c0 - c1
                uv = 0 if u_var is v_var else visitor.relations.get((u_var, v_var))
                vu = 0 if u_var is v_var else visitor.relations.get((v_var, u_var))
                narrowed = res_type
                if vu is not None:
                    narrowed = narrowed.narrow(
                        ast.ComparisonOperator.GREATER_EQUAL, -vu + delta, True
                    )
                if uv is not None and narrowed is not BOTTOM:
                    narrowed = narrowed.narrow(
                        ast.ComparisonOperator.LESS_EQUAL, uv + delta, True
                    )
                if narrowed is not BOTTOM:
                    res_type = narrowed
        fact = _fact(res_type)
        if left.may_run_user_code or right.may_run_user_code:
            visitor.invalidate()
        return fact

    return _prop


def _make_unary_propagator(operator: ast.UnaryOperator):
    def _prop(visitor: _Propagation, node: Combination):
        operand = visitor.type_of(node.args[0])
        if operand is BOTTOM:
            return NO_FACT
        fact = _fact(operand.unary(operator))
        if operand.may_run_user_code:
            visitor.invalidate()
        return fact

    return _prop


def _interval_comparison(
    operator: ast.ComparisonOperator, left: ValueType, right: ValueType
) -> ValueType | None:
    if operator is ast.ComparisonOperator.LESS:
        if (
            left.maximum is not None
            and right.minimum is not None
            and left.maximum < right.minimum
        ):
            return BOOLEAN_TRUE
        if (
            left.minimum is not None
            and right.maximum is not None
            and left.minimum >= right.maximum
        ):
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.LESS_EQUAL:
        if (
            left.maximum is not None
            and right.minimum is not None
            and left.maximum <= right.minimum
        ):
            return BOOLEAN_TRUE
        if (
            left.minimum is not None
            and right.maximum is not None
            and left.minimum > right.maximum
        ):
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.GREATER:
        if (
            left.minimum is not None
            and right.maximum is not None
            and left.minimum > right.maximum
        ):
            return BOOLEAN_TRUE
        if (
            left.maximum is not None
            and right.minimum is not None
            and left.maximum <= right.minimum
        ):
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.GREATER_EQUAL:
        if (
            left.minimum is not None
            and right.maximum is not None
            and left.minimum >= right.maximum
        ):
            return BOOLEAN_TRUE
        if (
            left.maximum is not None
            and right.minimum is not None
            and left.maximum < right.minimum
        ):
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.EQUAL:
        if (
            left.minimum is not None
            and left.maximum is not None
            and right.minimum is not None
            and right.maximum is not None
            and left.minimum == left.maximum == right.minimum == right.maximum
        ):
            return BOOLEAN_TRUE
        if (
            left.maximum is not None
            and right.minimum is not None
            and left.maximum < right.minimum
        ) or (
            left.minimum is not None
            and right.maximum is not None
            and left.minimum > right.maximum
        ):
            return BOOLEAN_FALSE
    elif operator is ast.ComparisonOperator.NOT_EQUAL:
        if (
            left.maximum is not None
            and right.minimum is not None
            and left.maximum < right.minimum
        ) or (
            left.minimum is not None
            and right.maximum is not None
            and left.minimum > right.maximum
        ):
            return BOOLEAN_TRUE
        if (
            left.minimum is not None
            and left.maximum is not None
            and right.minimum is not None
            and right.maximum is not None
            and left.minimum == left.maximum == right.minimum == right.maximum
        ):
            return BOOLEAN_FALSE
    return None


def _make_comparison_propagator(operator: ast.ComparisonOperator):
    def _prop(visitor: _Propagation, node: Combination):
        args = node.args
        left = _equality_target_type(visitor, args[0])
        right = _equality_target_type(visitor, args[1])
        if left is BOTTOM or right is BOTTOM:
            return NO_FACT
        res_type = left.comparison(operator, right)
        if (
            res_type is BOOLEAN
            and left.fits(PYTHON_INTEGER)
            and right.fits(PYTHON_INTEGER)
            and not left.may_be_unbound
            and not right.may_be_unbound
        ):
            if visitor.walk.seq_len_keys:
                iv_cmp = _interval_comparison(operator, left, right)
                if iv_cmp is not None:
                    res_type = iv_cmp
            if res_type is BOOLEAN and visitor.relations:
                left_term = _extract_linear_term(visitor, args[0])
                right_term = _extract_linear_term(visitor, args[1])
                if left_term is not None and right_term is not None:
                    rel_cmp = _relational_comparison(
                        operator,
                        visitor.relations,
                        left_term[0],
                        right_term[0],
                        left_term[1] - right_term[1],
                    )
                    if rel_cmp is not None:
                        res_type = rel_cmp
        fact = _fact(res_type)
        if operator not in (
            ast.ComparisonOperator.IS,
            ast.ComparisonOperator.IS_NOT,
        ) and (left.may_run_user_code or right.may_run_user_code):
            visitor.invalidate()
        return fact

    return _prop


def _constrain_identity_comparison(
    operator: ast.ComparisonOperator,
    visitor: _Propagation,
    node: Combination,
    state: dict,
    truth: bool,
) -> bool:
    if len(node.args) != 2:
        return False
    left, right = node.args
    is_same = (operator is ast.ComparisonOperator.IS) == truth
    left_type = visitor.type_of(left)
    right_type = visitor.type_of(right)
    cmp = left_type.comparison(
        ast.ComparisonOperator.IS if is_same else ast.ComparisonOperator.IS_NOT,
        right_type,
    )
    if cmp is BOOLEAN_FALSE:
        state[_DEAD_EDGE] = True
        return True
    changed = False
    for operand, other in ((left, right), (right, left)):
        other_type = visitor.type_of(other)
        local = visitor.origins.get(operand)
        if local is not None and local in visitor.state:
            current = state[local]
            narrowed = current.type.narrow_identity(other_type, is_same)
            if narrowed is BOTTOM:
                state[_DEAD_EDGE] = True
                return True
            if narrowed != current.type:
                state[local] = visitor.narrowed_fact(current, narrowed)
                changed = True
        subj_var = visitor.type_of_origins.get(operand) or (
            visitor.type_of_origins.get(local) if local is not None else None
        )
        if subj_var is not None and subj_var in visitor.state:
            subj_cur = state[subj_var]
            narrowed_subj = subj_cur.type.narrow_by_runtime_class(other_type, is_same)
            if narrowed_subj is BOTTOM:
                state[_DEAD_EDGE] = True
                return True
            if narrowed_subj != subj_cur.type:
                state[subj_var] = visitor.narrowed_fact(subj_cur, narrowed_subj)
                changed = True
    return changed


class _ConstantTextLiteralType(NodeVisitor, LeafVisitor, ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_Constant(self, leaf):
        return leaf.value.accept(self)

    def visit_TextLiteral(self, node: ast.TextLiteral):
        if len(node.value.encode("utf-8", "surrogatepass")) <= 64:
            return INTERNED_TEXTS.get(node.value) or TextType(node.value)
        return None


_CONSTANT_TEXT_LITERAL_TYPE = _ConstantTextLiteralType()


def _equality_target_type(visitor: _Propagation, lvar) -> ValueType:
    other_type = visitor.type_of(lvar)
    if (
        other_type.fits(TEXT_TYPE)
        and other_type.exact_text() is None
        and len(lvar.uses) == 1
    ):
        literal_type = lvar.uses[0].accept(_CONSTANT_TEXT_LITERAL_TYPE)
        if literal_type is not None:
            return literal_type
    return other_type


def _make_comparison_constrainer(operator: ast.ComparisonOperator):
    if operator in (ast.ComparisonOperator.IS, ast.ComparisonOperator.IS_NOT):

        def _constrain_is(
            visitor: _Propagation, node: Combination, state: dict, truth: bool
        ) -> bool:
            return _constrain_identity_comparison(operator, visitor, node, state, truth)

        return _constrain_is

    swapped = _SWAPPED.get(
        operator,
        operator
        if operator in (ast.ComparisonOperator.EQUAL, ast.ComparisonOperator.NOT_EQUAL)
        else None,
    )

    def _constrain_ord(
        visitor: _Propagation, node: Combination, state: dict, truth: bool
    ) -> bool:
        if len(node.args) != 2:
            return False
        left, right = node.args
        left_type = _equality_target_type(visitor, left)
        right_type = _equality_target_type(visitor, right)
        cmp = left_type.comparison(operator, right_type)
        if (cmp is BOOLEAN_FALSE and truth) or (cmp is BOOLEAN_TRUE and not truth):
            state[_DEAD_EDGE] = True
            return True
        left_term = _extract_linear_term(visitor, left)
        right_term = _extract_linear_term(visitor, right)
        edge_rels = None
        if (
            left_term is not None
            and right_term is not None
            and left_type.fits(PYTHON_INTEGER)
            and right_type.fits(PYTHON_INTEGER)
            and not left_type.may_be_unbound
            and not right_type.may_be_unbound
        ):
            u_var, c0 = left_term
            v_var, c1 = right_term
            delta = c0 - c1
            if visitor.relations:
                rel_cmp = _relational_comparison(
                    operator, visitor.relations, u_var, v_var, delta
                )
                if (rel_cmp is BOOLEAN_FALSE and truth) or (
                    rel_cmp is BOOLEAN_TRUE and not truth
                ):
                    state[_DEAD_EDGE] = True
                    return True
            eff_op = operator if truth else _NEGATED_COMPARISON.get(operator, operator)
            edge_rels = (
                visitor.edge_relations
                if visitor.edge_relations is not None
                else visitor.relations
            ).copy()
            feasible = True
            if eff_op is ast.ComparisonOperator.LESS:
                feasible = _tighten_relation(edge_rels, u_var, v_var, -1 - delta)
            elif eff_op is ast.ComparisonOperator.LESS_EQUAL:
                feasible = _tighten_relation(edge_rels, u_var, v_var, -delta)
            elif eff_op is ast.ComparisonOperator.GREATER:
                feasible = _tighten_relation(edge_rels, v_var, u_var, -1 + delta)
            elif eff_op is ast.ComparisonOperator.GREATER_EQUAL:
                feasible = _tighten_relation(edge_rels, v_var, u_var, delta)
            elif eff_op is ast.ComparisonOperator.EQUAL:
                feasible = _tighten_relation(
                    edge_rels, u_var, v_var, -delta
                ) and _tighten_relation(edge_rels, v_var, u_var, delta)
            if not feasible:
                state[_DEAD_EDGE] = True
                return True
            visitor.edge_relations = edge_rels
        changed = False
        for operand, other_eq_type, other, op, term in (
            (left, right_type, right, operator, left_term),
            (right, left_type, left, swapped, right_term),
        ):
            if op is None:
                continue
            local = visitor.origins.get(operand)
            offset = 0
            if local is None and term is not None:
                local, offset = term
            if local is None or local not in visitor.state:
                continue
            target_locals = (
                (local, visitor.walk.temp_source[local])
                if local in visitor.walk.temp_source
                else (local,)
            )
            other_type = visitor.type_of(other)
            bound = _comparison_bound(op, other_type, truth)
            eq_target = (
                other_eq_type
                if offset == 0
                and op
                in (
                    ast.ComparisonOperator.EQUAL,
                    ast.ComparisonOperator.NOT_EQUAL,
                )
                else None
            )
            for target_local in target_locals:
                if target_local not in state:
                    continue
                current = state[target_local]
                if current is UNBOUND_FACT:
                    continue
                narrowed = current.type
                if eq_target is not None:
                    is_equal = (op is ast.ComparisonOperator.EQUAL) == truth
                    narrowed = narrowed.narrow_equality(eq_target, is_equal)
                if bound is not None and narrowed is not BOTTOM:
                    narrowed = narrowed.narrow(op, bound - offset, truth)
                if narrowed is BOTTOM:
                    state[_DEAD_EDGE] = True
                    return True
                if narrowed != current.type:
                    state[target_local] = visitor.narrowed_fact(current, narrowed)
                    changed = True
        if edge_rels is not None or changed:
            active_rels = edge_rels if edge_rels is not None else visitor.relations
            if active_rels and _apply_relations_to_state(active_rels, state, visitor):
                if _DEAD_EDGE in state:
                    return True
                changed = True
        return changed

    return _constrain_ord


for _op_name, _op_enum in _BINARY_OPERATORS.items():
    defpropagate(_op_name)(_make_binary_propagator(_op_enum))

for _op_name, _op_enum in _UNARY_OPERATORS.items():
    defpropagate(_op_name)(_make_unary_propagator(_op_enum))

for _op_name, _op_enum in _COMPARISON_OPERATORS.items():
    defpropagate(_op_name)(_make_comparison_propagator(_op_enum))
    defconstrain(_op_name)(_make_comparison_constrainer(_op_enum))


@defconstrain("%unary_not")
def _constrain_unary_not(
    visitor: _Propagation, node: Combination, state: dict, truth: bool
) -> bool:
    if len(node.args) != 1:
        return False
    narrowed_edge = (
        node.args[0].use().accept(_ConstrainComparison(visitor, state, not truth))
    )
    if narrowed_edge is _DEAD_EDGE:
        state[_DEAD_EDGE] = True
        return True
    if narrowed_edge is not None:
        state.update(narrowed_edge)
        return True
    return False


@defpropagate("%build_tuple")
def _propagate_build_tuple(visitor: _Propagation, node: Combination):
    args = node.args
    arity = len(args)
    elements = tuple(visitor.type_of(arg) for arg in args) if 0 < arity <= 64 else None
    return _tuple_fact(arity, elements)


@defpropagate("%build_list")
def _propagate_build_list(visitor: _Propagation, node: Combination):
    args = node.args
    if not args:
        return EMPTY_LIST_FACT
    list_type = EMPTY_LIST_TYPE
    for arg in args:
        list_type = list_type.with_element(visitor.type_of(arg))
    return _fact(list_type)


@defpropagate("%build_dict")
def _propagate_build_dict(visitor: _Propagation, node: Combination):
    args = node.args
    if not args:
        return EMPTY_DICT_FACT
    if len(args) == 1:
        dict_type, may_call = visitor.type_of(args[0]).accept(_BUILD_DICT_TYPE)
        if may_call:
            visitor.invalidate()
        return _fact(dict_type)
    dict_type = EMPTY_DICT_TYPE
    for key_arg, val_arg in zip(args[0::2], args[1::2], strict=True):
        dict_type = dict_type.with_entry(
            visitor.type_of(key_arg), visitor.type_of(val_arg)
        )
    if any(visitor.type_of(key_arg).may_run_user_code for key_arg in args[0::2]):
        visitor.invalidate()
    return _fact(dict_type)


@defpropagate("%build_set")
def _propagate_build_set(visitor: _Propagation, node: Combination):
    args = node.args
    if not args:
        return EMPTY_SET_FACT
    if len(args) == 1:
        set_type, may_call = visitor.type_of(args[0]).accept(_BUILD_SET_TYPE)
        if may_call:
            visitor.invalidate()
        return _fact(set_type)
    set_type = EMPTY_SET_TYPE
    for arg in args:
        set_type = set_type.with_element(visitor.type_of(arg))
    if any(visitor.type_of(arg).may_run_user_code for arg in args):
        visitor.invalidate()
    return _fact(set_type)


@defpropagate("%empty_set")
def _propagate_empty_set(visitor: _Propagation, node: Combination):
    return EMPTY_SET_FACT


@defpropagate("%set_add")
def _propagate_set_add(visitor: _Propagation, node: Combination):
    args = node.args
    if len(args) != 2:
        return None
    recv_type = visitor.type_of(args[0])
    elem_type = visitor.type_of(args[1])
    if recv_type is BOTTOM or elem_type is BOTTOM:
        return NO_FACT
    if elem_type.may_run_user_code:
        visitor.invalidate()
    return NONE_FACT


@defpropagate("%set_discard")
@defpropagate("%set_remove")
def _propagate_set_discard_or_remove(visitor: _Propagation, node: Combination):
    args = node.args
    if len(args) != 2:
        return None
    recv_type = visitor.type_of(args[0])
    elem_type = visitor.type_of(args[1])
    if recv_type is BOTTOM or elem_type is BOTTOM:
        return NO_FACT
    if elem_type.may_run_user_code:
        visitor.invalidate()
    return NONE_FACT


for _set_op_name, _set_op_enum in (
    ("%set_and", ast.BinaryOperator.BIT_AND),
    ("%set_or", ast.BinaryOperator.BIT_OR),
    ("%set_sub", ast.BinaryOperator.SUBTRACT),
    ("%set_xor", ast.BinaryOperator.BIT_XOR),
):
    defpropagate(_set_op_name)(_make_binary_propagator(_set_op_enum))


@defpropagate("%range_prepare")
def _propagate_range_prepare(visitor: _Propagation, node: Combination):
    state = linked(node.info, node, "info")
    r_key = visitor.walk.range_stop_keys.get(state)
    if r_key is not None:
        visitor._drop_relations_for(r_key)
        step_pos = False
        if 1 <= len(state.arguments) <= 2:
            step_pos = True
        elif len(state.arguments) == 3:
            st = visitor.type_of(state.arguments[2])
            step_pos = (
                st.fits(PYTHON_INTEGER)
                and not st.may_be_unbound
                and st.minimum is not None
                and st.minimum > 0
            )
        if step_pos:
            stop_lvar = (
                state.arguments[0] if len(state.arguments) == 1 else state.arguments[1]
            )
            stop_fact = visitor.fact_of(stop_lvar)
            visitor.assign(r_key, stop_fact)
            if (
                stop_fact.type.fits(PYTHON_INTEGER)
                and not stop_fact.type.may_be_unbound
            ):
                linear = None
                stop_local = visitor.origins.get(stop_lvar)
                if stop_local is not None and stop_local in visitor.walk.local_vars_set:
                    linear = (
                        visitor.walk.temp_source.get(stop_local, stop_local),
                        0,
                        0,
                    )
                elif len(stop_lvar.uses) == 1:
                    linear = stop_lvar.uses[0].accept(_LinearUpdate(visitor))
                if linear is not None:
                    src_var, d_min, d_max = linear
                    if d_max is not None:
                        _tighten_relation(visitor.relations, r_key, src_var, d_max)
                    if d_min is not None:
                        _tighten_relation(visitor.relations, src_var, r_key, -d_min)
        else:
            visitor.assign(r_key, UNBOUND_FACT)
    if visitor.is_proven_fixnum_range(node):
        return BOOLEAN_TRUE_FACT
    return BOOLEAN_FACT


@defpropagate("len")
def _propagate_len(visitor: _Propagation, node: Combination):
    if len(node.args) == 1:
        arg_type = visitor.type_of(node.args[0])
        if arg_type is BOTTOM:
            return NO_FACT
        if arg_type.accept(_IS_BUILTIN_SEQUENCE_TYPE):
            seq_len = visitor.sequence_length_type(node.args[0])
            if seq_len is not None:
                return _fact(seq_len)
            return _SEQ_LEN_BOUND_FACT
    visitor.invalidate()
    return _SEQ_LEN_BOUND_FACT


@defpropagate("%range_available")
def _propagate_range_available(visitor: _Propagation, node: Combination):
    return BOOLEAN_FACT


@defpropagate("%has_binding")
def _propagate_has_binding(visitor: _Propagation, node: Combination):
    return BOOLEAN_FACT


@defpropagate("%min2")
def _propagate_min2(visitor: _Propagation, node: Combination):
    if len(node.args) != 2:
        return UNKNOWN_FACT
    left = visitor.type_of(node.args[0])
    right = visitor.type_of(node.args[1])
    if left is BOTTOM or right is BOTTOM:
        return NO_FACT
    if not (left.fits(REAL) and right.fits(REAL)):
        visitor.invalidate()
    return _fact(fndb.derive_min2_types(left, right))


@defpropagate("%max2")
def _propagate_max2(visitor: _Propagation, node: Combination):
    if len(node.args) != 2:
        return UNKNOWN_FACT
    left = visitor.type_of(node.args[0])
    right = visitor.type_of(node.args[1])
    if left is BOTTOM or right is BOTTOM:
        return NO_FACT
    if not (left.fits(REAL) and right.fits(REAL)):
        visitor.invalidate()
    return _fact(fndb.derive_max2_types(left, right))


@defpropagate("%math_sqrt")
def _propagate_math_sqrt(visitor: _Propagation, node: Combination):
    if len(node.args) != 1:
        return UNKNOWN_FACT
    arg = visitor.type_of(node.args[0])
    if arg is BOTTOM:
        return NO_FACT
    if arg.may_run_user_code:
        visitor.invalidate()
    return FLOAT_FACT


@defpropagate("%sequence_available")
def _propagate_sequence_available(visitor: _Propagation, node: Combination):
    if len(node.args) == 1 and visitor.type_of(node.args[0]).is_empty_iterator():
        return BOOLEAN_FALSE_FACT
    return BOOLEAN_FACT


@defpropagate("%isinstance")
def _propagate_instance_check(visitor: _Propagation, node: Combination):
    if not node.args:
        return BOOLEAN_FACT
    value_type = visitor.type_of(node.args[0])
    if value_type is BOTTOM:
        return NO_FACT
    class_types = [visitor.type_of(arg) for arg in node.args[1:]]
    if any(cls is BOTTOM for cls in class_types):
        return NO_FACT
    if not all(cls.fits_class() for cls in class_types):
        visitor.invalidate()
        return BOOLEAN_FACT
    if any(value_type.is_instance_of(cls) is True for cls in class_types):
        return BOOLEAN_TRUE_FACT
    return BOOLEAN_FACT


@defpropagate("%range_advance")
def _propagate_range_advance(visitor: _Propagation, node: Combination):
    state = linked(node.info, node, "info")
    return _fact(visitor.range_type(state))


@defpropagate("%unpack")
def _propagate_unpack(visitor: _Propagation, node: Combination):
    args = node.args
    visitor.invalidate()
    seq_contract = visitor.walk.lvar_contracts.get(args[0])
    if seq_contract is not None and node.lvar is not None:
        visitor.walk.lvar_contracts[node.lvar] = seq_contract
    arity = visitor.type_of(args[1]).exact_integer()
    if arity is not None and arity >= 0:
        seq_type = visitor.type_of(args[0])
        if seq_type is BOTTOM:
            return NO_FACT
        unpacked = seq_type.unpacked_tuple_type(arity)
        return _tuple_fact(unpacked.arity, unpacked.elements)
    return _UNBOUNDED_TUPLE_FACT


@defpropagate("%unpack_ref")
def _propagate_unpack_ref(visitor: _Propagation, node: Combination):
    args = node.args
    seq_type = visitor.type_of(args[0])
    if seq_type is BOTTOM:
        return NO_FACT
    index_type = visitor.type_of(args[1])
    static_item = seq_type.flow_getitem_type(index_type)
    if static_item is BOTTOM:
        return NO_FACT
    seq_contract = visitor.walk.lvar_contracts.get(args[0])
    index = index_type.exact_integer()
    tup = seq_type.accept(_AS_TUPLE_TYPE)
    arity = tup.arity if tup is not None else None
    elem_contract = (
        seq_contract.unpack_element_contract(arity, index)
        if seq_contract is not None and arity is not None and index is not None
        else None
    )
    item_type = visitor.apply_element_contract(node, static_item, elem_contract)
    return _fact(item_type)


@defpropagate("%resolve_method")
def _propagate_resolve_method(visitor: _Propagation, node: Combination):
    return None


@defpropagate("%getattr")
def _propagate_getattr(visitor: _Propagation, node: Combination):
    if node.args and node.info is not None:
        return _predicted_field_result(visitor, node, node.info.name)


def _predicted_field_result(visitor, node, name):
    recv_type = visitor.type_of(node.args[0])
    declared = visitor.walk.instance_declared_field(recv_type, name)
    if declared is not None:
        contract, is_unsafe = declared
        if node.lvar is not None and contract.has_element_contracts():
            visitor.walk.lvar_contracts[node.lvar] = contract
        if is_unsafe:
            visitor.apply_element_contract(
                node, UNKNOWN, contract, trusted_static=False
            )
        else:
            visitor.walk.element_contracts.pop(node, None)
        return _fact(contract.fact)
    field_type = visitor.walk.instance_field_type(recv_type, name)
    if field_type is UNKNOWN:
        return None
    if (
        field_type.fits(PYTHON_INTEGER)
        or field_type.fits_text(TEXT_TYPE)
        or field_type.fits_dict()
    ):
        visitor.apply_element_contract(
            node, UNKNOWN, InferredReturnContract(f"attribute {name}", field_type)
        )
    return _fact(field_type)


def _propagate_instance_field(visitor: _Propagation, node: Combination):
    if len(node.args) == 2:
        # IR1 optimization imports this module during initialization.
        from .ir1opt import bound_constant_text

        name = bound_constant_text(node.args[1])
        if name is None:
            name = visitor.type_of(node.args[1]).interned_text()
        if name is not None:
            return _predicted_field_result(visitor, node, name)


def _propagate_set_instance_field(visitor: _Propagation, node: Combination):
    # Releasing the previous reference can run weak-reference callbacks.
    visitor.invalidate()
    return NONE_FACT


@defpropagate("%physical_field_ref")
def _propagate_physical_field_ref(visitor: _Propagation, node: Combination):
    return _predicted_field_result(visitor, node, node.info.name)


@defpropagate("%physical_field_set")
def _propagate_physical_field_set(visitor: _Propagation, node: Combination):
    return _propagate_set_instance_field(visitor, node)


@defpropagate("%make_function")
def _propagate_make_function(visitor: _Propagation, node: Combination):
    args = node.args
    lambda_ = made_lambda(node)
    definition = linked(node.info, node, "info")
    parameters = lambda_.signature.default_parameters
    for value, contract, index in zip(
        args[1:], definition.default_contracts, parameters, strict=True
    ):
        if contract is not None:
            visitor.walk.annotations.check(
                contract,
                visitor.type_of(value),
                definition.location,
                ("local", lambda_, lambda_.vars[index].name),
            )
    return Fact(FunctionType(target=lambda_), lambda_, True)


@defpropagate("%call_resolved")
def _propagate_call_resolved(visitor: _Propagation, node: Combination):
    args = node.args
    resolved = (
        args[0].uses[0].accept(_RESOLVED_METHOD) if len(args[0].uses) == 1 else None
    )
    if resolved is not None:
        receiver_lvar, method_name = resolved
        receiver_type = visitor.type_of(receiver_lvar)
        if receiver_type is BOTTOM:
            return NO_FACT
        call_info = linked(node.info, node, "info")
        has_keywords = bool(call_info.keywords)
        arg_types = tuple(visitor.type_of(arg) for arg in args[1:])
        if any(t is BOTTOM for t in arg_types):
            return NO_FACT
        method = visitor.walk.instance_method(receiver_type, method_name)
        if method is not None:
            # The class's method, bound to the receiver: predicted, not
            # proven, because class attributes stay mutable. Code generation
            # guards the resolved function's identity before calling the
            # method's body directly, as it guards predicted global calls;
            # like those, the prediction seeds the callee's guarded entry
            # contexts but publishes no return type.
            if not any(kw is None for kw in call_info.keywords):
                receiver_class = receiver_type.instance_class()
                seeded_receiver = (
                    (receiver_type.dispatch_domain() or receiver_type)
                    if receiver_type.fits_set()
                    else InstanceType(
                        receiver_class,
                        fixed_slots=visitor.walk.class_facts[
                            receiver_class
                        ].fixed_slots,
                    )
                    if receiver_class is not None
                    else receiver_type
                )
                visitor.walk.method_calls.append(
                    CallSite(
                        method,
                        (
                            _fact(seeded_receiver),
                            *(visitor.fact_of(arg) for arg in args[1:]),
                        ),
                        False,
                        (None, *call_info.keywords) if call_info.keywords else (),
                    )
                )
            visitor.walk.method_targets[node] = method
            if not receiver_type.fits_set():
                visitor.invalidate()
                return UNKNOWN_FACT
        if (
            len(receiver_lvar.uses) == 1
            and visitor.walk.lambda_.policy.safety < 3
            and node.lexenv.policy.safety < 3
        ):
            mod_name = receiver_lvar.uses[0].accept(_ImportedModuleReceiver())
            if mod_name is not None:
                mod_target = (
                    visitor.walk.module_targets.get((mod_name, method_name))
                    if visitor.walk.module_targets is not None
                    else None
                )
                mod_res = UNKNOWN
                node.predicted = mod_target
                if mod_target is not None:
                    mod_res = mod_target.propagate_call(
                        node, visitor, args[1:], arg_types, call_info.keywords
                    )
                    if mod_res is BOTTOM:
                        return NO_FACT
                if (
                    mod_res is UNKNOWN
                    and not has_keywords
                    and visitor.walk.module_returns is not None
                ):
                    mod_res = visitor.walk.module_returns.get(
                        (mod_name, method_name), UNKNOWN
                    )
                visitor.invalidate()
                return _fact(mod_res)
        (
            result_type,
            _updated_receiver,
            _child_arg_index,
            _returns_child,
            may_call,
        ) = receiver_type.method_call(
            method_name,
            arg_types,
            has_keywords,
            visitor.walk.storage_methods,
        )
        if may_call:
            visitor.invalidate()
        recv_contract = visitor.walk.lvar_contracts.get(receiver_lvar)
        res_contract = (
            recv_contract.method_result_contract(method_name, arg_types)
            if recv_contract is not None and not has_keywords
            else None
        )
        result_type = visitor.apply_element_contract(node, result_type, res_contract)
        return _fact(result_type)
    visitor.invalidate()
    return UNKNOWN_FACT


@defpropagate("%extend_arguments")
def _propagate_extend_arguments(visitor: _Propagation, node: Combination):
    args = node.args
    if len(args) != 2:
        return None
    recv_var = visitor.origins.get(args[0])
    recv_type = visitor.type_of(args[0])
    seq_type = visitor.type_of(args[1])
    tup = seq_type.accept(_AS_TUPLE_TYPE)
    if recv_var is not None and recv_type == EMPTY_LIST_TYPE and tup is not None:
        visitor.tuple_list_origins[recv_var] = tup
    visitor.invalidate()
    return NONE_FACT


@defpropagate("%tuple_from_list")
def _propagate_tuple_from_list(visitor: _Propagation, node: Combination):
    args = node.args
    if len(args) != 1:
        return None
    src_var = visitor.origins.get(args[0])
    if src_var is not None and src_var in visitor.tuple_list_origins:
        tup = visitor.tuple_list_origins[src_var]
        return _tuple_fact(tup.arity, tup.elements)
    list_type = visitor.type_of(args[0])
    elem_type = list_type.iterate_type().next_item_type()
    if elem_type is BOTTOM:
        return _tuple_fact(0, ())
    if elem_type is not UNKNOWN:
        return _tuple_fact(1, (elem_type,))
    return _UNBOUNDED_TUPLE_FACT


def _propagate_call_with_helper(
    visitor: _Propagation, node: Combination, *, expanded_positional: bool
):
    args = node.args
    if not args:
        return None
    callee_lvar = args[0]
    if expanded_positional and len(args) == 2:
        seq_type = visitor.type_of(args[1])
        if seq_type is BOTTOM:
            return NO_FACT
        tup = seq_type.accept(_AS_TUPLE_TYPE)
        if tup is not None and tup.elements is not None:
            arg_types = tup.elements
            keywords: tuple[str | None, ...] = ()
        elif tup is not None and tup.arity == 0:
            arg_types = ()
            keywords = ()
        else:
            arg_types = (seq_type,)
            keywords = ("*",)
    else:
        arg_types = tuple(visitor.type_of(arg) for arg in args[1:])
        keywords = ("*" * len(arg_types),)
    callee_fact = visitor.fact_of(callee_lvar)
    target = (
        callee_fact.target
        or visitor.walk.imported_target(callee_lvar)
        or visitor.walk.builtin_target(callee_lvar)
    )
    resolved = visitor.walk.resolve_call(
        callee_fact,
        arg_types,
        node.source,
        keywords,
    )
    if resolved is BOTTOM:
        return NO_FACT
    if resolved is UNKNOWN and target is not None:
        resolved = target.derive_return_type(arg_types, keywords)
        if resolved is BOTTOM:
            return NO_FACT
    if resolved is UNKNOWN and visitor.walk.imported_returns:
        callee_name = callee_lvar.use().accept(_IMPORTED_CALLEE_NAME)
        if callee_name is not None:
            resolved = visitor.walk.imported_returns.get(callee_name, UNKNOWN)
    builtin_type = UNKNOWN
    if resolved is UNKNOWN:
        builtin_type = visitor.walk.builtin_return_type(
            callee_lvar, arg_types, keywords
        )
        if builtin_type is BOTTOM:
            return NO_FACT
        resolved = builtin_type
    visitor.invalidate()
    return _fact(resolved)


@defpropagate("%call_expanded")
def _propagate_call_expanded(visitor: _Propagation, node: Combination):
    return _propagate_call_with_helper(visitor, node, expanded_positional=False)


@defpropagate("%call_expanded_positional")
def _propagate_call_expanded_positional(visitor: _Propagation, node: Combination):
    return _propagate_call_with_helper(visitor, node, expanded_positional=True)


@defpropagate("%import")
def _propagate_import(visitor: _Propagation, node: Combination):
    visitor.invalidate()
    return UNKNOWN_FACT


@defpropagate("%catch_exception")
def _propagate_catch_exception(visitor: _Propagation, node: Combination):
    visitor.invalidate()
    return UNKNOWN_FACT


@defpropagate("%format")
def _propagate_format(visitor: _Propagation, node: Combination):
    visitor.invalidate()
    return TEXT_FACT


@defpropagate("%getitem")
def _propagate_getitem(visitor: _Propagation, node: Combination):
    args = node.args
    receiver_type = visitor.type_of(args[0])
    index_type = visitor.type_of(args[1])
    if receiver_type is BOTTOM or index_type is BOTTOM:
        visitor.walk.in_bounds_nodes.discard(node)
        return NO_FACT
    static_item = receiver_type.flow_getitem_type(index_type)
    if static_item is BOTTOM:
        visitor.walk.in_bounds_nodes.discard(node)
        return NO_FACT
    if (
        not receiver_type.accept(_IS_BUILTIN_SEQUENCE_TYPE)
        and receiver_type.may_run_user_code
    ) or index_type.may_run_user_code:
        visitor.invalidate()
    if index_type.fits(PYTHON_INTEGER) and not index_type.may_be_unbound:
        visitor.check_in_bounds_access(node, args[0], args[1])
        bounds = receiver_type.valid_index_bounds()
        if bounds is not None:
            visitor.prove_argument_type(args[1], IntegerType(bounds[0], bounds[1]))
    else:
        visitor.walk.in_bounds_nodes.discard(node)
    recv_contract = visitor.walk.lvar_contracts.get(args[0])
    elem_contract = (
        recv_contract.getitem_contract(index_type)
        if recv_contract is not None
        else None
    )
    item_type = visitor.apply_element_contract(
        node,
        static_item,
        elem_contract,
        trusted_static=not receiver_type.accept(_IS_MUTABLE_CONTAINER_TYPE),
    )
    return _fact(item_type)


@defpropagate("%getslice")
def _propagate_getslice(visitor: _Propagation, node: Combination):
    args = node.args
    if not args:
        return None
    if any(visitor.type_of(arg) is BOTTOM for arg in args):
        return NO_FACT
    receiver_type = visitor.type_of(args[0])
    if (
        not receiver_type.accept(_IS_BUILTIN_SEQUENCE_TYPE)
        and receiver_type.may_run_user_code
    ) or any(visitor.type_of(arg).may_run_user_code for arg in args[1:]):
        visitor.invalidate()
    recv_contract = visitor.walk.lvar_contracts.get(args[0])
    if (
        recv_contract is not None
        and node.lvar is not None
        and recv_contract.has_element_contracts()
    ):
        visitor.walk.lvar_contracts[node.lvar] = recv_contract
    arg_types = [visitor.type_of(arg) for arg in args[1:4]]
    return _fact(receiver_type.getslice_type(*arg_types))


@defpropagate("%setitem")
def _propagate_setitem(visitor: _Propagation, node: Combination):
    args = node.args
    if len(args) != 3:
        return None
    recv_type = visitor.type_of(args[0])
    key_type = visitor.type_of(args[1])
    if not recv_type.fits_list(LIST_TYPE) or key_type.may_run_user_code:
        visitor.invalidate()
    if recv_type.fits_list(LIST_TYPE):
        is_int_index = key_type.fits(PYTHON_INTEGER) and not key_type.may_be_unbound
        if is_int_index:
            visitor.check_in_bounds_access(node, args[0], args[1])
            bounds = recv_type.valid_index_bounds()
            if bounds is not None:
                visitor.prove_argument_type(args[1], IntegerType(bounds[0], bounds[1]))
        else:
            visitor.walk.in_bounds_nodes.discard(node)
    else:
        visitor.walk.in_bounds_nodes.discard(node)
    return NONE_FACT


@defpropagate("%context_enter")
def _propagate_context_enter(visitor: _Propagation, node: Combination):
    args = node.args
    if len(args) != 1:
        return None
    operand_type = visitor.type_of(args[0])
    if operand_type is BOTTOM:
        return NO_FACT
    visitor.invalidate()
    if operand_type.fits_iterator() and operand_type.is_stream:
        return _fact(operand_type)
    return UNKNOWN_FACT


@defpropagate("%iterate")
def _propagate_iterate(visitor: _Propagation, node: Combination):
    args = node.args
    if len(args) != 1:
        return None
    operand_type = visitor.type_of(args[0])
    if operand_type is BOTTOM:
        return NO_FACT
    if (
        not operand_type.accept(_IS_BUILTIN_SEQUENCE_TYPE)
        and operand_type.may_run_user_code
    ):
        visitor.invalidate()
    recv_contract = visitor.walk.lvar_contracts.get(args[0])
    if recv_contract is not None and node.lvar is not None:
        iter_elem = recv_contract.iter_element_contract()
        if iter_elem is not None:
            visitor.walk.iter_contracts[node.lvar] = iter_elem
    return _fact(operand_type.flow_iterate_type())


@defpropagate("%next_item")
def _propagate_next_item(visitor: _Propagation, node: Combination):
    iterator_var = _loop_iterator_var(node.block)
    if iterator_var is not None and iterator_var in visitor.state:
        iter_type = visitor.state[iterator_var].type
        if iter_type.is_empty_iterator():
            return NO_FACT
        item_type = iter_type.next_item_type()
        if iter_type.may_run_user_code:
            visitor.invalidate()
        elem_contract = visitor.walk.iter_contracts.get(iterator_var)
        if elem_contract is not None:
            if (
                node.lvar is not None
                and node.lvar.dest is not None
                and node.lvar.dest.accept(_IS_UNPACK_NODE)
            ):
                visitor.walk.lvar_contracts[node.lvar] = elem_contract
            else:
                item_type = visitor.apply_element_contract(
                    node, item_type, elem_contract, trusted_static=False
                )
        if item_type is not UNKNOWN:
            return _fact(item_type)
    visitor.invalidate()
    return UNKNOWN_FACT


def _loop_headers(entry, successors) -> tuple[frozenset, frozenset]:
    """Blocks that are targets of DFS back edges, where intervals need widening."""
    colors = {entry: 1}
    stack = [(entry, iter(successors[entry]))]
    headers = set()
    back_edges = set()
    while stack:
        block, edges = stack[-1]
        target = next(edges, None)
        if target is None:
            colors[block] = 2
            stack.pop()
        elif colors.get(target) == 1:
            headers.add(target)
            back_edges.add((block, target))
        elif target not in colors:
            colors[target] = 1
            stack.append((target, iter(successors[target])))
    return frozenset(headers), frozenset(back_edges)


@dataclass(slots=True)
class LambdaGraph:
    """Static per-lambda CFG and def-use structure reused across propagations."""

    blocks: tuple
    block_nodes: dict
    handlers: dict
    entry: object
    successors: dict
    predecessors: dict
    consumers: dict
    headers: frozenset
    back_edges: frozenset
    global_names: tuple
    indirect: tuple
    volatile_keys: tuple
    call_nodes: tuple
    thresholds: tuple[int, ...] = _BASE_WIDENING_THRESHOLDS
    has_seq_length_ops: bool = False
    range_states: tuple = ()


_THRESHOLD_COMPARISON_INFOS = frozenset(
    (
        fndb.COMPARE[ast.ComparisonOperator.LESS],
        fndb.COMPARE[ast.ComparisonOperator.LESS_EQUAL],
        fndb.COMPARE[ast.ComparisonOperator.GREATER],
        fndb.COMPARE[ast.ComparisonOperator.GREATER_EQUAL],
        fndb.COMPARE[ast.ComparisonOperator.NOT_EQUAL],
    )
)


class _ConstantThreshold(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_Constant(self, leaf):
        val = constant_type(leaf.value).exact_integer()
        if val is not None and FIXNUM_MIN < val < FIXNUM_MAX:
            return val
        return None


_CONSTANT_THRESHOLD = _ConstantThreshold()


class _GraphScan(NodeVisitor, LeafVisitor):
    """Collects global variable names and full/guarded call sites for a lambda."""

    def __init__(self, consumers):
        self.global_names: set[str] = set()
        self.call_nodes: list[tuple[object, Combination]] = []
        self.threshold_set: set[int] = set(_BASE_WIDENING_THRESHOLDS)
        self.consumers = consumers
        self.block = None
        self.has_seq_length_ops: bool = False
        self.range_states: list = []

    def _add_lvar_threshold(self, lvar) -> None:
        if len(lvar.uses) == 1:
            val = lvar.uses[0].accept(_CONSTANT_THRESHOLD)
            if val is not None:
                self.threshold_set.update((val - 1, val, val + 1))

    def visit_Ref(self, node):
        node.leaf.accept(self)

    def visit_Set(self, node):
        node.var.accept(self)

    def visit_GlobalVar(self, leaf):
        self.global_names.add(leaf.name)
        if leaf.name == "len":
            self.has_seq_length_ops = True

    def visit_Combination(self, node):
        if node.kind in (CallKind.FULL, CallKind.GUARDED) and not any(
            kw in ("*", "**") for kw in node.info.keywords
        ):
            self.call_nodes.append((self.block, node))
        if node.kind is CallKind.KNOWN:
            if node.fun_info in _THRESHOLD_COMPARISON_INFOS:
                for arg in node.args:
                    self._add_lvar_threshold(arg)
            if node.fun_info is fndb.RANGE_PREPARE and node.info is not None:
                self.range_states.append(node.info)
                self.has_seq_length_ops = True
                for arg in node.info.arguments:
                    self._add_lvar_threshold(arg)
            elif node.fun_info is not None and node.fun_info.name in (
                "_text_length",
                "_storage_length",
                "len",
                "%getslice",
                "_text_slice",
                "%_list_fill",
                "%getitem",
                "%setitem",
                "_storage_read",
                "_storage_write",
            ):
                self.has_seq_length_ops = True
            if node.args:
                for arg in node.args:
                    for use in arg.uses:
                        if use.block is not self.block:
                            self.consumers[use].add(self.block)
            if node.fun_info is fndb.RANGE_ADVANCE and node.info is not None:
                for arg in node.info.arguments:
                    self._add_lvar_threshold(arg)
                    for use in arg.uses:
                        if use.block is not self.block:
                            self.consumers[use].add(self.block)


def build_lambda_graph(lambda_: Lambda) -> LambdaGraph:
    blocks = tuple(linked(lambda_.component, lambda_, "component").blocks())
    block_nodes = {block: tuple(block.nodes()) for block in blocks}
    handlers = {block: block_exception_target(block) for block in blocks}
    entry = linked(lambda_.bind, lambda_, "bind").block
    successors = {block: tuple(_successors(block, handlers[block])) for block in blocks}
    predecessors_sets = defaultdict(set)
    consumers = defaultdict(set)
    scan = _GraphScan(consumers)
    for block in blocks:
        scan.block = block
        for target in successors[block]:
            predecessors_sets[target].add(block)
        for node in block_nodes[block]:
            lvar = node.value_lvar
            if lvar is not None and lvar.dest is not None:
                dest_block = lvar.dest.block
                if dest_block is not block:
                    consumers[node].add(dest_block)
            node.accept(scan)
    consumers = {node: tuple(blocks_set) for node, blocks_set in consumers.items()}
    predecessors = {block: tuple(predecessors_sets[block]) for block in blocks}
    headers, back_edges = _loop_headers(entry, successors)
    global_names = tuple(scan.global_names)
    call_nodes = tuple(scan.call_nodes)
    thresholds = tuple(sorted(scan.threshold_set))
    closure = (
        tuple(lambda_.environment.closure) if lambda_.environment is not None else ()
    )
    indirect = tuple(
        var
        for var in (*lambda_.locals, *closure)
        if var.indirect
        and (
            var.home is not lambda_
            or any(s.home_lambda is not lambda_ for s in var.sets)
        )
    )
    volatile_keys = (
        *((_GLOBAL, name) for name in global_names),
        *indirect,
    )
    return LambdaGraph(
        blocks=blocks,
        block_nodes=block_nodes,
        handlers=handlers,
        entry=entry,
        successors=successors,
        predecessors=predecessors,
        consumers=consumers,
        headers=headers,
        back_edges=back_edges,
        global_names=global_names,
        indirect=indirect,
        volatile_keys=volatile_keys,
        call_nodes=call_nodes,
        thresholds=thresholds,
        has_seq_length_ops=scan.has_seq_length_ops,
        range_states=tuple(scan.range_states),
    )


class _GlobalCalleeName(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_GlobalVar(self, leaf):
        return leaf.name

    def visit_LambdaVar(self, leaf):
        if leaf.hidden and len(leaf.sets) == 1:
            value = leaf.sets[0].value
            if value is not None and len(value.uses) == 1:
                return value.uses[0].accept(self)
        return None


class _ImportedCalleeName(_GlobalCalleeName):
    def visit_LambdaVar(self, leaf):
        source = super().visit_LambdaVar(leaf)
        if source is not None:
            return source
        if len(leaf.sets) == 1 and (leaf.home is None or leaf not in leaf.home.vars):
            return leaf.name
        return None


_GLOBAL_CALLEE_NAME = _GlobalCalleeName()
_IMPORTED_CALLEE_NAME = _ImportedCalleeName()


class _Walk:
    """One propagation over a lambda under given parameter facts."""

    def __init__(
        self,
        lambda_: Lambda,
        global_targets,
        parameters,
        resolve_call,
        annotations,
        *,
        graph: LambdaGraph | None = None,
        proven_globals: frozenset[str] = frozenset(),
        rebound_globals: frozenset[str] = frozenset(),
        global_constants: Mapping[str, ValueType] | None = None,
        imported_returns: Mapping[str, ValueType] | None = None,
        imported_targets: (Mapping[str, object] | None) = None,
        module_returns: Mapping[tuple[str, str], ValueType] | None = None,
        module_targets: Mapping[tuple[str, str], object] | None = None,
        storage_methods: Mapping[tuple[str, str], ValueType] | None = None,
        builtin_return_types: Mapping[str, ValueType] | None = None,
        builtin_targets: (Mapping[str, object] | None) = None,
        class_facts: Mapping | None = None,
        recursive_targets: frozenset[Lambda] = frozenset(),
        unique_targets: frozenset[Lambda] = frozenset(),
        is_specialized_context: bool = False,
        for_return_derivation: bool = False,
    ):
        self.lambda_ = lambda_
        self.global_targets = global_targets
        self.resolve_call = resolve_call
        self.annotations = annotations
        self.rebound_globals = rebound_globals
        self.global_constants = global_constants
        self.imported_returns = imported_returns
        self.imported_targets = imported_targets
        self.module_returns = module_returns
        self.module_targets = module_targets
        self.storage_methods = storage_methods
        self.builtin_return_types = builtin_return_types
        self.builtin_targets = builtin_targets
        self.class_facts = class_facts or {}
        self.recursive_targets = recursive_targets
        self.unique_targets = unique_targets
        self.is_specialized_context = is_specialized_context
        self.for_return_derivation = for_return_derivation
        # Calls this walk resolved to methods of typed classes.
        self.method_calls: list[CallSite] = []
        self.method_targets: dict[object, Lambda] = {}
        self.facts: dict[object, Fact] = {}
        if graph is None:
            graph = build_lambda_graph(lambda_)
        self.graph = graph
        self.blocks = graph.blocks
        self.block_nodes = graph.block_nodes
        self.handlers = graph.handlers
        self.entry = graph.entry
        self.successors = graph.successors
        self.predecessors = graph.predecessors
        self.consumers = graph.consumers
        self.headers = graph.headers
        self.back_edges = graph.back_edges
        self.global_names = graph.global_names
        self.indirect = graph.indirect
        self.volatile_keys = graph.volatile_keys
        self.thresholds = graph.thresholds
        self.local_vars_tuple = tuple(var for var in lambda_.locals if not var.indirect)
        self.local_vars_set = frozenset(self.local_vars_tuple)
        self.seq_len_keys: dict[LambdaVar, tuple[str, LambdaVar]] = (
            {var: (_SEQ_LEN, var) for var in self.local_vars_tuple}
            if graph.has_seq_length_ops
            else {}
        )
        self.range_stop_keys: dict[object, tuple[str, object]] = {
            rs: (_RANGE_STOP, rs) for rs in graph.range_states
        }
        self.relational_vars_tuple = (
            self.local_vars_tuple
            + tuple(self.seq_len_keys.values())
            + tuple(self.range_stop_keys.values())
        )
        self.relational_vars_set = frozenset(self.relational_vars_tuple)
        self.temp_source: dict[object, object] = {}
        self.local_contracts = {
            var: contract
            for var in lambda_.locals
            if (contract := lambda_.local_annotations.get(var.name)) is not None
            and ("local", lambda_, var.name) not in annotations.ignored
        }
        parameter_facts = dict(zip(lambda_.vars, parameters, strict=True))
        self.initial: dict[object, Fact] = {
            var: _fact(parameter_facts[var]) if var in parameter_facts else UNBOUND_FACT
            for var in lambda_.locals
        }
        for r_key in self.range_stop_keys.values():
            self.initial[r_key] = UNBOUND_FACT
        for var, seq_key in self.seq_len_keys.items():
            if var in parameter_facts:
                p_type = parameter_facts[var]
                exact_len = p_type.exact_length() if not p_type.may_be_unbound else None
                if exact_len is not None:
                    self.initial[seq_key] = _fact(IntegerType(exact_len, exact_len))
                elif p_type.accept(_IS_BUILTIN_SEQUENCE_TYPE):
                    self.initial[seq_key] = _SEQ_LEN_BOUND_FACT
                else:
                    self.initial[seq_key] = UNBOUND_FACT
            else:
                self.initial[seq_key] = UNBOUND_FACT
        self.element_contracts: dict[object, object] = {}
        self.in_bounds_nodes: set[object] = set()
        self.var_contracts: dict[object, object] = {
            var: contract
            for var, contract in self.local_contracts.items()
            if var in parameter_facts
            and parameter_facts[var] is not UNKNOWN
            and parameter_facts[var].fits(contract.checked_type)
        }
        self.lvar_contracts: dict[object, object] = {}
        self.iter_contracts: dict[object, object] = {}
        closure = lambda_.environment.closure if lambda_.environment is not None else ()
        for var in closure:
            vtype = var.optimistic_type
            resolved_type = (
                vtype if vtype is not None and vtype is not BOTTOM else UNKNOWN
            )
            if any(s.value is None for s in var.sets):
                resolved_type = resolved_type.with_unbound()
            target = (
                made_function(var.sets[0].value.uses[0])
                if resolved_type is FUNCTION_TYPE
                and len(var.sets) == 1
                and not var.indirect
                and var.home is not None
                and var not in var.home.vars
                and var.sets[0].value is not None
                and len(var.sets[0].value.uses) == 1
                else None
            )
            self.initial[var] = (
                Fact(resolved_type, target, True)
                if target is not None
                else _fact(resolved_type)
            )
        # What user code may leave behind in module globals and variables
        # captured by nested functions.
        invalidated_globals: dict[object, Fact] = {}
        for name in self.global_names:
            const_type = (
                global_constants.get(name, UNKNOWN)
                if global_constants is not None
                else UNKNOWN
            )
            target = global_targets.get(name)
            if target is None and imported_targets is not None:
                target = imported_targets.get(name)
            if target is not None:
                invalidated_globals[(_GLOBAL, name)] = Fact(
                    const_type
                    if const_type is not UNKNOWN
                    else (FUNCTION_TYPE if name in proven_globals else UNKNOWN),
                    target,
                    name in proven_globals,
                )
            else:
                invalidated_globals[(_GLOBAL, name)] = _fact(const_type)
        self.invalidated = {
            **invalidated_globals,
            **{
                var: (
                    self.initial.get(var, UNKNOWN_FACT)
                    if var in closure
                    else (
                        UNKNOWN_UNBOUND_FACT
                        if any(s.value is None for s in var.sets)
                        else UNKNOWN_FACT
                    )
                )
                for var in self.indirect
            },
        }
        self.initial.update(invalidated_globals)

    def imported_target(
        self, fun_lvar
    ) -> Lambda | ClassConstructorTarget | ForeignCallTarget | None:
        if self.imported_targets is None:
            return None
        name = fun_lvar.use().accept(_IMPORTED_CALLEE_NAME)
        if name is None:
            return None
        return self.imported_targets.get(name)

    def instance_method(self, receiver_type: ValueType, name: str) -> Lambda | None:
        """The method ``name`` resolves to on instances of the receiver's
        class, when the receiver is a typed instance and the class defines
        it as a plain function."""
        facts = self.class_facts.get(receiver_type.instance_class())
        if facts is None:
            return None
        return facts.methods.get(name)

    def instance_field_type(self, receiver_type: ValueType, name: str) -> ValueType:
        """The predicted field type on instances of the receiver's class,
        restricted to shapes whose consumers guard their runtime operations."""
        return receiver_type.accept(
            _InstanceFieldType(self.class_facts, self.storage_methods, name)
        )

    def instance_declared_field(self, receiver_type: ValueType, name: str):
        """Return ``(contract, is_unsafe)`` when ``name`` is a declared field
        on the receiver's class."""
        return receiver_type.accept(_InstanceDeclaredField(self.class_facts, name))

    def allocated_instance_type(self, class_type: ValueType) -> ValueType:
        # Physical allocation bypasses __new__. Its result belongs to the
        # implementation class, independently of constructor result predictions.
        targets = tuple(
            target
            for mapping in (
                self.imported_targets,
                self.module_targets,
                self.builtin_targets,
            )
            if mapping is not None
            for target in mapping.values()
        )
        return class_type.allocate_instance_type().accept(
            _AllocatedImplementation(targets)
        )

    def builtin_target(
        self, fun_lvar
    ) -> Lambda | ClassConstructorTarget | ForeignCallTarget | None:
        return self.builtin_target_named(fun_lvar.use().accept(_IMPORTED_CALLEE_NAME))

    def builtin_target_named(
        self, name: str | None
    ) -> Lambda | ClassConstructorTarget | ForeignCallTarget | None:
        """The builtins-unit definition a reference to global ``name``
        resolves to, when this module never binds that global itself."""
        if (
            self.lambda_.kind is FunctionalKind.TOPLEVEL
            or self.lambda_.policy.safety >= 3
            or self.builtin_targets is None
        ):
            return None
        if name is None or name in self.rebound_globals or name in self.global_targets:
            return None
        return self.builtin_targets.get(name)

    def foreign_builtin_function(self, name: str) -> object | None:
        """The separately compiled builtins-unit function global ``name``
        names, as a predicted call target. Calls through the name stay
        guarded by the callee's identity, so a runtime rebinding still runs.
        Builtin constructors and functions defined in this compilation keep
        their declared return derivations rather than a predicted entry."""
        target = self.builtin_target_named(name)
        if target is None or target.as_lambda() is not None:
            return None
        return target if target.as_function() is not None else None

    def builtin_return_type(
        self,
        fun_lvar,
        arg_types: tuple[ValueType, ...],
        keywords: tuple[str | None, ...] = (),
    ) -> ValueType:
        if self.lambda_.policy.safety >= 3:
            return UNKNOWN
        name = fun_lvar.use().accept(_IMPORTED_CALLEE_NAME)
        if self.lambda_.kind is FunctionalKind.TOPLEVEL and self.lambda_.is_protocol:
            return UNKNOWN
        if name is None or name in self.rebound_globals or name in self.global_targets:
            return UNKNOWN
        bootstrap_target = BOOTSTRAP_BUILTIN_TARGETS.get(name)
        if bootstrap_target is not None:
            derived = bootstrap_target.derive_return_type(arg_types, keywords)
            if derived is not UNKNOWN:
                return derived
        if self.builtin_targets is not None:
            target = self.builtin_targets.get(name)
            if target is not None:
                derived = target.derive_return_type(arg_types, keywords)
                if derived is not UNKNOWN:
                    return derived
        if not keywords and self.builtin_return_types is not None:
            return self.builtin_return_types.get(name, UNKNOWN)
        return UNKNOWN

    def element_preserving_builtin_contract(self, fun_lvar, elem_contract):
        if (
            self.lambda_.kind is FunctionalKind.TOPLEVEL
            or self.lambda_.policy.safety >= 3
        ):
            return None
        name = fun_lvar.use().accept(_IMPORTED_CALLEE_NAME)
        if name is None or name in self.rebound_globals or name in self.global_targets:
            return None
        if name in ("list", "sorted"):
            return ListContract("list", elem_contract)
        if name == "set":
            return SetContract("set", "mule_dict_set", elem_contract)
        if name == "frozenset":
            return SetContract("frozenset", "mule_dict_frozenset", elem_contract)
        return None

    def invalidate_globals(self, state):
        """Forget facts that user code may change."""
        if self.invalidated:
            state.update(self.invalidated)

    def narrow_return_parameters(
        self, body: Body, truth: bool
    ) -> tuple[ValueType, ...] | None:
        if not self.lambda_.vars or not body.return_states:
            return None
        # The returned values' predicates read their operands' types from the
        # propagation that produced ``body``; this walk has not run.
        self.facts = body.facts
        joined: list[ValueType] = [BOTTOM] * len(self.lambda_.vars)
        found_any = False
        for use, ret_state, origins, type_of_origins in body.return_states:
            ret_type = body.facts.get(use, NO_FACT).type
            if ret_type is BOTTOM:
                continue
            ret_truth = ret_type.truth_value()
            if (truth and ret_truth is False) or (not truth and ret_truth is True):
                continue
            path_state = ret_state
            if ret_truth is None:
                prop = _Propagation(self, ret_state, origins.copy())
                prop.type_of_origins = type_of_origins.copy()
                narrowed = use.accept(_ConstrainComparison(prop, ret_state, truth))
                if narrowed is _DEAD_EDGE:
                    continue
                if narrowed is not None:
                    path_state = narrowed
            found_any = True
            for i, var in enumerate(self.lambda_.vars):
                var_fact = path_state.get(var, NO_FACT)
                if not var_fact.type.may_be_unbound:
                    joined[i] = joined[i].join(var_fact.type)
        if not found_any:
            return None
        return tuple(joined)

    def run(self) -> Body:
        incoming, edge_states, out_states = {}, {}, {}
        in_relations, edge_relations = {}, {}
        return_block_origins: dict[object, tuple[dict, dict]] = {}
        return_blocks = (
            frozenset(use.block for use in self.lambda_.return_.result.uses)
            if self.lambda_.return_ is not None
            else frozenset()
        )
        widened_keys = defaultdict(set)
        if self.propagate(
            incoming,
            edge_states,
            out_states,
            widened_keys,
            return_blocks,
            return_block_origins,
            in_relations,
            edge_relations,
            widening=True,
        ):
            self.propagate(
                incoming,
                edge_states,
                out_states,
                widened_keys,
                return_blocks,
                return_block_origins,
                in_relations,
                edge_relations,
                widening=False,
            )
        self.resolve_call = None
        returned = BOTTOM
        facts = self.facts
        calls = tuple(
            CallSite(
                (callee := facts.get(node.fun.use(), NO_FACT)).target,
                tuple(facts.get(arg.use(), NO_FACT) for arg in node.args),
                callee.proven,
                node.info.keywords
                if node.info is not None and node.info.keywords
                else (),
            )
            for block, node in self.graph.call_nodes
            if block in incoming
        ) + tuple(self.method_calls)
        self_type: ValueType | None = None
        return_states = ()
        if self.lambda_.return_ is not None:
            self_var = self.lambda_.vars[0] if self.lambda_.vars else None
            joined_self = BOTTOM
            collected_returns = []
            for use in self.lambda_.return_.result.uses:
                if use.block in incoming:
                    returned = returned.join(facts.get(use, NO_FACT).type)
                    if use.block in out_states:
                        origins, type_of_origins = return_block_origins.get(
                            use.block, ({}, {})
                        )
                        collected_returns.append(
                            (use, out_states[use.block], origins, type_of_origins)
                        )
                        if self_var is not None:
                            self_fact = out_states[use.block].get(self_var, NO_FACT)
                            if not self_fact.type.may_be_unbound:
                                joined_self = joined_self.join(self_fact.type)
            return_states = tuple(collected_returns)
            if self_var is not None and joined_self is not BOTTOM:
                self_type = joined_self
        return Body(
            facts,
            returned,
            calls,
            self_type,
            return_states,
            dict(self.method_targets),
            dict(self.element_contracts),
            frozenset(self.in_bounds_nodes),
        )

    def propagate(
        self,
        incoming,
        edge_states,
        out_states,
        widened_keys,
        return_blocks,
        return_block_origins,
        in_relations,
        edge_relations,
        *,
        widening: bool,
    ) -> bool:
        visits = defaultdict(int)
        key_visits = defaultdict(int)
        did_widen = False
        rpo = {block: idx for idx, block in enumerate(self.blocks)}
        initial = (
            [self.entry]
            if widening
            else [block for block in self.blocks if block in incoming]
        )
        current_heap = [(rpo[block], block) for block in initial]
        heapq.heapify(current_heap)
        next_heap: list[tuple[int, object]] = []
        queued = set(initial)
        cross_changed = set()
        visited_edges = set()
        predecessors = self.predecessors
        successors = self.successors
        block_nodes = self.block_nodes
        handlers = self.handlers
        consumers = self.consumers
        headers = self.headers
        back_edges = self.back_edges
        thresholds = self.thresholds
        facts = self.facts
        visitor = _Propagation(self, {}, {})

        edge_constrainer = _ConstrainEdge(None, visitor, {}, edge_states)
        cmp_constrainer = _ConstrainComparison(visitor, {}, False)
        edge_constrainer.cmp_constrainer = cmp_constrainer

        while current_heap or next_heap:
            if not current_heap:
                current_heap, next_heap = next_heap, current_heap
            cur_rpo, block = heapq.heappop(current_heap)
            queued.remove(block)
            operand_changed = block in cross_changed
            if operand_changed:
                cross_changed.remove(block)
            parent_rels = []
            rel_states = []
            for parent in predecessors[block]:
                edge_key = parent, block
                if edge_key in edge_states:
                    parent_rels.append(edge_relations.get(edge_key, {}))
                    rel_states.append(edge_states[edge_key])
            if widening or block not in headers:
                parents = rel_states.copy()
                stale_backs = None
            else:
                parents = []
                stale_backs = []
                for parent in predecessors[block]:
                    edge_key = parent, block
                    if edge_key not in edge_states:
                        continue
                    if edge_key in back_edges and edge_key not in visited_edges:
                        stale_backs.append(edge_states[edge_key])
                    else:
                        parents.append(edge_states[edge_key])
            if block is self.entry:
                parents.append(self.initial)
                parent_rels.append({})
                rel_states.append(self.initial)
            if not parents:
                continue
            rels = _join_relations_list(parent_rels, rel_states)
            if not widening and (block in headers or visits[block] >= 3):
                rels = _widen_relations(in_relations.get(block, {}), rels)
            state = parents[0]
            for parent in parents[1:]:
                state, _ = join_states(state, parent)
            if stale_backs:
                w_keys = widened_keys.get(block)
                if w_keys:
                    state = state.copy()
                    for back_state in stale_backs:
                        for key in w_keys:
                            state[key] = join_fact(state[key], back_state[key])
            previous = incoming.get(block)
            if widening:
                if previous is None:
                    changed = True
                else:
                    state, changed = join_states(previous, state)
                old_rels = in_relations.get(block)
                if old_rels is not None:
                    if (block in headers and visits[block] >= 2) or visits[block] >= 4:
                        rels = _widen_relations(old_rels, rels)
                    else:
                        rels = _join_relations_list([old_rels, rels], [previous, state])
                rel_changed = rels != old_rels
                if rel_changed:
                    in_relations[block] = rels
                if changed or rel_changed:
                    visits[block] += 1
                    if changed and block in headers and block in incoming:
                        old_in = incoming[block]
                        widened = {}
                        for key, val in state.items():
                            old_val = old_in[key]
                            if (
                                val is not old_val
                                and val != old_val
                                and any(
                                    (pred, block) in back_edges
                                    and (pred, block) in edge_states
                                    and join_fact(
                                        old_val, edge_states[pred, block][key]
                                    )
                                    is not old_val
                                    for pred in predecessors[block]
                                )
                            ):
                                key_visits[block, key] += 1
                                widened_keys[block].add(key)
                                if key_visits[block, key] in (3, 4):
                                    w_val = widen_fact(old_val, val, thresholds)
                                elif key_visits[block, key] > 4:
                                    w_val = widen_fact(old_val, val)
                                else:
                                    w_val = val
                                if (
                                    key in self.local_vars_set
                                    and key.type is not None
                                    and w_val.type.fits(PYTHON_INTEGER)
                                ):
                                    w_val = Fact(
                                        w_val.type.annotated_integer(key.type),
                                        w_val.target,
                                        w_val.proven,
                                    )
                                widened[key] = w_val
                            else:
                                widened[key] = val
                        if widened != state:
                            did_widen = True
                            state = widened
                    incoming[block] = state
                elif not operand_changed:
                    continue
            else:
                old_rels = in_relations.get(block)
                if visits[block] >= 3 and previous is not None:
                    state, state_changed = join_states(previous, state)
                    if state_changed and block in headers:
                        new_state = {}
                        for k, v in state.items():
                            if v is not previous[k] and v != previous[k]:
                                w_v = widen_fact(previous[k], v)
                                if (
                                    k in self.local_vars_set
                                    and k.type is not None
                                    and w_v.type.fits(PYTHON_INTEGER)
                                ):
                                    w_v = Fact(
                                        w_v.type.annotated_integer(k.type),
                                        w_v.target,
                                        w_v.proven,
                                    )
                                new_state[k] = w_v
                            else:
                                new_state[k] = v
                        state = new_state
                else:
                    state_changed = state is not previous and state != previous
                rel_changed = rels != old_rels
                if visits[block] == 0 or state_changed or rel_changed:
                    visits[block] += 1
                    incoming[block] = state
                    in_relations[block] = rels
                else:
                    if previous is not None:
                        state = previous
                    if not operand_changed:
                        continue
            # The visitor copies its state on first mutation.
            handler = handlers[block]
            has_handler = handler is not None
            visitor.reset(state, has_handler, in_relations.get(block))
            terminated = False
            for node in block_nodes[block]:
                fact = node.accept(visitor)
                if fact is not None:
                    if fact is NO_FACT:
                        terminated = True
                    previous_fact = facts.get(node, NO_FACT)
                    if widening:
                        # join_fact returns ``previous_fact`` itself when nothing changed.
                        updated = join_fact(previous_fact, fact)
                        changed = updated is not previous_fact
                    else:
                        updated = fact
                        changed = (
                            updated is not previous_fact and updated != previous_fact
                        )
                    if changed:
                        facts[node] = updated
                        for consumer in consumers.get(node, ()):
                            cross_changed.add(consumer)
                            if consumer not in queued:
                                c_rpo = rpo[consumer]
                                heapq.heappush(
                                    current_heap if c_rpo > cur_rpo else next_heap,
                                    (c_rpo, consumer),
                                )
                                queued.add(consumer)
                if has_handler:
                    visitor.note_handler_state()
                if terminated:
                    break
            state = visitor.state
            out_states[block] = state
            if block in return_blocks and not terminated:
                return_block_origins[block] = (
                    visitor.origins.copy(),
                    visitor.type_of_origins.copy(),
                )
            last = block.last
            for target in successors[block]:
                if target is handler:
                    if visitor.handler_state is not None:
                        edge = visitor.handler_state
                        self.invalidate_globals(edge)
                    elif visitor.globals_invalidated or not self.invalidated:
                        edge = state
                    else:
                        edge = state.copy()
                        self.invalidate_globals(edge)
                    edge_rel = (
                        visitor.handler_relations
                        if visitor.handler_relations is not None
                        else visitor.relations.copy()
                    )
                elif terminated:
                    continue
                elif last is None:
                    edge = state
                    edge_rel = visitor.relations.copy()
                else:
                    visitor.edge_relations = None
                    edge_constrainer.target = target
                    edge_constrainer.state = state
                    edge = last.accept(edge_constrainer)
                    if edge is None:
                        continue
                    edge_rel = (
                        visitor.edge_relations
                        if visitor.edge_relations is not None
                        else visitor.relations.copy()
                    )
                key = block, target
                if not widening:
                    visited_edges.add(key)
                old_edge = edge_states.get(key)
                old_rel = edge_relations.get(key)
                edge_relations[key] = edge_rel
                if (
                    old_edge is None
                    or (old_edge is not edge and old_edge != edge)
                    or old_rel != edge_rel
                ):
                    edge_states[key] = edge
                    if target not in queued:
                        t_rpo = rpo[target]
                        heapq.heappush(
                            current_heap
                            if t_rpo > cur_rpo and key not in back_edges
                            else next_heap,
                            (t_rpo, target),
                        )
                        queued.add(target)
        return did_widen

    def constrain_edge(self, block, target, visitor, state, edge_states=None):
        """Narrow an integer local on a branch edge tested against a constant,
        or return ``None`` when a constant boolean test makes the edge dead."""
        last = block.last
        if last is not None:
            return last.accept(_ConstrainEdge(target, visitor, state, edge_states))
        return state


class _NarrowTruthiness(ValueTypeVisitor):
    """Narrow a value type along a direct truthiness branch edge."""

    def __init__(self, truth: bool):
        self.truth = truth

    def visit_ValueType(self, fact):
        if self.truth:
            return fact.narrow_none(False)
        return fact

    def visit_UnionType(self, fact):
        if fact.may_be_unbound:
            return fact
        return fact.map(
            lambda member: (
                BOTTOM
                if member.truth_value() is not None
                and member.truth_value() != self.truth
                else member.accept(self)
            )
        )

    def visit_NoneType(self, fact):
        return fact

    def visit_BooleanType(self, fact):
        if not fact.may_be_unbound:
            return BOOLEAN_TRUE if self.truth else BOOLEAN_FALSE
        return fact

    def visit_IntegerType(self, fact):
        if not fact.may_be_unbound:
            if (
                not self.truth
                and fact.minimum is not None
                and fact.minimum <= 0
                and (fact.maximum is None or fact.maximum >= 0)
            ):
                return SMALL_INTS[0]
            if self.truth:
                if fact.minimum == 0 and (fact.maximum is None or fact.maximum > 0):
                    return fact.narrow(ast.ComparisonOperator.GREATER, 0, True)
                if fact.maximum == 0 and (fact.minimum is None or fact.minimum < 0):
                    return fact.narrow(ast.ComparisonOperator.LESS, 0, True)
        return fact


_TRUE_NARROWING = _NarrowTruthiness(True)
_FALSE_NARROWING = _NarrowTruthiness(False)


class _ConstrainEdge(NodeVisitor):
    """Only a two-way branch narrows its edges."""

    def __init__(self, target, visitor: _Propagation, state, edge_states=None):
        self.target = target
        self.visitor = visitor
        self.state = state
        self.edge_states = edge_states
        self.cmp_constrainer: _ConstrainComparison | None = None

    def visit_Node(self, node):
        return self.state

    def visit_If(self, node):
        if node.consequent is node.alternative:
            return self.state
        truth = self.target is node.consequent
        test_type = self.visitor.type_of(node.test)
        if test_type is BOTTOM:
            return None
        test_truth = test_type.truth_value()
        if (
            (truth and test_truth is False) or (not truth and test_truth is True)
        ) and not (
            node.block in self.visitor.walk.headers
            and self.visitor.origins.get(node.test) in self.visitor.walk.volatile_keys
        ):
            return None
        cmp_c = self.cmp_constrainer
        if cmp_c is not None:
            cmp_c.state = self.state
            cmp_c.truth = truth
        else:
            cmp_c = _ConstrainComparison(self.visitor, self.state, truth)
        narrowed_edge = node.test.use().accept(cmp_c)
        if narrowed_edge is _DEAD_EDGE:
            return None
        if narrowed_edge is not None:
            return narrowed_edge
        if not self.visitor.origins:
            return self.state
        local = self.visitor.origins.get(node.test)
        if local is not None and local in self.visitor.state:
            walk = self.visitor.walk
            nodes = walk.block_nodes.get(node.block, ())
            test_ref = node.test.use()
            if (
                self.edge_states is not None
                and len(nodes) == 2
                and nodes[0] is test_ref
                and nodes[1] is node
                and local not in walk.indirect
                and node.block is not walk.entry
                and node.block not in walk.headers
            ):
                pending_edges = [
                    (pred, node.block)
                    for pred in walk.predecessors.get(node.block, ())
                    if (pred, node.block) in self.edge_states
                ]
                seen_edges = set(pending_edges)
                feasible = []
                filtered = False
                while pending_edges:
                    u, v = pending_edges.pop()
                    if (
                        len(walk.block_nodes.get(u, ())) == 1
                        and u is not walk.entry
                        and u not in walk.headers
                    ):
                        in_edges = [
                            (gp, u)
                            for gp in walk.predecessors.get(u, ())
                            if (gp, u) in self.edge_states
                        ]
                        if in_edges:
                            for edge_key in in_edges:
                                if edge_key not in seen_edges:
                                    seen_edges.add(edge_key)
                                    pending_edges.append(edge_key)
                            continue
                    pred_edge = self.edge_states[u, v]
                    pred_fact = pred_edge.get(local)
                    pred_truth = (
                        pred_fact.type.truth_value() if pred_fact is not None else None
                    )
                    if (truth and pred_truth is False) or (
                        not truth and pred_truth is True
                    ):
                        filtered = True
                    else:
                        feasible.append(pred_edge)
                if filtered:
                    if not feasible:
                        return None
                    edge = feasible[0].copy()
                    for pred_edge in feasible[1:]:
                        edge, _ = join_states(edge, pred_edge)
                    if test_ref.final:
                        edge[local] = UNBOUND_FACT
                    else:
                        current = edge[local]
                        narrowed = current.type.accept(
                            _TRUE_NARROWING if truth else _FALSE_NARROWING
                        )
                        if narrowed is BOTTOM:
                            return None
                        if narrowed != current.type:
                            edge[local] = self.visitor.narrowed_fact(current, narrowed)
                    return edge
            current = self.visitor.state[local]
            narrowed = current.type.accept(
                _TRUE_NARROWING if truth else _FALSE_NARROWING
            )
            if narrowed is BOTTOM:
                return None
            if narrowed != current.type:
                edge = self.state.copy()
                edge[local] = self.visitor.narrowed_fact(current, narrowed)
                return edge
        return self.state


class _PrecedingLeafSet(NodeVisitor):
    def __init__(self, leaf):
        self.leaf = leaf

    def visit_Node(self, node):
        return None

    def visit_Set(self, node: Set):
        if (
            node.var is self.leaf
            and node.value is not None
            and len(node.value.uses) == 1
        ):
            return node.value.uses[0]
        return None


class _ConstrainComparison(NodeVisitor):
    """A known comparison or predicate call on a branch edge narrows its operands."""

    def __init__(self, visitor: _Propagation, state, truth: bool):
        self.visitor = visitor
        self.state = state
        self.truth = truth

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.prev is not None and node.prev.use is not None:
            producer = node.prev.use.accept(_PrecedingLeafSet(node.leaf))
            if producer is not None and producer.block is node.block:
                return producer.accept(self)
        return None

    def visit_Combination(self, node):
        if node.kind is not CallKind.KNOWN:
            target = self.visitor.resolve_call_target(node)
            if target is not None:
                edge = self.state.copy()
                if target.constrain_call(node, self.visitor, edge, self.truth):
                    return _DEAD_EDGE if edge.get(_DEAD_EDGE) else edge
            return None
        info = node.fun_info
        if info is not None and info.constrain_branch is not None:
            edge = self.state.copy()
            if info.constrain_branch(self.visitor, node, edge, self.truth):
                return _DEAD_EDGE if edge.get(_DEAD_EDGE) else edge
        return None


def _successors(block, handler):
    """Normal successors inside the component plus the error destination."""
    for successor in block.succ:
        if successor.start is not None:
            yield successor
    if handler is not None:
        yield handler


def _derive_builtin_len(
    arg_types: tuple[ValueType, ...],
    keywords: tuple[str | None, ...] = (),
    *,
    receiver_type: ValueType | None = None,
) -> ValueType:
    if len(arg_types) == 1 and not keywords:
        if arg_types[0] is BOTTOM:
            return BOTTOM
        exact = arg_types[0].exact_length()
        if exact is not None:
            return IntegerType(exact, exact)
    return IntegerType(0, (1 << 60) - 1)


BOOTSTRAP_CLASS_TARGETS: dict[str, ClassConstructorTarget] = {
    "int": ClassConstructorTarget(base_type=INTEGER, cls_type=CLASS_INT_TYPE),
    "bool": ClassConstructorTarget(base_type=BOOLEAN, cls_type=CLASS_BOOL_TYPE),
    "float": ClassConstructorTarget(base_type=FLOAT, cls_type=CLASS_FLOAT_TYPE),
    "str": ClassConstructorTarget(base_type=TEXT_TYPE, cls_type=CLASS_STR_TYPE),
    "TextStorage": ClassConstructorTarget(base_type=TEXT_TYPE, cls_type=CLASS_STR_TYPE),
    "bytes": ClassConstructorTarget(base_type=BYTES_TYPE, cls_type=CLASS_BYTES_TYPE),
    "BytesStorage": ClassConstructorTarget(
        base_type=BYTES_TYPE, cls_type=CLASS_BYTES_TYPE
    ),
    "bytearray": ClassConstructorTarget(
        base_type=BYTEARRAY_TYPE, cls_type=CLASS_BYTEARRAY_TYPE
    ),
    "tuple": ClassConstructorTarget(base_type=TupleType(), cls_type=CLASS_TUPLE_TYPE),
    "TupleStorage": ClassConstructorTarget(
        base_type=TupleType(), cls_type=CLASS_TUPLE_TYPE
    ),
    "list": ClassConstructorTarget(base_type=LIST_TYPE, cls_type=CLASS_LIST_TYPE),
    "ListStorage": ClassConstructorTarget(
        base_type=LIST_TYPE, cls_type=CLASS_LIST_TYPE
    ),
    "dict": ClassConstructorTarget(base_type=DICT_TYPE, cls_type=CLASS_DICT_TYPE),
    "set": ClassConstructorTarget(base_type=SET_TYPE, cls_type=CLASS_SET_TYPE),
    "frozenset": ClassConstructorTarget(
        base_type=FROZENSET_TYPE, cls_type=CLASS_FROZENSET_TYPE
    ),
    "object": ClassConstructorTarget(base_type=UNKNOWN, cls_type=CLASS_OBJECT_TYPE),
    "type": ClassConstructorTarget(
        base_type=ANY_CLASS_TYPE,
        cls_type=CLASS_TYPE_TYPE,
        records_type_origin=True,
    ),
}


def _derive_builtin_min(
    arg_types: tuple[ValueType, ...],
    keywords: tuple[str | None, ...] = (),
    *,
    receiver_type: ValueType | None = None,
) -> ValueType:
    if len(arg_types) == 2 and not keywords:
        return fndb.derive_min2_types(arg_types[0], arg_types[1])
    return UNKNOWN


def _derive_builtin_max(
    arg_types: tuple[ValueType, ...],
    keywords: tuple[str | None, ...] = (),
    *,
    receiver_type: ValueType | None = None,
) -> ValueType:
    if len(arg_types) == 2 and not keywords:
        return fndb.derive_max2_types(arg_types[0], arg_types[1])
    return UNKNOWN


def _derive_builtin_abs(
    arg_types: tuple[ValueType, ...],
    keywords: tuple[str | None, ...] = (),
    *,
    receiver_type: ValueType | None = None,
) -> ValueType:
    if len(arg_types) == 1 and not keywords:
        return fndb.derive_abs_type(arg_types[0])
    return UNKNOWN


def _derive_builtin_enumerate(
    arg_types: tuple[ValueType, ...],
    keywords: tuple[str | None, ...] = (),
    receiver_type: ValueType | None = None,
) -> ValueType:
    if 1 <= len(arg_types) <= 2 and not keywords:
        if len(arg_types) == 2 and not arg_types[1].fits(PYTHON_INTEGER):
            return UNKNOWN
        iter_t = arg_types[0].flow_iterate_type()
        elem_t = iter_t.next_item_type() if iter_t.fits_iterator() else UNKNOWN
        idx_t = IntegerType(0, (1 << 60) - 1) if len(arg_types) == 1 else INTEGER
        return IteratorType(TupleType(2, (idx_t, elem_t)))
    return IteratorType(TupleType(2))


def _derive_builtin_zip(
    arg_types: tuple[ValueType, ...],
    keywords: tuple[str | None, ...] = (),
    receiver_type: ValueType | None = None,
) -> ValueType:
    if not arg_types:
        return IteratorType(TupleType(0, ()))
    elems = []
    for arg in arg_types:
        iter_t = arg.flow_iterate_type()
        elems.append(iter_t.next_item_type() if iter_t.fits_iterator() else UNKNOWN)
    return IteratorType(
        TupleType(
            len(arg_types),
            None if all(e is UNKNOWN for e in elems) else tuple(elems),
        )
    )


def _derive_builtin_iter(
    arg_types: tuple[ValueType, ...],
    keywords: tuple[str | None, ...] = (),
    receiver_type: ValueType | None = None,
) -> ValueType:
    if len(arg_types) == 1 and not keywords:
        return arg_types[0].flow_iterate_type()
    return ITERATOR_TYPE


def _derive_builtin_next(
    arg_types: tuple[ValueType, ...],
    keywords: tuple[str | None, ...] = (),
    receiver_type: ValueType | None = None,
) -> ValueType:
    if len(arg_types) == 1 and not keywords:
        return arg_types[0].next_item_type()
    if len(arg_types) == 2 and not keywords:
        item = arg_types[0].next_item_type()
        return UNKNOWN if item is UNKNOWN else item.join(arg_types[1])
    return UNKNOWN


def _derive_builtin_reversed(
    arg_types: tuple[ValueType, ...],
    keywords: tuple[str | None, ...] = (),
    receiver_type: ValueType | None = None,
) -> ValueType:
    if len(arg_types) == 1 and not keywords:
        iter_t = arg_types[0].flow_iterate_type()
        if iter_t.fits_iterator():
            return iter_t
    return ITERATOR_TYPE


def _derive_builtin_sorted(
    arg_types: tuple[ValueType, ...],
    keywords: tuple[str | None, ...] = (),
    receiver_type: ValueType | None = None,
) -> ValueType:
    if len(arg_types) >= 1:
        iter_t = arg_types[0].flow_iterate_type()
        if iter_t.fits_iterator() and iter_t.element is not BOTTOM:
            return ListType(iter_t.element)
    return LIST_TYPE


_TEXT_STREAM_TYPE = IteratorType(TEXT_TYPE, is_stream=True)
_BINARY_STREAM_TYPE = IteratorType(BYTES_TYPE, is_stream=True)


def _derive_builtin_open(
    arg_types: tuple[ValueType, ...],
    keywords: tuple[str | None, ...] = (),
    receiver_type: ValueType | None = None,
) -> ValueType:
    if not arg_types or None in keywords:
        return UNKNOWN
    pos_count = len(arg_types) - len(keywords)
    if pos_count < 1:
        return UNKNOWN
    if "mode" in keywords:
        if pos_count >= 2:
            return UNKNOWN
        mode_type = arg_types[pos_count + keywords.index("mode")]
    elif pos_count >= 2:
        mode_type = arg_types[1]
    else:
        return _TEXT_STREAM_TYPE
    mode_lit = mode_type.exact_text()
    if mode_lit is None or not any(ch in mode_lit for ch in "rwxa"):
        return UNKNOWN
    if "b" not in mode_lit:
        return _TEXT_STREAM_TYPE
    if "t" not in mode_lit:
        return _BINARY_STREAM_TYPE
    return UNKNOWN


BOOTSTRAP_BUILTIN_TARGETS: dict[str, ClassConstructorTarget | ForeignCallTarget] = {
    **BOOTSTRAP_CLASS_TARGETS,
    "open": ForeignCallTarget(
        return_type=UNKNOWN,
        derive_fn=_derive_builtin_open,
    ),
    "len": ForeignCallTarget(
        return_type=IntegerType(0, (1 << 60) - 1),
        derive_fn=_derive_builtin_len,
    ),
    "min": ForeignCallTarget(
        return_type=UNKNOWN,
        derive_fn=_derive_builtin_min,
    ),
    "max": ForeignCallTarget(
        return_type=UNKNOWN,
        derive_fn=_derive_builtin_max,
    ),
    "abs": ForeignCallTarget(
        return_type=UNKNOWN,
        derive_fn=_derive_builtin_abs,
    ),
    "enumerate": ForeignCallTarget(
        return_type=IteratorType(TupleType(2)),
        derive_fn=_derive_builtin_enumerate,
    ),
    "zip": ForeignCallTarget(
        return_type=ITERATOR_TYPE,
        derive_fn=_derive_builtin_zip,
    ),
    "iter": ForeignCallTarget(
        return_type=ITERATOR_TYPE,
        derive_fn=_derive_builtin_iter,
    ),
    "next": ForeignCallTarget(
        return_type=UNKNOWN,
        derive_fn=_derive_builtin_next,
    ),
    "reversed": ForeignCallTarget(
        return_type=ITERATOR_TYPE,
        derive_fn=_derive_builtin_reversed,
    ),
    "sorted": ForeignCallTarget(
        return_type=LIST_TYPE,
        derive_fn=_derive_builtin_sorted,
    ),
    "range": ForeignCallTarget(return_type=IteratorType(INTEGER)),
    "ord": ForeignCallTarget(return_type=IntegerType(0, 0x10FFFF)),
    "chr": ForeignCallTarget(return_type=TEXT_TYPE),
    "input": ForeignCallTarget(return_type=TEXT_TYPE),
}


def propagate(
    lambda_: Lambda,
    global_targets,
    parameters,
    resolve_call,
    annotations,
    *,
    graph: LambdaGraph | None = None,
    proven_globals: frozenset[str] = frozenset(),
    rebound_globals: frozenset[str] = frozenset(),
    global_constants: Mapping[str, ValueType] | None = None,
    imported_returns: Mapping[str, ValueType] | None = None,
    imported_targets: (Mapping[str, object] | None) = None,
    module_returns: Mapping[tuple[str, str], ValueType] | None = None,
    module_targets: Mapping[tuple[str, str], object] | None = None,
    storage_methods: Mapping[tuple[str, str], ValueType] | None = None,
    builtin_return_types: Mapping[str, ValueType] | None = None,
    builtin_targets: (Mapping[str, object] | None) = None,
    class_facts: Mapping | None = None,
    recursive_targets: frozenset[Lambda] = frozenset(),
    unique_targets: frozenset[Lambda] = frozenset(),
    is_specialized_context: bool = False,
    for_return_derivation: bool = False,
) -> Body:
    """Propagate facts through ``lambda_`` from the given parameter types.

    ``resolve_call(callee_fact, argument_types, location, keywords)`` supplies
    the result type of a call; local-call analysis provides it so that proven
    local calls receive their callee's published return type. Annotation
    contradictions are recorded in ``annotations``, the module's
    ``AnnotationLedger``.
    """
    return _Walk(
        lambda_,
        global_targets,
        parameters,
        resolve_call,
        annotations,
        graph=graph,
        proven_globals=proven_globals,
        rebound_globals=rebound_globals,
        global_constants=global_constants,
        imported_returns=imported_returns,
        imported_targets=imported_targets,
        module_returns=module_returns,
        module_targets=module_targets,
        storage_methods=storage_methods,
        builtin_return_types=builtin_return_types,
        builtin_targets=builtin_targets,
        class_facts=class_facts,
        recursive_targets=recursive_targets,
        unique_targets=unique_targets,
        is_specialized_context=is_specialized_context,
        for_return_derivation=for_return_derivation,
    ).run()


__all__ = [
    "BOOTSTRAP_BUILTIN_TARGETS",
    "NO_FACT",
    "Body",
    "CallSite",
    "Fact",
    "LambdaGraph",
    "build_lambda_graph",
    "constant_type",
    "join_fact",
    "propagate",
    "widen_fact",
]
