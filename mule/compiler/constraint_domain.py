"""Lattice facts, relational state, call graph, and bootstrap builtin targets for constraint propagation."""

from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass, field

from mule import ast

from . import fndb
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
    IntegerType,
    IteratorType,
    ListType,
    TextType,
    TupleType,
    ValueType,
    ValueTypeVisitor,
)
from .fndb import INTERNED_TEXTS, SMALL_INTS, SMALL_TUPLES, constant_type
from .ir1util import block_exception_target
from .node import (
    CallKind,
    Cast,
    ClassConstructorTarget,
    Combination,
    ForeignCallTarget,
    Lambda,
    LeafVisitor,
    NodeVisitor,
    Ref,
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
