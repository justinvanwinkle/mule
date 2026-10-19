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

from collections import defaultdict, deque
from dataclasses import dataclass

from mule import ast

from .ctype import (
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    BYTES_TYPE,
    DICT_TYPE,
    EMPTY_DICT_TYPE,
    EMPTY_LIST_TYPE,
    FLOAT,
    FUNCTION_TYPE,
    IMMORTAL,
    IMMORTAL_UNBOUND,
    INTEGER,
    ITERATOR_TYPE,
    LIST_TYPE,
    NONE_TYPE,
    PYTHON_INTEGER,
    REAL,
    TEXT_TYPE,
    UNBOUND,
    UNKNOWN,
    UNKNOWN_UNBOUND,
    IntegerType,
    IteratorType,
    ListType,
    TextType,
    TupleType,
    ValueType,
    ValueTypeVisitor,
)
from .ir1util import block_exception_target, made_function, made_lambda
from .node import (
    CallKind,
    Cast,
    Combination,
    FunctionalKind,
    If,
    Lambda,
    LeafVisitor,
    NodeVisitor,
    Ref,
    Set,
    linked,
)


@dataclass(frozen=True, slots=True)
class Fact:
    type: ValueType = UNKNOWN
    target: Lambda | None = None
    proven: bool = False


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
        IMMORTAL_FACT,
        IMMORTAL_UNBOUND_FACT,
        _EMPTY_ITERATOR_FACT,
        _EMPTY_DICT_ITEMS_ITERATOR_FACT,
    )
}


def _fact(value_type: ValueType) -> Fact:
    canonical = _CANONICAL_FACTS.get(id(value_type))
    return Fact(value_type) if canonical is None else canonical


@dataclass(frozen=True, slots=True)
class CallSite:
    target: Lambda | None
    arguments: tuple[ValueType, ...]
    proven: bool = False


@dataclass(slots=True)
class Body:
    """What one propagation over a lambda established."""

    facts: dict[object, Fact]
    return_type: ValueType
    calls: tuple[CallSite, ...]

    def types(self) -> dict[object, ValueType]:
        return {
            node: fact.type.normalized_bottom() for node, fact in self.facts.items()
        }

    def targets(self) -> dict[object, Lambda]:
        return {
            node: fact.target
            for node, fact in self.facts.items()
            if fact.target is not None
        }


def join_fact(left: Fact, right: Fact) -> Fact:
    # Joins mostly meet the same fact on both sides. Returning the existing
    # object keeps later joins and state comparisons on the identity path.
    if left is right or left == right:
        return left
    joined = left.type.merge_information(right.type, left, right, _combine)
    return left if joined == left else joined


def _combine(a: Fact, b: Fact) -> Fact:
    target = a.target if a.target is b.target else None
    joined_type = a.type.join(b.type)
    if target is None:
        return _fact(joined_type)
    return Fact(joined_type, target, a.proven and b.proven)


def join_states(left: dict, right: dict) -> tuple[dict, bool]:
    """Join two variable states over the same keys. The flag reports whether
    the result differs from ``left``."""
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


def widen_fact(old: Fact, new: Fact) -> Fact:
    widened = old.type.widen(new.type)
    if widened is new.type:
        return new
    if new.target is None:
        return _fact(widened)
    return Fact(widened, new.target, new.proven)


_SMALL_INTS = {i: IntegerType(i, i) for i in range(-16, 257)}
_SMALL_INT_FACTS = {i: Fact(vt) for i, vt in _SMALL_INTS.items()}
_SMALL_TUPLES = tuple(TupleType(i) for i in range(16))
_SMALL_TUPLE_FACTS = tuple(Fact(vt) for vt in _SMALL_TUPLES)
_UNBOUNDED_TUPLE = TupleType(None)
_UNBOUNDED_TUPLE_FACT = Fact(_UNBOUNDED_TUPLE)
_INTERNED_TEXTS = {
    "": TextType(""),
    **{chr(code): TextType(chr(code)) for code in range(256)},
}
_INTERNED_TEXT_FACTS = {s: Fact(vt) for s, vt in _INTERNED_TEXTS.items()}
_CANONICAL_FACTS.update({id(fact.type): fact for fact in _INTERNED_TEXT_FACTS.values()})


def _tuple_fact(
    arity: int | None, elements: tuple[ValueType, ...] | None = None
) -> Fact:
    if arity is None:
        return _UNBOUNDED_TUPLE_FACT
    if elements is not None and not all(el is UNKNOWN for el in elements):
        return Fact(TupleType(arity, elements))
    if 0 <= arity < 16:
        return _SMALL_TUPLE_FACTS[arity]
    return Fact(TupleType(arity))


class _LiteralFacts(ast.Visitor):
    def visit_Node(self, node):
        return UNKNOWN

    def visit_IntegerLiteral(self, node):
        value = node.value
        cached = _SMALL_INTS.get(value)
        return IntegerType(value, value) if cached is None else cached

    def visit_BooleanLiteral(self, node):
        return BOOLEAN_TRUE if node.value else BOOLEAN_FALSE

    def visit_FloatLiteral(self, node):
        return FLOAT

    def visit_NoneLiteral(self, node):
        return NONE_TYPE

    def visit_TextLiteral(self, node):
        return _INTERNED_TEXTS.get(node.value, TEXT_TYPE)

    def visit_BytesLiteral(self, node):
        return BYTES_TYPE

    def visit_Tuple(self, node):
        arity = len(node.elements)
        if 0 < arity <= 64:
            elements = tuple(el.accept(self) for el in node.elements)
            if not all(el is UNKNOWN for el in elements):
                return TupleType(arity, elements)
        return _SMALL_TUPLES[arity] if arity < 16 else TupleType(arity)


_LITERAL_FACTS = _LiteralFacts()


def constant_type(literal) -> ValueType:
    return literal.accept(_LITERAL_FACTS)


_GLOBAL = object()


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


class _Propagation(NodeVisitor, LeafVisitor):
    """Processes one block's nodes against the current variable state."""

    def __init__(self, walk, state, origins):
        self.walk = walk
        self.state = state
        self.origins = origins
        self.handler_state: dict[object, Fact] | None = None
        # Keys assigned since the handler state last absorbed the state.
        self.changed = set()
        self._current_ref: Ref | None = None

    def reset(self, state: dict[object, Fact], has_handler: bool) -> None:
        self.state = state
        self.origins.clear()
        self.changed.clear()
        self.handler_state = state.copy() if has_handler else None

    def assign(self, key, fact):
        self.state[key] = fact
        self.changed.add(key)

    def fact_of(self, lvar) -> Fact:
        return self.walk.facts.get(lvar.use(), NO_FACT)

    def type_of(self, lvar) -> ValueType:
        return self.fact_of(lvar).type

    def invalidate(self):
        self.walk.invalidate_globals(self.state)
        self.changed.update(self.walk.volatile_keys)

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
        return _fact(constant_type(leaf.value))

    def visit_GlobalVar(self, leaf):
        fact = self.state[(_GLOBAL, leaf.name)]
        ref = self._current_ref
        if (
            fact.type is UNKNOWN
            and ref is not None
            and ref.derived_type is not None
            and ref.derived_type is not UNKNOWN
            and self.walk.lambda_.policy.safety < 3
            and ref.lexenv.policy.safety < 3
        ):
            target = fact.target or self.walk.global_targets.get(leaf.name)
            if target is not None:
                return Fact(ref.derived_type, target, fact.proven)
            return _fact(ref.derived_type)
        if fact.target is None:
            target = self.walk.global_targets.get(leaf.name)
            if target is not None:
                fact = Fact(fact.type, target)
        return fact

    def visit_LambdaVar(self, leaf):
        state = self.state
        if leaf not in state:
            # A free variable of an enclosing function: any value.
            return UNKNOWN_FACT
        fact = state[leaf]
        ref = linked(self._current_ref, leaf, "reference")
        if ref.final:
            self.forget_origin(leaf)
            self.assign(leaf, UNBOUND_FACT)
        elif ref.lvar is not None:
            self.origins[ref.lvar] = leaf
        return fact

    def visit_Lambda(self, leaf):
        return Fact(FUNCTION_TYPE, leaf, True)

    def visit_Set(self, node: Set):
        key = _key(node.var)
        if key not in self.state:
            return None
        old = self.state[key]
        if node.value is None:
            self.assign(key, UNBOUND_FACT)
        else:
            fact = self.fact_of(node.value)
            contract = self.walk.local_contracts.get(key)
            if contract is not None and not contract.contradicts(fact.type):
                refined = contract.refine(fact.type)
                if refined is not fact.type:
                    fact = (
                        _fact(refined)
                        if fact.target is None
                        else Fact(refined, fact.target, fact.proven)
                    )
            self.assign(key, fact)
        # Origins are locals, so this changes nothing for a global.
        self.forget_origin(node.var)
        return old

    def visit_Cast(self, node: Cast):
        value = self.type_of(node.value)
        if self.walk.annotations.check(
            node.contract, value, node.location, node.declaration
        ):
            return _fact(value)
        return _fact(node.contract.refine(value))

    def visit_Combination(self, node: Combination):
        if node.kind in (CallKind.FULL, CallKind.GUARDED):
            info = linked(node.info, node, "info")
            arg_types = tuple(self.type_of(arg) for arg in node.args)
            resolved = self.walk.resolve_call(
                self.fact_of(node.fun),
                arg_types,
                info.location,
                info.keywords,
            )
            if resolved is UNKNOWN and not info.keywords:
                resolved = self.walk.builtin_return_type(node.fun, arg_types)
            fact = _fact(resolved)
            self.invalidate()
            return fact
        return self.derive_known(node)

    def derive_known(self, node: Combination):
        info = node.fun_info
        assert info is not None
        name = info.name
        args = node.args
        if name in _BINARY_OPERATORS:
            left, right = self.type_of(args[0]), self.type_of(args[1])
            fact = _fact(left.binary(_BINARY_OPERATORS[name], right))
            if left.may_run_user_code or right.may_run_user_code:
                self.invalidate()
            return fact
        if name in _UNARY_OPERATORS:
            operand = self.type_of(args[0])
            fact = _fact(operand.unary(_UNARY_OPERATORS[name]))
            if operand.may_run_user_code:
                self.invalidate()
            return fact
        if name in _COMPARISON_OPERATORS:
            operator = _COMPARISON_OPERATORS[name]
            left, right = self.type_of(args[0]), self.type_of(args[1])
            fact = _fact(left.comparison(operator, right))
            if operator not in (
                ast.ComparisonOperator.IS,
                ast.ComparisonOperator.IS_NOT,
            ) and (left.may_run_user_code or right.may_run_user_code):
                self.invalidate()
            return fact
        if name == "%build_tuple":
            arity = len(args)
            elements = (
                tuple(self.type_of(arg) for arg in args) if 0 < arity <= 64 else None
            )
            return _tuple_fact(arity, elements)
        if name == "%build_list":
            if not args:
                return EMPTY_LIST_FACT
            list_type = EMPTY_LIST_TYPE
            for arg in args:
                list_type = list_type.with_element(self.type_of(arg))
            return _fact(list_type)
        if name == "%build_dict":
            if not args:
                return EMPTY_DICT_FACT
            if len(args) == 1:
                dict_type, may_call = self.type_of(args[0]).accept(_BUILD_DICT_TYPE)
                if may_call:
                    self.invalidate()
                return _fact(dict_type)
            dict_type = EMPTY_DICT_TYPE
            for key_arg, val_arg in zip(args[0::2], args[1::2], strict=True):
                dict_type = dict_type.with_entry(
                    self.type_of(key_arg), self.type_of(val_arg)
                )
            if any(self.type_of(key_arg).may_run_user_code for key_arg in args[0::2]):
                self.invalidate()
            return _fact(dict_type)
        if name == "%range_prepare":
            if self.is_proven_fixnum_range(node):
                return BOOLEAN_TRUE_FACT
            return BOOLEAN_FACT
        if name == "%unpack":
            self.invalidate()
            arity = self.type_of(args[1]).exact_integer()
            if arity is not None and arity >= 0:
                seq_type = self.type_of(args[0])
                if seq_type is BOTTOM:
                    return NO_FACT
                unpacked = seq_type.unpacked_tuple_type(arity)
                return _tuple_fact(unpacked.arity, unpacked.elements)
            return _UNBOUNDED_TUPLE_FACT
        if name == "%unpack_ref":
            seq_type = self.type_of(args[0])
            if seq_type is BOTTOM:
                return NO_FACT
            return _fact(seq_type.getitem_type(self.type_of(args[1])))
        if name == "%make_function":
            lambda_ = made_lambda(node)
            definition = linked(node.info, node, "info")
            parameters = lambda_.signature.default_parameters
            for value, contract, index in zip(
                args[1:], definition.default_contracts, parameters, strict=True
            ):
                if contract is not None:
                    self.walk.annotations.check(
                        contract,
                        self.type_of(value),
                        definition.location,
                        ("local", lambda_, lambda_.vars[index].name),
                    )
            return Fact(FUNCTION_TYPE, lambda_, True)
        if name == "%call_resolved":
            resolved = (
                args[0].uses[0].accept(_RESOLVED_METHOD)
                if len(args[0].uses) == 1
                else None
            )
            if resolved is not None:
                receiver_lvar, method_name = resolved
                receiver_type = self.type_of(receiver_lvar)
                has_keywords = bool(linked(node.info, node, "info").keywords)
                arity = len(args) - 1
                if method_name == "append" and arity == 1 and not has_keywords:
                    elem_type = self.type_of(args[1])
                    recv_var = self.origins.get(receiver_lvar)
                    if recv_var is not None and not recv_var.indirect:
                        self.assign(
                            recv_var, _fact(receiver_type.with_element(elem_type))
                        )
                    if (
                        not receiver_type.may_be_unbound
                        and receiver_type.fits(LIST_TYPE)
                        and not elem_type.may_be_unbound
                    ):
                        return NONE_FACT
                if method_name == "get" and arity in (1, 2) and not has_keywords:
                    default_type = self.type_of(args[2]) if arity == 2 else NONE_TYPE
                    res_type = receiver_type.dict_get_type(default_type)
                    if res_type is not UNKNOWN:
                        if self.type_of(args[1]).may_run_user_code:
                            self.invalidate()
                        return _fact(res_type)
                if method_name == "items" and arity == 0 and not has_keywords:
                    if receiver_type == EMPTY_DICT_TYPE:
                        return _EMPTY_DICT_ITEMS_ITERATOR_FACT
                    iter_type = receiver_type.dict_items_iterator_type()
                    if iter_type is not UNKNOWN:
                        return _fact(iter_type)
                if method_name == "keys" and arity == 0 and not has_keywords:
                    if receiver_type == EMPTY_DICT_TYPE:
                        return _EMPTY_ITERATOR_FACT
                    iter_type = receiver_type.dict_keys_iterator_type()
                    if iter_type is not UNKNOWN:
                        return _fact(iter_type)
                if method_name == "values" and arity == 0 and not has_keywords:
                    if receiver_type == EMPTY_DICT_TYPE:
                        return _EMPTY_ITERATOR_FACT
                    iter_type = receiver_type.dict_values_iterator_type()
                    if iter_type is not UNKNOWN:
                        return _fact(iter_type)
                result_type = receiver_type.method_return_type(
                    method_name, arity, has_keywords
                )
                self.invalidate()
                return _fact(result_type)
            self.invalidate()
            return UNKNOWN_FACT
        if name in ("%import", "%catch_exception"):
            self.invalidate()
            return UNKNOWN_FACT
        if name == "%format":
            self.invalidate()
            return TEXT_FACT
        if name == "%getitem":
            receiver_type = self.type_of(args[0])
            index_type = self.type_of(args[1])
            if receiver_type.may_run_user_code or index_type.may_run_user_code:
                self.invalidate()
            return _fact(receiver_type.getitem_type(index_type))
        if name == "%setitem" and len(args) == 3:
            recv_var = self.origins.get(args[0])
            recv_type = self.type_of(args[0])
            key_type = self.type_of(args[1])
            val_type = self.type_of(args[2])
            if not recv_type.fits_list(LIST_TYPE) or key_type.may_run_user_code:
                self.invalidate()
            if recv_var is not None and not recv_var.indirect:
                if recv_type.fits_list(LIST_TYPE):
                    updated = (
                        recv_type.with_element(val_type)
                        if key_type.fits(PYTHON_INTEGER)
                        else recv_type.with_element(
                            val_type.iterate_type().next_item_type()
                        )
                    )
                else:
                    updated = recv_type.with_entry(key_type, val_type)
                self.assign(recv_var, _fact(updated))
            return _fact(NONE_TYPE)
        if name == "%iterate" and len(args) == 1:
            operand_type = self.type_of(args[0])
            if operand_type.may_run_user_code:
                self.invalidate()
            if operand_type == EMPTY_LIST_TYPE or operand_type == EMPTY_DICT_TYPE:
                return _EMPTY_ITERATOR_FACT
            return _fact(operand_type.iterate_type())
        if name == "%next_item":
            iterator_var = _loop_iterator_var(node.block)
            if iterator_var is not None and iterator_var in self.state:
                iter_type = self.state[iterator_var].type
                if iter_type == _EMPTY_ITERATOR_TYPE:
                    return NO_FACT
                item_type = iter_type.next_item_type()
                if iter_type.may_run_user_code:
                    self.invalidate()
                if item_type is not UNKNOWN:
                    return _fact(item_type)
            self.invalidate()
            return UNKNOWN_FACT
        if name == "%getslice":
            receiver_type = self.type_of(args[0])
            if receiver_type.may_run_user_code or any(
                self.type_of(arg).may_run_user_code for arg in args[1:]
            ):
                self.invalidate()
            if receiver_type.fits_text(TEXT_TYPE):
                return TEXT_FACT
            if receiver_type is BYTES_TYPE:
                return BYTES_FACT
            if receiver_type.fits_list(LIST_TYPE):
                return _fact(receiver_type)
            return UNKNOWN_FACT
        if name == "_storage_append" and len(args) == 2:
            recv_var = self.origins.get(args[0])
            recv_type = self.type_of(args[0])
            elem_type = self.type_of(args[1])
            if (
                recv_var is not None
                and not recv_var.indirect
                and recv_type.fits_list(LIST_TYPE)
            ):
                self.assign(recv_var, _fact(recv_type.with_element(elem_type)))
        elif name == "_storage_write" and len(args) == 3:
            recv_var = self.origins.get(args[0])
            recv_type = self.type_of(args[0])
            elem_type = self.type_of(args[2])
            if (
                recv_var is not None
                and not recv_var.indirect
                and recv_type.fits_list(LIST_TYPE)
            ):
                self.assign(recv_var, _fact(recv_type.with_element(elem_type)))
        elif name == "_storage_read" and len(args) == 2:
            recv_type = self.type_of(args[0])
            idx_type = self.type_of(args[1])
            return _fact(recv_type.getitem_type(idx_type))
        elif (
            name
            in (
                "_text_length",
                "_text_codepoint",
                "_text_character",
                "_text_slice",
            )
            and args
        ):
            recv_var = self.origins.get(args[0])
            if (
                recv_var is not None
                and not recv_var.indirect
                and self.type_of(args[0]) is UNKNOWN
            ):
                self.assign(recv_var, TEXT_FACT)
        elif name in ("_bytes_length", "_bytes_byte", "_bytes_slice") and args:
            recv_var = self.origins.get(args[0])
            if (
                recv_var is not None
                and not recv_var.indirect
                and self.type_of(args[0]) is UNKNOWN
            ):
                self.assign(recv_var, BYTES_FACT)
        if info.derive_type is not None:
            value_type = info.derive_type(node)
            if value_type is not None:
                if info.may_call or info.unsafe:
                    self.invalidate()
                return _fact(value_type)
        if info.protocol is not None:
            self.invalidate()
            return UNKNOWN_FACT
        if name in (
            "%range_available",
            "%sequence_available",
            "%has_binding",
        ):
            return BOOLEAN_FACT
        if name == "%range_advance":
            state = linked(node.info, node, "info")
            return _fact(self.range_type(state))
        if info.result:
            return UNKNOWN_FACT
        return None

    def is_proven_fixnum_range(self, node: Combination) -> bool:
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
        state = linked(node.info, node, "info")
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
                step = arguments[2] if len(arguments) == 3 else _SMALL_INTS[1]
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
        if node.fun_info is not None and node.fun_info.name == "%resolve_method":
            return node.args[0], linked(node.info, node, "info").name
        return None


_RESOLVED_METHOD = _ResolvedMethod()


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


class _SequenceIteratorVar(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.fun_info is not None
            and node.fun_info.name == "%sequence_available"
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


class _GraphScan(NodeVisitor, LeafVisitor):
    """Collects global variable names and full/guarded call sites for a lambda."""

    def __init__(self, consumers):
        self.global_names: set[str] = set()
        self.call_nodes: list[tuple[object, Combination]] = []
        self.consumers = consumers
        self.block = None

    def visit_Ref(self, node):
        node.leaf.accept(self)

    def visit_Set(self, node):
        node.var.accept(self)

    def visit_GlobalVar(self, leaf):
        self.global_names.add(leaf.name)

    def visit_Combination(self, node):
        if node.kind in (CallKind.FULL, CallKind.GUARDED) and not node.info.keywords:
            self.call_nodes.append((self.block, node))
        if node.kind is CallKind.KNOWN and node.args:
            for arg in node.args:
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


_GLOBAL_CALLEE_NAME = _GlobalCalleeName()
_LENGTH_TYPE = IntegerType(0, (1 << 48) - 1)
_CODEPOINT_TYPE = IntegerType(0, 0x10FFFF)


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
    ):
        self.lambda_ = lambda_
        self.global_targets = global_targets
        self.resolve_call = resolve_call
        self.annotations = annotations
        self.rebound_globals = rebound_globals
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
        closure = lambda_.environment.closure if lambda_.environment is not None else ()
        for var in closure:
            vtype = var.optimistic_type
            resolved_type = (
                vtype if vtype is not None and vtype is not BOTTOM else UNKNOWN
            )
            target = (
                made_function(var.sets[0].value.uses[0])
                if resolved_type is FUNCTION_TYPE
                and len(var.sets) == 1
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
        invalidated_globals: dict[object, Fact] = {
            (_GLOBAL, name): (
                Fact(
                    FUNCTION_TYPE if name in proven_globals else UNKNOWN,
                    target,
                    name in proven_globals,
                )
                if (target := global_targets.get(name)) is not None
                else UNKNOWN_FACT
            )
            for name in self.global_names
        }
        self.invalidated = {
            **invalidated_globals,
            **{
                var: (
                    self.initial.get(var, UNKNOWN_FACT)
                    if var in closure
                    else UNKNOWN_FACT
                )
                for var in self.indirect
            },
        }
        self.initial.update(invalidated_globals)

    def builtin_return_type(
        self, fun_lvar, arg_types: tuple[ValueType, ...]
    ) -> ValueType:
        if (
            self.lambda_.kind is FunctionalKind.TOPLEVEL
            or self.lambda_.policy.safety >= 3
            or self.lambda_.name.startswith(
                (
                    "mule_sequences_",
                    "mule_format_",
                    "mule_arguments_",
                    "mule_native_class_",
                    "mule_print_",
                )
            )
        ):
            return UNKNOWN
        name = fun_lvar.use().accept(_GLOBAL_CALLEE_NAME)
        if name is None or name in self.rebound_globals or name in self.global_targets:
            return UNKNOWN
        arity = len(arg_types)
        if name == "len" and arity == 1:
            return _LENGTH_TYPE
        if name == "int" and arity <= 2:
            return INTEGER
        if name == "bool" and arity <= 1:
            return BOOLEAN
        if name == "float" and arity <= 1:
            return FLOAT
        if name in ("str", "repr", "ascii", "chr", "hex", "oct", "bin") and arity <= 1:
            return TEXT_TYPE
        if name == "ord" and arity == 1:
            return _CODEPOINT_TYPE
        if name == "list" and arity <= 1:
            if arity == 1:
                elem = arg_types[0].iterate_type().next_item_type()
                if elem is not UNKNOWN:
                    return ListType(elem)
            return LIST_TYPE
        if name == "iter" and arity == 1:
            it_type = arg_types[0].iterate_type()
            return ITERATOR_TYPE if it_type is UNKNOWN else it_type
        if name == "reversed" and arity == 1:
            it_type = arg_types[0].iterate_type()
            return ITERATOR_TYPE if it_type is UNKNOWN else it_type
        if name == "range" and 1 <= arity <= 3:
            if arity == 1 and arg_types[0].fits(PYTHON_INTEGER):
                stop = arg_types[0]
                if stop.maximum is not None and stop.maximum >= 1:
                    return IteratorType(IntegerType(0, stop.maximum - 1))
                return IteratorType(INTEGER)
            if all(arg.fits(PYTHON_INTEGER) for arg in arg_types):
                return IteratorType(INTEGER)
        if name == "next" and 1 <= arity <= 2:
            elem = arg_types[0].next_item_type()
            if arity == 2:
                return elem.join(arg_types[1])
            return elem
        if name == "tuple" and arity <= 1:
            return _UNBOUNDED_TUPLE
        if name == "abs" and arity == 1:
            arg = arg_types[0]
            if arg.fits(PYTHON_INTEGER):
                if arg.minimum is not None and arg.minimum >= 0:
                    return IntegerType(arg.minimum, arg.maximum)
                if arg.minimum is not None and arg.maximum is not None:
                    lo = (
                        0
                        if arg.minimum <= 0 <= arg.maximum
                        else min(abs(arg.minimum), abs(arg.maximum))
                    )
                    hi = max(abs(arg.minimum), abs(arg.maximum))
                    return IntegerType(lo, hi)
                return IntegerType(0, None)
            if arg is FLOAT:
                return FLOAT
        if name == "max" and arity >= 2:
            if all(arg.fits(PYTHON_INTEGER) for arg in arg_types):
                mins = [arg.minimum for arg in arg_types if arg.minimum is not None]
                lo = max(mins) if mins else None
                hi = (
                    max(arg.maximum for arg in arg_types)
                    if all(arg.maximum is not None for arg in arg_types)
                    else None
                )
                return IntegerType(lo, hi)
            if all(arg.fits_text(TEXT_TYPE) for arg in arg_types):
                return TEXT_TYPE
            if all(arg is FLOAT for arg in arg_types):
                return FLOAT
        if name == "min" and arity >= 2:
            if all(arg.fits(PYTHON_INTEGER) for arg in arg_types):
                lo = (
                    min(arg.minimum for arg in arg_types)
                    if all(arg.minimum is not None for arg in arg_types)
                    else None
                )
                maxs = [arg.maximum for arg in arg_types if arg.maximum is not None]
                hi = min(maxs) if maxs else None
                return IntegerType(lo, hi)
            if all(arg.fits_text(TEXT_TYPE) for arg in arg_types):
                return TEXT_TYPE
            if all(arg is FLOAT for arg in arg_types):
                return FLOAT
        return UNKNOWN

    def invalidate_globals(self, state):
        """Forget facts that user code may change."""
        if self.invalidated:
            state.update(self.invalidated)

    def run(self) -> Body:
        incoming, edge_states = {}, {}
        widened_keys = defaultdict(set)
        if self.propagate(incoming, edge_states, widened_keys, widening=True):
            self.propagate(incoming, edge_states, widened_keys, widening=False)
        self.resolve_call = None
        returned = BOTTOM
        facts = self.facts
        calls = tuple(
            CallSite(
                (callee := facts.get(node.fun.use(), NO_FACT)).target,
                tuple(facts.get(arg.use(), NO_FACT).type for arg in node.args),
                callee.proven,
            )
            for block, node in self.graph.call_nodes
            if block in incoming
        )
        if self.lambda_.return_ is not None:
            for use in self.lambda_.return_.result.uses:
                if use.block in incoming:
                    returned = returned.join(facts.get(use, NO_FACT).type)
        return Body(facts, returned, calls)

    def propagate(self, incoming, edge_states, widened_keys, *, widening: bool) -> bool:
        visits = defaultdict(int)
        key_visits = defaultdict(int)
        did_widen = False
        initial = (
            [self.entry]
            if widening
            else [block for block in self.blocks if block in incoming]
        )
        pending, queued = deque(initial), set(initial)
        cross_changed = set()
        visited_edges = set()
        predecessors = self.predecessors
        successors = self.successors
        block_nodes = self.block_nodes
        handlers = self.handlers
        consumers = self.consumers
        headers = self.headers
        back_edges = self.back_edges
        facts = self.facts
        visitor = _Propagation(self, {}, {})

        while pending:
            block = pending.popleft()
            queued.remove(block)
            operand_changed = block in cross_changed
            if operand_changed:
                cross_changed.remove(block)
            if widening or block not in headers:
                parents = [
                    edge_states[parent, block]
                    for parent in predecessors[block]
                    if (parent, block) in edge_states
                ]
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
            if not parents:
                continue
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
                if changed:
                    visits[block] += 1
                    if block in headers and block in incoming:
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
                                if key_visits[block, key] > 2:
                                    widened[key] = widen_fact(old_val, val)
                                else:
                                    widened[key] = val
                            else:
                                widened[key] = val
                        if widened != state:
                            did_widen = True
                            state = widened
                    incoming[block] = state
                elif not operand_changed:
                    continue
            elif state != previous:
                if visits[block] < 3:
                    visits[block] += 1
                    incoming[block] = state
                elif not operand_changed:
                    continue
                else:
                    state = previous
            elif not operand_changed:
                continue
            # The visitor updates its state in place.
            state = state.copy()
            handler = handlers[block]
            has_handler = handler is not None
            visitor.reset(state, has_handler)
            for node in block_nodes[block]:
                fact = node.accept(visitor)
                if fact is not None:
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
                                pending.append(consumer)
                                queued.add(consumer)
                if has_handler:
                    visitor.note_handler_state()
            for target in successors[block]:
                if target is handler:
                    edge = linked(visitor.handler_state, visitor, "handler state")
                    self.invalidate_globals(edge)
                else:
                    edge = self.constrain_edge(block, target, visitor, state)
                    if edge is None:
                        continue
                key = block, target
                if not widening:
                    visited_edges.add(key)
                old_edge = edge_states.get(key)
                if old_edge is None or (old_edge is not edge and old_edge != edge):
                    edge_states[key] = edge
                    if target not in queued:
                        pending.append(target)
                        queued.add(target)
        return did_widen

    def constrain_edge(self, block, target, visitor, state):
        """Narrow an integer local on a branch edge tested against a constant,
        or return ``None`` when a constant boolean test makes the edge dead."""
        last = block.last
        if last is not None:
            return last.accept(_ConstrainEdge(target, visitor, state))
        return state


class _NarrowTruthiness(ValueTypeVisitor):
    """Narrow a value type along a direct truthiness branch edge."""

    def __init__(self, truth: bool):
        self.truth = truth

    def visit_ValueType(self, fact):
        if self.truth:
            return fact.narrow_none(False)
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
                return _SMALL_INTS[0]
            if self.truth:
                if fact.minimum == 0:
                    return fact.narrow(ast.ComparisonOperator.GREATER, 0, True)
                if fact.maximum == 0:
                    return fact.narrow(ast.ComparisonOperator.LESS, 0, True)
        return fact


_TRUE_NARROWING = _NarrowTruthiness(True)
_FALSE_NARROWING = _NarrowTruthiness(False)


class _ConstrainEdge(NodeVisitor):
    """Only a two-way branch narrows its edges."""

    def __init__(self, target, visitor: _Propagation, state):
        self.target = target
        self.visitor = visitor
        self.state = state

    def visit_Node(self, node):
        return self.state

    def visit_If(self, node):
        if node.consequent is node.alternative:
            return self.state
        truth = self.target is node.consequent
        test_type = self.visitor.type_of(node.test)
        if (truth and test_type is BOOLEAN_FALSE) or (
            not truth and test_type is BOOLEAN_TRUE
        ):
            return None
        if not self.visitor.origins:
            return self.state
        narrowed_edge = node.test.use().accept(
            _ConstrainComparison(self.visitor, self.state, truth)
        )
        if narrowed_edge is not None:
            return narrowed_edge
        local = self.visitor.origins.get(node.test)
        if local is not None and local in self.visitor.state:
            current = self.visitor.state[local]
            narrowed = current.type.accept(
                _TRUE_NARROWING if truth else _FALSE_NARROWING
            )
            if narrowed != current.type:
                edge = self.state.copy()
                edge[local] = (
                    _fact(narrowed)
                    if current.target is None
                    else Fact(narrowed, current.target, current.proven)
                )
                return edge
        return self.state


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
    if effective is ast.ComparisonOperator.EQUAL:
        return other_type.exact_integer()
    return None


class _ConstrainComparison(NodeVisitor):
    """A known comparison of a local against an integer interval narrows the local."""

    def __init__(self, visitor: _Propagation, state, truth: bool):
        self.visitor = visitor
        self.state = state
        self.truth = truth

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node):
        if node.kind is not CallKind.KNOWN:
            return None
        name = node.fun_info.name
        if name == "%unary_not" and len(node.args) == 1:
            return (
                node.args[0]
                .use()
                .accept(_ConstrainComparison(self.visitor, self.state, not self.truth))
            )
        if name in ("_text_storage", "_bytes_storage") and len(node.args) == 1:
            if not self.truth:
                return None
            local = self.visitor.origins.get(node.args[0])
            if local is None or local not in self.visitor.state:
                return None
            current = self.visitor.state[local]
            if current.type is not UNKNOWN:
                return None
            edge = self.state.copy()
            edge[local] = TEXT_FACT if name == "_text_storage" else BYTES_FACT
            return edge
        operator = _COMPARISON_OPERATORS.get(name)
        if operator is None:
            return None
        visitor = self.visitor
        left, right = node.args
        swapped = _SWAPPED.get(
            operator,
            operator
            if operator
            in (
                ast.ComparisonOperator.EQUAL,
                ast.ComparisonOperator.NOT_EQUAL,
                ast.ComparisonOperator.IS,
                ast.ComparisonOperator.IS_NOT,
            )
            else None,
        )
        edge = None
        for operand, other, op in (
            (left, right, operator),
            (right, left, swapped),
        ):
            if op is None:
                continue
            local = visitor.origins.get(operand)
            if local is None or local not in visitor.state:
                continue
            other_type = visitor.type_of(other)
            if op in (ast.ComparisonOperator.IS, ast.ComparisonOperator.IS_NOT):
                if other_type is not NONE_TYPE:
                    continue
                current = (edge or visitor.state)[local]
                narrowed = current.type.narrow_none(
                    (op is ast.ComparisonOperator.IS) == self.truth
                )
            else:
                bound = _comparison_bound(op, other_type, self.truth)
                if bound is None:
                    continue
                current = (edge or visitor.state)[local]
                narrowed = current.type.narrow(op, bound, self.truth)
            if narrowed == current.type:
                continue
            if edge is None:
                edge = self.state.copy()
            edge[local] = (
                _fact(narrowed)
                if current.target is None
                else Fact(narrowed, current.target, current.proven)
            )
        return edge


def _successors(block, handler):
    """Normal successors inside the component plus the error destination."""
    for successor in block.succ:
        if successor.start is not None:
            yield successor
    if handler is not None:
        yield handler


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
    ).run()


__all__ = [
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
