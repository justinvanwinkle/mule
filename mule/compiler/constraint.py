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
    FLOAT,
    FUNCTION_TYPE,
    IMMORTAL,
    IMMORTAL_UNBOUND,
    INTEGER,
    LIST_TYPE,
    NONE_TYPE,
    PYTHON_INTEGER,
    REAL,
    TEXT_TYPE,
    UNBOUND,
    UNKNOWN,
    UNKNOWN_UNBOUND,
    IntegerType,
    TupleType,
    ValueType,
)
from .ir1util import block_exception_target, made_lambda
from .node import (
    CallKind,
    Cast,
    Combination,
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
IMMORTAL_FACT = Fact(IMMORTAL)
IMMORTAL_UNBOUND_FACT = Fact(IMMORTAL_UNBOUND)

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
        IMMORTAL_FACT,
        IMMORTAL_UNBOUND_FACT,
    )
}


def _fact(value_type: ValueType) -> Fact:
    canonical = _CANONICAL_FACTS.get(id(value_type))
    return Fact(value_type) if canonical is None else canonical


@dataclass(frozen=True, slots=True)
class CallSite:
    target: Lambda | None
    arguments: tuple[ValueType, ...]


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


def _tuple_fact(arity: int | None) -> Fact:
    if arity is None:
        return _UNBOUNDED_TUPLE_FACT
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
        return TEXT_TYPE

    def visit_BytesLiteral(self, node):
        return BYTES_TYPE

    def visit_Tuple(self, node):
        arity = len(node.elements)
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
            self.assign(key, self.fact_of(node.value))
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
            fact = _fact(
                self.walk.resolve_call(
                    self.fact_of(node.fun),
                    tuple(self.type_of(arg) for arg in node.args),
                    info.location,
                    info.keywords,
                )
            )
            self.invalidate()
            return fact
        return self.derive_known(node)

    def derive_known(self, node: Combination):
        info = node.fun_info
        assert info is not None
        name = info.name
        args = node.args
        if info.derive_type is not None:
            value_type = info.derive_type(node)
            if value_type is not None:
                if info.may_call or info.unsafe:
                    self.invalidate()
                return _fact(value_type)
        if name == "%build_tuple":
            return _tuple_fact(len(args))
        if name == "%build_list":
            return LIST_FACT
        if name == "%unpack":
            self.invalidate()
            arity = self.type_of(args[1]).exact_integer()
            return _tuple_fact(arity if arity is not None and arity >= 0 else None)
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
            if (
                len(args) == 2
                and not linked(node.info, node, "info").keywords
                and (receiver := args[0].use().accept(_EXACT_LIST_APPEND_RECEIVER))
                is not None
                and self.type_of(receiver).is_exact_list
            ):
                return NONE_FACT
            self.invalidate()
            return UNKNOWN_FACT
        if name in ("%import", "%catch_exception"):
            self.invalidate()
            return UNKNOWN_FACT
        if name == "%format":
            self.invalidate()
            return TEXT_FACT
        if info.protocol is not None:
            self.invalidate()
            return UNKNOWN_FACT
        if name in (
            "%range_prepare",
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


class _ExactListAppend(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.fun_info is not None
            and node.fun_info.name == "%resolve_method"
            and linked(node.info, node, "info").name == "append"
        ):
            return node.args[0]
        return None


_EXACT_LIST_APPEND_RECEIVER = _ExactListAppend()


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


def _loop_headers(entry, successors) -> frozenset:
    """Blocks that are targets of DFS back edges, where intervals need widening."""
    colors = {entry: 1}
    stack = [(entry, iter(successors[entry]))]
    headers = set()
    while stack:
        block, edges = stack[-1]
        target = next(edges, None)
        if target is None:
            colors[block] = 2
            stack.pop()
        elif colors.get(target) == 1:
            headers.add(target)
        elif target not in colors:
            colors[target] = 1
            stack.append((target, iter(successors[target])))
    return frozenset(headers)


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
    global_names: tuple
    indirect: tuple
    volatile_keys: tuple
    call_nodes: tuple


class _GraphScan(NodeVisitor, LeafVisitor):
    """Collects global variable names and full/guarded call sites for a lambda."""

    def __init__(self):
        self.global_names: set[str] = set()
        self.call_nodes: list[tuple[object, Combination]] = []
        self.block = None

    def reset(self) -> None:
        self.global_names.clear()
        self.call_nodes.clear()
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


_GRAPH_SCAN = _GraphScan()


def build_lambda_graph(lambda_: Lambda) -> LambdaGraph:
    blocks = tuple(linked(lambda_.component, lambda_, "component").blocks())
    block_nodes = {block: tuple(block.nodes()) for block in blocks}
    handlers = {block: block_exception_target(block) for block in blocks}
    entry = linked(lambda_.bind, lambda_, "bind").block
    successors = {block: tuple(_successors(block, handlers[block])) for block in blocks}
    predecessors_sets = defaultdict(set)
    consumers = {}
    scan = _GRAPH_SCAN
    scan.reset()
    for block in blocks:
        scan.block = block
        for target in successors[block]:
            predecessors_sets[target].add(block)
        for node in block_nodes[block]:
            lvar = node.value_lvar
            if lvar is not None and lvar.dest is not None:
                dest_block = lvar.dest.block
                if dest_block is not block:
                    consumers[node] = dest_block
            node.accept(scan)
    predecessors = {block: tuple(predecessors_sets[block]) for block in blocks}
    headers = _loop_headers(entry, successors)
    global_names = tuple(scan.global_names)
    call_nodes = tuple(scan.call_nodes)
    scan.reset()
    indirect = tuple(var for var in lambda_.locals if var.indirect)
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
        global_names=global_names,
        indirect=indirect,
        volatile_keys=volatile_keys,
        call_nodes=call_nodes,
    )


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
    ):
        self.lambda_ = lambda_
        self.global_targets = global_targets
        self.resolve_call = resolve_call
        self.annotations = annotations
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
        self.global_names = graph.global_names
        self.indirect = graph.indirect
        self.volatile_keys = graph.volatile_keys
        parameter_facts = dict(zip(lambda_.vars, parameters, strict=True))
        self.initial: dict[object, Fact] = {
            var: _fact(parameter_facts[var]) if var in parameter_facts else UNBOUND_FACT
            for var in lambda_.locals
        }
        # What user code may leave behind in module globals and variables
        # captured by nested functions.
        invalidated_globals: dict[object, Fact] = {
            (_GLOBAL, name): (
                Fact(UNKNOWN, target)
                if (target := global_targets.get(name)) is not None
                else UNKNOWN_FACT
            )
            for name in self.global_names
        }
        self.invalidated = {
            **invalidated_globals,
            **dict.fromkeys(self.indirect, UNKNOWN_FACT),
        }
        self.initial.update(invalidated_globals)

    def invalidate_globals(self, state):
        """Forget facts that user code may change."""
        if self.invalidated:
            state.update(self.invalidated)

    def run(self) -> Body:
        incoming, edge_states = {}, {}
        if self.propagate(incoming, edge_states, widening=True):
            self.propagate(incoming, edge_states, widening=False)
        self.resolve_call = None
        returned = BOTTOM
        facts = self.facts
        calls = tuple(
            CallSite(
                facts.get(node.fun.use(), NO_FACT).target,
                tuple(facts.get(arg.use(), NO_FACT).type for arg in node.args),
            )
            for block, node in self.graph.call_nodes
            if block in incoming
        )
        if self.lambda_.return_ is not None:
            for use in self.lambda_.return_.result.uses:
                if use.block in incoming:
                    returned = returned.join(facts.get(use, NO_FACT).type)
        return Body(facts, returned, calls)

    def propagate(self, incoming, edge_states, *, widening: bool) -> bool:
        visits = defaultdict(int)
        did_widen = False
        initial = (
            [self.entry]
            if widening
            else [block for block in self.blocks if block in incoming]
        )
        pending, queued = deque(initial), set(initial)
        cross_changed = set()
        predecessors = self.predecessors
        successors = self.successors
        block_nodes = self.block_nodes
        handlers = self.handlers
        consumers = self.consumers
        headers = self.headers
        facts = self.facts
        visitor = _Propagation(self, {}, {})

        while pending:
            block = pending.popleft()
            queued.remove(block)
            operand_changed = block in cross_changed
            if operand_changed:
                cross_changed.remove(block)
            parents = [
                edge_states[parent, block]
                for parent in predecessors[block]
                if (parent, block) in edge_states
            ]
            if block is self.entry:
                parents.append(self.initial)
            if not parents:
                continue
            state = parents[0]
            for parent in parents[1:]:
                state, _ = join_states(state, parent)
            previous = incoming.get(block)
            if widening:
                if previous is None:
                    changed = True
                else:
                    state, changed = join_states(previous, state)
                if changed:
                    visits[block] += 1
                    if block in headers and block in incoming and visits[block] > 3:
                        old_in = incoming[block]
                        widened = {
                            key: widen_fact(old_in[key], val)
                            for key, val in state.items()
                        }
                        if widened != state:
                            did_widen = True
                            state = widened
                    incoming[block] = state
                elif not operand_changed:
                    continue
            elif state != previous:
                if visits[block] < 2:
                    visits[block] += 1
                    incoming[block] = state
                else:
                    state = previous
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
                        consumer = consumers.get(node)
                        if consumer is not None:
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
                key = block, target
                old_edge = edge_states.get(key)
                if old_edge is None or (old_edge is not edge and old_edge != edge):
                    edge_states[key] = edge
                    if target not in queued:
                        pending.append(target)
                        queued.add(target)
        return did_widen

    def constrain_edge(self, block, target, visitor, state):
        """Narrow an integer local on a branch edge tested against a constant."""
        last = block.last
        if last is not None and visitor.origins:
            narrowed = last.accept(_ConstrainEdge(target, visitor, state))
            if narrowed is not None:
                return narrowed
        return state


class _ConstrainEdge(NodeVisitor):
    """Only a two-way branch narrows its edges."""

    def __init__(self, target, visitor: _Propagation, state):
        self.target = target
        self.visitor = visitor
        self.state = state

    def visit_Node(self, node):
        return None

    def visit_If(self, node):
        if node.consequent is node.alternative:
            return None
        truth = self.target is node.consequent
        return node.test.use().accept(
            _ConstrainComparison(self.visitor, self.state, truth)
        )


class _ConstrainComparison(NodeVisitor):
    """A known comparison of a local against a constant narrows the local."""

    def __init__(self, visitor: _Propagation, state, truth: bool):
        self.visitor = visitor
        self.state = state
        self.truth = truth

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node):
        if node.kind is not CallKind.KNOWN:
            return None
        operator = _COMPARISON_OPERATORS.get(node.fun_info.name)
        if operator is None:
            return None
        visitor = self.visitor
        left, right = node.args
        local = visitor.origins.get(left)
        bound = visitor.type_of(right).exact_integer()
        if local is None:
            local = visitor.origins.get(right)
            bound = visitor.type_of(left).exact_integer()
            operator = _SWAPPED.get(operator, operator)
        if local is None or bound is None or local not in visitor.state:
            return None
        current = visitor.state[local]
        narrowed = current.type.narrow(operator, bound, self.truth)
        if narrowed == current.type:
            return None
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
