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
    BOTTOM,
    FLOAT,
    PYTHON_INTEGER,
    UNBOUND,
    UNKNOWN,
    BooleanType,
    BytesType,
    FunctionType,
    IntegerType,
    ListType,
    NoneType,
    TextType,
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
    return Fact(
        a.type.join(b.type), target, target is not None and a.proven and b.proven
    )


def join_states(left: dict, right: dict) -> tuple[dict, bool]:
    """Join two variable states over the same keys. The flag reports whether
    the result differs from ``left``."""
    joined, changed = {}, False
    for key, fact in left.items():
        other = right[key]
        if other is not fact:
            other = join_fact(fact, other)
            if other is not fact:
                changed = True
                fact = other
        joined[key] = fact
    return joined, changed


def widen_fact(old: Fact, new: Fact) -> Fact:
    return Fact(old.type.widen(new.type), new.target, new.proven)


class _LiteralFacts(ast.Visitor):
    def visit_Node(self, node):
        return UNKNOWN

    def visit_IntegerLiteral(self, node):
        return IntegerType(node.value, node.value)

    def visit_BooleanLiteral(self, node):
        return BooleanType(int(node.value), int(node.value))

    def visit_FloatLiteral(self, node):
        return FLOAT

    def visit_NoneLiteral(self, node):
        return NoneType()

    def visit_TextLiteral(self, node):
        return TextType()

    def visit_BytesLiteral(self, node):
        return BytesType()

    def visit_Tuple(self, node):
        return TupleType(len(node.elements))


def constant_type(literal) -> ValueType:
    return literal.accept(_LiteralFacts())


_GLOBAL = object()


def _key(var):
    return var.accept(_StateKey())


class _StateKey(LeafVisitor):
    """A local is its own state key; a global is keyed by name, since
    every reference converts to its own ``GlobalVar``."""

    def visit_Leaf(self, leaf):
        raise TypeError(f"{leaf!r} is not a variable with propagated facts")

    def visit_LambdaVar(self, leaf):
        return leaf

    def visit_GlobalVar(self, leaf):
        return (_GLOBAL, leaf.name)


class _Propagation(NodeVisitor):
    """Processes one block's nodes against the current variable state."""

    def __init__(self, walk, state, origins):
        self.walk = walk
        self.state = state
        self.origins = origins
        self.handler_state: dict[object, Fact] | None = None
        # Keys assigned since the handler state last absorbed the state.
        self.changed = set()

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
        self.origins = {
            lvar: origin for lvar, origin in self.origins.items() if origin is not var
        }

    def note_handler_state(self):
        """Join the state after a node into what an error edge carries.
        Only keys assigned since the previous node can differ."""
        if self.handler_state is not None:
            handler = self.handler_state
            for key in self.changed:
                handler[key] = join_fact(handler[key], self.state[key])
        self.changed.clear()

    # Nodes

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(_ReferenceFact(self, node))

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
            return Fact(value)
        return Fact(node.contract.refine(value))

    def visit_Combination(self, node: Combination):
        if node.kind in (CallKind.FULL, CallKind.GUARDED):
            info = linked(node.info, node, "info")
            fact = Fact(
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
                return Fact(value_type)
        if name == "%build_tuple":
            return Fact(TupleType(len(args)))
        if name == "%build_list":
            return Fact(ListType())
        if name == "%unpack":
            self.invalidate()
            arity = self.type_of(args[1]).exact_integer()
            return Fact(TupleType(arity if arity is not None and arity >= 0 else None))
        if name in _BINARY_OPERATORS:
            left, right = self.type_of(args[0]), self.type_of(args[1])
            fact = Fact(left.binary(_BINARY_OPERATORS[name], right))
            if left.may_run_user_code or right.may_run_user_code:
                self.invalidate()
            return fact
        if name in _UNARY_OPERATORS:
            operand = self.type_of(args[0])
            fact = Fact(operand.unary(_UNARY_OPERATORS[name]))
            if operand.may_run_user_code:
                self.invalidate()
            return fact
        if name in _COMPARISON_OPERATORS:
            operator = _COMPARISON_OPERATORS[name]
            left, right = self.type_of(args[0]), self.type_of(args[1])
            fact = Fact(left.comparison(operator, right))
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
            return Fact(FunctionType(), lambda_, True)
        if name == "%call_resolved":
            if (
                len(args) == 2
                and not linked(node.info, node, "info").keywords
                and args[0].use().accept(_ExactListAppend(self))
            ):
                return Fact(NoneType())
            self.invalidate()
            return Fact()
        if name in ("%import", "%catch_exception"):
            self.invalidate()
            return Fact()
        if name == "%format":
            self.invalidate()
            return Fact(TextType())
        if info.protocol is not None:
            self.invalidate()
            return Fact()
        if name in (
            "%range_prepare",
            "%range_available",
            "%sequence_available",
            "%has_binding",
        ):
            return Fact(BooleanType())
        if name == "%range_advance":
            state = linked(node.info, node, "info")
            return Fact(self.range_type(state))
        if info.result:
            return Fact()
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
                step = arguments[2] if len(arguments) == 3 else IntegerType(1, 1)
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
    def __init__(self, propagation: _Propagation):
        self.propagation = propagation

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.fun_info is not None
            and node.fun_info.name == "%resolve_method"
            and linked(node.info, node, "info").name == "append"
            and self.propagation.type_of(node.args[0]).is_exact_list
        )


class _ReferenceFact(LeafVisitor):
    """The fact a reference yields, updating the propagation state."""

    def __init__(self, propagation: _Propagation, ref: Ref):
        self.propagation = propagation
        self.ref = ref

    def visit_Leaf(self, leaf):
        # A known function's reference is a compile-time operand with no fact.
        return None

    def visit_Constant(self, leaf):
        return Fact(constant_type(leaf.value))

    def visit_GlobalVar(self, leaf):
        propagation = self.propagation
        fact = propagation.state[_key(leaf)]
        if fact.target is None:
            fact = Fact(fact.type, propagation.walk.global_targets.get(leaf.name))
        return fact

    def visit_LambdaVar(self, leaf):
        propagation = self.propagation
        if leaf not in propagation.state:
            # A free variable of an enclosing function: any value.
            return UNKNOWN_FACT
        fact = propagation.state[leaf]
        if self.ref.final:
            propagation.forget_origin(leaf)
            propagation.assign(leaf, UNBOUND_FACT)
        elif self.ref.lvar is not None:
            propagation.origins[self.ref.lvar] = leaf
        return fact

    def visit_Lambda(self, leaf):
        return Fact(FunctionType(), leaf, True)


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


def _loop_headers(entry, successors) -> set:
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
    return headers


class _Walk:
    """One propagation over a lambda under given parameter facts."""

    def __init__(
        self, lambda_: Lambda, global_targets, parameters, resolve_call, annotations
    ):
        self.lambda_ = lambda_
        self.global_targets = global_targets
        self.resolve_call = resolve_call
        self.annotations = annotations
        self.facts: dict[object, Fact] = {}
        self.blocks = list(linked(lambda_.component, lambda_, "component").blocks())
        self.entry = linked(lambda_.bind, lambda_, "bind").block
        self.successors = {block: tuple(_successors(block)) for block in self.blocks}
        self.predecessors = defaultdict(set)
        self.consumers = defaultdict(set)
        for block in self.blocks:
            for target in self.successors[block]:
                self.predecessors[target].add(block)
            for node in block.nodes():
                for lvar in _operands(node):
                    for producer in lvar.uses:
                        self.consumers[producer].add(block)
        self.headers = _loop_headers(self.entry, self.successors)
        self.global_names = {
            leaf.name
            for block in self.blocks
            for node in block.nodes()
            for leaf in _global_leaves(node)
        }
        self.indirect = [var for var in lambda_.locals if var.indirect]
        self.volatile_keys = [
            *((_GLOBAL, name) for name in self.global_names),
            *self.indirect,
        ]
        parameter_facts = dict(zip(lambda_.vars, parameters, strict=True))
        self.initial: dict[object, Fact] = {
            var: Fact(parameter_facts[var]) if var in parameter_facts else UNBOUND_FACT
            for var in lambda_.locals
        }
        # What user code may leave behind in module globals and variables
        # captured by nested functions.
        invalidated_globals: dict[object, Fact] = {
            (_GLOBAL, name): Fact(UNKNOWN, global_targets.get(name))
            for name in self.global_names
        }
        self.invalidated = {
            **invalidated_globals,
            **{var: UNKNOWN_FACT for var in self.indirect},
        }
        self.initial.update(invalidated_globals)

    def invalidate_globals(self, state):
        """Forget facts that user code may change."""
        state.update(self.invalidated)

    def run(self) -> Body:
        incoming, edge_states = {}, {}
        if self.propagate(incoming, edge_states, widening=True):
            self.propagate(incoming, edge_states, widening=False)
        returned = BOTTOM
        call_sites = _CallSites(self.facts)
        for block in incoming:
            for node in block.nodes():
                node.accept(call_sites)
        if self.lambda_.return_ is not None:
            for use in self.lambda_.return_.result.uses:
                if use.block in incoming:
                    returned = returned.join(self.facts.get(use, NO_FACT).type)
        return Body(self.facts, returned, tuple(call_sites.calls))

    def propagate(self, incoming, edge_states, *, widening: bool) -> bool:
        visits = defaultdict(int)
        did_widen = False
        initial = (
            [self.entry]
            if widening
            else [block for block in self.blocks if block in incoming]
        )
        pending, queued = deque(initial), set(initial)

        def schedule(block):
            if block not in queued:
                pending.append(block)
                queued.add(block)

        while pending:
            block = pending.popleft()
            queued.remove(block)
            parents = [
                edge_states[parent, block]
                for parent in self.predecessors[block]
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
                    if (
                        block in self.headers
                        and block in incoming
                        and visits[block] > 3
                    ):
                        widened = {
                            key: widen_fact(incoming[block][key], state[key])
                            for key in state
                        }
                        if widened != state:
                            did_widen = True
                            state = widened
                    incoming[block] = state
            elif state != previous:
                if visits[block] < 2:
                    visits[block] += 1
                    incoming[block] = state
                else:
                    state = previous
            # The visitor updates its state in place.
            state = dict(state)
            handler = block_exception_target(block)
            visitor = _Propagation(self, state, {})
            if handler is not None:
                visitor.handler_state = dict(state)
            for node in block.nodes():
                fact = node.accept(visitor)
                if fact is not None:
                    previous = self.facts.get(node, NO_FACT)
                    if widening:
                        # join_fact returns ``previous`` itself when nothing changed.
                        updated = join_fact(previous, fact)
                        changed = updated is not previous
                    else:
                        updated = fact
                        changed = updated != previous
                    if changed:
                        self.facts[node] = updated
                        for consumer in self.consumers[node]:
                            schedule(consumer)
                if handler is not None:
                    visitor.note_handler_state()
            for target in self.successors[block]:
                if target is handler:
                    edge = dict(linked(visitor.handler_state, visitor, "handler state"))
                    self.invalidate_globals(edge)
                else:
                    edge = dict(state)
                    self.constrain_edge(block, target, visitor, edge)
                key = block, target
                if edge_states.get(key) != edge:
                    edge_states[key] = edge
                    schedule(target)
        return did_widen

    def constrain_edge(self, block, target, visitor, edge):
        """Narrow an integer local on a branch edge tested against a constant."""
        last = block.last
        if last is not None:
            last.accept(_ConstrainEdge(target, visitor, edge))


class _CallSites(NodeVisitor):
    """Full calls without keywords, as the callee and argument facts that
    local-call analysis summarizes."""

    def __init__(self, facts: dict[object, Fact]):
        self.facts = facts
        self.calls: list[CallSite] = []

    def visit_Combination(self, node):
        if node.kind not in (CallKind.FULL, CallKind.GUARDED) or node.info.keywords:
            return
        self.calls.append(
            CallSite(
                self.facts.get(node.fun.use(), NO_FACT).target,
                tuple(self.facts.get(arg.use(), NO_FACT).type for arg in node.args),
            )
        )


class _ConstrainEdge(NodeVisitor):
    """Only a two-way branch narrows its edges."""

    def __init__(self, target, visitor: _Propagation, edge):
        self.target = target
        self.visitor = visitor
        self.edge = edge

    def visit_If(self, node):
        if node.consequent is node.alternative:
            return
        truth = self.target is node.consequent
        node.test.use().accept(_ConstrainComparison(self.visitor, self.edge, truth))


class _ConstrainComparison(NodeVisitor):
    """A known comparison of a local against a constant narrows the local."""

    def __init__(self, visitor: _Propagation, edge, truth: bool):
        self.visitor = visitor
        self.edge = edge
        self.truth = truth

    def visit_Combination(self, node):
        if node.kind is not CallKind.KNOWN:
            return
        operator = _COMPARISON_OPERATORS.get(node.fun_info.name)
        if operator is None:
            return
        visitor = self.visitor
        left, right = node.args
        local = visitor.origins.get(left)
        bound = visitor.type_of(right).exact_integer()
        if local is None:
            local = visitor.origins.get(right)
            bound = visitor.type_of(left).exact_integer()
            operator = _SWAPPED.get(operator, operator)
        if local is None or bound is None or local not in visitor.state:
            return
        current = visitor.state[local]
        self.edge[local] = Fact(
            current.type.narrow(operator, bound, self.truth),
            current.target,
            current.proven,
        )


class _Operands(NodeVisitor):
    def visit_Node(self, node):
        return ()

    def visit_If(self, node):
        return (node.test,)

    def visit_Set(self, node):
        return () if node.value is None else (node.value,)

    def visit_Combination(self, node):
        if node.fun_info is not None and node.fun_info.name == "%range_advance":
            return (node.fun, *node.args, *linked(node.info, node, "info").arguments)
        return (node.fun, *node.args)

    def visit_Return(self, node):
        return (node.result,)

    def visit_Cast(self, node):
        return (node.value,)

    def visit_Exit(self, node):
        return () if node.value is None else (node.value,)


def _operands(node):
    return node.accept(_Operands())


def _successors(block):
    """Normal successors inside the component plus the error destination."""
    for successor in block.succ:
        if successor.start is not None:
            yield successor
    target = block_exception_target(block)
    if target is not None:
        yield target


def _global_leaves(node):
    return node.accept(_GlobalLeaves())


class _GlobalLeaves(NodeVisitor, LeafVisitor):
    """The module globals a node reads or writes."""

    def visit_Node(self, node):
        return ()

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_Set(self, node):
        return node.var.accept(self)

    def visit_Leaf(self, leaf):
        return ()

    def visit_GlobalVar(self, leaf):
        return (leaf,)


def propagate(
    lambda_: Lambda, global_targets, parameters, resolve_call, annotations
) -> Body:
    """Propagate facts through ``lambda_`` from the given parameter types.

    ``resolve_call(callee_fact, argument_types, location, keywords)`` supplies
    the result type of a call; local-call analysis provides it so that proven
    local calls receive their callee's published return type. Annotation
    contradictions are recorded in ``annotations``, the module's
    ``AnnotationLedger``.
    """
    return _Walk(lambda_, global_targets, parameters, resolve_call, annotations).run()


__all__ = [
    "NO_FACT",
    "Body",
    "CallSite",
    "Fact",
    "constant_type",
    "join_fact",
    "propagate",
    "widen_fact",
]
