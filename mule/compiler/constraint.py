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
    UNBOUND,
    UNKNOWN,
    BooleanType,
    BytesType,
    FunctionType,
    IntegerType,
    NoneType,
    TextType,
    ValueType,
)
from .ir1util import block_exception_target
from .node import (
    CallKind,
    Cast,
    Combination,
    Constant,
    GlobalVar,
    If,
    Lambda,
    LambdaVar,
    NodeVisitor,
    Ref,
    Set,
)


@dataclass(frozen=True, slots=True)
class Fact:
    type: ValueType = UNKNOWN
    target: Lambda | None = None
    proven: bool = False


NO_FACT = Fact(BOTTOM)


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
    def combine(a, b):
        target = a.target if a.target is b.target else None
        return Fact(
            a.type.join(b.type), target, target is not None and a.proven and b.proven
        )

    return left.type.merge_information(right.type, left, right, combine)


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


def constant_type(literal) -> ValueType:
    return literal.accept(_LiteralFacts())


_GLOBAL = object()


def _key(var):
    return var if isinstance(var, LambdaVar) else (_GLOBAL, var.name)


class _Propagation(NodeVisitor):
    """Processes one block's nodes against the current variable state."""

    def __init__(self, walk, state, origins):
        self.walk = walk
        self.state = state
        self.origins = origins
        self.handler_state = None

    def fact_of(self, lvar) -> Fact:
        return self.walk.facts.get(lvar.use(), NO_FACT)

    def type_of(self, lvar) -> ValueType:
        return self.fact_of(lvar).type

    def invalidate(self):
        self.walk.invalidate_globals(self.state)

    def forget_origin(self, var):
        self.origins = {
            lvar: origin for lvar, origin in self.origins.items() if origin is not var
        }

    def note_handler_state(self):
        if self.handler_state is not None:
            self.handler_state = {
                key: join_fact(self.handler_state[key], self.state[key])
                for key in self.state
            }

    # Nodes

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        leaf = node.leaf
        if isinstance(leaf, Constant):
            return Fact(constant_type(leaf.value))
        if isinstance(leaf, GlobalVar):
            fact = self.state[_key(leaf)]
            if fact.target is None:
                fact = Fact(fact.type, self.walk.global_targets.get(leaf.name))
            return fact
        if isinstance(leaf, LambdaVar):
            if leaf not in self.state:
                # A free variable of an enclosing function: any value.
                return Fact(UNKNOWN)
            fact = self.state[leaf]
            if node.final:
                self.forget_origin(leaf)
                self.state[leaf] = Fact(UNBOUND)
            elif node.lvar is not None:
                self.origins[node.lvar] = leaf
            return fact
        if isinstance(leaf, Lambda):
            return Fact(FunctionType(), leaf, True)
        return None

    def visit_Set(self, node: Set):
        key = _key(node.var)
        if key not in self.state:
            return
        if node.value is None:
            self.state[key] = Fact(UNBOUND)
        else:
            self.state[key] = self.fact_of(node.value)
        if isinstance(node.var, LambdaVar):
            self.forget_origin(node.var)

    def visit_Cast(self, node: Cast):
        value = self.type_of(node.value)
        node.contract.validate(value, node.location)
        return Fact(node.contract.refine(value))

    def visit_Combination(self, node: Combination):
        if node.kind in (CallKind.FULL, CallKind.GUARDED):
            info = node.info
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
            lambda_ = args[0].use().leaf
            for value, contract in zip(
                args[1:], node.info.default_contracts, strict=True
            ):
                if contract is not None:
                    contract.validate(self.type_of(value), node.info.location)
            return Fact(FunctionType(), lambda_, True)
        if name in ("%import", "%call_resolved", "%catch_exception"):
            self.invalidate()
            return Fact()
        if name == "%format":
            self.invalidate()
            return Fact(TextType())
        if node.fun_info.protocol is not None:
            self.invalidate()
            return Fact()
        if name in ("%range_prepare", "%range_available", "%has_binding"):
            return Fact(BooleanType())
        if name == "%range_advance":
            return Fact(IntegerType(node.info.minimum, node.info.maximum))
        if node.fun_info.result:
            return Fact()
        return None

    def visit_If(self, node: If):
        if self.type_of(node.test).may_run_user_code:
            self.invalidate()


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

    def __init__(self, lambda_: Lambda, global_targets, parameters, resolve_call):
        self.lambda_ = lambda_
        self.global_targets = global_targets
        self.resolve_call = resolve_call
        self.facts: dict[object, Fact] = {}
        self.blocks = list(lambda_.component.blocks())
        self.entry = lambda_.bind.block
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
        parameter_facts = dict(zip(lambda_.vars, parameters, strict=True))
        self.initial = {
            var: Fact(parameter_facts.get(var, UNBOUND)) for var in lambda_.locals
        }
        self.initial.update(
            {
                (_GLOBAL, name): Fact(UNKNOWN, global_targets.get(name))
                for name in self.global_names
            }
        )

    def invalidate_globals(self, state):
        """Forget facts that user code may change: module globals and
        variables captured by nested functions."""
        for name in self.global_names:
            state[_GLOBAL, name] = Fact(UNKNOWN, self.global_targets.get(name))
        for var in self.indirect:
            state[var] = Fact(UNKNOWN)

    def run(self) -> Body:
        incoming, edge_states = {}, {}
        visits = defaultdict(int)
        pending, queued = deque([self.entry]), {self.entry}

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
            state = dict(parents[0])
            for parent in parents[1:]:
                state = {key: join_fact(state[key], parent[key]) for key in state}
            if block in incoming:
                state = {
                    key: join_fact(incoming[block][key], state[key]) for key in state
                }
            if incoming.get(block) != state:
                visits[block] += 1
                if block in self.headers and block in incoming and visits[block] > 3:
                    state = {
                        key: widen_fact(incoming[block][key], state[key])
                        for key in state
                    }
                incoming[block] = dict(state)
            handler = block_exception_target(block)
            visitor = _Propagation(self, state, {})
            if handler is not None:
                visitor.handler_state = dict(state)
            for node in block.nodes():
                fact = node.accept(visitor)
                if fact is not None:
                    joined = join_fact(self.facts.get(node, NO_FACT), fact)
                    if self.facts.get(node, NO_FACT) != joined:
                        self.facts[node] = joined
                        for consumer in self.consumers[node]:
                            schedule(consumer)
                visitor.note_handler_state()
            for target in self.successors[block]:
                if target is handler:
                    edge = dict(visitor.handler_state)
                    self.invalidate_globals(edge)
                else:
                    edge = dict(state)
                    self.constrain_edge(block, target, visitor, edge)
                key = block, target
                if edge_states.get(key) != edge:
                    edge_states[key] = edge
                    schedule(target)
        returned = BOTTOM
        calls = []
        for block in incoming:
            for node in block.nodes():
                if (
                    isinstance(node, Combination)
                    and node.kind in (CallKind.FULL, CallKind.GUARDED)
                    and not node.info.keywords
                ):
                    calls.append(
                        CallSite(
                            self.facts.get(node.fun.use(), NO_FACT).target,
                            tuple(
                                self.facts.get(arg.use(), NO_FACT).type
                                for arg in node.args
                            ),
                        )
                    )
        if self.lambda_.return_ is not None:
            for use in self.lambda_.return_.result.uses:
                if use.block in incoming:
                    returned = returned.join(self.facts.get(use, NO_FACT).type)
        return Body(self.facts, returned, tuple(calls))

    def constrain_edge(self, block, target, visitor, edge):
        """Narrow an integer local on a branch edge tested against a constant."""
        last = block.last
        if not isinstance(last, If) or last.consequent is last.alternative:
            return
        producer = last.test.use()
        if not isinstance(producer, Combination) or producer.kind is not CallKind.KNOWN:
            return
        operator = _COMPARISON_OPERATORS.get(producer.fun_info.name)
        if operator is None:
            return
        left, right = producer.args
        local = visitor.origins.get(left)
        bound = visitor.type_of(right).exact_integer()
        if local is None:
            local = visitor.origins.get(right)
            bound = visitor.type_of(left).exact_integer()
            operator = _SWAPPED.get(operator, operator)
        if local is None or bound is None or local not in visitor.state:
            return
        current = visitor.state[local]
        edge[local] = Fact(
            current.type.narrow(operator, bound, target is last.consequent),
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
    if isinstance(node, Ref) and isinstance(node.leaf, GlobalVar):
        yield node.leaf
    if isinstance(node, Set) and isinstance(node.var, GlobalVar):
        yield node.var


def propagate(lambda_: Lambda, global_targets, parameters, resolve_call) -> Body:
    """Propagate facts through ``lambda_`` from the given parameter types.

    ``resolve_call(callee_fact, argument_types, location, keywords)`` supplies
    the result type of a call; local-call analysis provides it so that proven
    local calls receive their callee's published return type.
    """
    return _Walk(lambda_, global_targets, parameters, resolve_call).run()


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
