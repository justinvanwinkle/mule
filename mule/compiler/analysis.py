"""Conservative flow facts and guarded local-call specializations.

IR operations select analysis visitors. Semantic value classes perform their
own joins, arithmetic, narrowing, and containment through virtual methods;
callable predictions stay separate from proven function identity.
"""

from collections import defaultdict, deque
from dataclasses import dataclass, field

from mule import ast
from mule.compiler import ir
from mule.compiler.value_facts import (
    BOOLEAN,
    BOTTOM,
    FIXNUM_MAX,
    FIXNUM_MIN,
    FLOAT,
    INTEGER,
    UNBOUND,
    UNKNOWN,
    BooleanType,
    BottomType,
    BytesType,
    FloatType,
    FunctionType,
    ImmortalType,
    IntegerType,
    NoneType,
    TextType,
    UnboundType,
    UnknownType,
    ValueType,
)

__all__ = [
    "BOOLEAN",
    "BOTTOM",
    "FIXNUM_MAX",
    "FIXNUM_MIN",
    "FLOAT",
    "INTEGER",
    "UNBOUND",
    "UNKNOWN",
    "Analysis",
    "BooleanType",
    "BottomType",
    "BytesType",
    "Context",
    "FloatType",
    "FunctionType",
    "ImmortalType",
    "IntegerType",
    "NoneType",
    "TextType",
    "UnboundType",
    "UnknownType",
    "ValueType",
    "analyze",
]


@dataclass(slots=True)
class Analysis:
    types: dict[str, dict[str, ValueType]]
    targets: dict[str, dict[str, str]]
    variants: dict[str, tuple[Context, ...]] = field(default_factory=dict)
    returns: dict[str, ValueType] = field(default_factory=dict)


@dataclass(frozen=True, slots=True)
class Context:
    """An internal body valid only after its parameter guards succeed."""

    id: str
    parameters: tuple[ValueType, ...]
    types: dict[str, ValueType]
    targets: dict[str, str]
    return_type: ValueType


@dataclass(frozen=True, slots=True)
class _CallSite:
    target: str | None
    arguments: tuple[ValueType, ...]


@dataclass(slots=True)
class _Body:
    types: dict[str, ValueType]
    targets: dict[str, str]
    return_type: ValueType
    calls: tuple[_CallSite, ...]


@dataclass(frozen=True, slots=True)
class _Fact:
    type: ValueType = UNKNOWN
    target: str | None = None
    proven: bool = False


_NO_FACT = _Fact(BOTTOM)


def _join_type(left: ValueType, right: ValueType) -> ValueType:
    return left.join(right)


def _join_fact(left: _Fact, right: _Fact) -> _Fact:
    def combine(a, b):
        target = a.target if a.target == b.target else None
        return _Fact(
            a.type.join(b.type), target, target is not None and a.proven and b.proven
        )

    return left.type.merge_information(right.type, left, right, combine)


def _widen(old: _Fact, new: _Fact) -> _Fact:
    return _Fact(old.type.widen(new.type), new.target, new.proven)


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


def _constant(value: ast.Literal) -> ValueType:
    return value.accept(_LiteralFacts())


def _binary(operator, left: ValueType, right: ValueType) -> ValueType:
    return left.binary(operator, right)


def _unary(operator, operand: ValueType) -> ValueType:
    return operand.unary(operator)


def _loop_headers(entry: str, successors: dict[str, tuple[str, ...]]) -> set[str]:
    """Find DFS back-edge targets, where intervals need a convergence limit."""
    colors = {entry: 1}
    stack = [(entry, iter(successors[entry]))]
    headers = set()
    while stack:
        label, edges = stack[-1]
        target = next(edges, None)
        if target is None:
            colors[label] = 2
            stack.pop()
        elif colors.get(target) == 1:
            headers.add(target)
        elif target not in colors:
            colors[target] = 1
            stack.append((target, iter(successors[target])))
    return headers


class _InstructionAnalysis(ir.Visitor):
    def __init__(
        self, state, values, origins, global_targets, invalidate, resolve_call
    ):
        self.state = state
        self.values = values
        self.origins = origins
        self.global_targets = global_targets
        self.invalidate = invalidate
        self.resolve_call = resolve_call

    def forget_origin(self, name):
        self.origins = {
            result: origin for result, origin in self.origins.items() if origin != name
        }

    def visit_Constant(self, instruction):
        return _Fact(_constant(instruction.value))

    def visit_DefineFunction(self, instruction):
        return _Fact(FunctionType(), instruction.function, True)

    def visit_DefineClass(self, instruction):
        return _Fact()

    def visit_LoadAttribute(self, instruction):
        return _Fact()

    def visit_StoreAttribute(self, instruction):
        pass

    def visit_ResolveMethod(self, instruction):
        return _Fact()

    def visit_CallMethod(self, instruction):
        self.invalidate(self.state)
        return _Fact()

    def visit_Load(self, instruction):
        name = instruction.name
        fact = self.state[name if instruction.local else ("global", name)]
        if not instruction.local and fact.target is None:
            fact = _Fact(fact.type, self.global_targets.get(name))
        if instruction.local:
            if instruction.clear:
                self.forget_origin(name)
                self.state[name] = _Fact(UNBOUND)
            else:
                self.origins[instruction.result] = name
        return fact

    def visit_Store(self, instruction):
        if instruction.local:
            self.state[instruction.name] = self.values[instruction.value]
            self.forget_origin(instruction.name)
        else:
            self.state["global", instruction.name] = self.values[instruction.value]

    def visit_Binary(self, instruction):
        left, right = (
            self.values[instruction.left].type,
            self.values[instruction.right].type,
        )
        fact = _Fact(left.binary(instruction.operator, right))
        if left.may_run_user_code or right.may_run_user_code:
            self.invalidate(self.state)
        return fact

    def visit_Unary(self, instruction):
        value = self.values[instruction.operand].type
        fact = _Fact(value.unary(instruction.operator))
        if value.may_run_user_code:
            self.invalidate(self.state)
        return fact

    def visit_Compare(self, instruction):
        left, right = (
            self.values[instruction.left].type,
            self.values[instruction.right].type,
        )
        fact = _Fact(left.comparison(instruction.operator, right))
        if instruction.operator not in {
            ast.ComparisonOperator.IS,
            ast.ComparisonOperator.IS_NOT,
        } and (left.may_run_user_code or right.may_run_user_code):
            self.invalidate(self.state)
        return fact

    def visit_Call(self, instruction):
        fact = _Fact(
            self.resolve_call(
                self.values[instruction.callee],
                tuple(self.values[arg].type for arg in instruction.args),
            )
        )
        self.invalidate(self.state)
        return fact


class _TruthEffects(ir.Visitor):
    def __init__(self, values, state, invalidate):
        self.values, self.state, self.invalidate = values, state, invalidate

    def visit_Branch(self, instruction):
        if self.values[instruction.condition].type.may_run_user_code:
            self.invalidate(self.state)


class _ComparisonConstraint(ir.Visitor):
    def __init__(self, truth, values, origins, state, edge):
        self.truth, self.values, self.origins = truth, values, origins
        self.state, self.edge = state, edge

    def visit_Compare(self, instruction):
        operator = instruction.operator
        local = self.origins.get(instruction.left)
        bound = self.values[instruction.right].type.exact_integer()
        if local is None:
            local = self.origins.get(instruction.right)
            bound = self.values[instruction.left].type.exact_integer()
            operator = {
                ast.ComparisonOperator.LESS: ast.ComparisonOperator.GREATER,
                ast.ComparisonOperator.LESS_EQUAL: ast.ComparisonOperator.GREATER_EQUAL,
                ast.ComparisonOperator.GREATER: ast.ComparisonOperator.LESS,
                ast.ComparisonOperator.GREATER_EQUAL: ast.ComparisonOperator.LESS_EQUAL,
            }.get(operator, operator)
        if local is not None and bound is not None:
            current = self.state[local]
            self.edge[local] = _Fact(
                current.type.narrow(operator, bound, self.truth),
                current.target,
                current.proven,
            )


class _BranchConstraint(ir.Visitor):
    def __init__(self, target, producers, values, origins, state, edge):
        self.target, self.producers, self.values = target, producers, values
        self.origins, self.state, self.edge = origins, state, edge

    def visit_Branch(self, instruction):
        if instruction.true == instruction.false:
            return
        producer = self.producers.get(instruction.condition)
        if producer is not None:
            producer.accept(
                _ComparisonConstraint(
                    self.target == instruction.true,
                    self.values,
                    self.origins,
                    self.state,
                    self.edge,
                )
            )


class _Summarize(ir.Visitor):
    def __init__(self, values):
        self.values = values
        self.returned = BOTTOM
        self.calls = []

    def visit_Return(self, instruction):
        self.returned = self.returned.join(self.values[instruction.value].type)

    def visit_Call(self, instruction):
        self.calls.append(
            _CallSite(
                self.values[instruction.callee].target,
                tuple(self.values[arg].type for arg in instruction.args),
            )
        )


class _GlobalBindings(ir.Visitor):
    def __init__(self, definitions, targets):
        self.definitions, self.targets = definitions, targets

    def visit_Store(self, instruction):
        if not instruction.local and instruction.value in self.definitions:
            self.targets[instruction.name] = self.definitions[instruction.value]


def _analyze_function(function, global_targets, parameters, resolve_call) -> _Body:
    blocks = {block.label: block for block in function.blocks}
    entry = function.blocks[0].label
    successors = {
        label: block.terminator.successors() for label, block in blocks.items()
    }
    predecessors = defaultdict(set)
    consumers = defaultdict(set)
    producers = {}
    values = {}
    for block in function.blocks:
        for target in successors[block.label]:
            predecessors[target].add(block.label)
        for instruction in block.instructions:
            result = instruction.defined_result()
            if result is not None:
                producers[result] = instruction
                values[result] = _NO_FACT
        for operation in [*block.instructions, block.terminator]:
            for operand in operation.operands():
                consumers[operand].add(block.label)
    headers = _loop_headers(entry, successors)
    parameter_types = dict(zip(function.parameters, parameters, strict=True))
    initial = {
        name: _Fact(parameter_types.get(name, UNBOUND)) for name in function.locals
    }
    global_names = {
        name
        for block in function.blocks
        for instruction in block.instructions
        for name in instruction.global_names()
    }
    initial.update(
        {
            ("global", name): _Fact(UNKNOWN, global_targets.get(name))
            for name in global_names
        }
    )
    incoming, edge_states = {}, {}
    visits = defaultdict(int)
    pending, queued = deque([entry]), {entry}

    def schedule(label):
        if label not in queued:
            pending.append(label)
            queued.add(label)

    def invalidate_globals(state):
        for name in global_names:
            state["global", name] = _Fact(UNKNOWN, global_targets.get(name))

    while pending:
        label = pending.popleft()
        queued.remove(label)
        parents = [
            edge_states[parent, label]
            for parent in predecessors[label]
            if (parent, label) in edge_states
        ]
        if label == entry:
            parents.append(initial)
        if not parents:
            continue
        state = dict(parents[0])
        for parent in parents[1:]:
            state = {name: _join_fact(state[name], parent[name]) for name in state}
        if label in incoming:
            state = {
                name: _join_fact(incoming[label][name], state[name]) for name in state
            }
        if incoming.get(label) != state:
            visits[label] += 1
            if label in headers and label in incoming and visits[label] > 3:
                state = {
                    name: _widen(incoming[label][name], state[name]) for name in state
                }
            incoming[label] = dict(state)
        visitor = _InstructionAnalysis(
            state, values, {}, global_targets, invalidate_globals, resolve_call
        )
        for instruction in blocks[label].instructions:
            fact = instruction.accept(visitor)
            if fact is not None:
                result = instruction.defined_result()
                joined = _join_fact(values[result], fact)
                if values[result] != joined:
                    values[result] = joined
                    for consumer in consumers[result]:
                        schedule(consumer)
        terminator = blocks[label].terminator
        terminator.accept(_TruthEffects(values, state, invalidate_globals))
        for target in successors[label]:
            edge = dict(state)
            terminator.accept(
                _BranchConstraint(
                    target, producers, values, visitor.origins, state, edge
                )
            )
            key = label, target
            if edge_states.get(key) != edge:
                edge_states[key] = edge
                schedule(target)
    types = {result: fact.type.normalized_bottom() for result, fact in values.items()}
    targets = {
        result: fact.target
        for result, fact in values.items()
        if fact.target is not None
    }
    summary = _Summarize(values)
    for label in incoming:
        block = blocks[label]
        block.terminator.accept(summary)
        for instruction in block.instructions:
            instruction.accept(summary)
    return _Body(types, targets, summary.returned, tuple(summary.calls))


def analyze(module: ir.Module) -> Analysis:
    """Return proven value facts and speculative, guard-required call targets."""
    ir.verify(module)
    global_targets = {}
    for function in module.functions:
        definitions = {}
        for block in function.blocks:
            for instruction in block.instructions:
                definition = instruction.function_definition()
                if definition is not None:
                    result, target = definition
                    definitions[result] = target
        visitor = _GlobalBindings(definitions, global_targets)
        for block in function.blocks:
            for instruction in block.instructions:
                instruction.accept(visitor)
    return _interprocedural(module, global_targets)


def _fixnum_domain(type_: ValueType) -> ValueType | None:
    return type_.fixnum_domain()


def _with_defined_arguments(arguments, continuation):
    if not arguments:
        return continuation()
    return arguments[0].when_defined(
        lambda: _with_defined_arguments(arguments[1:], continuation)
    )


def _interprocedural(module: ir.Module, global_targets: dict[str, str]) -> Analysis:
    """Solve local-call arguments/results before publishing specialization facts.

    General entries model externally supplied values. Observed integer contexts
    and an all-fixnum context describe optional guarded entries, never changes
    to a function's public contract. Like SBCL's optimistic local-call solving,
    return summaries start empty, grow by union, and widen cyclic intervals.
    """
    functions = {function.name: function for function in module.functions}
    full = IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    parameters = {}
    for function in module.functions:
        parameters[function.name, "general"] = (UNKNOWN,) * len(function.parameters)
        if function.parameters:
            parameters[function.name, "fixnum"] = (full,) * len(function.parameters)
    summaries = {key: BOTTOM for key in parameters}
    return_changes = defaultdict(int)
    argument_changes = defaultdict(int)
    bodies = {}

    while True:
        changed = False
        previous_summaries = dict(summaries)
        previous_parameters = dict(parameters)

        def resolve(
            callee: _Fact,
            arguments: tuple[ValueType, ...],
            context_parameters=previous_parameters,
            context_summaries=previous_summaries,
        ) -> ValueType:
            return callee.type.when_defined(
                lambda: _with_defined_arguments(
                    arguments,
                    lambda: resolve_defined(
                        callee, arguments, context_parameters, context_summaries
                    ),
                )
            )

        def resolve_defined(callee, arguments, context_parameters, context_summaries):
            # A name's predicted target is never sufficient to publish a return
            # type. Only an already-loaded value with proven identity is local.
            if not callee.proven or callee.target not in functions:
                return UNKNOWN
            target = functions[callee.target]
            if len(arguments) != len(target.parameters):
                return UNKNOWN
            for context in ("observed", "fixnum", "general"):
                key = target.name, context
                expected = context_parameters.get(key)
                if expected is not None and all(
                    actual.fits(wanted)
                    for actual, wanted in zip(arguments, expected, strict=True)
                ):
                    return context_summaries[key]
            return UNKNOWN

        for key, expected in previous_parameters.items():
            body = _analyze_function(
                functions[key[0]], global_targets, expected, resolve
            )
            bodies[key] = body
            returned = _join_type(summaries[key], body.return_type)
            if returned != summaries[key]:
                return_changes[key] += 1
                if return_changes[key] > 3:
                    returned = _widen(_Fact(summaries[key]), _Fact(returned)).type
                if returned != summaries[key]:
                    summaries[key] = returned
                    changed = True

        # The broad guard context is not evidence that callers actually supplied
        # that broad interval. Propagate observations through general/observed
        # entries, including recursive calls, independently of that context.
        seeds = {}
        for key, body in bodies.items():
            if key[1] == "fixnum":
                continue
            for call in body.calls:
                target = functions.get(call.target)
                if (
                    target is None
                    or not target.parameters
                    or len(call.arguments) != len(target.parameters)
                ):
                    continue
                domains = tuple(_fixnum_domain(type_) for type_ in call.arguments)
                if any(domain is None for domain in domains):
                    continue
                if target.name not in seeds:
                    seeds[target.name] = domains
                else:
                    seeds[target.name] = tuple(
                        _join_type(old, new)
                        for old, new in zip(seeds[target.name], domains, strict=True)
                    )
        contexts_changed = False
        for name, seed in seeds.items():
            key = name, "observed"
            old = parameters.get(key)
            if old is not None:
                seed = tuple(_join_type(a, b) for a, b in zip(old, seed, strict=True))
                if old != seed:
                    argument_changes[key] += 1
                    if argument_changes[key] > 3:
                        seed = tuple(
                            _fixnum_domain(_widen(_Fact(a), _Fact(b)).type)
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
            summaries = {key: BOTTOM for key in parameters}
            return_changes.clear()
        if not changed:
            break

    types, targets, variants, returns = {}, {}, {}, {}
    for function in module.functions:
        key = function.name, "general"
        types[function.name] = bodies[key].types
        targets[function.name] = bodies[key].targets
        returns[function.name] = summaries[key].normalized_bottom()
        entries = []
        for identifier in ("observed", "fixnum"):
            key = function.name, identifier
            if key not in parameters:
                continue
            if (
                identifier == "observed"
                and parameters[key] == parameters[function.name, "fixnum"]
            ):
                continue
            body = bodies[key]
            returned = summaries[key].normalized_bottom()
            entries.append(
                Context(identifier, parameters[key], body.types, body.targets, returned)
            )
        variants[function.name] = tuple(entries)
    return Analysis(types, targets, variants, returns)
