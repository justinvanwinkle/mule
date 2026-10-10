"""Conservative IR facts for representation selection and guarded calls.

As in SBCL's IR1 propagation, facts are joined until control-flow edges settle;
growing loop intervals are widened before publishing the result. Callable
predictions are separate from proven types: Python globals remain mutable, so a
predicted native entry always needs a runtime identity guard in LLVM lowering.
"""

from collections import defaultdict, deque
from dataclasses import dataclass, field, replace

from mule import ast
from mule.compiler import ir

FIXNUM_MIN = -(2**62)
FIXNUM_MAX = 2**62 - 1


@dataclass(frozen=True, slots=True)
class ValueType:
    kind: str = "unknown"
    minimum: int | None = None
    maximum: int | None = None
    may_be_unbound: bool = False

    @property
    def is_fixnum(self) -> bool:
        return (
            self.kind == "integer"
            and not self.may_be_unbound
            and self.minimum is not None
            and self.maximum is not None
            and FIXNUM_MIN <= self.minimum <= self.maximum <= FIXNUM_MAX
        )

    @property
    def is_immortal(self) -> bool:
        return self.kind in {"bool", "none", "immortal"} and not self.may_be_unbound


UNKNOWN = ValueType()
INTEGER = ValueType("integer")
BOOLEAN = ValueType("bool", 0, 1)
FLOAT = ValueType("float")
_BOTTOM = ValueType("bottom")
_UNBOUND = ValueType("unbound", may_be_unbound=True)


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


_NO_FACT = _Fact(_BOTTOM)


def _join_type(left: ValueType, right: ValueType) -> ValueType:
    if left.kind == "bottom":
        return right
    if right.kind == "bottom":
        return left
    if left.kind == "unbound":
        return replace(right, may_be_unbound=True)
    if right.kind == "unbound":
        return replace(left, may_be_unbound=True)
    unbound = left.may_be_unbound or right.may_be_unbound
    if left.kind != right.kind:
        if left.kind in {"none", "bool", "immortal"} and right.kind in {
            "none",
            "bool",
            "immortal",
        }:
            return ValueType("immortal", may_be_unbound=unbound)
        return replace(UNKNOWN, may_be_unbound=unbound)
    minimum = (
        min(left.minimum, right.minimum)
        if left.minimum is not None and right.minimum is not None
        else None
    )
    maximum = (
        max(left.maximum, right.maximum)
        if left.maximum is not None and right.maximum is not None
        else None
    )
    return ValueType(left.kind, minimum, maximum, unbound)


def _join_fact(left: _Fact, right: _Fact) -> _Fact:
    if left.type.kind == "bottom":
        return right
    if right.type.kind == "bottom":
        return left
    target = left.target if left.target == right.target else None
    return _Fact(
        _join_type(left.type, right.type),
        target,
        target is not None and left.proven and right.proven,
    )


def _widen(old: _Fact, new: _Fact) -> _Fact:
    type_ = new.type
    if old.type.kind == type_.kind == "integer":
        minimum = type_.minimum
        maximum = type_.maximum
        if old.type.minimum is None or (
            minimum is not None and minimum < old.type.minimum
        ):
            minimum = None
        if old.type.maximum is None or (
            maximum is not None and maximum > old.type.maximum
        ):
            maximum = None
        type_ = replace(type_, minimum=minimum, maximum=maximum)
    return _Fact(type_, new.target, new.proven)


def _constant(value) -> ValueType:
    if isinstance(value, bool):
        return ValueType("bool", int(value), int(value))
    if value is None:
        return ValueType("none")
    if isinstance(value, int):
        return ValueType("integer", value, value)
    if isinstance(value, float):
        return FLOAT
    if isinstance(value, str):
        return ValueType("text")
    if isinstance(value, bytes):
        return ValueType("bytes")
    return UNKNOWN


def _integer_bounds(type_: ValueType) -> tuple[int | None, int | None] | None:
    if type_.kind in {"integer", "bool"} and not type_.may_be_unbound:
        return type_.minimum, type_.maximum
    return None


def _binary(operator, left: ValueType, right: ValueType) -> ValueType:
    if left.kind == "bottom" or right.kind == "bottom":
        return _BOTTOM
    a, b = _integer_bounds(left), _integer_bounds(right)
    numeric = {"integer", "bool", "float"}
    if left.may_be_unbound or right.may_be_unbound:
        return UNKNOWN
    if left.kind not in numeric or right.kind not in numeric:
        return UNKNOWN
    if left.kind == "float" or right.kind == "float":
        if operator in {
            ast.BinaryOperator.ADD,
            ast.BinaryOperator.SUBTRACT,
            ast.BinaryOperator.MULTIPLY,
            ast.BinaryOperator.DIVIDE,
            ast.BinaryOperator.FLOOR_DIVIDE,
            ast.BinaryOperator.MODULO,
            ast.BinaryOperator.POWER,
        }:
            return FLOAT
        return UNKNOWN
    if operator == ast.BinaryOperator.DIVIDE:
        return FLOAT
    if operator == ast.BinaryOperator.MATRIX_MULTIPLY:
        return UNKNOWN
    assert a is not None and b is not None
    lo, hi = a
    rlo, rhi = b
    if operator == ast.BinaryOperator.POWER:
        # Integer powers produce floats for negative exponents. A widened
        # exponent interval must not silently turn that union into an integer.
        if rhi is not None and rhi < 0:
            return FLOAT
        if rlo is None or rlo < 0:
            return UNKNOWN
    if operator == ast.BinaryOperator.ADD:
        return ValueType(
            "integer",
            lo + rlo if lo is not None and rlo is not None else None,
            hi + rhi if hi is not None and rhi is not None else None,
        )
    if operator == ast.BinaryOperator.SUBTRACT:
        return ValueType(
            "integer",
            lo - rhi if lo is not None and rhi is not None else None,
            hi - rlo if hi is not None and rlo is not None else None,
        )
    boolean_bits = left.kind == right.kind == "bool" and operator in {
        ast.BinaryOperator.BIT_AND,
        ast.BinaryOperator.BIT_OR,
        ast.BinaryOperator.BIT_XOR,
    }
    if None in (lo, hi, rlo, rhi):
        if operator == ast.BinaryOperator.BIT_AND:
            for bound in (a, b):
                if bound[0] is not None and bound[0] >= 0 and bound[1] is not None:
                    return ValueType("integer", 0, bound[1])
        return INTEGER
    minimum = maximum = None
    match operator:
        case ast.BinaryOperator.MULTIPLY:
            values = [lo * rlo, lo * rhi, hi * rlo, hi * rhi]
            minimum, maximum = min(values), max(values)
        case (
            ast.BinaryOperator.BIT_AND
            | ast.BinaryOperator.BIT_OR
            | ast.BinaryOperator.BIT_XOR
        ):
            if lo == hi and rlo == rhi:
                if operator == ast.BinaryOperator.BIT_AND:
                    minimum = maximum = lo & rlo
                elif operator == ast.BinaryOperator.BIT_OR:
                    minimum = maximum = lo | rlo
                else:
                    minimum = maximum = lo ^ rlo
            elif operator == ast.BinaryOperator.BIT_AND and (lo >= 0 or rlo >= 0):
                minimum = 0
                maximum = (
                    min(hi, rhi) if lo >= 0 and rlo >= 0 else hi if lo >= 0 else rhi
                )
            elif lo >= 0 and rlo >= 0:
                minimum, maximum = 0, (1 << max(hi.bit_length(), rhi.bit_length())) - 1
            elif left.is_fixnum and right.is_fixnum:
                minimum, maximum = FIXNUM_MIN, FIXNUM_MAX
        case ast.BinaryOperator.LEFT_SHIFT | ast.BinaryOperator.RIGHT_SHIFT:
            if 0 <= rlo <= rhi <= 256:
                if operator == ast.BinaryOperator.LEFT_SHIFT:
                    values = [lo << rlo, lo << rhi, hi << rlo, hi << rhi]
                else:
                    values = [lo >> rlo, lo >> rhi, hi >> rlo, hi >> rhi]
                minimum, maximum = min(values), max(values)
        case ast.BinaryOperator.FLOOR_DIVIDE | ast.BinaryOperator.MODULO:
            if lo == hi and rlo == rhi and rlo != 0:
                minimum = maximum = (
                    lo // rlo
                    if operator == ast.BinaryOperator.FLOOR_DIVIDE
                    else lo % rlo
                )
        case ast.BinaryOperator.POWER:
            if rhi <= 32 and max(abs(lo), abs(hi)).bit_length() <= 128:
                bases = {lo, hi} | {value for value in (-1, 0, 1) if lo <= value <= hi}
                values = [
                    base**exponent for base in bases for exponent in range(rlo, rhi + 1)
                ]
                minimum, maximum = min(values), max(values)
    return ValueType("bool" if boolean_bits else "integer", minimum, maximum)


def _unary(operator, operand: ValueType) -> ValueType:
    if operator == ast.UnaryOperator.NOT:
        return BOOLEAN
    if operand.kind == "bottom":
        return _BOTTOM
    if operand.kind == "float" and operator != ast.UnaryOperator.INVERT:
        return FLOAT
    bounds = _integer_bounds(operand)
    if bounds is None:
        return UNKNOWN
    minimum, maximum = bounds
    if operator == ast.UnaryOperator.POSITIVE:
        return ValueType("integer", minimum, maximum)
    adjustment = 1 if operator == ast.UnaryOperator.INVERT else 0
    return ValueType(
        "integer",
        -maximum - adjustment if maximum is not None else None,
        -minimum - adjustment if minimum is not None else None,
    )


def _successors(block: ir.Block) -> tuple[str, ...]:
    match block.terminator:
        case ir.Jump(target=target):
            return (target,)
        case ir.Branch(true=yes, false=no):
            return (yes, no)
        case _:
            return ()


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


def _narrow(type_: ValueType, operator, bound: int, truth: bool) -> ValueType:
    if type_.kind != "integer" or type_.may_be_unbound:
        return type_
    if not truth:
        operator = {
            ast.ComparisonOperator.LESS: ast.ComparisonOperator.GREATER_EQUAL,
            ast.ComparisonOperator.LESS_EQUAL: ast.ComparisonOperator.GREATER,
            ast.ComparisonOperator.GREATER: ast.ComparisonOperator.LESS_EQUAL,
            ast.ComparisonOperator.GREATER_EQUAL: ast.ComparisonOperator.LESS,
            ast.ComparisonOperator.EQUAL: ast.ComparisonOperator.NOT_EQUAL,
            ast.ComparisonOperator.NOT_EQUAL: ast.ComparisonOperator.EQUAL,
        }.get(operator, operator)
    minimum, maximum = type_.minimum, type_.maximum
    if operator in {ast.ComparisonOperator.LESS, ast.ComparisonOperator.LESS_EQUAL}:
        upper = bound - (operator == ast.ComparisonOperator.LESS)
        maximum = min(maximum, upper) if maximum is not None else upper
    elif operator in {
        ast.ComparisonOperator.GREATER,
        ast.ComparisonOperator.GREATER_EQUAL,
    }:
        lower = bound + (operator == ast.ComparisonOperator.GREATER)
        minimum = max(minimum, lower) if minimum is not None else lower
    elif operator == ast.ComparisonOperator.EQUAL:
        minimum = max(minimum, bound) if minimum is not None else bound
        maximum = min(maximum, bound) if maximum is not None else bound
    # Contradictions are kept conservative rather than declaring an edge dead.
    if minimum is not None and maximum is not None and minimum > maximum:
        return type_
    return replace(type_, minimum=minimum, maximum=maximum)


def _analyze_function(
    function: ir.Function,
    global_targets: dict[str, str],
    parameters: tuple[ValueType, ...],
    resolve_call,
) -> _Body:
    blocks = {block.label: block for block in function.blocks}
    entry = function.blocks[0].label
    successors = {label: _successors(block) for label, block in blocks.items()}
    predecessors = defaultdict(set)
    consumers = defaultdict(set)
    producers = {}
    values = {}
    for block in function.blocks:
        for target in successors[block.label]:
            predecessors[target].add(block.label)
        for instruction in block.instructions:
            result = getattr(instruction, "result", None)
            if result is not None:
                producers[result] = instruction
                values[result] = _NO_FACT
        for operation in [*block.instructions, block.terminator]:
            for operand in ir.operands(operation):
                consumers[operand].add(block.label)
    headers = _loop_headers(entry, successors)
    parameter_types = dict(zip(function.parameters, parameters, strict=True))
    initial = {
        name: _Fact(parameter_types.get(name, _UNBOUND)) for name in function.locals
    }
    global_names = {
        instruction.name
        for block in function.blocks
        for instruction in block.instructions
        if isinstance(instruction, (ir.Load, ir.Store)) and not instruction.local
    }
    initial.update(
        {
            ("global", name): _Fact(UNKNOWN, global_targets.get(name))
            for name in global_names
        }
    )
    incoming = {}
    edge_states = {}
    visits = defaultdict(int)
    pending = deque([entry])
    queued = {entry}

    def schedule(label):
        if label not in queued:
            pending.append(label)
            queued.add(label)

    def invalidate_globals(state):
        for name in global_names:
            state["global", name] = _Fact(UNKNOWN, global_targets.get(name))

    def may_run_user_code(*operands):
        primitive = {"integer", "bool", "float", "none", "text", "bytes", "immortal"}
        return any(operand.kind not in primitive for operand in operands)

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
        origins = {}
        for instruction in blocks[label].instructions:
            fact = None
            match instruction:
                case ir.Constant(value=value):
                    fact = _Fact(_constant(value))
                case ir.DefineFunction(function=target):
                    fact = _Fact(ValueType("function"), target, True)
                case ir.Load(name=name, local=local, clear=clear):
                    fact = state[name if local else ("global", name)]
                    if not local and fact.target is None:
                        fact = _Fact(fact.type, global_targets.get(name))
                    if local:
                        if clear:
                            origins = {
                                result: origin
                                for result, origin in origins.items()
                                if origin != name
                            }
                            state[name] = _Fact(_UNBOUND)
                        else:
                            origins[instruction.result] = name
                case ir.Store(name=name, value=value, local=True):
                    state[name] = values[value]
                    origins = {
                        result: origin
                        for result, origin in origins.items()
                        if origin != name
                    }
                case ir.Store(name=name, value=value, local=False):
                    state["global", name] = values[value]
                case ir.Binary(operator=operator, left=left, right=right):
                    fact = _Fact(
                        _binary(operator, values[left].type, values[right].type)
                    )
                    if may_run_user_code(values[left].type, values[right].type):
                        invalidate_globals(state)
                case ir.Unary(operator=operator, operand=operand):
                    fact = _Fact(_unary(operator, values[operand].type))
                    if may_run_user_code(values[operand].type):
                        invalidate_globals(state)
                case ir.Compare(operator=operator, left=left, right=right):
                    primitive = {"integer", "bool", "float", "none", "text", "bytes"}
                    known = (
                        values[left].type.kind in primitive
                        and values[right].type.kind in primitive
                    )
                    always_bool = operator in {
                        ast.ComparisonOperator.IS,
                        ast.ComparisonOperator.IS_NOT,
                        ast.ComparisonOperator.IN,
                        ast.ComparisonOperator.NOT_IN,
                    }
                    fact = _Fact(BOOLEAN if known or always_bool else UNKNOWN)
                    if operator not in {
                        ast.ComparisonOperator.IS,
                        ast.ComparisonOperator.IS_NOT,
                    } and may_run_user_code(values[left].type, values[right].type):
                        invalidate_globals(state)
                case ir.Call(callee=callee, args=args):
                    fact = _Fact(
                        resolve_call(
                            values[callee], tuple(values[arg].type for arg in args)
                        )
                    )
                    # Any call can rebind globals. A callee loaded before an
                    # argument's call remains a separate, already-owned value.
                    invalidate_globals(state)
            if fact is not None:
                result = instruction.result
                joined = _join_fact(values[result], fact)
                if values[result] != joined:
                    values[result] = joined
                    for consumer in consumers[result]:
                        schedule(consumer)
        terminator = blocks[label].terminator
        if isinstance(terminator, ir.Branch) and may_run_user_code(
            values[terminator.condition].type
        ):
            invalidate_globals(state)
        for target in successors[label]:
            edge = dict(state)
            if (
                isinstance(terminator, ir.Branch)
                and terminator.true != terminator.false
            ):
                comparison = producers.get(terminator.condition)
                if isinstance(comparison, ir.Compare):
                    operator = comparison.operator
                    local = origins.get(comparison.left)
                    bound = values[comparison.right].type
                    if local is None:
                        local = origins.get(comparison.right)
                        bound = values[comparison.left].type
                        operator = {
                            ast.ComparisonOperator.LESS: ast.ComparisonOperator.GREATER,
                            ast.ComparisonOperator.LESS_EQUAL: ast.ComparisonOperator.GREATER_EQUAL,
                            ast.ComparisonOperator.GREATER: ast.ComparisonOperator.LESS,
                            ast.ComparisonOperator.GREATER_EQUAL: ast.ComparisonOperator.LESS_EQUAL,
                        }.get(operator, operator)
                    if (
                        local is not None
                        and bound.kind == "integer"
                        and bound.minimum is not None
                        and bound.minimum == bound.maximum
                    ):
                        type_ = _narrow(
                            state[local].type,
                            operator,
                            bound.minimum,
                            target == terminator.true,
                        )
                        edge[local] = _Fact(
                            type_, state[local].target, state[local].proven
                        )
            key = label, target
            if edge_states.get(key) != edge:
                edge_states[key] = edge
                schedule(target)
    types = {
        result: fact.type if fact.type.kind != "bottom" else UNKNOWN
        for result, fact in values.items()
    }
    targets = {
        result: fact.target
        for result, fact in values.items()
        if fact.target is not None
    }
    returned = _BOTTOM
    calls = []
    for label in incoming:
        block = blocks[label]
        if isinstance(block.terminator, ir.Return):
            returned = _join_type(returned, values[block.terminator.value].type)
        for instruction in block.instructions:
            if isinstance(instruction, ir.Call):
                calls.append(
                    _CallSite(
                        values[instruction.callee].target,
                        tuple(values[arg].type for arg in instruction.args),
                    )
                )
    return _Body(types, targets, returned, tuple(calls))


def analyze(module: ir.Module) -> Analysis:
    """Return proven value facts and speculative, guard-required call targets."""
    ir.verify(module)
    global_targets = {}
    for function in module.functions:
        definitions = {
            instruction.result: instruction.function
            for block in function.blocks
            for instruction in block.instructions
            if isinstance(instruction, ir.DefineFunction)
        }
        for block in function.blocks:
            for instruction in block.instructions:
                if (
                    isinstance(instruction, ir.Store)
                    and not instruction.local
                    and instruction.value in definitions
                ):
                    global_targets[instruction.name] = definitions[instruction.value]
    return _interprocedural(module, global_targets)


def _fits(actual: ValueType, expected: ValueType) -> bool:
    if actual.kind == "bottom" or actual.may_be_unbound:
        return False
    if expected.kind == "unknown":
        return True
    if actual.kind != expected.kind:
        return False
    return (
        expected.minimum is None
        or actual.minimum is not None
        and actual.minimum >= expected.minimum
    ) and (
        expected.maximum is None
        or actual.maximum is not None
        and actual.maximum <= expected.maximum
    )


def _fixnum_domain(type_: ValueType) -> ValueType | None:
    """Limit an observed integer interval to the domain an entry can guard."""
    if type_.kind != "integer" or type_.may_be_unbound:
        return None
    minimum = (
        max(FIXNUM_MIN, type_.minimum) if type_.minimum is not None else FIXNUM_MIN
    )
    maximum = (
        min(FIXNUM_MAX, type_.maximum) if type_.maximum is not None else FIXNUM_MAX
    )
    return ValueType("integer", minimum, maximum) if minimum <= maximum else None


def _interprocedural(module: ir.Module, global_targets: dict[str, str]) -> Analysis:
    """Solve local-call arguments/results before publishing specialization facts.

    General entries model externally supplied values. Observed integer contexts
    and an all-fixnum context describe optional guarded entries, never changes
    to a function's public contract. Like SBCL's optimistic local-call solving,
    return summaries start empty, grow by union, and widen cyclic intervals.
    """
    functions = {function.name: function for function in module.functions}
    full = ValueType("integer", FIXNUM_MIN, FIXNUM_MAX)
    parameters = {}
    for function in module.functions:
        parameters[function.name, "general"] = (UNKNOWN,) * len(function.parameters)
        if function.parameters:
            parameters[function.name, "fixnum"] = (full,) * len(function.parameters)
    summaries = {key: _BOTTOM for key in parameters}
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
            if callee.type.kind == "bottom" or any(
                arg.kind == "bottom" for arg in arguments
            ):
                return _BOTTOM
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
                    _fits(actual, wanted)
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
                summaries.setdefault(key, _BOTTOM)
                changed = True
                contexts_changed = True
        if contexts_changed:
            # A new, narrower callee context can replace a broad summary used
            # provisionally by callers. Solve returns afresh for the current
            # context graph rather than permanently retaining that broad type.
            summaries = {key: _BOTTOM for key in parameters}
            return_changes.clear()
        if not changed:
            break

    types, targets, variants, returns = {}, {}, {}, {}
    for function in module.functions:
        key = function.name, "general"
        types[function.name] = bodies[key].types
        targets[function.name] = bodies[key].targets
        returns[function.name] = (
            summaries[key] if summaries[key].kind != "bottom" else UNKNOWN
        )
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
            returned = summaries[key] if summaries[key].kind != "bottom" else UNKNOWN
            entries.append(
                Context(identifier, parameters[key], body.types, body.targets, returned)
            )
        variants[function.name] = tuple(entries)
    return Analysis(types, targets, variants, returns)
