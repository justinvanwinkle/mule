"""Conservative IR facts for representation selection and guarded calls.

As in SBCL's IR1 propagation, facts are joined until control-flow edges settle;
growing loop intervals are widened before publishing the result. Callable
predictions are separate from proven types: Python globals remain mutable, so a
predicted native entry always needs a runtime identity guard in LLVM lowering.
"""

from collections import defaultdict, deque
from dataclasses import dataclass, replace

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


@dataclass(frozen=True, slots=True)
class _Fact:
    type: ValueType = UNKNOWN
    target: str | None = None


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
    return _Fact(_join_type(left.type, right.type), target)


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
    return _Fact(type_, new.target)


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
        case ast.BinaryOperator.ADD:
            minimum, maximum = lo + rlo, hi + rhi
        case ast.BinaryOperator.SUBTRACT:
            minimum, maximum = lo - rhi, hi - rlo
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


def _analyze_function(function: ir.Function, global_targets: dict[str, str]):
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
    initial = {
        name: _Fact(UNKNOWN if name in function.parameters else _UNBOUND)
        for name in function.locals
    }
    incoming = {}
    edge_states = {}
    visits = defaultdict(int)
    pending = deque([entry])
    queued = {entry}

    def schedule(label):
        if label not in queued:
            pending.append(label)
            queued.add(label)

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
                    fact = _Fact(ValueType("function"), target)
                case ir.Load(name=name, local=local, clear=clear):
                    fact = (
                        state[name]
                        if local
                        else _Fact(UNKNOWN, global_targets.get(name))
                    )
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
                case ir.Binary(operator=operator, left=left, right=right):
                    fact = _Fact(
                        _binary(operator, values[left].type, values[right].type)
                    )
                case ir.Unary(operator=operator, operand=operand):
                    fact = _Fact(_unary(operator, values[operand].type))
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
                case ir.Call():
                    fact = _Fact(UNKNOWN)
            if fact is not None:
                result = instruction.result
                joined = _join_fact(values[result], fact)
                if values[result] != joined:
                    values[result] = joined
                    for consumer in consumers[result]:
                        schedule(consumer)
        for target in successors[label]:
            edge = dict(state)
            terminator = blocks[label].terminator
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
                        edge[local] = _Fact(type_, state[local].target)
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
    return types, targets


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
    types, targets = {}, {}
    for function in module.functions:
        types[function.name], targets[function.name] = _analyze_function(
            function, global_targets
        )
    return Analysis(types, targets)
