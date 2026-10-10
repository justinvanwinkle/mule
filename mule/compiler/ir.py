"""Language-level control flow for the initial native compiler.

Control edges and single-consumer expression results are separate. Variables
remain explicit; converting this representation to LLVM is a backend task.
"""

from dataclasses import dataclass, field

from mule import ast


@dataclass(frozen=True, slots=True)
class Constant:
    result: str
    value: str | bytes | int | float | bool | None


@dataclass(frozen=True, slots=True)
class Load:
    result: str
    name: str
    local: bool
    clear: bool = False


@dataclass(frozen=True, slots=True)
class Store:
    name: str
    value: str
    local: bool


@dataclass(frozen=True, slots=True)
class Binary:
    result: str
    operator: ast.BinaryOperator
    left: str
    right: str
    inplace: bool = False


@dataclass(frozen=True, slots=True)
class Unary:
    result: str
    operator: ast.UnaryOperator
    operand: str


@dataclass(frozen=True, slots=True)
class Compare:
    result: str
    operator: ast.ComparisonOperator
    left: str
    right: str


@dataclass(frozen=True, slots=True)
class Call:
    result: str
    callee: str
    args: tuple[str, ...]


@dataclass(frozen=True, slots=True)
class DefineFunction:
    result: str
    function: str
    public_name: str


@dataclass(frozen=True, slots=True)
class Discard:
    value: str


type Instruction = (
    Constant | Load | Store | Binary | Unary | Compare | Call | DefineFunction | Discard
)


@dataclass(frozen=True, slots=True)
class Jump:
    target: str


@dataclass(frozen=True, slots=True)
class Branch:
    condition: str
    true: str
    false: str


@dataclass(frozen=True, slots=True)
class Return:
    value: str


type Terminator = Jump | Branch | Return


@dataclass(slots=True)
class Block:
    label: str
    instructions: list[Instruction] = field(default_factory=list)
    terminator: Terminator | None = None


@dataclass(slots=True)
class Function:
    name: str
    parameters: tuple[str, ...]
    locals: tuple[str, ...]
    blocks: list[Block] = field(default_factory=list)


@dataclass(slots=True)
class Module:
    functions: list[Function]
    entry: str


def operands(operation: Instruction | Terminator) -> tuple[str, ...]:
    match operation:
        case Store(value=value) | Return(value=value) | Discard(value=value):
            return (value,)
        case Unary(operand=value) | Branch(condition=value):
            return (value,)
        case Binary(left=left, right=right) | Compare(left=left, right=right):
            return (left, right)
        case Call(callee=callee, args=args):
            return (callee, *args)
        case _:
            return ()


def verify(module: Module) -> None:
    """Reject malformed graphs before emission, including value ownership errors."""
    functions = {function.name for function in module.functions}
    if len(functions) != len(module.functions) or module.entry not in functions:
        raise ValueError("duplicate function or missing module entry")
    for function in module.functions:
        _verify_function(function, functions)


def _verify_function(function: Function, functions: set[str]) -> None:
    if not function.blocks:
        raise ValueError(f"{function.name}: missing entry block")
    blocks = {block.label: block for block in function.blocks}
    if len(blocks) != len(function.blocks):
        raise ValueError(f"{function.name}: duplicate block")
    if len(set(function.locals)) != len(function.locals) or not set(
        function.parameters
    ) <= set(function.locals):
        raise ValueError("invalid parameter/local declarations")
    definitions = {}
    successors = {}
    predecessors = {label: set() for label in blocks}
    for block in function.blocks:
        if block.terminator is None:
            raise ValueError(f"{function.name}/{block.label}: unterminated block")
        targets = ()
        if isinstance(block.terminator, Jump):
            targets = (block.terminator.target,)
        elif isinstance(block.terminator, Branch):
            targets = (block.terminator.true, block.terminator.false)
        if not set(targets) <= blocks.keys():
            raise ValueError("control edge to missing block")
        successors[block.label] = targets
        for target in targets:
            predecessors[target].add(block.label)
        for index, operation in enumerate(block.instructions):
            if (
                isinstance(operation, DefineFunction)
                and operation.function not in functions
            ):
                raise ValueError("reference to missing function")
            if (
                isinstance(operation, (Load, Store))
                and operation.local
                and operation.name not in function.locals
            ):
                raise ValueError(f"undeclared local {operation.name}")
            result = getattr(operation, "result", None)
            if result is not None:
                if result in definitions:
                    raise ValueError(f"multiply defined value {result}")
                definitions[result] = (block.label, index)

    entry = function.blocks[0].label
    reachable = set()
    pending = [entry]
    while pending:
        label = pending.pop()
        if label not in reachable:
            reachable.add(label)
            pending.extend(successors[label])
    dominators = {
        label: ({entry} if label == entry else set(reachable)) for label in reachable
    }
    changed = True
    while changed:
        changed = False
        for label in reachable - {entry}:
            parents = predecessors[label] & reachable
            incoming = set.intersection(*(dominators[parent] for parent in parents))
            new = {label} | incoming
            if new != dominators[label]:
                dominators[label] = new
                changed = True

    consumed = set()
    for block in function.blocks:
        for index, operation in enumerate([*block.instructions, block.terminator]):
            for value in operands(operation):
                if value not in definitions or value in consumed:
                    raise ValueError(f"undefined or multiply consumed value {value}")
                producer, position = definitions[value]
                if producer == block.label:
                    if position >= index:
                        raise ValueError(f"value {value} used before definition")
                elif (
                    block.label in reachable and producer not in dominators[block.label]
                ):
                    raise ValueError(f"value {value} does not dominate its use")
                consumed.add(value)
    if consumed != definitions.keys():
        raise ValueError(
            f"unconsumed results in {function.name}: {definitions.keys() - consumed}"
        )
